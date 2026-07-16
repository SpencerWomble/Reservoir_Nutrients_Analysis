# =====================================================================
# Distribution + summary EDA, parameterized by analyte
# Run the whole script to produce plots for every analyte in
# `analyte_config`. Output goes to  output/<ANALYTE>/  as .png files.
#
# To run a single analyte interactively, see the bottom of the file.
# =====================================================================


library(tidyverse)
library(here)

# ---------------------------------------------------------------------
# 1. Analyte-independent setup (defined once)
# ---------------------------------------------------------------------

season_levels <- c("winter", "spring", "summer", "fall")

assign_season <- function(month) {
  dplyr::case_when(
    month %in% c(12, 1, 2)  ~ "winter",
    month %in% c(3, 4, 5)   ~ "spring",
    month %in% c(6, 7, 8)   ~ "summer",
    month %in% c(9, 10, 11) ~ "fall"
  )
}

ggtheme <- theme_bw() +
  theme(text = element_text(size = 14),
        panel.grid.minor = element_blank())

site_labs <- c("center_hill"  = "Center Hill",
               "dale_hollow"  = "Dale Hollow",
               "percy_priest" = "Percy Priest")

# ---------------------------------------------------------------------
# 2. Per-analyte config: the single source of truth.
#    Add or edit an entry here to change what gets produced.
#    - characteristic: must match the `characteristic` column EXACTLY
#    - value_cap:      upper filter on measure_value (analyte-specific!)
#    - axis_label:     expression used for x axes
# ---------------------------------------------------------------------

analyte_config <- list(
  TP = list(
    characteristic = "Phosphorus",
    value_cap      = 5,
    axis_label     = expression(TP ~ mg ~ L^{-1})
  ),
  NOx = list(
    characteristic = "Inorganic nitrogen (nitrate and nitrite)", # <-- CONFIRM this matches your data
    value_cap      = 15,                          # <-- SET an appropriate NOx cap
    axis_label     = expression(NO[x] ~ mg ~ L^{-1})
  )
)

# ---------------------------------------------------------------------
# 3. Helpers
# ---------------------------------------------------------------------

prep_data <- function(raw, cfg) {
  raw |>
    dplyr::filter(characteristic == cfg$characteristic,
                  measure_value < cfg$value_cap) |>
    dplyr::mutate(
      date_time = as.POSIXct(date_time),
      date      = lubridate::as_date(date_time),
      year      = lubridate::year(date_time),
      doy       = lubridate::yday(date_time),
      season    = factor(assign_season(month), levels = season_levels),
      below_mdl = dplyr::if_else(measure_value < mdl, 1, 0)
    ) |> 
    dplyr::filter(year >1980)
}

# Density of measure_value, optionally on the log scale.
plot_density <- function(data, log_scale = FALSE, facet_var = NULL) {
  d <- data |>
    dplyr::mutate(.x = if (log_scale) log(measure_value) else measure_value)

  p <- ggplot(d, aes(.x)) +
    geom_density(fill = "grey85", color = "grey30") +
    theme_bw()

  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(.data[[facet_var]]),
                        labeller = as_labeller(site_labs))
  }
  p
}

# ---------------------------------------------------------------------
# 4. The EDA itself: build every plot, save it, return the list.
# ---------------------------------------------------------------------

run_eda <- function(key, cfg, raw, out_root = here::here("Figures")) {
  message("Processing ", key, " ...")

  data    <- prep_data(raw, cfg)
  out_dir <- file.path(out_root, key)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # local saver so every plot lands in this analyte's folder
  save_plot <- function(p, name, width = 8, height = 6) {
    ggsave(file.path(out_dir, paste0(name, ".png")),
           plot = p, width = width, height = height, dpi = 300)
  }

  ## --- season counts ---
  season_count <- dplyr::count(data, season, site) |> 
    dplyr::mutate(season = stringr::str_to_title(season))
 
  p_season <- ggplot(season_count, aes(season, n)) +
    geom_col() +
    facet_wrap(~ site, labeller = as_labeller(site_labs)) +
    labs(x = "Season", y = "Count") +
    ggtheme
  save_plot(p_season, "01_season_counts")

  ## --- year x season x site coverage ---
  coverage <- data |>
    dplyr::count(site, year, season, name = "Count") |>
    tidyr::complete(site, year, season, fill = list(Count = 0))

  p_counts <- ggplot(coverage, aes(year, season, fill = Count)) +
    geom_tile(color = "grey90") +
    geom_text(aes(label = Count), size = 2.7) +
    facet_wrap(~ site, ncol = 1, labeller = as_labeller(site_labs)) +
    scale_fill_viridis_c(option = "mako", direction = -1) +
    labs(title = "Sample count by year by season",
         x = NULL, y = NULL, fill = "n") +
    ggtheme
  save_plot(p_counts, "02_coverage_counts", height = 8)

  p_comp <- coverage |>
    dplyr::group_by(site, year) |>
    dplyr::mutate(prop = Count / sum(Count)) |>
    dplyr::ungroup() |>
    ggplot(aes(year, prop, fill = season)) +
    geom_col(width = 0.9) +
    facet_wrap(~ site, ncol = 1, labeller = as_labeller(site_labs)) +
    scale_fill_viridis_d(option = "mako", end = 0.9) +
    labs(title = "Seasonal composition of sampling, within year",
         x = NULL, y = "Proportion of samples", fill = "Season") +
    ggtheme
  save_plot(p_comp, "03_seasonal_composition", height = 8)

  ## --- censoring summary (written to csv) ---
  cens_summary <- data |>
    dplyr::group_by(site, season) |>
    dplyr::summarise(
      n        = dplyr::n(),
      n_cens   = sum(below_mdl, na.rm = TRUE),
      pct_cens = round(100 * n_cens / n, 1),
      .groups  = "drop"
    ) |>
    dplyr::arrange(site, season)
  readr::write_csv(cens_summary, file.path(out_dir, "censoring_summary.csv"))

  ## --- MDL plots: only if this analyte actually has MDL info ---
 # if (any(!is.na(data$mdl))) {
  #  cens_by_year <- data |>
   #   dplyr::group_by(site, year) |>
    #  dplyr::summarise(pct_cens = round(100 * mean(below_mdl, na.rm = TRUE), 1),
     #                  n = dplyr::n(), .groups = "drop")

    # p_cens <- ggplot(cens_by_year, aes(year, pct_cens, color = site)) +
     # geom_line() + geom_point() +
     # labs(title = "Percent below MDL by year", x = NULL, y = "% censored") +
     # theme_minimal(base_size = 11)
    # save_plot(p_cens, "04_pct_censored_by_year")

    p_mdl <- ggplot(data, aes(date, mdl, color = site)) +
      geom_point(alpha = 0.5, size = 1) +
      labs(title = "Detection limit over time", 
          x = NULL, 
          y = "MDL") +
      ggtheme +
      facet_wrap(~site, labeller = as_labeller(site_labs))
    save_plot(p_mdl, "05_mdl_over_time")
 # } else {
  #  message("  No MDL data for ", key, " - skipping censoring plots.")
  #}

  ## --- histogram with detected values marked ---
  p_hist <- ggplot(data, aes(measure_value)) +
    geom_histogram(color = "white") +
    labs(x = cfg$axis_label, y = "Count") +
    ggtheme
  save_plot(p_hist, "06_histogram")

  ## --- density plots ---
  p_dens <- plot_density(data, log_scale = FALSE) +
    labs(x = cfg$axis_label, y = "Density") + ggtheme
  save_plot(p_dens, "07_density_raw")

  p_dens_log <- plot_density(data, log_scale = TRUE) +
    labs(x = "log(value)", y = "Density") + ggtheme
  save_plot(p_dens_log, "08_density_log")

  p_dens_site <- plot_density(data, log_scale = FALSE, facet_var = "site") +
    labs(x = cfg$axis_label, y = "Density") + ggtheme
  save_plot(p_dens_site, "09_density_by_site", width = 10)


  p_timeseries <- data |> 
    ggplot(aes(x=date, y=measure_value))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~site, labeller = as_labeller(site_labs))
    labs(y = cfg$axis_label, x = "Date") +
    ggtheme
  save_plot(p_dens_site, "10_analyte_over_time", width = 10)

# make season labeller
  season_labs<- c("winter" = "Winter", "spring" = "Spring")

p_violin_season<- data |> 
  dplyr::mutate(season = stringr::str_to_title(season)) |> 
  ggplot(aes(x=site, y=measure_value)) +
  geom_violin() +
  scale_x_discrete(label = c("Center Hill", "Dale Hollow", "Percy Priest")) +
  facet_wrap(~season) +
  labs(y = cfg$axis_label, x = "Site") +
  ggtheme
save_plot(p_dens_site, "11_violin_by_season", width = 10)
  
dens_by_season <- data |> 
  dplyr::mutate(season = stringr::str_to_title(season)) |> 
  ggplot(aes(x=measure_value, color = season, fill = season)) +
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~site, labeller = as_labeller(site_labs)) +
  labs(y = cfg$axis_label, x = "Site", color = "Season") +
  ggtheme

  invisible(list(
    season = p_season, counts = p_counts, comp = p_comp,
    hist = p_hist, dens = p_dens, dens_log = p_dens_log, dens_site = p_dens_site,
    timeseries = p_timeseries, seaon_violin = p_violin_season
  ))
}

# ---------------------------------------------------------------------
# 5. Run it
# ---------------------------------------------------------------------

raw <- readr::read_csv(here::here("data", "cleaned_data_long.csv"))

# All analytes at once:
plots <- purrr::imap(analyte_config, \(cfg, key) run_eda(key, cfg, raw))

# Just one (interactive), e.g. to inspect a plot in the Positron viewer:
# nox <- run_eda("NOx", analyte_config$NOx, raw)
# nox$dens_site