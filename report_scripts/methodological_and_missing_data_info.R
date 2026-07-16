


library(tidyverse)

txt_size = theme(text = element_text(size = 14))

str(
  df<- read.csv("data/cleaned_data_long.csv")
)


# take out data before 1980

df<- df |> 
  dplyr::filter(year >=1980)


# rename the long name for Nox

df<- df |> 
  mutate(characteristic = if_else(characteristic == 
    "Inorganic nitrogen (nitrate and nitrite)", "nox", characteristic),
    characteristic = if_else(characteristic == "Phosphorus", "tp", characteristic),
    site = factor(site)
  ) |> 
  dplyr::filter(characteristic == "nox" | characteristic == "tp" & sample_fraction == "Total")

unique(df$characteristic)

glimpse(df)
str(df)


#----------------------------------------------

# make a label for the site labels

site_labs<- c("center_hill" = "Center Hill", "dale_hollow" =  "Dale Hollow", "percy_priest" = "J. Percy Priest")


#----------------------------------------------

# make summary table of the data

summary_table <- df |>
  filter(characteristic %in% c("nox", "tp")) |>
  mutate(
    censored = measure_value <= mdl & !is.na(mdl),

    value_summary = if_else(
      censored,
      mdl / 2,
      measure_value
    )
  ) |>
  group_by(site, characteristic) |>
  summarise(
    n_obs = n(),

    pct_censored = round(
      100 * mean(censored, na.rm = TRUE),
      1
    ),

    mean = mean(value_summary, na.rm = TRUE),
    median = median(value_summary, na.rm = TRUE),
    sd = sd(value_summary, na.rm = TRUE),

    iqr = IQR(
      value_summary,
      na.rm = TRUE
    ),

    minimum = min(
      value_summary,
      na.rm = TRUE
    ),

    maximum = max(
      value_summary,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) |>
  mutate(
    across(
      c(mean, median, sd, iqr, minimum, maximum),
      ~ round(.x, 3)
    )
  ) |>
  arrange(
    characteristic,
    site
  )

summary_table



#----------------------------------------------

# Get number of stations per reservoir

df |> 
  distinct(id, .keep_all = TRUE) |> 
  dplyr::group_by(site) |> 
  dplyr::count() |> 
  dplyr::ungroup()

#----------------------------------------------

# get number of observations per site

# number of Nox and TP observations
df |>
  dplyr::filter(characteristic == "nox" | characteristic == "tp" & sample_fraction == "Total") |>
  dplyr::group_by(characteristic) |> 
  dplyr::count() |> 
  dplyr::ungroup()

# number of Nox and TP observations by site
df |>
  dplyr::filter(characteristic == "nox" | characteristic == "tp" & sample_fraction == "Total") |>
  dplyr::group_by(characteristic, site) |> 
  dplyr::count() |> 
  dplyr::ungroup()


#--------------------------------------------------

# how many censored observations did we have

df |>
  dplyr::filter(
    characteristic == "nox" |
      (characteristic == "tp" &
         sample_fraction == "Total")
  ) |>
  dplyr::mutate(
    censored = if_else(
      measure_value <= mdl,
      "censored",
      "not_censored"
    ) 
  ) |>
  dplyr::count(characteristic, censored) |>
  dplyr::group_by(characteristic) |>
  dplyr::mutate(
    percent = n / sum(n) * 100
  ) |>
  dplyr::ungroup()
#--------------------------------------------------


#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------

# Missing data section


#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------

# Seasonal Missingness

# make rough season column
df<- df |> 
  dplyr::mutate(season = factor(
    case_when(
    month %in% c(12,1,2) ~ "Winter",
    month %in% c(3,4,5) ~ "Spring",
    month %in% c(6,7,8) ~ "Summer",
    month %in% c(9,10,11) ~ "Fall"
  ))
)


# get counts per reservoir per season per analyte
print(season_obs<-df |> 
  dplyr::group_by(site, season, characteristic) |> 
  dplyr::count() |> 
  dplyr::ungroup() |> 
  arrange(season,characteristic, site)
)

# make a plot of the number of observations per season per site

season_obs_plt<- function(analyte){
  season_obs |> 
    dplyr::filter(characteristic == analyte) |> 
    ggplot(aes(season, y = n))+
    geom_col()+
    scale_color_brewer(palette = "Dark2")+
    theme_bw()+
    theme(text = element_text(size = 14))+
    labs(x = "Season", y = "Count (n)",
        title = paste(str_to_title(analyte), "Observations per Season"))+
    facet_wrap(~site, labeller = as_labeller(site_labs))

}


# season obs for nox
season_obs_plt(analyte = "nox")

# season obs for tp
season_obs_plt(analyte = "tp")+
  labs(x = "Season", y = "Count (n)",
        title = "TP Observations per Season")

#--------------------------------------------------



#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------

# Trend plots

#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------

# temporal trend over time for each reservoir
# make a date coloumn
df<- df |> 
  mutate(start_date = lubridate::date(start_date),
         characteristic = factor(characteristic)
)


trend_plt <- function(data, analyte, units) {

  data |>
    filter(characteristic == analyte) |>
    ggplot(aes(start_date, measure_value)) +
    geom_point() +
    theme_bw() +
    theme(text = element_text(size = 14)) +
    labs(x = "Date", y = units) +
    facet_wrap(~site, labeller = as_labeller(site_labs))
}

# trend plot for Nox
trend_plt(df, analyte = "nox", units = "Nox (mg/L)")+
  geom_smooth()


# trend plot for TP
trend_plt(df, analyte = "tp", units = "TP (mg/L)")+
  geom_smooth()

#--------------------------------------------------

# seasonal averages

seasonal_avgs<-df |> 
  group_by(characteristic, season, site) |> 
  summarise(avg = mean(measure_value)) |> 
  ungroup()


season_avg_ptl <- function(analyte, units){

  seasonal_avgs |>
    filter(characteristic == analyte) |>
    ggplot(aes(season, avg)) +
    geom_col() +
    theme_bw() +
    txt_size +
    facet_wrap(~site, labeller = as_labeller(site_labs)) +
    labs(
      x = "Season",
      y = sprintf("Average %s (mg/L)", units)
    )
}

# seasonal avg for nox
season_avg_ptl(analyte = "nox", units = "Nox")

# seasonal avg for TP
season_avg_ptl(analyte = "tp", units = "TP")

#--------------------------------------------------

# seasonal trend plots

season_trend_plt<- function(data, analyte){

avg_df <- data |> 
  dplyr::filter(characteristic == analyte) |> 
  dplyr::group_by(season, year, site) |> 
  dplyr::summarise(yr_season_avg = mean(measure_value, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(season) |> 
  dplyr::filter(n() > 2) |> 
  dplyr::ungroup() |> 
  droplevels()

p1 <- ggplot(avg_df, aes(year, yr_season_avg, col = season)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  scale_color_brewer(palette = "Dark2",
        name = "Season")+
  theme_bw() +
  txt_size +
  facet_wrap(~site, labeller = as_labeller(site_labs)) +
  labs(
    x = "Year", 
    y = paste0(str_to_title(analyte), " (mg/L)"),
    title = paste0("Seasonal Trends in ", str_to_title(analyte))
                  )
  

  return(p1)

}

# seasonal trends in nox
season_trend_plt(data = df, analyte = "nox")

# seasonal trends in TP
season_trend_plt(data = df, analyte = "tp")+
  labs(title = "Seasonal Trends in TP",
        y = "TP (mg/L)")

#------------------------------------------------------------------

# trends in distance from dam and nutrient content

dam_dist_plt<- function(analyte){

  df |> 
    dplyr::filter(characteristic == analyte) |> 
    ggplot(aes(x=total_distance, y = measure_value))+
    geom_smooth(se=FALSE)+
    geom_point()+
    theme_bw()+
    txt_size+
    facet_wrap(~site, labeller = as_labeller(site_labs))+
    labs(title = paste("Trends in Distance from Dam and",str_to_title(analyte), "Concentration"),
          x = "Total Distance (km)",
        y = paste(str_to_title(analyte), "(mg/L)")
      )
}

# distance from dam and nox
dam_dist_plt(analyte = "nox")+
    scale_y_continuous(limits = c(0, 1.3))

# distance from dam and tp
dam_dist_plt(analyte = "tp")+
      labs(title = paste("Trends in Distance from Dam and TP Concentration"),
          x = "Total Distance (km)",
        y = "TP (mg/L)")+
  scale_y_continuous(limits = c(0, 0.4))
      

#------------------------------------------------------
#------------------------------------------------------

# Violin plots

#------------------------------------------------------
#------------------------------------------------------

# violin plot overall

df |> 
  ggplot(aes(x=site, y=measure_value))+
  geom_violin()+
  theme_bw()+
  txt_size


dens_plt<- function(analyte){
  df |> 
    dplyr::filter(characteristic == analyte) |> 
    ggplot(aes(x=measure_value))+
    geom_density()+
    theme_bw()+
    theme(panel.spacing.x = unit(0.5, "cm"))+
    txt_size+
    labs(y="Density")+
    facet_wrap(~site, labeller = as_labeller(site_labs))
}


# density plot for NOx
dens_plt(analyte = "nox")+
  labs(x = expression(NO[x]~"(mg/L)"),
        title = expression(NO[x]~Density))

