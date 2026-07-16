
# Load packages
pacman::p_load(tidyverse, brms, tidybayes, bayesplot)


# -------- Data prep --------

df <- read.csv(here::here("data", "cleaned_data_long.csv")) |>
  dplyr::filter(characteristic == "Phosphorus" & sample_fraction == "Total") |> 
  dplyr::select(site, monitoring_location_identifier, id, date_time, 
                year, month, decimal_date, total_distance, 
                characteristic, measure_value, mdl, measure_value_half_dl) |> 
  dplyr::filter(year > 1980 & measure_value < 5) |> # filter out the outlier that is like 6 mg/L
  dplyr::mutate(
    doy  = yday(date_time),
    site = factor(site, levels = c("center_hill", "dale_hollow", "percy_priest")),
    id   = factor(id),
  ) |>
  # filter to just include TP data
  dplyr::arrange(decimal_date, site, id)


str(df)


# set seed
set.seed(99)



# create a modeling data frame
mod_df<- df


# Add censoring column - very important step

mod_df<- mod_df |> 
  dplyr::mutate(cens = factor(if_else(measure_value < mdl, "left", "none")))


#--------------------------------

# center and scale model parameters (where it makes sense)
mod_df<- mod_df |> 
  dplyr::mutate(
    across(
      c(total_distance, doy, decimal_date), ~ scale(.x),
                .names = "{.col}_s"
              )
            )

str(mod_df)


# check distribution of TP data

mod_df |> 
  dplyr::filter(measure_value <5) |> 
  ggplot(aes(x=measure_value))+
  geom_density()

# not particularly normal


mod_df |> 
  dplyr::mutate(log_tp = log(measure_value)) |> 
  ggplot(aes(x=log_tp))+
  geom_density()
# much better when log transformed


#---------------------------

# log transform TP

mod_df<- mod_df |> 
  dplyr::mutate(log_tp = log(measure_value))

#---------------------------






#---------------------------

# Initial Modeling

#---------------------------

mean(na.omit(mod_df$measure_value))

prior1 <- c(
  prior(normal(-2, 1),  class = "Intercept"), # note these are on the log scale
  prior(normal(0, 0.5), class = "b"),
  prior(exponential(2), class = "sigma")
)

# Run a simple censored model
simple_mod <- brm(measure_value | cens(cens) ~ total_distance_s + doy_s + site + decimal_date_s + (1 | id),
              data = mod_df,
              family = lognormal(),
              prior = prior1,
              chains  = 4, iter = 4000, cores = 4,
              backend = "cmdstanr")

# get summary
summary(simple_mod)


# retrieve prior samples from the posterior fit
fit_prior_only <- update(
  simple_mod,
  silent = TRUE,
  refresh = 0,
  sample_prior = "only"
)

#----------------------------

# Evaluate the priors

#----------------------------

library(brms)
library(bayesplot)

# Basic density overlay: prior-predictive draws vs observed data
pp_check(fit_prior_only, ndraws = 100) +
  ggplot2::coord_cartesian(xlim = c(0, quantile(mod_df$measure_value, 0.99, na.rm = TRUE) * 3))

#----------------------------

# check by hand by compairing the values estimated by the priors to the actual data

#----------------------------

# Draw prior predictive at the observed covariate values
yrep <- posterior_predict(fit_prior_only)

# Summarize
quantile(yrep, probs = c(0.01, 0.25, 0.5, 0.75, 0.99, 1)) #PROBLEM ON THE HIGH END!!!!!!!!!!!!!!!!!!!!!!!!

# Compare to your actual observed range (including MDL floor)
range(mod_df$measure_value, na.rm = TRUE)
