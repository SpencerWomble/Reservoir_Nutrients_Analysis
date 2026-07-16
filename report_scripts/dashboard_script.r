

pacman::p_load(tidyverse, writexl)

df<- read.csv("data/cleaned_data_long.csv")

str(df)

#---------------------------

# Items for dashboard

# 3-5 KPIs (cards)
# One large timeseries plot in center
# - Color-coded sections of the TS plot indicating low, moderate, and high concentrations
# Smalller, story-supporting visuals on the bottom row
# Filters for reservoirs that change the KPIs
# Filters for date ranges, seasons

#--------------------------

# Create data frame containing on NOx and TP data to use in the dashboard

filt_df<- df |> 
  dplyr::filter(characteristic == "Inorganic nitrogen (nitrate and nitrite)" |
                characteristic == "Phosphorus" & sample_fraction == "Total") |> 
  dplyr::filter(year > 1980) |> 
  dplyr::arrange(start_date)


filt_df |> 
  group_by(characteristic) |> 
  dplyr::summarise(max = max(measure_value, na.rm = TRUE)) |> 
  ungroup()


write_xlsx(filt_df, "data/dashboard_files/cleaned_df_long_NOx_TP.xlsx")


y_month<- filt_df |> 
  dplyr::group_by(month, year) |> 
  summarise(mean_measure_val = mean(measure_value, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(ymonth = )

head(y_month)


nox_monthly_ts <- df %>%
  filter(characteristic == "Inorganic nitrogen (nitrate and nitrite)" & year > 1980) %>%
  mutate(month_date = floor_date(as.POSIXct(date_time), "month")) %>%
  group_by(month_date) %>%
  summarise(
    mean_nox = mean(measure_value, na.rm = TRUE),  # mg/L
    .groups = "drop"
  )

ggplot(nox_monthly_ts, aes(x = month_date, y = mean_nox)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_point(color = "steelblue", size = 1.5) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "Average Monthly NOx Concentration Over Time",
    x = "Year",
    y = "Mean NOx Concentration (mg/L)"
  ) +
  theme_minimal()

