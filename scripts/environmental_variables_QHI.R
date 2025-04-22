# ====================================================
# Script Name: environmental_variables_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-04-07
# Last Modified: 2025-04-22
# Description: This script compiles and tidies all environmental data into a single dataset per date and location.
# Dependencies: copernicus_browser_qhi_2024.csv, met_station_pauline_cove_22010631.csv, en_climate_hourly_YT_2100636_06-2024_P1H.csv, en_climate_hourly_YT_2100636_07-2024_P1H.csv, en_climate_hourly_YT_2100636_08-2024_P1H.csv, QHI_sun_data_2024.csv, QHI_sunrise_daily.csv, QHI_location_temperature_daily.csv, R packages: tidyverse, lubridate, hms, janitor
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(lubridate)
library(hms)
library(janitor)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

#### Load data ----
cloud_cover <- read_csv("/Volumes/TundraBUZZ/data/raw/copernicus_browser_qhi_2024.csv")
met_station_pauline_cove <- read_csv("/Volumes/TundraBUZZ/data/raw/MetStation_data_QHI_2024/MetStation_PaulineCove_29Jul2024/met_station_pauline_cove_22010631.csv", skip = 1)
eccc_june <- read_csv("/Volumes/TundraBUZZ/data/raw/ECCC_climate_hourly_herschel/en_climate_hourly_YT_2100636_06-2024_P1H.csv")
eccc_july <- read_csv("/Volumes/TundraBUZZ/data/raw/ECCC_climate_hourly_herschel/en_climate_hourly_YT_2100636_07-2024_P1H.csv")
eccc_aug <- read_csv("/Volumes/TundraBUZZ/data/raw/ECCC_climate_hourly_herschel/en_climate_hourly_YT_2100636_08-2024_P1H.csv")
sun_data <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/raw/QHI_sun_data_2024.csv")
sunrise_daily <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/raw/QHI_sunrise_daily.csv")
QHI_temp_daily <- read.csv("/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_daily.csv")


#### Merge and tidy ECCC dataset ----
# Bind datasets
eccc_all <- bind_rows(eccc_june, eccc_july, eccc_aug) %>%
  filter(`Date/Time (LST)` >= as.POSIXct("2024-06-21 00:00:00"),
         `Date/Time (LST)` <= as.POSIXct("2024-08-12 23:59:59"))

# Select relevant columns (non NA)
eccc_summer_2024_hourly <- eccc_all %>%
  select(`Date/Time (LST)`, Year, Month, Day, `Time (LST)`, `Wind Dir (10s deg)`, `Wind Spd (km/h)`, `Stn Press (kPa)`)

# Clean names, mutate wind dir into degrees
eccc_summer_2024_hourly <- eccc_summer_2024_hourly %>%
  clean_names() %>%
  mutate(wind_dir_deg = wind_dir_10s_deg*10) %>%
  select(- wind_dir_10s_deg)

# Rename columns
eccc_summer_2024_hourly <- eccc_summer_2024_hourly %>%
  rename(
    datetime = date_time_lst,
    time_hour = time_lst,
  )

# Aggregate into daily dataset
eccc_summer_2024_daily <- eccc_summer_2024_hourly %>%
  mutate(date = as_date(datetime)) %>%  #
  group_by(date) %>%
  summarise(
    mean_wind_speed = mean(wind_spd_km_h, na.rm = TRUE),
    max_wind_speed = max(wind_spd_km_h, na.rm = TRUE),
    mean_stn_press_k_pa = mean(stn_press_k_pa, na.rm = TRUE),
    predominant_wind_dir = names(sort(table(wind_dir_deg), decreasing = TRUE)[1]),
    .groups = "drop"
  )


#### Select relative humidity from met_station_pauline_cove ----
# Select RH, mutate datetime as POSIXct
met_station_pauline_cove <- met_station_pauline_cove %>%
  select(`Date Time, GMT-06:00`, `RH, % (LGR S/N: 22010631, SEN S/N: 21741589, LBL: air_rh_1m)`) %>%
  rename(
    datetime = `Date Time, GMT-06:00`,
    air_rh_1m = `RH, % (LGR S/N: 22010631, SEN S/N: 21741589, LBL: air_rh_1m)`,
  ) %>%
  mutate(
    datetime = mdy_hms(datetime)  
  )

# Summarize hourly
met_station_pauline_cove_hourly <- met_station_pauline_cove %>%
  mutate(
    datetime = floor_date(datetime, "hour")  # Truncate datetime to the hour
  ) %>%
  group_by(datetime) %>%
  summarise(
    avg_air_rh_1m = mean(air_rh_1m, na.rm = TRUE),  # Average relative humidity per hour
    .groups = "drop"
  )

# Summarize daily
met_station_pauline_cove_daily <- met_station_pauline_cove %>%
  mutate(
    date = as_date(datetime)  # Extract the date from datetime
  ) %>%
  group_by(date) %>%
  summarise(
    avg_air_rh_1m = mean(air_rh_1m, na.rm = TRUE),  # Average relative humidity per day
    .groups = "drop"
  )




#### Correct timezone for sun_data ---- 
str(sun_data)

# Convert the datetime column to the "America/Whitehorse" timezone
sun_data$datetime <- with_tz(sun_data$datetime, tzone = "America/Whitehorse")




#### Combine hourly variables ----

environmental_variables_hourly <- eccc_summer_2024_hourly %>%
  left_join(met_station_pauline_cove_hourly, by = "datetime") %>%
  left_join(sun_data, by = "datetime")

# write_csv(environmental_variables_hourly, "/Users/alexandrebeauchemin/TundraBUZZ_github/data/clean/environmental_variables_hourly.csv")




#### Combine daily variables ----

environmental_variables_daily <- eccc_summer_2024_daily %>%
  left_join(met_station_pauline_cove_daily, by = "date") %>%
  left_join(sunrise_daily, by = "date") %>%
  left_join(cloud_cover, by = "date") %>%
  select(-"week")

# write_csv(environmental_variables_daily, "/Users/alexandrebeauchemin/TundraBUZZ_github/data/clean/environmental_variables_daily.csv")

environmental_variables_daily <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/clean/environmental_variables_daily.csv")


QHI_temp_daily_micro <- QHI_temp_daily %>%
  select(datetime, location_id, microclimate, value, cumulative_GDD0) %>%
  group_by(datetime, microclimate) %>%
  summarize(value = mean(value),
            cumulative_GDD0 = mean(cumulative_GDD0)) %>%
  mutate(date = as.Date(datetime))

# Join by date
env_combined <- environmental_variables_daily %>%
  left_join(QHI_temp_daily_micro, by = "date")

environmental_daily_filtered <- env_combined %>%
  select(-c(max_wind_speed, predominant_wind_dir, lat, lon, sunrise, sunset, datetime))

env_long <- environmental_daily_filtered %>%
  pivot_longer(
    cols = -c(date, microclimate),
    names_to = "variable",
    values_to = "value"
  )

#### Plot all environmental variables ----

big_env_plot <- ggplot(env_long, aes(x = date, y = value, colour = microclimate)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  theme_classic(base_size = 12) +
  labs(x = "Date", y = "Value") +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/environmental_variables_plot_QHI2024.pdf",
  plot = big_env_plot,
  width = 16,       # adjust based on layout
  height = 10
)

