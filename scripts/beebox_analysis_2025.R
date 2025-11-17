# Test script for beebox climate drivers of activity

# Load required libraries
library(tidyverse)
library(lubridate)
library(hms)
library(myClim) ## logger data reading
library(foreach) ## efficient loop
library(data.table) ## efficient data.frame  
library(stringr) ## efficient character manipulation
library(broom)
library(akima)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Load data
beebox_hourly_temp_2025 <- read_csv("/Volumes/TundraBUZZ/data/raw/beebox_hourly_temp_2025.csv")
summary_flightbuzzes_ARUQ_2025 <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/summary_flightbuzzes_ARUQ_2025.csv")
QHI_clim_data_2025 <- read_csv("/Volumes/TundraBUZZ/QHI_clim_data_2025.csv")


# Filter for BEEBOX and aggregate to hourly

summary_hourly_BEEBOX_2025 <- summary_flightbuzzes_ARUQ_2025 %>%
  filter(location_id == "BEEBOX") %>%
  mutate(hour = floor_date(datetime, "hour")) %>%  # round down to start of the hour
  group_by(location_id, microclimate2, date_utc, hour) %>%
  summarise(
    total_duration_above_threshold = sum(total_duration_above_threshold, na.rm = TRUE),
    .groups = "drop"
  )

beebox_hourly_temp_2025 <- beebox_hourly_temp_2025 %>%
  mutate(
    time_of_day = as_hms(datetime)  # extract the time (HH:MM:SS) from datetime
  )

ggplot(summary_hourly_BEEBOX_2025, 
       aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point() +
  ylim(0,150)

ggplot(beebox_hourly_temp_2025, 
       aes(x = time_of_day, y = value)) +
  geom_point()

ggplot(QHI_clim_data_2025, 
       aes(x = time, y = solar_radiation)) +
  geom_point() +
  ylim(0,1000) 


# Merge the two dataframes by the hourly datetime column
beebox_hourly_merged_2025 <- summary_hourly_BEEBOX_2025 %>%
  left_join(
    beebox_hourly_temp_2025 %>% 
      select(datetime, value, cumulative_GDD5, cumulative_GDD0),
    by = c("hour" = "datetime")
  )

# Convert bee+temp dataset to local time
beebox_hourly_merged_2025 <- beebox_hourly_merged_2025 %>%
  mutate(datetime_local = with_tz(hour, tzone = "America/Whitehorse"))

lubridate::tz(beebox_hourly_merged_2025$datetime_local)
lubridate::tz(QHI_clim_data_2025$datetime_local)

QHI_clim_data_2025 <- QHI_clim_data_2025 %>%
  mutate(datetime_local = force_tz(datetime, tzone = "America/Whitehorse"))

# Now merge with climate data
merged_full_2025 <- beebox_hourly_merged_2025 %>%
  left_join(
    QHI_clim_data_2025 %>% select(datetime, everything()),
    by = c("datetime_local" = "datetime_local")
  ) %>%
  drop_na()  # remove any rows with NA in any column

# add a time_of_day column for plotting
merged_full_2025 <- merged_full_2025 %>%
  mutate(time_of_day = as_hms(datetime_local)) %>%
  filter(solar_radiation > 0)

merged_full_2025 <- merged_full_2025 %>%
  filter(!(date >= ymd_hms("2025-06-25 12:00:00") & 
             date <= ymd_hms("2025-06-29 17:00:00")))

ggplot(merged_full_2025, 
       aes(x = value, 
           y = solar_radiation, 
           size = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +       # alpha makes overlapping points easier to see
  #ylim(0, 800) +
  labs(
    x = "Temperature (°C)", 
    y = "Solar Radiation (W/m²)",
    size = "Bee Activity (s)"
  ) +
  theme_classic()

ggplot(merged_full_2025, 
       aes(x = value, 
           y = solar_radiation, 
           size = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +
  stat_density_2d(aes(weight = total_duration_above_threshold, 
                      color = ..level..),
                  geom = "contour",
                  bins = 10,
                  linewidth = 0.7) +  # change size -> linewidth
  scale_color_viridis_c(option = "plasma") +
  labs(
    x = "Temperature (°C)", 
    y = "Solar Radiation (W/m²)",
    size = "Bee Activity (s)",
    color = "Activity density"
  ) +
  theme_classic()

ggplot(merged_full_2025, aes(x = value, y = solar_radiation, color = total_duration_above_threshold, size = total_duration_above_threshold)) +
  geom_point(alpha = 0.7) +  # alpha for better visibility if points overlap
  scale_color_viridis_c(option = "plasma") +  # good color gradient for continuous variable
  scale_size(range = c(1, 6)) +  # adjust point sizes
  theme_classic() +
  labs(
    x = "Temperature (°C)",
    y = "Solar Radiation (W/m²)",
    color = "Bumblebee detections (s)",
    size = "Bumblebee detections (s)"
  )

# Bin the data
heatmap_data <- merged_full_2025 %>%
  mutate(
    temp_bin = cut(value, breaks = 30),        # adjust number of bins as needed
    solar_bin = cut(solar_radiation, breaks = 30)
  ) %>%
  group_by(temp_bin, solar_bin) %>%
  summarise(mean_activity = mean(total_duration_above_threshold, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    temp_mid = as.numeric(sub("\\((.+),.*", "\\1", temp_bin)) + 
      diff(range(merged_full_2025$value))/60,  # approximate mid-point
    solar_mid = as.numeric(sub("\\((.+),.*", "\\1", solar_bin)) + 
      diff(range(merged_full_2025$solar_radiation))/60
  )

# Plot heatmap
ggplot(heatmap_data, aes(x = temp_mid, y = solar_mid, fill = mean_activity)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  theme_classic() +
  labs(
    x = "Temperature (°C)",
    y = "Solar Radiation (W/m²)",
    fill = "Mean Bee Activity"
  )

ggplot(merged_full_2025, aes(x = value, y = solar_radiation, z = total_duration_above_threshold)) +
  stat_summary_2d(
    fun = mean,       # computes mean activity in each 2D bin
    bins = 10         # increase for higher resolution
  ) +
  scale_fill_viridis_c(option = "plasma") +
  theme_classic() +
  labs(
    x = "Temperature (°C)",
    y = "Solar Radiation (W/m²)",
    fill = "Bumblebee detections (s)"
  )




ggplot(merged_full_2025, 
       aes(x = solar_radiation, y = total_duration_above_threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  theme_classic() +
  labs(
    x = "Solar Radiation (W/m²)",
    y = "Bumblebee Detections (s)")

ggplot(merged_full_2025, 
       aes(x = value, y = total_duration_above_threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  theme_classic() +
  ylim(0,100) +
  labs(
    x = "Temperature (°C)",
    y = "Bumblebee Detections (s)")



