#### Test script for beebox climate drivers of activity ####
#### Script written by Alex Beauchemin. Last edited: Nov 17, 2025# ####

# Load required libraries ----
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



# Merge the two dataframes by the hourly datetime column
beebox_hourly_merged_2025 <- beebox_hourly_temp_2025 %>% 
  select(datetime, value, cumulative_GDD5, cumulative_GDD0) %>%
  left_join(summary_hourly_BEEBOX_2025,
    by = c("datetime" = "hour")
  )

# Convert bee+temp dataset to local time
beebox_hourly_merged_2025 <- beebox_hourly_merged_2025 %>%
  mutate(datetime_local = with_tz(datetime, tzone = "America/Whitehorse"))

lubridate::tz(beebox_hourly_merged_2025$datetime_local)
lubridate::tz(QHI_clim_data_2025$datetime_local)

QHI_clim_data_2025 <- QHI_clim_data_2025 %>%
  mutate(datetime_local = force_tz(datetime, tzone = "America/Whitehorse"))


# Aggregate climate data by hour
QHI_clim_hourly_2025 <- QHI_clim_data_2025 %>%
  filter(!(date >= ymd_hms("2025-06-25 12:00:00") & 
             date <= ymd_hms("2025-06-29 17:00:00"))) %>%
  group_by(hour = lubridate::floor_date(datetime_local, unit = "hour")) %>%  # round datetime to nearest hour
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")  # take mean of numeric variables



# Now merge with climate data
merged_full_2025 <- QHI_clim_hourly_2025 %>%
  left_join(
    beebox_hourly_merged_2025,
    by = c("hour" = "datetime_local")
  ) %>%
  mutate(
    total_duration_above_threshold = replace_na(total_duration_above_threshold, 0)
  ) %>%
  rename(datetime_local = hour)

#### HERE



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

ggplot(merged_full_2025 %>% filter(total_duration_above_threshold < 200), aes(x = value, y = solar_radiation, z = total_duration_above_threshold)) +
  stat_summary_2d(
    fun = mean,       # computes mean activity in each 2D bin
    bins = 15         # increase for higher resolution
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
  ylim(0,200) +
  theme_classic() +
  labs(
    x = "Solar Radiation (W/m²)",
    y = "Bumblebee Detections (s)")

ggplot(merged_full_2025, 
       aes(x = value, y = total_duration_above_threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  theme_classic() +
  ylim(0,200) +
  labs(
    x = "Temperature (°C)",
    y = "Bumblebee Detections (s)")



QHI_bumblebee_nest_2025 <- merged_full_2025 %>%
  dplyr::select(datetime_local, time_of_day, total_duration_above_threshold, value, solar_radiation, wind_speed, gust_speed, wind_direction, air_pressure, datetime) %>% 
  rename(
    datetime_local = datetime_local,      
    time_of_day = time_of_day,           
    bumblebee_buzz_hourly = total_duration_above_threshold,
    temperature_C = value,            
    solar_radiation = solar_radiation,   
    wind_speed_kmh = wind_speed,      
    gust_speed_kmh = gust_speed,       
    wind_dir_deg = wind_direction,     
    atm_pressure_kpa = air_pressure,     
    datetime_utc = datetime
  ) %>%
  mutate(bumblebee_buzz_hourly = bumblebee_buzz_hourly/0.15)

QHI_bumblebee_nest_2025 <- QHI_bumblebee_nest_2025 %>%
  filter(datetime_utc <= ymd_hms("2025-08-12 15:00:00"))


ggplot(QHI_bumblebee_nest_2025, 
       aes(x = time_of_day, y = bumblebee_buzz_hourly)) +
  geom_point() 

ggplot(QHI_bumblebee_nest_2025, 
       aes(x = time_of_day, y = temperature_C)) +
  geom_point()

ggplot(QHI_bumblebee_nest_2025, 
       aes(x = time_of_day, y = solar_radiation)) +
  geom_point() 

ggplot(QHI_bumblebee_nest_2025, 
       aes(x = datetime_local, y = bumblebee_buzz_hourly)) +
  geom_smooth(method = "loess", se = TRUE) +
  geom_point() 

hist(QHI_bumblebee_nest_2025$temperature_C)
hist(QHI_bumblebee_nest_2025$solar_radiation)   
hist(QHI_bumblebee_nest_2025$wind_speed_kmh)     
hist(QHI_bumblebee_nest_2025$gust_speed_kmh)     
hist(QHI_bumblebee_nest_2025$wind_dir_deg)     
hist(QHI_bumblebee_nest_2025$atm_pressure_kpa)


hist(QHI_bumblebee_nest_2025$bumblebee_buzz_hourly[
  QHI_bumblebee_nest_2025$bumblebee_buzz_hourly < 20
], breaks = 100)

hist(QHI_bumblebee_nest_2025$bumblebee_buzz_hourly[
  QHI_bumblebee_nest_2025$bumblebee_buzz_hourly > 0
], breaks = 100)


ggplot(QHI_bumblebee_nest_2025, 
       aes(x = solar_radiation, y = bumblebee_buzz_hourly)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  ylim(0,1000) +
  theme_classic() +
  labs(
    x = "Solar Radiation (W/m²)",
    y = "Bumblebee Detections (s)")

ggplot(QHI_bumblebee_nest_2025, 
       aes(x = temperature_C, y = bumblebee_buzz_hourly)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  theme_classic() +
  ylim(0,1000) +
  labs(
    x = "Temperature (°C)",
    y = "Bumblebee Detections (s)")

library(mgcv)

QHI_bumblebee_nest_2025 <- QHI_bumblebee_nest_2025 %>%
  mutate(
    doy = yday(datetime_local)
  )

library(glmmTMB)

glmm_zinb <- glmmTMB(
  bumblebee_buzz_hourly ~ 
    temperature_C + solar_radiation +
    wind_speed_kmh + atm_pressure_kpa +
    (1 | doy) + (1 | time_of_day),
  ziformula = ~ temperature_C + solar_radiation,
  family = nbinom2,
  data = QHI_bumblebee_nest_2025
)

summary(glmm_zinb)



# Prepare a time index
QHI_bumblebee_nest_2025 <- QHI_bumblebee_nest_2025 %>%
  filter(datetime_utc >= ymd_hms("2025-06-17 07:00:00")) %>%
  arrange(datetime_local) %>%
  mutate(time_index = 1:n(),
         AR_start = ifelse(time_of_day == as_hms("00:00:00"), TRUE, FALSE)) # TRUE at start of each day

QHI_bumblebee_nest_2025 <- QHI_bumblebee_nest_2025 %>%
  mutate(time_of_day_hour = as.numeric(time_of_day)/3600)

library(brms)

QHI_bumblebee_nest_2025 <- QHI_bumblebee_nest_2025 %>%
  mutate(bumblebee_buzz_hourly = as.integer(bumblebee_buzz_hourly))

# Fit a zero-inflated NB with AR(1) autocorrelation #### did not converge, 13 divergent transitions, not the fuzziest caterpillars --> simplify model?
brms_model <- brm(
  bumblebee_buzz_hourly ~ 
    s(temperature_C) +
    s(time_of_day_hour, bs = "cc") +
    solar_radiation +
    wind_speed_kmh +
    atm_pressure_kpa +
    s(doy) +
    ar(time_index, p = 1),
  family = zero_inflated_negbinomial(),
  data = QHI_bumblebee_nest_2025,
  chains = 4, cores = 4
)


#write.csv(QHI_bumblebee_nest_2025, "/Volumes/IGUTCHAQ/outputs/clean/QHI_bumblebee_nest_2025.csv", row.names = FALSE)
QHI_bumblebee_nest_test <- read_csv("/Volumes/IGUTCHAQ/outputs/clean/QHI_bumblebee_nest_2025.csv")

