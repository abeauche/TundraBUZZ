# ====================================================
# Script Name: flight_buzzes_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-21
# Last Modified: 2025-03-31
# Description: This script TBD.
# Dependencies: summary_flightbuzzes_ARUQ_2024.csv, QHI_sun_data_2024.csv, QHI_sunrise_sunset_filtered.csv, aru_temp_daily_micro.csv, aru_temp_hourly_micro.csv, location_mapping_TundraBUZZ.csv
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(lubridate)
library(suncalc)
library(hms)
library(lme4)
library(lmerTest) 
library(mgcv)
library(visreg)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

# Set latitude and longitude for Qikiqtaruk - Herschel Island
lat <- 69.57
lon <- -138.91


#### Using tidy datasets ----
# Load data
summary_flightbuzzes_ARUQ_2024 <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/summary_flightbuzzes_ARUQ_2024.csv")
QHI_sun_data_2024 <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/raw/QHI_sun_data_2024.csv")
QHI_sunrise_sunset_filtered <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/raw/QHI_sunrise_sunset_filtered.csv")
QHI_temp_daily <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv")
QHI_temp_hourly <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv")
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")



#### Format dataset ----
# Change location_id and microclimate to factor
summary_flightbuzzes_ARUQ_2024$location_id <- as.factor(summary_flightbuzzes_ARUQ_2024$location_id) 
summary_flightbuzzes_ARUQ_2024$microclimate <- as.factor(summary_flightbuzzes_ARUQ_2024$microclimate)
  
# Check levels and table
levels(summary_flightbuzzes_ARUQ_2024$location_id)
table(summary_flightbuzzes_ARUQ_2024$location_id)
levels(summary_flightbuzzes_ARUQ_2024$microclimate)
table(summary_flightbuzzes_ARUQ_2024$microclimate)

# Check timezone
attr(summary_flightbuzzes_ARUQ_2024$datetime, "tzone")

# Ensure datetime is in "America/Whitehorse" 
summary_flightbuzzes_ARUQ_2024 <- summary_flightbuzzes_ARUQ_2024 %>%
  mutate(datetime = force_tz(datetime, tzone = "America/Whitehorse"))




#### Data visualization ----
# Plot flight buzzes over time
ggplot(summary_flightbuzzes_ARUQ_2024, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  
  labs(title = "Flight Buzzes Over Time (Threshold = 8)",
       x = "Datetime", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  ylim(0, 150) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)


# Plot flight buzzes by time of day
ggplot(summary_flightbuzzes_ARUQ_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  labs(title = "Flight Buzzes Over a 24-hour Period - ARUQ0 (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)


# Plot flight buzzes by time of day, week
ggplot(summary_flightbuzzes_ARUQ_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id + week)


# Plot flight buzzes by time of day, week with sunrise and sunset
ggplot(summary_flightbuzzes_ARUQ_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~week) +
  geom_vline(data = QHI_sunrise_sunset_filtered, aes(xintercept = avg_sunrise), 
             linetype = "dashed", color = "orange", size = 1) +
  geom_vline(data = QHI_sunrise_sunset_filtered, aes(xintercept = avg_sunset), 
             linetype = "dashed", color = "orange4", size = 1) +
  geom_text(data = QHI_sunrise_sunset_filtered, 
            aes(x = avg_sunrise, y = 45, label = "Sunrise"), 
            color = "orange", angle = 90, vjust = -0.5, size = 3) +
  geom_text(data = QHI_sunrise_sunset_filtered, 
            aes(x = avg_sunset, y = 45, label = "Sunset"), 
            color = "orange4", angle = 90, vjust = -0.5, size = 3)



######## IN PROGRESS ######### -----
#### MICROCLIMATE DATA ----

# Read in microclimate data
aru_temp_daily_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv") #### RENAMED QHI_temp_daily
aru_temp_hourly_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv") #### RENAMED QHI_temp_hourly

# Filter for ARUQ0
aru_temp_daily_micro <- aru_temp_daily_micro %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day))

aruq0_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ0") %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, sep="-"), 
                               format="%Y-%m-%d", tz="UTC"))






#### DEBUG
aru_temp_hourly_micro <- aru_temp_hourly_micro %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day),
         hour = as.numeric(hour))

aruq0_data_plot_h <- aru_temp_hourly_micro %>%
  filter(aru_name == "ARUQ0") %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, hour), format="%Y-%m-%d %H", tz="UTC"))

# Aggregate total duration above threshold per day
summary_pred_duration_agg_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with daily microclimate data
merged_data_aruq0 <- summary_pred_duration_agg_ARUQ0 %>%
  left_join(aruq0_data_plot, by = c("date" = "datetime")) %>%
  filter(!is.na(month))  # Remove NAs in month column

# Aggregate total duration above threshold per hour
summary_pred_duration_agg_hourly_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date_time_hour = floor_date(datetime, "hour")) %>%
  group_by(date_time_hour) %>%
  summarise(total_duration_hour = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with hourly microclimate data
merged_data_aruq0_hourly <- summary_pred_duration_agg_hourly_ARUQ0 %>%
  left_join(aruq0_data_plot_h, by = c("date_time_hour" = "datetime"))

# Plot total flight buzz duration vs. mean temperature
ggplot(merged_data_aruq0, aes(x = mean_value, y = total_duration_day)) +
  geom_point(aes(color = as.factor(month))) +
  geom_smooth(method = "lm", aes(color = as.factor(month)), se = FALSE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature - ARUQ0",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  geom_vline(xintercept = c(6, 12.6), color = c("orange", "orange4"), linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Log-transformed plot
ggplot(merged_data_aruq0, aes(x = mean_value, y = log(total_duration_day))) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "skyblue4", se = TRUE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Log Total Flight Buzz Duration (s)") +
  theme_classic()



#### Work with ARUQ456 data ----
# Define threshold and filter data
threshold <- 8  
ARUQ456_2024_above_threshold <- ARUQ456_2024_pred_mapped %>%
  filter(BUZZ > threshold) %>%
  mutate(duration_above_threshold = 0.15)  # Each segment is 0.3s, so each overlap segment is 0.15s

# Save csv
write.csv(ARUQ456_2024_above_threshold, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ456_2024_pred_above_threshold.csv", row.names = FALSE)

# Summarize total duration above threshold per datetime
summary_pred_duration_ARUQ456_2024 <- ARUQ456_2024_above_threshold %>%
  group_by(datetime, location_id, microclimate) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold), .groups = "drop")

# Convert datetime to UTC-7
summary_pred_duration_ARUQ456_2024 <- summary_pred_duration_ARUQ456_2024 %>%
  mutate(time_of_day = hms::as_hms(format(datetime, "%H:%M:%S")))

# Save csv
write.csv(summary_pred_duration_ARUQ456_2024, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/summary_ARUQ456_2024_pred.csv", row.names = FALSE)


# Plot flight buzzes over time
ggplot(summary_pred_duration_ARUQ456_2024, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  
  labs(title = "Flight Buzzes Over Time (Threshold = 8)",
       x = "Datetime", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  #ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)

# Group by week for aggregation
summary_pred_duration_ARUQ456_2024 <- summary_pred_duration_ARUQ456_2024 %>%
  mutate(week = floor_date(datetime, "week"))

# Get sunrise and sunset times
lat <- 69.57
lon <- -138.91
# summary_pred_duration_ARUQ0_2024_sun <- summary_pred_duration_ARUQ0_2024_month %>%
#  mutate(date = as.Date(datetime))

# sun_times <- getSunlightTimes(data = data.frame(date = unique(summary_pred_duration_ARUQ0_2024_sun$date), lat = lat, lon = lon),
#                              keep = c("sunrise", "sunset"))

# Plot flight buzzes by time of day
ggplot(summary_pred_duration_ARUQ456_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~week + location_id)

# Plot flight buzzes by time of day
ggplot(summary_pred_duration_ARUQ456_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = loess) +
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
  #facet_wrap(~location_id)


#### Work with ARUQ4 data ----
# Define threshold and filter data
threshold <- 8  
ARUQ4_2024_above_threshold <- ARUQ4_2024_pred_mapped %>%
  filter(BUZZ > threshold) %>%
  mutate(duration_above_threshold = 0.15)  # Each segment is 0.3s, so each overlap segment is 0.15s

# Save csv
write.csv(ARUQ4_2024_above_threshold, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ4_2024_pred_above_threshold.csv", row.names = FALSE)

# Summarize total duration above threshold per datetime
summary_pred_duration_ARUQ4_2024 <- ARUQ4_2024_above_threshold %>%
  group_by(datetime, location_id, microclimate) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold), .groups = "drop")

# Convert datetime to UTC-7
summary_pred_duration_ARUQ4_2024 <- summary_pred_duration_ARUQ4_2024 %>%
  mutate(time_of_day = hms::as_hms(format(datetime, "%H:%M:%S")))

# Save csv
write.csv(summary_pred_duration_ARUQ4_2024, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/summary_ARUQ4_2024_pred.csv", row.names = FALSE)


# Plot flight buzzes over time
ggplot(summary_pred_duration_ARUQ4_2024, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  
  labs(title = "Flight Buzzes Over Time (Threshold = 8)",
       x = "Datetime", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  #ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Plot flight buzzes over time
ggplot(summary_pred_duration_ARUQ4_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point() +  
  labs(title = "Flight Buzzes Over Time (Threshold = 8)",
       x = "Datetime", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  #ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#### Microclimate data ----
# Read in microclimate data
aru_temp_daily_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv")
aru_temp_hourly_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv")

# Clean aru_temp
aru_temp_daily_micro <- aru_temp_daily_micro %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day))

# Filter for ARUQ0
aruq0_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ0") %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, sep="-"), 
                               format="%Y-%m-%d", tz="UTC"))

# Filter for ARUQ56
aruq56_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name %in% c("ARUQ5", "ARUQ6")) %>%
  mutate(location_id = recode(aru_name, "ARUQ5" = "WARM1", "ARUQ6" = "COOL1")) %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, sep="-"), 
                               format="%Y-%m-%d", tz="UTC"))

# Filter for ARUQ4
aruq4_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ4") %>%
  mutate(location_id = recode(aru_name, "ARUQ4" = "COOL4")) %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, sep="-"), 
                               format="%Y-%m-%d", tz="UTC"))

# Aggregate total duration above threshold per day
summary_pred_duration_agg_ARUQ456_2024 <- summary_pred_duration_ARUQ456_2024 %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, location_id) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with daily microclimate data
merged_data_ARUQ56 <- summary_pred_duration_agg_ARUQ456_2024 %>%
  filter(location_id %in% c("WARM1", "COOL1")) %>%
  left_join(aruq56_data_plot, by = c("date" = "datetime", "location_id")) %>%
  filter(!is.na(month))  # Remove NAs in month column

hist(log(merged_data_ARUQ56$mean_value))
hist(log(merged_data_ARUQ56$total_duration_day))


# Fit separate models for each location_id
models_ARUQ56 <- merged_data_ARUQ56 %>%
  group_by(location_id) %>%
  do(model = lm(log(total_duration_day) ~ log(mean_value), data = .))

# Generate predictions for each location_id
predictions_ARUQ56 <- merged_data_ARUQ56 %>%
  group_by(location_id) %>%
  do({
    model <- lm(log(total_duration_day) ~ log(mean_value), data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      mean_value = .$mean_value,
      total_duration_day = .$total_duration_day,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })

# Plot total flight buzz duration vs. mean temperature
ggplot(predictions_ARUQ56, aes(x = mean_value, y = total_duration_day)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "skyblue4", size = 1) +  # Regression line
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +  # Confidence interval
  
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = max(predictions_ARUQ56$total_duration_day, na.rm = TRUE) * 0.8, 
           label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = max(predictions_ARUQ56$total_duration_day, na.rm = TRUE) * 0.8, 
           label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  ylim(0,500) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)



class(aruq56_data_plot$datetime)
aruq56_data_plot$date <- as.POSIXct(aruq56_data_plot$datetime)
class(summary_pred_duration_agg_ARUQ456_2024$date)




# Convert date columns to Date or POSIXct
summary_pred_duration_agg_ARUQ456_2024$date <- as.Date(summary_pred_duration_agg_ARUQ456_2024$date)
aruq56_data_plot$date <- as.Date(aruq56_data_plot$date)

summary_pred_duration_agg_ARUQ456_2024 <- summary_pred_duration_agg_ARUQ456_2024 %>%
  filter(!location_id == "WARM2")

ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_agg_ARUQ456_2024, aes(x = date, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Temperature curve
  geom_smooth(data = aruq56_data_plot, aes(x = date, y = mean_value), color = "orange3", method = "loess", alpha = 0.7, se = FALSE) +
  geom_smooth(data = summary_pred_duration_agg_ARUQ456_2024, aes(x = date, y = total_duration_day/24), color = "skyblue4", method = "loess", se = TRUE) +
  geom_line(data = aruq56_data_plot, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq56_data_plot, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Customizing the plot
  labs(
    title = "Flight Buzzes and Temperature Over Time",
    x = "Datetime", 
    y = "Mean Flight Buzz Duration per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())
  ) +
  
  # Ensure the x-axis is properly formatted and limited
  scale_x_date(limits = c(as.Date("2024-06-20"), max(summary_pred_duration_agg_ARUQ456_2024$date, na.rm = TRUE))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") + 
  
  facet_wrap(~location_id)



#### Plotting ARUQ4 ----
# Aggregate total duration above threshold per day
summary_pred_duration_agg_ARUQ4_2024 <- summary_pred_duration_ARUQ4_2024 %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, location_id) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with daily microclimate data
merged_data_ARUQ4 <- summary_pred_duration_agg_ARUQ4_2024 %>%
  filter(location_id == "COOL4") %>%
  left_join(aruq4_data_plot, by = c("date" = "datetime", "location_id")) %>%
  filter(!is.na(month))  # Remove NAs in month column

hist(log(merged_data_ARUQ4$mean_value))
hist(log(merged_data_ARUQ4$total_duration_day))


# Fit separate models for each location_id
models_ARUQ4 <- merged_data_ARUQ4 %>%
  group_by(location_id) %>%
  do(model = lm(log(total_duration_day) ~ log(mean_value), data = .))

# Generate predictions for each location_id
predictions_ARUQ4 <- merged_data_ARUQ4 %>%
  group_by(location_id) %>%
  do({
    model <- lm(log(total_duration_day) ~ log(mean_value), data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      mean_value = .$mean_value,
      total_duration_day = .$total_duration_day,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })

# Plot total flight buzz duration vs. mean temperature
ggplot(predictions_ARUQ4, aes(x = mean_value, y = total_duration_day)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "skyblue4", size = 1) +  # Regression line
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +  # Confidence interval
  
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = max(predictions_ARUQ4$total_duration_day, na.rm = TRUE) * 0.8, 
           label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = max(predictions_ARUQ4$total_duration_day, na.rm = TRUE) * 0.8, 
           label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  ylim(0,200) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)



class(aruq4_data_plot$datetime)
aruq4_data_plot$date <- as.POSIXct(aruq4_data_plot$datetime)
class(summary_pred_duration_agg_ARUQ4_2024$date)



# Convert date columns to Date or POSIXct
summary_pred_duration_agg_ARUQ4_2024$date <- as.Date(summary_pred_duration_agg_ARUQ4_2024$date)
aruq4_data_plot$date <- as.Date(aruq4_data_plot$date)

ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_agg_ARUQ4_2024, aes(x = date, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Temperature curve
  geom_smooth(data = aruq4_data_plot, aes(x = date, y = mean_value), color = "orange3", method = "loess", alpha = 0.7, se = FALSE) +
  geom_smooth(data = summary_pred_duration_agg_ARUQ4_2024, aes(x = date, y = total_duration_day/24), color = "skyblue4", method = "loess", se = TRUE) +
  geom_line(data = aruq4_data_plot, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq4_data_plot, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Customizing the plot
  labs(
    title = "Flight Buzzes and Temperature Over Time - ARUQ4",
    x = "Datetime", 
    y = "Mean Flight Buzz Duration per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())
  ) +
  
  # Ensure the x-axis is properly formatted and limited
  scale_x_date(limits = c(as.Date("2024-06-20"), max(summary_pred_duration_agg_ARUQ4_2024$date, na.rm = TRUE))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom")




#### POLCAM DATA ----

polcam <- read.csv("/Volumes/TundraBUZZ/data/raw/POLCAM_daily_quick_raw.csv")


# Merge to replace aru_id with location_id
polcam_mapped <- polcam %>%
  select(-c(site,year)) %>%
  left_join(location_mapping, by = c("plot" = "polcam_id")) %>%
  select(-c(aru_id, plot,tomst_id,site,year,notes,observer))  # Remove aru_id, now using location_id

str(polcam_mapped)

# Summing the flower counts per day and location_id
polcam_mapped$flower_count <- rowSums(polcam_mapped[, c("sal_arc_fem", "sal_arc_male", "sal_rich_fem", 
                                                        "sal_rich_male", "sal_ret_open", "sal_pul_fem", 
                                                        "sal_pul_male", "lup_arc", "ped_all", "ped_cap", 
                                                        "dry_int", "astra", "oxy", "stel_long", "sen", 
                                                        "cast", "cas_tet", "pyr_gra", "bis_viv")], na.rm = TRUE)

# Summarize the total flower count by day and location
flower_summary <- polcam_mapped %>%
  group_by(date, location_id) %>%
  summarise(total_flower_count = sum(flower_count), .groups = "drop")



#### ARUQ56 ----

# Merge the flower count data with the other datasets
merged_data <- merge(summary_pred_duration_agg_ARUQ456_2024, flower_summary, by = c("date", "location_id"))
merged_data <- merge(merged_data, aruq56_data_plot, by = c("date", "location_id"))


# Filter out rows where total_flower_count is zero
merged_data_filtered <- merged_data %>%
  filter(total_flower_count > 0)

# Plot
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = merged_data, aes(x = date, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Plot the temperature curve
  geom_smooth(data = merged_data, aes(x = date, y = mean_value), color = "orange3", method = "loess", alpha = 0.7, se = FALSE) +
  geom_smooth(data = merged_data, aes(x = date, y = total_duration_day/24), color = "skyblue4", method = "loess", se = FALSE) +
  geom_line(data = merged_data, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = merged_data, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Add the total flower count as a new series
  geom_smooth(data = merged_data_filtered, aes(x = date, y = total_flower_count), color = "darkolivegreen", size = 1.2, se = FALSE) +
  geom_point(data = merged_data_filtered, aes(x = date, y = total_flower_count), color = "darkolivegreen", size = 2, alpha = 0.7, se = FALSE) +
  
  # Customizing the plot
  labs(
    title = "Flight Buzzes, Temperature, and Flower Count Over Time",
    x = "Datetime", 
    y = "Mean Flight Buzz Duration per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature | Green: Flower Count"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())
  ) +
  
  # Ensure the x-axis is properly formatted and limited
  scale_x_date(limits = c(as.Date("2024-06-20"), max(merged_data$date, na.rm = TRUE))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") + 
  
  facet_wrap(~location_id)




# Filter out rows with NA, NaN, or Inf values
merged_data_clean <- merged_data %>%
  filter(!is.na(total_duration_day), !is.na(total_flower_count)) %>%
  filter(!is.nan(total_duration_day), !is.nan(total_flower_count)) %>%
  filter(total_duration_day != Inf, total_flower_count != Inf)


hist(log(merged_data_clean$total_flower_count))

# Fit separate models for each location_id
models_ARUQ56_flowering <- merged_data_clean %>%
  group_by(location_id) %>%
  do(model = lm(log(total_duration_day) ~ total_flower_count, data = .))

# Generate predictions for each location_id
predictions_ARUQ56_flowering <- merged_data_clean %>%
  group_by(location_id) %>%
  do({
    model <- lm(log(total_duration_day) ~ total_flower_count, data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      total_flower_count = .$total_flower_count,  # Ensure this column is included
      total_duration_day = .$total_duration_day,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })


# Plotting
ggplot(merged_data_clean, aes(x = total_flower_count, y = total_duration_day)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_line(data = predictions_ARUQ56_flowering, aes(x = total_flower_count, y = predicted), color = "skyblue4", size = 1) +
  geom_ribbon(data = predictions_ARUQ56_flowering, aes(x = total_flower_count, ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +
  # Add model coefficients and p-values as text annotations
  # geom_text(data = predictions_ARUQ56_flowering %>%
  #            group_by(location_id) %>%
  #            slice(1), 
  #          aes(x = 1, y = 1600, label = paste("Intercept =", round(coef(models_ARUQ56_flowering)[1], 2), "\np-value =", round(summary(models_ARUQ56_flowering)$coefficients[1,4], 4)),
  #              color = "black", size = 5)) +  # Intercept annotation with p-value
  #geom_text(data = predictions_ARUQ56_flowering %>%
  #            group_by(location_id) %>%
  #            slice(1),
  #          aes(x = 1, y = 1400, label = paste("Slope =", round(coef(models_ARUQ56_flowering)[2], 2), "\np-value =", round(summary(models_ARUQ56_flowering)$coefficients[2,4], 4)),
  #              color = "black", size = 5)) +  # Slope annotation with p-value
  labs(
    title = "Total Flight Buzz Duration vs Flowering Count",
    x = "Daily Flowering Count",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  theme_classic() +
  facet_wrap(~location_id)





#### TEST ARUQ56 ----
# Fit linear model
model_merged_data_clean <- lm(total_duration_day ~ total_flower_count + mean_value, 
                         data = merged_data_clean)

summary(model_merged_data_clean)
plot(model_merged_data_clean)  # Residuals vs. fitted values
qqnorm(resid(model_merged_data_clean))
qqline(resid(model_merged_data_clean))


merged_data_clean$datetime <- as.POSIXct(merged_data_clean$datetime)  # Ensure datetime is POSIXct
merged_data_clean$datetime_num <- as.numeric(merged_data_clean$datetime) / (60*60*24)  # Convert to days


# Fit GAMM model
model_merged_gam <- gam(total_duration_day ~ 
                         s(datetime_num, bs = "cs") +  
                         total_flower_count + mean_value + location_id,                
                       data = merged_data_clean, 
                       method = "REML")

summary(model_merged_gam)


# Plot effect of date on activity
plot(model_merged_gam, pages = 1, shade = TRUE)

# Check residuals
gam.check(model_merged_gam)


# Fit interaction GAMM model
model_merged_gam_interaction <- gam(total_duration_day ~ 
                                     s(datetime_num, bs = "cs") +  
                                     total_flower_count * mean_value + location_id,  # Interaction term
                                   data = merged_data_clean, 
                                   method = "REML")

summary(model_merged_gam_interaction)


# Visualize interaction
visreg(model_merged_gam_interaction, "total_flower_count", by = "mean_value", overlay = TRUE)









#### ARUQ4 ----
# Merge the flower count data with the other datasets
merged_data_aruq4 <- merge(summary_pred_duration_agg_ARUQ4_2024, flower_summary, by = c("date", "location_id"))
merged_data_aruq4 <- merge(merged_data_aruq4, aruq4_data_plot, by = c("date", "location_id"))


# Filter out rows where total_flower_count is zero
merged_data_filtered_aruq4 <- merged_data_aruq4 %>%
  filter(total_flower_count > 0)

# Plot
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = merged_data_aruq4, aes(x = date, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Plot the temperature curve
  geom_smooth(data = merged_data_aruq4, aes(x = date, y = mean_value), color = "orange3", method = "loess", alpha = 0.7, se = FALSE) +
  geom_smooth(data = merged_data_aruq4, aes(x = date, y = total_duration_day/24), color = "skyblue4", method = "loess", se = FALSE) +
  geom_line(data = merged_data_aruq4, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = merged_data_aruq4, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Add the total flower count as a new series
  geom_smooth(data = merged_data_filtered_aruq4, aes(x = date, y = total_flower_count), color = "darkolivegreen", size = 1.2, se = FALSE) +
  geom_point(data = merged_data_filtered_aruq4, aes(x = date, y = total_flower_count), color = "darkolivegreen", size = 2, alpha = 0.7, se = FALSE) +
  
  # Customizing the plot
  labs(
    title = "Flight Buzzes, Temperature, and Flower Count Over Time - ARUQ4",
    x = "Datetime", 
    y = "Mean Flight Buzz Duration per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature | Green: Flower Count"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 25),
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())
  ) +
  
  # Ensure the x-axis is properly formatted and limited
  scale_x_date(limits = c(as.Date("2024-06-20"), max(merged_data_aruq4$date, na.rm = TRUE))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom")




# Filter out rows with NA, NaN, or Inf values
merged_data_clean_aruq4 <- merged_data_aruq4 %>%
  filter(!is.na(total_duration_day), !is.na(total_flower_count)) %>%
  filter(!is.nan(total_duration_day), !is.nan(total_flower_count)) %>%
  filter(total_duration_day != Inf, total_flower_count != Inf)


hist(log(merged_data_clean_aruq4$total_flower_count))

# Fit separate models for each location_id
models_ARUQ4_flowering <- merged_data_clean_aruq4 %>%
  group_by(location_id) %>%
  do(model = lm(log(total_duration_day) ~ total_flower_count, data = .))

# Generate predictions for each location_id
predictions_ARUQ4_flowering <- merged_data_clean_aruq4 %>%
  group_by(location_id) %>%
  do({
    model <- lm(log(total_duration_day) ~ total_flower_count, data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      total_flower_count = .$total_flower_count,  # Ensure this column is included
      total_duration_day = .$total_duration_day,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })


# Plotting
ggplot(merged_data_clean_aruq4, aes(x = total_flower_count, y = total_duration_day)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_line(data = predictions_ARUQ4_flowering, aes(x = total_flower_count, y = predicted), color = "skyblue4", size = 1) +
  geom_ribbon(data = predictions_ARUQ4_flowering, aes(x = total_flower_count, ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +
  # Add model coefficients and p-values as text annotations
  # geom_text(data = predictions_ARUQ56_flowering %>%
  #            group_by(location_id) %>%
  #            slice(1), 
  #          aes(x = 1, y = 1600, label = paste("Intercept =", round(coef(models_ARUQ56_flowering)[1], 2), "\np-value =", round(summary(models_ARUQ56_flowering)$coefficients[1,4], 4)),
  #              color = "black", size = 5)) +  # Intercept annotation with p-value
  #geom_text(data = predictions_ARUQ56_flowering %>%
  #            group_by(location_id) %>%
  #            slice(1),
  #          aes(x = 1, y = 1400, label = paste("Slope =", round(coef(models_ARUQ56_flowering)[2], 2), "\np-value =", round(summary(models_ARUQ56_flowering)$coefficients[2,4], 4)),
  #              color = "black", size = 5)) +  # Slope annotation with p-value
  labs(
    title = "Total Flight Buzz Duration vs Flowering Count - ARUQ4",
    x = "Daily Flowering Count",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  theme_classic() 



#### Test ARUQ4 ----
# Fit linear model
model_aruq4_merged <- lm(total_duration_day ~ total_flower_count + mean_value, 
              data = merged_data_aruq4)

summary(model_aruq4_merged)
plot(model_aruq4_merged)  # Residuals vs. fitted values
qqnorm(resid(model_aruq4_merged))
qqline(resid(model_aruq4_merged))


merged_data_aruq4$datetime <- as.POSIXct(merged_data_aruq4$datetime)  # Ensure datetime is POSIXct
merged_data_aruq4$datetime_num <- as.numeric(merged_data_aruq4$datetime) / (60*60*24)  # Convert to days


# Fit GAMM model
model_aruq4_gam <- gam(total_duration_day ~ 
                         s(datetime_num, bs = "cs") +  
                         total_flower_count +       
                         mean_value,                
                       data = merged_data_aruq4, 
                       method = "REML")

summary(model_aruq4_gam)


# Plot effect of date on activity
plot(model_aruq4_gam, pages = 1, shade = TRUE)

# Check residuals
gam.check(model_aruq4_gam)


# Fit interaction GAMM model
model_aruq4_gam_interaction <- gam(total_duration_day ~ 
                                     s(datetime_num, bs = "cs") +  
                                     total_flower_count * mean_value,  # Interaction term
                                   data = merged_data_aruq4, 
                                   method = "REML")

summary(model_aruq4_gam_interaction)


# Visualize interaction
visreg(model_aruq4_gam_interaction, "total_flower_count", by = "mean_value", overlay = TRUE)






#### DEBUG ----
aru_temp_hourly_micro <- aru_temp_hourly_micro %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day),
         hour = as.numeric(hour))

aruq0_data_plot_h <- aru_temp_hourly_micro %>%
  filter(aru_name == "ARUQ0") %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, hour), format="%Y-%m-%d %H", tz="UTC"))

# Aggregate total duration above threshold per day
summary_pred_duration_agg_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with daily microclimate data
merged_data_aruq0 <- summary_pred_duration_agg_ARUQ0 %>%
  left_join(aruq0_data_plot, by = c("date" = "datetime")) %>%
  filter(!is.na(month))  # Remove NAs in month column

# Aggregate total duration above threshold per hour
summary_pred_duration_agg_hourly_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date_time_hour = floor_date(datetime, "hour")) %>%
  group_by(date_time_hour) %>%
  summarise(total_duration_hour = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with hourly microclimate data
merged_data_aruq0_hourly <- summary_pred_duration_agg_hourly_ARUQ0 %>%
  left_join(aruq0_data_plot_h, by = c("date_time_hour" = "datetime"))

# Plot total flight buzz duration vs. mean temperature
ggplot(merged_data_aruq0, aes(x = mean_value, y = total_duration_day)) +
  geom_point(aes(color = as.factor(month))) +
  geom_smooth(method = "lm", aes(color = as.factor(month)), se = FALSE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature - ARUQ0",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  geom_vline(xintercept = c(6, 12.6), color = c("orange", "orange4"), linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Log-transformed plot
ggplot(merged_data_aruq0, aes(x = mean_value, y = log(total_duration_day))) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "skyblue4", se = TRUE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Log Total Flight Buzz Duration (s)") +
  theme_classic()
