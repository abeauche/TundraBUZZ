# ====================================================
# Script Name: flight_buzzes_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-21
# Last Modified: 2025-03-25
# Description: This script TBD.
# Dependencies: TBD, location_mapping_TundraBUZZ.csv
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(lubridate)
library(suncalc)
library(hms)

# Set working directory (if needed)
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Load data
ARUQ0_2024_pred <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ0_raw.csv")
ARUQ456_2024_pred <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ456_raw.csv")
location_mapping <- read.csv("./data/raw/location_mapping_TundraBUZZ.csv", stringsAsFactors = TRUE)

# Set seed for repeatability
set.seed(123)

# Extract datetime from file names
ARUQ0_2024_pred <- ARUQ0_2024_pred %>%
  mutate(datetime = as.POSIXct(gsub(".*_(\\d{8})_(\\d{6})\\.wav$", "\\1 \\2", file), format="%Y%m%d %H%M%S"))


#### Clean ARUQ456 dataset ----
# Extract aru_id and clean file structure naming
ARUQ456_2024_pred <- ARUQ456_2024_pred %>%
  mutate(
    file = sub("^.*\\\\", "", file),  # Remove path before backslash
    aru_id = str_extract(file, "ARUQ\\d+"),  # Extract "ARUQ5" or similar
    datetime = str_extract(file, "\\d{8}_\\d{6}")  # Extract "20240626_013000"
  )

# Filter out files not properly named
ARUQ456_2024_pred <- ARUQ456_2024_pred %>%
  filter(!is.na(aru_id))

# Change aru_id to factor, check levels and table
ARUQ456_2024_pred$aru_id <- as.factor(ARUQ456_2024_pred$aru_id)
levels(ARUQ456_2024_pred$aru_id)
table(ARUQ456_2024_pred$aru_id)

# Merge to replace aru_id with location_id
ARUQ456_2024_pred_mapped <- ARUQ456_2024_pred %>%
  left_join(location_mapping, by = "aru_id") %>%
  select(-c(aru_id, polcam_id,tomst_id,site,year))  # Remove aru_id, now using location_id

# Mutate datetime to POSIXct format
ARUQ456_2024_pred_mapped <- ARUQ456_2024_pred_mapped %>% 
  mutate(datetime = as.POSIXct(datetime, format="%Y%m%d_%H%M%S", tz="UTC")  # Convert to POSIXct
)

# Save csv
write.csv(ARUQ456_2024_pred_mapped, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ456_2024_pred_cleaned.csv", row.names = FALSE)

#### ----

#### Work with ARUQ0 data ----
# Define threshold and filter data
threshold <- 8  
ARUQ0_2024_pred_above_threshold <- ARUQ0_2024_pred %>%
  filter(BUZZ > threshold) %>%
  mutate(duration_above_threshold = 0.15)  # Each segment is 0.3s, so each overlap segment is 0.15s

# Summarize total duration above threshold per datetime
summary_pred_duration_ARUQ0_2024 <- ARUQ0_2024_pred_above_threshold %>%
  group_by(datetime) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold), .groups = "drop")

# Convert datetime to UTC-7
summary_pred_duration_ARUQ0_2024_tz <- summary_pred_duration_ARUQ0_2024 %>%
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT+7"),
         time_of_day = hms::as_hms(format(datetime, "%H:%M:%S")))

# Plot flight buzzes over time
ggplot(summary_pred_duration_ARUQ0_2024, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  
  labs(title = "Flight Buzzes Over Time - ARUQ0 (Threshold = 8)",
       x = "Datetime", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Group by week for aggregation
summary_pred_duration_ARUQ0_2024_month <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(week = floor_date(datetime, "week"))

# Get sunrise and sunset times
lat <- 69.57
lon <- -138.91
summary_pred_duration_ARUQ0_2024_sun <- summary_pred_duration_ARUQ0_2024_month %>%
  mutate(date = as.Date(datetime))

sun_times <- getSunlightTimes(data = data.frame(date = unique(summary_pred_duration_ARUQ0_2024_sun$date), lat = lat, lon = lon),
                              keep = c("sunrise", "sunset"))

# Plot flight buzzes by time of day
ggplot(summary_pred_duration_ARUQ0_2024_month, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  labs(title = "Flight Buzzes Over a 24-hour Period - ARUQ0 (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~week)

# Read in microclimate data
aru_temp_daily_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv")
aru_temp_hourly_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv")

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

# Plot total flight buzz duration vs. mean temperature
ggplot(merged_data_ARUQ56, aes(x = mean_value, y = total_duration_day)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  #geom_vline(xintercept = c(6, 12.6), color = c("orange", "orange4"), linetype = "dashed", size = 1, alpha = 0.7) +
  #annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  #annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)



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
