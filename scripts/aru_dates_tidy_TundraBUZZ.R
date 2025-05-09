# ====================================================
# Script Name: aru_dates_tidy_TundraBUZZ.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-04-12
# Last Modified: 2025-04-22
# Description: This script extracts all recording dates and creates a completed dataframe with temperature readings.
# Dependencies: flight_buzz_daily.csv, flight_buzz_hourly.csv, summary_flightbuzzes_ARUQ_2024.csv, mean_summer_temp_TundraBUZZ.csv, location_mapping_TundraBUZZ.csv, R packages: tidyverse, lubridate, suncalc, hms, lme4, lmerTest, mgcv, visreg, patchwork, cowplot, viridis.
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(lubridate)
library(hms)
library(lme4)
library(lmerTest) 
library(mgcv)
library(visreg)
library(patchwork)
library(cowplot)
library(viridis)
library(brms)
library(tidybayes)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

# Set latitude and longitude for Qikiqtaruk - Herschel Island
lat <- 69.57
lon <- -138.91


#### Using tidy datasets ----
# Load data
flight_buzz_daily <- read_csv("/Volumes/TundraBUZZ/data/clean/flight_buzz_daily.csv")
flight_buzz_hourly <- read_csv("/Volumes/TundraBUZZ/data/clean/flight_buzz_hourly.csv")
summary_flightbuzzes_ARUQ_2024 <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/summary_flightbuzzes_ARUQ_2024.csv")
mean_temp <- read_csv("/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")

# Make sure datetime is in proper format
summary_flightbuzzes_ARUQ_2024 <- summary_flightbuzzes_ARUQ_2024 %>%
  mutate(datetime = str_trim(datetime),
         datetime = str_replace_all(datetime, "/", "-"),  # replace slashes with dashes
         datetime = if_else(str_detect(datetime, "^\\d{4}-\\d{2}-\\d{2}$"),
                            paste0(datetime, " 00:00:00"),
                            datetime),
         datetime = ymd_hms(datetime, tz = "UTC"))

summary_flightbuzzes_ARUQ_2024 <- summary_flightbuzzes_ARUQ_2024 %>%
  mutate(datetime = with_tz(datetime, tzone = "America/Whitehorse"))


# Define the expected time step (30 minutes in this case)
time_step <- as.difftime(30, units = "mins")

# Round up to next 30-minute mark
round_up_30min <- function(time) {
  ceiling_date(time, unit = "30 minutes")
}

# Round down to previous 30-minute mark
round_down_30min <- function(time) {
  floor_date(time, unit = "30 minutes")
}

# Cleaned recording window with rounded times
recording_windows <- summary_flightbuzzes_ARUQ_2024 %>%
  group_by(location_id) %>%
  summarise(
    start = round_up_30min(min(datetime)),
    end   = round_down_30min(max(datetime)),
    .groups = "drop"
  )


# Generate full sequence of expected times per location
expected_times <- recording_windows %>%
  rowwise() %>%
  mutate(datetime_seq = list(seq(from = start, to = end, by = time_step))) %>%
  unnest(datetime_seq) %>%
  select(location_id, datetime = datetime_seq)



# Generate tibble with problem dates per location
bad_dates <- tibble(
  location_id = c("COOL2", "MOD1", "WARM2"), 
  error_date = as.POSIXct(c("2024-08-02 06:10:00", "2024-07-26 13:53:00", "2024-08-01 16:28:00"), tz = "America/Whitehorse"),
  re_start = as.POSIXct(c("2024-08-04 13:30:00", "2024-08-05 13:00:00", "2024-08-06 16:00:00"), tz = "America/Whitehorse")
)

# Merge bad dates with the expected_times dataset
recording_windows_clean <- expected_times %>%
  left_join(bad_dates, by = "location_id") %>%
  filter(is.na(error_date) | !(datetime >= error_date & datetime < re_start)) %>%
  select(-error_date, -re_start) 


# Order locations per mean_summer_temp
ordered_locations <- mean_temp %>%
  pull(location_id)

# Visualize active periods as tiles with space between tiles
active_aru_dates_plot <- ggplot(recording_windows_clean, aes(x = datetime, y = factor(location_id, levels = rev(ordered_locations)))) +
  geom_tile(color = "forestgreen", width = 0.8, height = 0.8) +  # Adjust width and height for space
  theme_classic() +
  labs(x = "2024 Growing Season", y = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10))  # Adjust y-axis text size for better readability

recording_windows_clean <- recording_windows_clean %>%
  left_join(location_mapping, by = "location_id") %>%
  select(location_id, datetime, microclimate2)

# Visualize active periods as tiles with space between tiles
active_aru_dates_plot <- ggplot(recording_windows_clean, aes(
  x = datetime,
  y = factor(location_id, levels = rev(ordered_locations)),
  fill = factor(microclimate2), 
  colour = factor(microclimate2)
)) +
  geom_tile(width = 0.8, height = 0.8) +
  theme_classic() +
  labs(x = "2024 Growing Season", y = "Site", colour = "Microclimate", fill = "Microclimate") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 10)
  ) +
  scale_fill_manual(values = c("grey44", "#440154", "forestgreen", "gold"))+
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold"))

# Save the plot as a PDF
ggsave("./outputs/figures/active_aru_dates_plot.pdf", 
       plot = active_aru_dates_plot,   
       width = 10, height = 6) 


# Join recording_windows_clean with summary_flightbuzzes_ARUQ_2024, filter dates within recording windows
flightbuzzes_ARUQ_2024_complete <- recording_windows_clean %>%
  left_join(summary_flightbuzzes_ARUQ_2024, by = c("location_id", "datetime")) %>%
  mutate(total_duration_above_threshold = ifelse(is.na(total_duration_above_threshold), 0, total_duration_above_threshold))  # Replace NAs with 0

flightbuzzes_ARUQ_2024_complete <- flightbuzzes_ARUQ_2024_complete %>%
  select(-microclimate2)

# Complete dataset
flightbuzzes_ARUQ_2024_complete <- flightbuzzes_ARUQ_2024_complete %>%
  mutate(
    # Extract time of day (hour, minute, second)
    time_of_day = format(datetime, "%H:%M:%S"),
    
    # Extract date in UTC format
    date_utc = as.Date(datetime, tz = "UTC"),
    
    # Extract week number from datetime
    week = floor_date(datetime, unit = "week")
  )

location_microclim <- location_mapping %>%
  select(location_id, microclimate2)

flightbuzzes_ARUQ_2024_complete <- flightbuzzes_ARUQ_2024_complete %>%
  left_join(location_microclim, by = "location_id")

# Save the dataset to a CSV file
write_csv(flightbuzzes_ARUQ_2024_complete, "./data/clean/flight_buzzes_complete_TundraBUZZ.csv")


# Convert to local date
flightbuzzes_ARUQ_2024_date <- flightbuzzes_ARUQ_2024_complete %>%
  mutate(
    date = as.Date(datetime, tz = "America/Whitehorse") 
  )

# Obtain total number of recording hours per date and location
complete_days <- flightbuzzes_ARUQ_2024_date %>%
  group_by(location_id, date) %>%
  summarise(
    num_hours = n_distinct(hour(datetime)),
    .groups = "drop"
  ) %>%
  filter(num_hours == 24)  # Keep only days with 24 hours of data (complete days)

# Aggregate data by day (for complete days)
daily_aggregated_data <- flightbuzzes_ARUQ_2024_date %>%  # Only keep complete days
  group_by(location_id, date) %>%
  summarise(
    daily_duration_above_threshold = sum(total_duration_above_threshold, na.rm = TRUE), 
    .groups = "drop"
  )
# Filter for complete days (by both location_id and date)
daily_aggregated_data <- daily_aggregated_data %>%
  semi_join(complete_days, by = c("location_id", "date"))

# View the aggregated dataset
head(daily_aggregated_data)

daily_flightbuzzes_ARUQ_2024_complete <- daily_aggregated_data %>%
  left_join(location_microclim, by = "location_id")

# Save the dataset to a CSV file
write_csv(daily_flightbuzzes_ARUQ_2024_complete, "./data/clean/daily_flight_buzzes_complete_TundraBUZZ.csv")
