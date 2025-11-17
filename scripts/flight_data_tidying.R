# ====================================================
# Script Name: flight_data_tidying.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-28
# Last Modified: 2025-04-10
# Description: This script wrangles raw recognizer outputs, combines them into a single organized dataframe (ARUQ_2024_pred_mapped.csv) with columns for file, start_time, end_time, BUZZ, datetime, location_id, microclimate. The script then sets a threshold and filters observations with a score above the confidence threshold, and exports the dataset with a new column for duration_above_threshold (ARUQ_2024_bumblebee_detections.csv). Last, the script aggregates duration above threshold per datetime (summary_flightbuzzes_ARUQ2024.csv) and per date (daily_summary_flightbuzzes_ARUQ2024.csv) and exports these to the local drive. 
# Dependencies: predictions_ARUQ0_raw.csv, predictions_ARUQ1_raw.csv, predictions_ARUQ2_raw.csv, predictions_ARUQ3_raw.csv, predictions_ARUQ4_raw.csv, predictions_ARUQ56_raw.csv, predictions_ARUQ7_raw.csv, predictions_ARUQ9_raw.csv, predictions_ARUQ10_raw.csv, location_mapping_TundraBUZZ.csv, R packages: tidyverse, lubridate, suncalc, hms.
# ====================================================

#### SETUP ----
# Load required libraries
# Load required libraries
library(tidyverse)
library(lubridate)
library(suncalc)
library(hms)

# Set working directory
setwd("/Volumes/TundraBUZZ/")

# Set seed for repeatability
set.seed(123)

# Load data
ARUQ0_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ00_pred.csv")
ARUQ1_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ01_pred.csv")
ARUQ2_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ02_pred.csv")
ARUQ3_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ03_pred.csv")
ARUQ4_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ04_pred.csv")
ARUQ5_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ05_pred.csv")
ARUQ6_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ06_pred.csv")
ARUQ7_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ07_pred.csv")
ARUQ8_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ08_pred.csv")
ARUQ10_2025_pred_raw <- read_csv("./outputs/recognizer_outputs/QHI_2025/QHI25_ARUQ10_pred.csv")
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")



#### CLEAN UP DATASETS ----

#### Merge dataframes ----
# List ARUQ dataframes
ARUQ_list <- mget(ls(pattern = "ARUQ\\d+_2025_pred_raw"))

# Bind dataframes together
ARUQ_2025_pred_raw <- bind_rows(ARUQ_list)
rm(list = ls(pattern = "ARUQ\\d+_2025_pred_raw"))
rm(ARUQ_list)

#### Clean ARUQ_2024_pred_raw dataset ----
# Extract aru_id and clean file structure naming
ARUQ_2025_pred_raw <- ARUQ_2025_pred_raw %>%
  mutate(
    file = sub("^.*\\\\", "", file),  # Remove any preceding path
    aru_number = str_extract(file, "(?<=QHI25-)\\d+"),  # Extract just the number after QHI25-
    aru_id = paste0("ARUQ", aru_number),  # Format as "ARUQ10"
    datetime = str_extract(file, "\\d{8}_\\d{6}")  # Extract "20250613_200000"
  ) %>%
  filter(!is.na(aru_id))

# Correct for missing time (i.e., "000000") in filenames
ARUQ_2025_pred_raw <- ARUQ_2025_pred_raw %>%
  mutate(
    datetime = ifelse(str_sub(datetime, 10, 15) == "000000", 
                      paste0(str_sub(datetime, 1, 8), "_000000"),  # Ensure "000000" for midnight
                      datetime)
  )

# Change aru_id to factor, check levels and table
ARUQ_2025_pred_raw$aru_id <- as.factor(ARUQ_2025_pred_raw$aru_id)
levels(ARUQ_2025_pred_raw$aru_id)
table(ARUQ_2025_pred_raw$aru_id)

ARUQ_2025_pred_raw <- ARUQ_2025_pred_raw %>%
  filter(!aru_id %in% c("ARUQ28", "ARUQNA")) %>%
  mutate(
    aru_id = str_replace(aru_id, "ARUQ0(\\d)", "ARUQ\\1")
  )

# Merge to replace aru_id with location_id
ARUQ_2025_pred_mapped <- ARUQ_2025_pred_raw %>%
  left_join(location_mapping, by = "aru_id") %>%
  select(-c(aru_id, polcam_id,tomst_id,site,year))  # Remove aru_id, now using location_id
table(ARUQ_2025_pred_mapped$location_id)
rm(ARUQ_2025_pred_raw)

# Save csv
write_csv(ARUQ_2025_pred_mapped, "./outputs/recognizer_outputs/clean/ARUQ_2025_pred_mapped.csv")

# ARUQ_2024_pred_mapped <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ_2024_pred_mapped.csv")




#### THRESHOLD and FILTER DATA ----

# Define threshold and filter data
threshold <- 8  
ARUQ_2025_bumblebee_detections <- ARUQ_2025_pred_mapped %>%
  filter(BUZZ > threshold) %>%
  mutate(duration_above_threshold = 0.15)  # Each segment is 0.3s, so each overlap segment is 0.15s

# Mutate datetime to POSIXct format
ARUQ_2025_bumblebee_detections <- ARUQ_2025_bumblebee_detections %>% 
  mutate(datetime = as.POSIXct(datetime, format="%Y%m%d_%H%M%S", tz="UTC")  # Convert to POSIXct
  )

# Save csv
write_csv(ARUQ_2025_bumblebee_detections, "./outputs/recognizer_outputs/clean/ARUQ_2025_bumblebee_detections.csv")
rm(ARUQ_2025_pred_mapped)

# Summarize total duration above threshold per datetime
summary_flightbuzzes_ARUQ_2025 <- ARUQ_2025_bumblebee_detections %>%
  group_by(datetime, location_id, microclimate2) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold), .groups = "drop")

# Convert datetime to POSIXct
summary_flightbuzzes_ARUQ_2025 <- summary_flightbuzzes_ARUQ_2025 %>% 
  mutate(datetime = as.POSIXct(datetime, format="%Y%m%d_%H%M%S", tz="UTC")  # Convert to POSIXct
  )

# Ensure datetime is in "America/Whitehorse" 
summary_flightbuzzes_ARUQ_2025 <- summary_flightbuzzes_ARUQ_2025 %>%
  mutate(datetime = force_tz(datetime, tzone = "America/Whitehorse"))

# Check timezone
attr(summary_flightbuzzes_ARUQ_2025$datetime, "tzone")

# Create time_of_day column
summary_flightbuzzes_ARUQ_2025 <- summary_flightbuzzes_ARUQ_2025 %>%
  mutate(time_of_day = hms::as_hms(format(datetime, "%H:%M:%S")))

# Group by week for aggregation
summary_flightbuzzes_ARUQ_2025 <- summary_flightbuzzes_ARUQ_2025 %>%
  mutate(week = floor_date(datetime, "week"))

# Extract the date part from the datetime column and ensure it's in Date format
summary_flightbuzzes_ARUQ_2025 <- summary_flightbuzzes_ARUQ_2025 %>%
  mutate(date_utc = as.Date(datetime))

write_csv(summary_flightbuzzes_ARUQ_2025, "./outputs/recognizer_outputs/clean/summary_flightbuzzes_ARUQ_2025.csv")



# Summarize total duration per local date
daily_summary_flightbuzzes_ARUQ_2025 <- summary_flightbuzzes_ARUQ_2025 %>%
  mutate(datetime_local = force_tz(datetime, tzone = "UTC"), 
         date = as.Date(datetime_local)) %>%
  group_by(date, location_id, microclimate2) %>%
  summarize(daily_duration_above_threshold = sum(total_duration_above_threshold), .groups = "drop")

# Export
write_csv(daily_summary_flightbuzzes_ARUQ_2025, "./outputs/recognizer_outputs/clean/daily_summary_flightbuzzes_ARUQ_2025.csv")





ggplot(daily_summary_flightbuzzes_ARUQ_2025, 
       aes(x = date, y = daily_duration_above_threshold)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, aes(fill = microclimate2, color = microclimate2)) +  # line of best fit with error ribbon
  facet_wrap(~ location_id, scales = "free_x") +  # keep y fixed, x free
  scale_y_continuous(limits = c(0, 2000)) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  scale_fill_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +          
  labs(
    x = "2025 Growing Season",
    y = "Daily Bumblebee Flight Buzz Detection (s)",
    color = "Microclimate",
    fill = "Microclimate"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
