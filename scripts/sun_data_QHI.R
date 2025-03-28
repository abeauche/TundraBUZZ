# ====================================================
# Script Name: sun_data_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-28
# Last Modified: 2025-03-28
# Description: This script uses the suncalc library to obtain sunrise, sunset, and sun altitude data for Qikiqtaruk - Herschel Island for the period from June 21, 2024, at 21:00:00, to August 12, 2024, at 23:59:59.
# Dependencies: summary_flightbuzzes_ARUQ_2024.csv
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(lubridate)
library(suncalc)
library(hms)

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

# Ensure datetime is in "America/Whitehorse" 
summary_flightbuzzes_ARUQ_2024 <- summary_flightbuzzes_ARUQ_2024 %>%
  mutate(datetime = force_tz(datetime, tzone = "America/Whitehorse"))



#### SUN DATA ----

### Sun altitude data

# Generate POSIXct list of times for 2024 summer
TundraBUZZ_times_2024 <- seq(from = as.POSIXct("2024-06-21 21:00:00", tz = "America/Whitehorse"), 
                             to = as.POSIXct("2024-08-12 23:59:59", tz = "America/Whitehorse"), by = "hour")

# Get sun data for each hour of the 2024 summer
sun_data_2024 <- getSunlightPosition(TundraBUZZ_times_2024, lat, lon)

write_csv(sun_data_2024, "/Users/alexandrebeauchemin/TundraBUZZ_github/data/raw/QHI_sun_data_2024.csv")


### Sunrise and sunset data

# Extract unique dates
unique_dates <- unique(summary_flightbuzzes_ARUQ_2024$date_utc)

# Get sunlight times for the given location and dates
sun_times <- getSunlightTimes(
  data = data.frame(date = unique_dates, lat = lat, lon = lon),
  keep = c("sunrise", "sunset")
)

# Convert suntimes to proper timezone
sun_times_tz <- sun_times %>%
  mutate(sunrise = with_tz(sunrise, tzone = "America/Whitehorse"),
         sunset = with_tz(sunset, tzone = "America/Whitehorse"), 
         week = floor_date(date, "week")) 


# Convert sunset times to numeric (seconds since midnight) and handle wraparound
sun_times_tz <- sun_times_tz %>%
  mutate(
    # Convert to America/Whitehorse timezone to ensure correct offset
    sunrise = with_tz(sunrise, tzone = "America/Whitehorse"),
    sunset = with_tz(sunset, tzone = "America/Whitehorse"),
    
    # Get seconds since midnight for sunrise and sunset
    sunrise_seconds = as.numeric(sunrise) %% 86400, 
    sunset_seconds = as.numeric(sunset) %% 86400, 
    
    # Handle wraparound for sunset (if negative, add 86400 seconds)
    sunset_seconds = if_else(sunset_seconds < 0, sunset_seconds + 86400, sunset_seconds)
  )

# Calculate average sunrise and sunset times per week
average_sun_times <- sun_times_tz %>%
  group_by(week) %>%
  summarise(
    avg_sunrise = mean(sunrise_seconds, na.rm = TRUE),   # Average sunrise in seconds
    avg_sunset = mean(sunset_seconds, na.rm = TRUE)      # Average sunset in seconds
  ) %>%
  mutate(
    # Convert seconds back to POSIXct with America/Whitehorse as the timezone
    avg_sunrise = as.POSIXct(avg_sunrise, origin = "1970-01-01", tz = "America/Whitehorse"),
    avg_sunset = as.POSIXct(avg_sunset, origin = "1970-01-01", tz = "America/Whitehorse"),
    
    # Ensure the result is properly formatted as hh:mm:ss
    avg_sunrise = format(avg_sunrise, "%H:%M:%S"),
    avg_sunset = format(avg_sunset, "%H:%M:%S")
  )

# Convert avg_sunrise and avg_sunset from character to POSIXct (hh:mm:ss format)
average_sun_times <- average_sun_times %>%
  mutate(
    avg_sunrise = as.POSIXct(avg_sunrise, format = "%H:%M:%S", tz = "America/Whitehorse"),
    avg_sunset = as.POSIXct(avg_sunset, format = "%H:%M:%S", tz = "America/Whitehorse")
  )

# Add 1 hour (3600 seconds) to avg_sunrise and avg_sunset
average_sun_times <- average_sun_times %>%
  mutate(
    avg_sunrise = avg_sunrise + 3600,  # Add 1 hour to avg_sunrise
    avg_sunset = avg_sunset + 3600     # Add 1 hour to avg_sunset
  )

# Only keep hh:mm:ss format
average_sun_times <- average_sun_times %>% 
  mutate(avg_sunrise = as_hms(avg_sunrise),  # Convert seconds to hms format
         avg_sunset = as_hms(avg_sunset)     # Convert seconds to hms format
  )
# View the result
average_sun_times

# Filter out NAs
average_sun_times_filtered <- average_sun_times %>%
  filter(!avg_sunrise == "NA")

write_csv(average_sun_times_filtered, "/Users/alexandrebeauchemin/TundraBUZZ_github/data/raw/QHI_sunrise_sunset_filtered.csv")
