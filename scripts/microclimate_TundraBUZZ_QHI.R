# ====================================================
# Script Name: microclimate_TundraBUZZ_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Dr. Jeremy Borderieux, Alex Beauchemin
# Date Created: 2025-03-27
# Last Modified: 2025-03-27
# Description: Reading TOMST data with myclim
# Dependencies: TBD, location_mapping_TundraBUZZ.csv
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(lubridate)
library(hms)
library(myClim) ## logger data reading
library(foreach) ## efficient loop
library(data.table) ## efficient data.frame  
library(stringr) ## efficient character manipulation

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Load files
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")
location_mapping <- location_mapping %>%
  select(location_id, tomst_id, microclimate)

# Set file path
tomst_file_path <- "/Volumes/TundraBUZZ/data/raw/QHI_TOMST_2024_fixed"

# Set seed for repeatability
set.seed(123)




#### Creating a meta_data file ----
# List full file paths and just filenames from TOMST directory
list_path <- list.files(tomst_file_path, full.names = TRUE)
list_files <- list.files(tomst_file_path, full.names = FALSE)

# Extract locality names from filenames (e.g., "TOMST123_QHI")
locality_name <- str_extract(list_files, "TOMST\\d+_QHI")

# Create a table of TOMST data file paths and associated metadata
files_table <- data.table(
  path = list_path,
  locality_id = locality_name,
  data_format = "TOMST"
)

# Create a separate table for locality metadata (e.g., timezone offsets)
locality_metadata <- data.table(
  locality_id = locality_name,
  tz_offset = -60 * 7  # Timezone offset in minutes (e.g., -7 hours for MST)
)




#### Read and Process Logger Data ----

# Read TOMST data using file and locality metadata
tms.f <- mc_read_data(files_table, locality_metadata)

# Overview and diagnostics
mc_info_count(tms.f)       # Count of localities, loggers, sensors
mc_info(tms.f)             # Summary per sensor
mc_info_meta(tms.f)        # Metadata per locality
mc_info_clean(tms.f)       # Cleaning log

# Optional: visualize data to identify unreliable loggers
# mc_plot_raster(tms.f)

# Remove unreliable sensors/localities
tms.f <- mc_filter(tms.f, localities = c("TOMST14_QHI", "TOMST8_QHI"), reverse = TRUE)

# Crop data to start from installation date (adjust date as needed)
tms.f <- mc_prep_crop(tms.f, start = as.POSIXct("2022-07-31", tz = "UTC"))

# ---- Virtual Sensor Calculations ----

# Volumetric Water Content (VWC) from raw soil moisture signal
# `soiltype = "universal"` is a safe default
tms.calc <- mc_calc_vwc(tms.f, soiltype = "universal")

# Growing Degree Days (GDD) from soil temperature at 3 cm depth
tms.calc <- mc_calc_gdd(tms.calc, sensor = "TMS_T3")

# Freezing Degree Days (FDD) from same sensor
tms.calc <- mc_calc_fdd(tms.calc, sensor = "TMS_T3")

# Estimate snow presence from 2 cm air temperature
tms.calc <- mc_calc_snow(tms.calc, sensor = "TMS_T2")




#### Aggregate into hourly data ----

# Aggregate to hourly statistics: mean, min, max
hourly.tms <- mc_agg(
  tms.calc,
  fun = c("mean", "min", "max"),
  period = "hour",
  min_coverage = 1,
  use_utc = TRUE  # use UTC time in aggregation
)

# Export the object out of the MC framework
hourly_dt <- data.table(mc_reshape_long(hourly.tms), na.rm = T)

# Add a local timezone version of datetime
hourly_dt[, datetime_local := with_tz(datetime, tz = "America/Whitehorse")]

# Select for airtemp
hourly_dt_T3 <- hourly_dt %>%
  filter(sensor_name == "TMS_T3_mean") ##only taking TMS_T3_mean
hourly_dt_T3 <- hourly_dt_T3 %>%
  select(locality_id, value, datetime_local)

# Export object
hourly_dt_T3[, serial_number:=NULL] ##removing useless col
hourly_dt_T3[, year := year(datetime_local)]
hourly_dt_T3[, month := month(datetime_local)]
hourly_dt_T3[, day := day(datetime_local)]
hourly_dt_T3[, week := week(datetime_local)]

# Filter for only 2024
hourly_dt_T3 <- hourly_dt_T3[year == 2024,]

# Filter for summer months
hourly_dt_T3_filtered <- hourly_dt_T3 %>%
  filter(month %in% c(6:8))

# Join with location data
hourly_temp_mapped <- location_mapping %>%
  mutate(tomst_id = paste0("TOMST", tomst_id, "_QHI")) %>%
  left_join(hourly_dt_T3_filtered, by = c("tomst_id" = "locality_id")) %>%
  mutate(datetime = datetime_local) %>%
  select(-tomst_id, -datetime_local)

write_csv(hourly_temp_mapped, "/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_hourly.csv")



#### Aggregate into daily data ----
daily.tms <- mc_agg(
  tms.calc,
  fun = c("mean", "percentile"),
  percentiles = c(0.05, 0.95),
  period = "day",
  min_coverage = 1,
  use_utc = FALSE
)

# Export the object out of the MC framework
daily_dt <- data.table(mc_reshape_long(daily.tms), na.rm = T)

# Clean and enhance metadata
daily_dt[, serial_number := NULL]
daily_dt[, datetime := ymd(datetime)]  # Ensure it's a Date
daily_dt[, year := year(datetime)]
daily_dt[, month := month(datetime)]
daily_dt[, day := day(datetime)]
daily_dt[, week := week(datetime)]

daily_dt_T3 <- daily_dt %>%
  filter(sensor_name == "TMS_T3_mean") ##only taking TMS_T3_mean

daily_dt_T3 <- daily_dt_T3 %>%
  select(locality_id, datetime, value, year, month, day, week)

# Filter for only 2024
daily_dt_T3 <- daily_dt_T3[year == 2024,]

# Filter for summer months
daily_dt_T3_filtered <- daily_dt_T3 %>%
  filter(month %in% c(6:8))

# Join with location data
daily_temp_mapped <- location_mapping %>%
  mutate(tomst_id = paste0("TOMST", tomst_id, "_QHI")) %>%
  left_join(daily_dt_T3_filtered, by = c("tomst_id" = "locality_id")) %>%
  select(-tomst_id)

write_csv(daily_temp_mapped, "/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_daily.csv")

# Order sites by mean summer temperature based on daily temperatures
ordered_site_temp_summer <- daily_temp_mapped %>%
  group_by(location_id) %>%
  summarize(summer_temp = mean(value, na.rm = TRUE)) %>%
  arrange(desc(summer_temp))

write_csv(ordered_site_temp_summer, "/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")





