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
library(broom)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Load files
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")
location_mapping <- location_mapping %>%
  select(location_id, tomst_id, microclimate, microclimate2)
location_mapping <- location_mapping %>%
  mutate(localities = paste0("TOMST", tomst_id, "_QHI"))

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

# Create localities vector
localities_to_filter <- location_mapping$localities

# Only filter for relevant locations
tms.f <- mc_filter(tms.f, localities = localities_to_filter)

# Crop data to start from installation date (adjust date as needed)
tms.f <- mc_prep_crop(tms.f, start = as.POSIXct("2022-07-31", tz = "UTC"))

# ---- Virtual Sensor Calculations ----
# Volumetric Water Content (VWC) from raw soil moisture signal
# soiltype = "universal" is a safe default
tms.calc <- mc_calc_vwc(tms.f, soiltype = "universal")

# Growing Degree Days (GDD) from soil temperature at 3 cm depth
tms.calc <- mc_calc_gdd(tms.calc, sensor = "TMS_T3")
tms.calc <- mc_calc_gdd(tms.calc, sensor = "TMS_T3", t_base = 0)

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

# Select for GDD5
hourly_dt_GDD5 <- hourly_dt %>%
  filter(sensor_name == "GDD5_mean") ##only taking TMS_T3_mean
hourly_dt_GDD5 <- hourly_dt_GDD5 %>%
  select(locality_id, value, datetime_local)

# Select for GDD0
hourly_dt_GDD0 <- hourly_dt %>%
  filter(sensor_name == "GDD0_mean") ##only taking TMS_T3_mean
hourly_dt_GDD0 <- hourly_dt_GDD0 %>%
  select(locality_id, value, datetime_local)

# Export object
hourly_dt_T3[, serial_number:=NULL] ##removing useless col
hourly_dt_T3[, year := year(datetime_local)]
hourly_dt_T3[, month := month(datetime_local)]
hourly_dt_T3[, day := day(datetime_local)]
hourly_dt_T3[, week := week(datetime_local)]

hourly_dt_GDD5[, serial_number:=NULL] ##removing useless col
hourly_dt_GDD5[, year := year(datetime_local)]
hourly_dt_GDD5[, month := month(datetime_local)]
hourly_dt_GDD5[, day := day(datetime_local)]
hourly_dt_GDD5[, week := week(datetime_local)]

hourly_dt_GDD0[, serial_number:=NULL] ##removing useless col
hourly_dt_GDD0[, year := year(datetime_local)]
hourly_dt_GDD0[, month := month(datetime_local)]
hourly_dt_GDD0[, day := day(datetime_local)]
hourly_dt_GDD0[, week := week(datetime_local)]

# Filter for only 2024
hourly_dt_T3 <- hourly_dt_T3[year == 2024,]
hourly_dt_GDD5 <- hourly_dt_GDD5[year == 2024,]
hourly_dt_GDD0 <- hourly_dt_GDD0[year == 2024,]

# Filter for summer months
hourly_dt_T3_filtered <- hourly_dt_T3 %>%
  filter(month %in% c(6:8))

# Join with location data
hourly_temp_mapped <- location_mapping %>%
  mutate(tomst_id = paste0("TOMST", tomst_id, "_QHI")) %>%
  left_join(hourly_dt_T3_filtered, by = c("tomst_id" = "locality_id")) %>%
  mutate(datetime = datetime_local) %>%
  select(-tomst_id, -datetime_local)

hourly_GDD5_mapped <- location_mapping %>%
  mutate(tomst_id = paste0("TOMST", tomst_id, "_QHI")) %>%
  left_join(hourly_dt_GDD5, by = c("tomst_id" = "locality_id")) %>%
  mutate(datetime = datetime_local) %>%
  select(-tomst_id, -datetime_local)

hourly_GDD0_mapped <- location_mapping %>%
  mutate(tomst_id = paste0("TOMST", tomst_id, "_QHI")) %>%
  left_join(hourly_dt_GDD0, by = c("tomst_id" = "locality_id")) %>%
  mutate(datetime = datetime_local) %>%
  select(-tomst_id, -datetime_local)

# Add cumulative GDD (based on mean hourly GDD)
hourly_GDD5_mapped <- hourly_GDD5_mapped %>%
  arrange(location_id, datetime) %>%
  group_by(location_id) %>%
  mutate(cumulative_GDD5 = cumsum(value)*4) %>%
  ungroup()
hourly_GDD0_mapped <- hourly_GDD0_mapped %>%
  arrange(location_id, datetime) %>%
  group_by(location_id) %>%
  mutate(cumulative_GDD0 = cumsum(value)*4) %>%
  ungroup()

# Join cumulative GDD5 and GDD0 to daily_temp_mapped by location_id and datetime
hourly_temp_mapped <- hourly_temp_mapped %>%
  left_join(
    hourly_GDD5_mapped %>%
      select(location_id, datetime, cumulative_GDD5),  # Select only the necessary columns
    by = c("location_id", "datetime")
  ) %>%
  left_join(
    hourly_GDD0_mapped %>%
      select(location_id, datetime, cumulative_GDD0),  # Select only the necessary columns
    by = c("location_id", "datetime")
  )

# write_csv(hourly_temp_mapped, "/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_hourly.csv")
# write_csv(hourly_GDD5_mapped, "/Volumes/TundraBUZZ/data/clean/QHI_location_GDD5_hourly.csv")
# write_csv(hourly_GDD0_mapped, "/Volumes/TundraBUZZ/data/clean/QHI_location_GDD0_hourly.csv")

#### ----
# hourly_temp_mapped <- read_csv("/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_hourly.csv")

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

# Select for GDD5
daily_dt_GDD5 <- daily_dt %>%
  filter(sensor_name == "GDD5_mean") ##only taking TMS_T3_mean
daily_dt_GDD5 <- daily_dt_GDD5 %>%
  select(locality_id, datetime, value, year, month, day, week)

# Select for GDD0
daily_dt_GDD0 <- daily_dt %>%
  filter(sensor_name == "GDD0_mean") ##only taking TMS_T3_mean
daily_dt_GDD0 <- daily_dt_GDD0 %>%
  select(locality_id, datetime, value, year, month, day, week)

daily_dt_T3 <- daily_dt_T3 %>%
  select(locality_id, datetime, value, year, month, day, week)

# Filter for only 2024
daily_dt_T3 <- daily_dt_T3[year == 2024,]
daily_dt_GDD5 <- daily_dt_GDD5[year == 2024,]
daily_dt_GDD0 <- daily_dt_GDD0[year == 2024,]

# Filter for summer months
daily_dt_T3_filtered <- daily_dt_T3 %>%
  filter(month %in% c(6:8))

# Join with location data
daily_temp_mapped <- location_mapping %>%
  mutate(tomst_id = paste0("TOMST", tomst_id, "_QHI")) %>%
  left_join(daily_dt_T3_filtered, by = c("tomst_id" = "locality_id")) %>%
  select(-tomst_id)

daily_GDD5_mapped <- location_mapping %>%
  mutate(tomst_id = paste0("TOMST", tomst_id, "_QHI")) %>%
  left_join(daily_dt_GDD5, by = c("tomst_id" = "locality_id")) %>%
  select(-tomst_id)
daily_GDD0_mapped <- location_mapping %>%
  mutate(tomst_id = paste0("TOMST", tomst_id, "_QHI")) %>%
  left_join(daily_dt_GDD0, by = c("tomst_id" = "locality_id")) %>%
  select(-tomst_id)

# Add cumulative GDD (based on mean hourly GDD)
daily_GDD5_mapped <- daily_GDD5_mapped %>%
  arrange(location_id, datetime) %>%
  group_by(location_id) %>%
  mutate(cumulative_GDD5 = cumsum(value)*96) %>%
  ungroup()
daily_GDD0_mapped <- daily_GDD0_mapped %>%
  arrange(location_id, datetime) %>%
  group_by(location_id) %>%
  mutate(cumulative_GDD0 = cumsum(value)*96) %>%
  ungroup()

# Join cumulative GDD5 and GDD0 to daily_temp_mapped by location_id and datetime
daily_temp_mapped <- daily_temp_mapped %>%
  left_join(
    daily_GDD5_mapped %>%
      select(location_id, datetime, cumulative_GDD5),  # Select only the necessary columns
    by = c("location_id", "datetime")
  ) %>%
  left_join(
    daily_GDD0_mapped %>%
      select(location_id, datetime, cumulative_GDD0),  # Select only the necessary columns
    by = c("location_id", "datetime")
  )

# write_csv(daily_temp_mapped, "/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_daily.csv")
# write_csv(daily_GDD5_mapped, "/Volumes/TundraBUZZ/data/clean/QHI_location_GDD5_daily.csv")
# write_csv(daily_GDD0_mapped, "/Volumes/TundraBUZZ/data/clean/QHI_location_GDD0_daily.csv")

#### ----
# daily_temp_mapped <- read_csv("/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_daily.csv")

#### Order sites by mean summer temperature based on daily temperatures ----
ordered_site_temp_summer <- daily_temp_mapped %>%
  group_by(location_id) %>%
  summarize(summer_temp = mean(value, na.rm = TRUE),
            summer_GDD0 = max(cumulative_GDD0, na.rm = TRUE),
            summer_GDD5 = max(cumulative_GDD5, na.rm = TRUE)) %>%
  arrange(desc(summer_temp))

# write_csv(ordered_site_temp_summer, "/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")

ordered_site_temp_summer <- read_csv("/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")

ordered_site_GDD0 <- daily_temp_mapped %>%
  group_by(location_id, microclimate) %>%
  summarize(summer_GDD0 = max(cumulative_GDD0, na.rm = TRUE)) %>%
  arrange(desc(summer_GDD0))

ordered_site_GDD5 <- daily_temp_mapped %>%
  group_by(location_id, microclimate) %>%
  summarize(summer_GDD5 = max(cumulative_GDD5, na.rm = TRUE)) %>%
  arrange(desc(summer_GDD5))



cumulative_GDD <- ggplot(daily_temp_mapped, aes(x = datetime, colour = microclimate, fill = microclimate)) +
  # Points for GDD0 and GDD5 with different shapes
  geom_point(aes(y = cumulative_GDD0, shape = "GDD0"), size = 2, alpha = 0.3) +
  geom_point(aes(y = cumulative_GDD5, shape = "GDD5"), size = 2, alpha = 0.3) +
  
  geom_line(aes(y = cumulative_GDD0, linetype = "GDD0", group = location_id), linewidth = 1, alpha = 0.1) +
  geom_line(aes(y = cumulative_GDD5, linetype = "GDD5", group = location_id), linewidth = 1, alpha = 0.1) +
  
  
  # Smooth lines with different linetypes
  geom_smooth(aes(y = cumulative_GDD0, linetype = "GDD0"), method = "loess", se = F) +
  geom_smooth(aes(y = cumulative_GDD5, linetype = "GDD5"), method = "loess", se = F) +
  
  # Labels and themes
  labs(x = "Date", 
       y = "Cumulative Growing-Degree-Days",
       colour = "Microclimate",
       fill = "Microclimate",
       shape = "GDD Threshold",
       linetype = "GDD Threshold") +
  
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  scale_fill_manual(values = c("#440154", "forestgreen", "gold")) +
  scale_shape_manual(values = c("GDD0" = 16, "GDD5" = 17)) +
  scale_linetype_manual(values = c("GDD0" = "solid", "GDD5" = "dashed")) +
  theme_classic()

# Save figure
ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/cumulative_GDD.pdf",
  plot = cumulative_GDD,
  width = 11,       # adjust based on layout
  height = 8
)

#### PLOTTING ----
#ordered_site_temp_summer <- read_csv("/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")
ordered_site_temp_summer <- ordered_site_temp_summer %>%
  left_join(location_mapping, by = "location_id")

ordered_site_temp_summer$location_id <- factor(ordered_site_temp_summer$location_id,
                                               levels = unique(ordered_site_temp_summer$location_id))
ordered_site_temp_summer$microclimate2 <- factor(ordered_site_temp_summer$microclimate2,
                                               levels = unique(ordered_site_temp_summer$microclimate2))

# Plot summer temp per site
clean_summer_temp_per_site <- ggplot(ordered_site_temp_summer, aes(x = location_id, y = summer_temp, colour = microclimate2)) +
  geom_boxplot() +
  labs(x = "Site",
       y = "Mean Summer Temperature (°C)",
       colour = "Microclimate") +
  theme_classic() +
  scale_colour_manual(values = c("grey44","gold", "forestgreen", "#440154"))

# Plot summer temp per microclimate
clean_summer_temp_per_microclimate <- ggplot(ordered_site_temp_summer, aes(x = microclimate2, y = summer_temp, fill = microclimate2)) +
  geom_boxplot() +
  labs(x = "Microclimate Classification",
       y = "Mean Summer Temperature (°C)",
       fill = "Microclimate") +
  theme_classic() +
  scale_fill_manual(values = c("grey44","gold", "forestgreen", "#440154"))


# Convert to long format
GDD_long <- ordered_site_temp_summer %>%
  pivot_longer(cols = c(summer_GDD0, summer_GDD5),
               names_to = "GDD_type",
               values_to = "GDD_value")

# Plot
GDD_by_temp <- ggplot(GDD_long, aes(x = summer_temp, y = GDD_value, colour = GDD_type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  scale_colour_manual(
    values = c("summer_GDD0" = "steelblue", "summer_GDD5" = "orange3"),
    labels = c("GDD (0°C base)", "GDD (5°C base)"),
    name = "GDD Type"
  ) +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Cumulative Growing Degree Days"
  )

ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/GDD_by_temp.pdf",
  plot = GDD_by_temp,
  width = 8,       # adjust based on layout
  height = 10
)

# Get the slope for each GDD type
slopes <- GDD_long %>%
  group_by(GDD_type) %>%
  do(tidy(lm(GDD_value ~ summer_temp, data = .))) %>%
  filter(term == "summer_temp") %>%
  select(GDD_type, estimate, std.error, statistic, p.value)

print(slopes)
