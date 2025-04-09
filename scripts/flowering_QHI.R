# ====================================================
# Script Name: flowering_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-04-08
# Last Modified: 2025-04-08
# Description: This script TBD.
# Dependencies: TBD
# ====================================================

#### SETUP ----
# Load required libraries
# Load required libraries
library(tidyverse)
library(lubridate)
library(hms)
library(ggridges)
library(cowplot)
library(ggnewscale)
library(viridis)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

# Load data
polcam_data <- read_csv("/Volumes/TundraBUZZ/data/raw/POLCAM_data.csv", skip = 1)
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")


#### Tidy data ----
# Select and format columns of interest
polcam_data <- polcam_data %>%
  mutate(date = as.POSIXct(`Date (year-month-day)`),
         plot = Plot)
polcam_data <- polcam_data %>%
  select(-c(Plot, Site, `Date (year-month-day)`, Time, Observer, Notes))

# Rename plot to location_id
location_mapping <- location_mapping %>%
  select(polcam_id, location_id)
polcam_data <- polcam_data %>%
  left_join(location_mapping, by = c("plot"="polcam_id")) %>%
  select(-plot)

# Pivot longer
polcam_data_long <- polcam_data %>%
  pivot_longer(
    cols = -c(date, location_id),  # keep these columns as-is
    names_to = "flower_type",                   # new column for former column names
    values_to = "count"                     # new column for values
  )
polcam_data_long <- polcam_data_long %>%
  mutate(species = sub("^([^_]+_[^_]+)_.*", "\\1", flower_type))

write_csv(polcam_data_long, "/Volumes/TundraBUZZ/data/clean/polcam_data_long.csv")

# Group per species
polcam_data_species <- polcam_data_long %>%
  group_by(species, location_id, date) %>%
  summarize(count = sum(count))



#### Summarize flowering data ----
# Calculate phenology metrics
flowering_metrics <- polcam_data_species %>%
  filter(count > 0) %>%
  group_by(location_id) %>%
  summarize(
    first_flowering = min(date),
    last_flowering = max(date),
    duration_days = as.numeric(difftime(last_flowering, first_flowering, units = "days")),
    .groups = "drop"
  )

# Calculate peak flowering (in case of ties, take middle date)
peak_flowering <- polcam_data_species %>%
  group_by(location_id) %>%
  filter(count == max(count, na.rm = TRUE), count > 0) %>%
  arrange(date) %>%
  summarize(
    peak_flowering_date = {
      n <- n()
      if (n %% 2 == 1) {
        date[ceiling(n / 2)]
      } else {
        # for even number of tied dates, take the later of the two middle ones
        date[n / 2 + 1]
      }
    },
    .groups = "drop"
  )

# Combine flowering metrics and peak date
flowering_summary <- flowering_metrics %>%
  left_join(peak_flowering, by = c("location_id"))


