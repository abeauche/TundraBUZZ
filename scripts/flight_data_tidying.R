# ====================================================
# Script Name: flight_data_tidying.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-28
# Last Modified: 2025-03-28
# Description: This script TBD.
# Dependencies: TBD, location_mapping_TundraBUZZ.csv
# ====================================================

#### SETUP ----
# Load required libraries
# Load required libraries
library(tidyverse)
library(lubridate)
library(suncalc)
library(hms)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

# Load data
ARUQ0_2024_pred_raw <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ0_raw.csv")
ARUQ4_2024_pred_raw <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ4_raw.csv")
ARUQ56_2024_pred_raw <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ56_raw.csv")
location_mapping <- read.csv("./data/raw/location_mapping_TundraBUZZ.csv", stringsAsFactors = TRUE)


#### CLEAN UP DATASETS ----

#### Clean ARUQ0_2024_pred_raw dataset ----

#### Clean ARUQ4_2024_pred_raw dataset ----

# Extract aru_id and clean file structure naming
ARUQ4_2024_pred <- ARUQ4_2024_pred %>%
  mutate(
    file = sub("^.*\\\\", "", file),  # Remove path before backslash
    aru_id = str_extract(file, "ARUQ\\d+"),  # Extract "ARUQ5" or similar
    datetime = str_extract(file, "\\d{8}_\\d{6}")  # Extract "20240626_013000"
  )

# Filter out files not properly named
ARUQ4_2024_pred <- ARUQ4_2024_pred %>%
  filter(!is.na(aru_id))

# Change aru_id to factor, check levels and table
ARUQ4_2024_pred$aru_id <- as.factor(ARUQ4_2024_pred$aru_id)
levels(ARUQ4_2024_pred$aru_id)
table(ARUQ4_2024_pred$aru_id)

# Merge to replace aru_id with location_id
ARUQ4_2024_pred_mapped <- ARUQ4_2024_pred %>%
  left_join(location_mapping, by = "aru_id") %>%
  select(-c(aru_id, polcam_id,tomst_id,site,year))  # Remove aru_id, now using location_id

# Mutate datetime to POSIXct format
ARUQ4_2024_pred_mapped <- ARUQ4_2024_pred_mapped %>% 
  mutate(datetime = as.POSIXct(datetime, format="%Y%m%d_%H%M%S", tz="UTC")  # Convert to POSIXct
  )

# Save csv
write.csv(ARUQ4_2024_pred_mapped, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ4_2024_pred_cleaned.csv", row.names = FALSE)


#### Clean ARUQ56_2024_pred_raw dataset ----
# Extract aru_id and clean file structure naming
ARUQ56_2024_pred <- ARUQ56_2024_pred %>%
  mutate(
    file = sub("^.*\\\\", "", file),  # Remove path before backslash
    aru_id = str_extract(file, "ARUQ\\d+"),  # Extract "ARUQ5" or similar
    datetime = str_extract(file, "\\d{8}_\\d{6}")  # Extract "20240626_013000"
  )

# Filter out files not properly named, filter out ARUQ4 and ARUQ9
ARUQ56_2024_pred <- ARUQ56_2024_pred %>%
  filter(!is.na(aru_id)) %>%
  filter(!aru_id %in% c("ARUQ4", "ARUQ9"))

# Change aru_id to factor, check levels and table
ARUQ56_2024_pred$aru_id <- as.factor(ARUQ56_2024_pred$aru_id)
levels(ARUQ56_2024_pred$aru_id)
table(ARUQ56_2024_pred$aru_id)

# Merge to replace aru_id with location_id
ARUQ56_2024_pred_mapped <- ARUQ56_2024_pred %>%
  left_join(location_mapping, by = "aru_id") %>%
  select(-c(aru_id, polcam_id,tomst_id,site,year))  # Remove aru_id, now using location_id

# Mutate datetime to POSIXct format
ARUQ56_2024_pred_mapped <- ARUQ56_2024_pred_mapped %>% 
  mutate(datetime = as.POSIXct(datetime, format="%Y%m%d_%H%M%S", tz="UTC")  # Convert to POSIXct
  )

# Save csv
write.csv(ARUQ56_2024_pred_mapped, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ56_2024_pred_cleaned.csv", row.names = FALSE)




