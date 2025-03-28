# ====================================================
# Script Name: obsolete_script.R
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
ARUQ4569_2024_pred_raw <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/obsolete/predictions_ARUQ4569_raw.csv")


#### Clean ARUQ4569_2024_pred_raw dataset ----
# Extract aru_id and clean file structure naming, filter aru_id = NA and incomplete ARUs
ARUQ56_2024_pred_raw <- ARUQ4569_2024_pred_raw %>%
  mutate(
    file = sub("^.*\\\\", "", file),  # Remove path before backslash
    aru_id = str_extract(file, "ARUQ\\d+"),  # Extract "ARUQ5" or similar
    datetime = str_extract(file, "\\d{8}_\\d{6}")  # Extract "20240626_013000"
  ) %>%
  filter(!is.na(aru_id)) %>%
  filter(!aru_id %in% c("ARUQ4", "ARUQ9")) %>%
  select(-aru_id, -datetime)

# Save csv
write.csv(ARUQ56_2024_pred_raw, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ56_raw.csv", row.names = FALSE)
