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
ARUQ0_2024_pred <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ0_raw.csv")
ARUQ4_2024_pred <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ4_raw.csv")
ARUQ56_2024_pred <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ56_raw.csv")
location_mapping <- read.csv("./data/raw/location_mapping_TundraBUZZ.csv", stringsAsFactors = TRUE)
