# ====================================================
# Script Name: flight_buzzes_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-21
# Last Modified: 2025-03-21
# Description: This script TBD.
# Dependencies: TBD, location_mapping_TundraBUZZ.csv
# ====================================================

# Load required packages
library(tidyverse)
library(vegan)
library(RColorBrewer)


# Set working directory (if needed)
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Load data
vegetation_raw <- read.csv("./data/raw/QHI_vegetation_2024_raw.csv", stringsAsFactors = TRUE)
location_mapping <- read.csv("./data/raw/location_mapping_TundraBUZZ.csv", stringsAsFactors = TRUE)

# Merge to replace aru_id with location_id
vegetation_raw <- vegetation_raw %>%
  left_join(location_mapping, by = "aru_id") %>%
  select(-c(aru_id, polcam_id,tomst_id,site,year,X,microclimate))  # Remove aru_id, now using location_id

# Set seed for repeatability
set.seed(123)