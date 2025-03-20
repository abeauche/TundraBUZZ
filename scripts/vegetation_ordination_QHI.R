# ====================================================
# Script Name: vegetation_ordination_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-20
# Last Modified: 2025-03-20
# Description: This script creates an ordination for vegetation plots based on vegetation composition data for 10 POLCAM sites on Qikiqtaruk-Herschel Island measured in the summer of 2024 by A. Beauchemin and E. Bowman.
# Dependencies: ARU_veg_survey_compiled.csv.
# ====================================================

# Load required packages
library(tidyverse)  # Example
library(vegan)


# Set working directory (if needed)
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Load data
vegetation_raw <- read.csv("/data_raw/QHI_vegetation_2024.csv", stringsAsFactors = TRUE)

# Define functions (if applicable)

# Main analysis code