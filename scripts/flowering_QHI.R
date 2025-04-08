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