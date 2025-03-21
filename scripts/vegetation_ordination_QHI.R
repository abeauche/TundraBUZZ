# ====================================================
# Script Name: vegetation_ordination_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-20
# Last Modified: 2025-03-20
# Description: This script creates an ordination for vegetation plots based on vegetation composition data for 10 POLCAM sites on Qikiqtaruk-Herschel Island measured in the summer of 2024 by A. Beauchemin and E. Bowman.
# Dependencies: XQuartz, ARU_veg_survey_compiled.csv.
# ====================================================

# Load required packages
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(rgl)


# Set working directory (if needed)
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Load data
vegetation_raw <- read.csv("./data/raw/QHI_vegetation_2024_raw.csv", stringsAsFactors = TRUE)



### Prepare datasets for ordination

# Group by ARU, plot, and species, summarize mean percent cover for both observers
vegetation_plot_summary <- vegetation_raw %>%
  group_by(aru_id, plot, class) %>%
  summarise(mean_percent_cover = mean(percent.cover, na.rm = TRUE))

vegetation_aru_summary <- vegetation_raw %>%
  group_by(aru_id, class) %>%
  summarise(mean_percent_cover = mean(percent.cover, na.rm = TRUE))

write.csv(vegetation_plot_summary, file = "./data/clean/QHI_vegetation_plot_2024.csv")
write.csv(vegetation_aru_summary, file = "./data/clean/QHI_vegetation_aru_2024.csv")


# Convert to wide format for NMDS
veg_matrix <- vegetation_aru_summary %>%
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = 0)

# Remove NA column if it exists
veg_matrix <- veg_matrix %>% select(-any_of("NA"))

# Convert to matrix
veg_comm_matrix <- as.matrix(veg_matrix[, -c(1,2)])
rownames(veg_comm_matrix) <- veg_matrix$aru_id


### NMDS ORDINATION - From https://ourcodingclub.github.io/tutorials/ordination/

# Calculate a dissimilarity matrix using bray-curtis distance, which is recommended for abundance data
dist <- vegdist(veg_comm_matrix,  method = "bray")

# Define a function NMDS.scree() that automatically performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

# Choose the optimal nr of dimensions
NMDS.scree(dist)
# k = 3 is a good inflexion point

# Increase trymax to 200 for more iterations
set.seed(123)
nmds_result <- metaMDS(veg_comm_matrix, k = 3, trymax = 200, distance = "bray")

# Check stress and plot results
print(nmds_result$stress)
stressplot(nmds_result)

# Visualize in a scatter plot for three NMDS dimensions
plot3d(nmds_result$points[,1], nmds_result$points[,2], nmds_result$points[,3], 
       col = "blue", size = 3, xlab = "NMDS1", ylab = "NMDS2", zlab = "NMDS3")





### IN PROGRESS





# Run NMDS ordination
set.seed(123)
nmds_result <- metaMDS(veg_comm_matrix, distance = "bray", k = 2, trymax = 100)

# Basic NMDS plot
plot(nmds_result, type = "t")
ordiplot(nmds_result, display = "sites", type = "n")
points(nmds_result$points, col = "blue", pch = 19)
text(nmds_result$points, labels = rownames(nmds_result$points), pos = 3, cex = 0.8)

# NMDS Scree Plot Function
NMDS.scree <- function(x) {
  plot(1:10, sapply(1:10, function(i) metaMDS(x, k = i, autotransform = FALSE)$stress),
       xlab = "# of Dimensions", ylab = "Stress", main = "NMDS Stress Plot", type = "b")
}

# Scree plot for NMDS
NMDS.scree(veg_comm_matrix)

# Define microclimate classification
vegetation_aru_summary <- vegetation_aru_summary %>%
  mutate(microclimate = case_when(
    aru_id %in% c("ARUQ5", "ARUQ9", "ARUQ7") ~ "Warm",
    aru_id %in% c("ARUQ3", "ARUQ8", "ARUQ10") ~ "Intermediate",
    aru_id %in% c("ARUQ6", "ARUQ1", "ARUQ2", "ARUQ4") ~ "Cool"
  ))

# Merge NMDS results with microclimate classifications
nmds_data <- data.frame(aru_id = rownames(nmds_result$points), nmds_result$points) %>%
  left_join(vegetation_aru_summary, by = "aru_id")

# Assign colors based on microclimate
color_palette <- brewer.pal(length(unique(nmds_data$microclimate)), "Set1")
nmds_data$color <- color_palette[as.factor(nmds_data$microclimate)]

# NMDS plot with microclimate classification
ordiplot(nmds_result, display = "sites", type = "n")
points(nmds_result$points, col = nmds_data$color, pch = 19)
text(nmds_result$points, labels = rownames(nmds_result$points), pos = 3, cex = 0.8)

# NMDS at Plot Level
veg_matrix_plot <- vegetation_plot_summary %>%
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = 0) %>%
  mutate(aru_plot = paste(aru_id, plot, sep = "_"))

veg_comm_matrix_plot <- veg_matrix_plot %>%
  select(-c(aru_id, plot, aru_plot)) %>%
  as.matrix()
rownames(veg_comm_matrix_plot) <- veg_matrix_plot$aru_plot

# Run NMDS ordination
set.seed(123)
nmds_result_plot <- metaMDS(veg_comm_matrix_plot, distance = "bray", k = 2, trymax = 100)

# NMDS plot by aru_id
aru_colors <- rainbow(length(unique(nmds_data$aru_id)))[as.factor(nmds_data$aru_id)]
ordiplot(nmds_result_plot, display = "sites", type = "n")
points(nmds_result_plot$points, col = aru_colors, pch = 19)
legend("topright", legend = unique(nmds_data$aru_id), col = unique(aru_colors), pch = 19, cex = 0.8)

