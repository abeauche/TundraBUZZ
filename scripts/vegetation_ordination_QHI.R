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
# install.packages("rgl")
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
  summarise(mean_percent_cover = mean(percent.cover, na.rm = TRUE)) %>%
  mutate(mean_percent_cover = ifelse(is.nan(mean_percent_cover), 0, mean_percent_cover))

vegetation_aru_summary <- vegetation_raw %>%
  group_by(aru_id, class) %>%
  summarise(mean_percent_cover = mean(percent.cover, na.rm = TRUE)) %>%
  mutate(mean_percent_cover = ifelse(is.nan(mean_percent_cover), 0, mean_percent_cover))

write.csv(vegetation_plot_summary, file = "./data/clean/QHI_vegetation_plot_2024.csv")
write.csv(vegetation_aru_summary, file = "./data/clean/QHI_vegetation_aru_2024.csv")


# Convert to wide format for NMDS
veg_matrix <- vegetation_aru_summary %>%
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = 0)

# Remove NA column if it exists
veg_matrix <- veg_matrix %>% select(-any_of("NA"))

# Convert to matrix
veg_comm_matrix <- as.matrix(veg_matrix[, -1])
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
pdf("./outputs/figures/NMDS_scree.pdf", width = 7, height = 6)
NMDS.scree(dist)
dev.off()
# k = 3 is a good inflexion point

# Increase trymax to 200 for more iterations
set.seed(123)
nmds_result <- metaMDS(veg_comm_matrix, k = 3, trymax = 200, distance = "bray")

# Check stress and plot results
print(nmds_result$stress)
pdf("./outputs/figures/NMDS_stress_k3.pdf", width = 7, height = 6)
stressplot(nmds_result)
dev.off()

# Merge the 'microclimate' classification with the NMDS data
nmds_data <- data.frame(aru_id = rownames(nmds_result$points), points = nmds_result$points) %>%
  left_join(veg_matrix %>% filter(aru_id %in% rownames(nmds_result$points)), by = "aru_id")

nmds_data <- nmds_data %>%
  mutate(microclimate = case_when(
    aru_id %in% c("ARUQ5", "ARUQ9", "ARUQ7") ~ "Warm",
    aru_id %in% c("ARUQ3", "ARUQ8", "ARUQ10") ~ "Intermediate",
    aru_id %in% c("ARUQ6", "ARUQ1", "ARUQ2", "ARUQ4") ~ "Cool"
  ))

# Export microclimate factor
microclimate_factor <- factor(nmds_data$microclimate)

# Create a color palette for the microclimate categories
colors <- c("cool" = "purple4",   # Blue for cool
            "intermediate" = "forestgreen", # Greenish-blue for intermediate
            "warm" = "gold")  # Red for warm

# Create 3D NMDS plot with color coding by microclimate
plot3d(nmds_result$points[,1], nmds_result$points[,2], nmds_result$points[,3], 
       col = colors[microclimate_factor], size = 3, xlab = "NMDS1", ylab = "NMDS2", zlab = "NMDS3")

# Add labels at the points (you can modify the labels as needed)
text3d(nmds_result$points[,1], nmds_result$points[,2], nmds_result$points[,3] + 0.02, 
       texts = rownames(nmds_result$points), cex = 0.7, col = "black")
text3d(nmds_result$species[,1], nmds_result$species[,2], nmds_result$species[,3], 
       texts = rownames(nmds_result$species), cex = 0.7, col = "grey")




### NMDS ORDINATION IN 2D FOR EASIER VISUALIZATION
# k = 2 is still below stress threshold of 0.2, still acceptable

# Run NMDS ordination
set.seed(123)
nmds_result_k2 <- metaMDS(veg_comm_matrix, distance = "bray", k = 2, trymax = 100)

# Check stress and plot results
print(nmds_result_k2$stress)
pdf("./outputs/figures/NMDS_stress_k2.pdf", width = 7, height = 6)
stressplot(nmds_result_k2)
dev.off()

# Basic NMDS plot
pdf("./outputs/figures/NMDS_result_k2.pdf", width = 7, height = 6)
plot(nmds_result_k2, type = "t")
dev.off()

ordiplot(nmds_result_k2, display = "sites", type = "n")
points(nmds_result_k2$points, col = "blue", pch = 19)
text(nmds_result_k2$points, labels = rownames(nmds_result_k2$points), pos = 3, cex = 0.8)

# Merge NMDS results with microclimate classifications
nmds_data_k2 <- data.frame(aru_id = rownames(nmds_result_k2$points), points = nmds_result_k2$points) %>%
  left_join(veg_matrix %>% filter(aru_id %in% rownames(nmds_result_k2$points)), by = "aru_id")

# Define microclimate classification
nmds_data_k2 <- nmds_data_k2 %>%
  mutate(microclimate = case_when(
    aru_id %in% c("ARUQ5", "ARUQ9", "ARUQ7") ~ "Warm",
    aru_id %in% c("ARUQ3", "ARUQ8", "ARUQ10") ~ "Intermediate",
    aru_id %in% c("ARUQ6", "ARUQ1", "ARUQ2", "ARUQ4") ~ "Cool"
  ))

# NMDS plot with microclimate classification
pdf("./outputs/figures/NMDS_microclim_k2.pdf", width = 8.2, height = 6)
ordiplot(nmds_result_k2, display = "sites", type = "n")
points(nmds_result_k2$points, col = colors[microclimate_factor], pch = 19)
text(nmds_result_k2$points, labels = rownames(nmds_result_k2$points), pos = 3, cex = 0.8)
legend("topleft", legend = unique(microclimate_factor), col = colors, pch = 19, title = "Microclimate")
dev.off()




### NMDS ORDINATION AT PLOT LEVEL
# Prepare data for NMDS
veg_matrix_plot <- vegetation_plot_summary %>%
  ungroup() %>%
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = 0) %>%
  mutate(aru_plot = paste(aru_id, plot, sep = "_"))

veg_comm_matrix_plot <- veg_matrix_plot %>%
  select(-c(aru_id, plot, aru_plot)) %>%
  as.matrix()
rownames(veg_comm_matrix_plot) <- veg_matrix_plot$aru_plot

# Calculate a dissimilarity matrix using bray-curtis distance
dist_plot <- vegdist(veg_comm_matrix_plot,  method = "bray")

# Choose the optimal nr of dimensions
pdf("./outputs/figures/NMDS_scree_plot.pdf", width = 7, height = 6)
NMDS.scree(dist_plot)
dev.off()
# k = 3-4 could be okay

# Run NMDS ordination
set.seed(123)
nmds_result_plot_k3 <- metaMDS(veg_comm_matrix_plot, distance = "bray", k = 3, trymax = 200)

# Check stress and plot results
print(nmds_result_plot_k3$stress)
pdf("./outputs/figures/NMDS_stress_plot_k3.pdf", width = 7, height = 6)
stressplot(nmds_result_plot_k3)
dev.off()

# Create colour vector
plot_colors <- rainbow(length(unique(veg_matrix_plot$aru_id)))[as.factor(veg_matrix_plot$aru_id)]

# Create 3D NMDS plot with color coding by site
plot3d(nmds_result_plot$points[,1], nmds_result_plot$points[,2], nmds_result_plot$points[,3], col = plot_colors, size = 3, xlab = "NMDS1", ylab = "NMDS2", zlab = "NMDS3")

# Add labels at the points (you can modify the labels as needed)
text3d(nmds_result_plot$points[,1], nmds_result_plot$points[,2], nmds_result_plot$points[,3] + 0.05, 
       texts = rownames(nmds_result_plot$points), cex = 0.7, col = "black")
text3d(nmds_result_plot$species[,1], nmds_result_plot$species[,2], nmds_result_plot$species[,3], 
       texts = rownames(nmds_result_plot$species), cex = 0.7, col = "grey")




### NMDS ORDINATION AT PLOT LEVEL IN 2D FOR EASIER VISUALIZATION
# stress at k = 2 still around 0.2, not great

# Run NMDS ordination
nmds_result_plot_k2 <- metaMDS(veg_comm_matrix_plot, distance = "bray", k = 2, trymax = 200)

# Check stress and plot results
print(nmds_result_plot_k2$stress)
pdf("./outputs/figures/NMDS_stress_plot_k2.pdf", width = 7, height = 6)
stressplot(nmds_result_plot_k2)
dev.off()

# Create 3D NMDS plot with color coding by site
plot(nmds_result_plot_k2, type = "t")

pdf("./outputs/figures/NMDS_plot_k2.pdf", width = 7, height = 6)
ordiplot(nmds_result_plot_k2, display = "sites", type = "n")
points(nmds_result_plot_k2$points, col = plot_colors, pch = 19)
text(nmds_result_plot_k2$points, labels = rownames(nmds_result_plot_k2$points), pos = 3, cex = 0.8)
dev.off()





