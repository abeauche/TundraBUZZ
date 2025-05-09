# ====================================================
# Script Name: vegetation_ordination_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-20
# Last Modified: 2025-03-26
# Description: This script creates an ordination for vegetation plots based on vegetation composition data for 10 POLCAM sites on Qikiqtaruk-Herschel Island measured in the summer of 2024 by A. Beauchemin and E. Bowman.
# Dependencies: XQuartz, QHI_vegetation_2024_raw.csv, location_mapping_TundraBUZZ.csv, mean_summer_temp_TundraBUZZ.csv; R packages: tiyverse, vegan, RColorBrewer, rgl
# ====================================================

#### SETUP ----
# Load required libraries 
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(rgl)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)


# Load data
vegetation_raw <- read.csv("./data/raw/QHI_vegetation_2024_raw.csv", stringsAsFactors = TRUE)
location_mapping <- read.csv("./data/raw/location_mapping_TundraBUZZ.csv", stringsAsFactors = TRUE)
mean_summer_temp <- read_csv("/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")

# Merge to replace aru_id with location_id
vegetation_raw <- vegetation_raw %>%
  left_join(location_mapping, by = "aru_id") %>%
  select(-c(aru_id, polcam_id,tomst_id,site,year,X,microclimate))  # Remove aru_id, now using location_id




#### Prepare datasets for ordination ----

# Group by ARU, plot, and species, summarize mean percent cover for both observers
vegetation_plot_summary <- vegetation_raw %>%
  group_by(location_id, plot, class) %>%
  summarise(mean_percent_cover = mean(percent.cover, na.rm = TRUE)) %>%
  mutate(mean_percent_cover = ifelse(is.nan(mean_percent_cover), 0, mean_percent_cover))

vegetation_aru_summary <- vegetation_raw %>%
  group_by(location_id, class) %>%
  summarise(mean_percent_cover = mean(percent.cover, na.rm = TRUE)) %>%
  mutate(mean_percent_cover = ifelse(is.nan(mean_percent_cover), 0, mean_percent_cover))

# write.csv(vegetation_plot_summary, file = "./data/clean/QHI_vegetation_plot_2024.csv")
# write.csv(vegetation_aru_summary, file = "./data/clean/QHI_vegetation_aru_2024.csv")


# Convert to wide format for NMDS
veg_matrix <- vegetation_aru_summary %>%
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = 0)

# Remove NA column if it exists
veg_matrix <- veg_matrix %>% select(-any_of("NA"))

# Convert to matrix
veg_comm_matrix <- as.matrix(veg_matrix[, -1])
rownames(veg_comm_matrix) <- veg_matrix$location_id



#### NMDS ORDINATION - From https://ourcodingclub.github.io/tutorials/ordination/ ----

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
# pdf("./outputs/figures/NMDS_scree.pdf", width = 7, height = 6)
NMDS.scree(dist)
# dev.off()
# k = 3 is a good inflexion point

# Increase trymax to 200 for more iterations
nmds_result <- metaMDS(veg_comm_matrix, k = 3, trymax = 200, distance = "bray")

# Check stress and plot results
print(nmds_result$stress)
#pdf("./outputs/figures/NMDS_stress_k3.pdf", width = 7, height = 6)
stressplot(nmds_result)
#dev.off()

# Merge the 'microclimate' classification with the NMDS data
nmds_data <- data.frame(location_id = rownames(nmds_result$points), points = nmds_result$points) %>%
  left_join(veg_matrix %>% filter(location_id %in% rownames(nmds_result$points)), by = "location_id")

nmds_data <- nmds_data %>%
  mutate(microclimate = case_when(
    location_id %in% c("WARM1", "WARM2", "WARM3") ~ "Warm",
    location_id %in% c("MOD1", "MOD2", "MOD3") ~ "Moderate",
    location_id %in% c("COOL1", "COOL2", "COOL3", "COOL4") ~ "Cool"
  ))

# Export microclimate factor
microclimate_factor <- factor(nmds_data$microclimate)

# Create a color palette for the microclimate categories
colors <- c("Cool" = "purple4",   # Blue for cool
            "Moderate" = "forestgreen", # Greenish-blue for moderate
            "Warm" = "gold")  # Red for warm

# Create 3D NMDS plot with color coding by microclimate
plot3d(nmds_result$points[,1], nmds_result$points[,2], nmds_result$points[,3], 
       col = colors[microclimate_factor], size = 3, xlab = "NMDS1", ylab = "NMDS2", zlab = "NMDS3")

# Add labels at the points (you can modify the labels as needed)
text3d(nmds_result$points[,1], nmds_result$points[,2], nmds_result$points[,3] + 0.02, 
       texts = rownames(nmds_result$points), cex = 0.7, col = "black")
text3d(nmds_result$species[,1], nmds_result$species[,2], nmds_result$species[,3], 
       texts = rownames(nmds_result$species), cex = 0.7, col = "grey")




#### NMDS ORDINATION IN 2D FOR EASIER VISUALIZATION ----
# k = 2 is still below stress threshold of 0.2, still acceptable

# Run NMDS ordination
nmds_result_k2 <- metaMDS(veg_comm_matrix, distance = "bray", k = 2, trymax = 100)

# Check stress and plot results
print(nmds_result_k2$stress)
# pdf("./outputs/figures/NMDS_stress_k2.pdf", width = 7, height = 6)
stressplot(nmds_result_k2)
# dev.off()

# Basic NMDS plot
# pdf("./outputs/figures/NMDS_result_k2.pdf", width = 7, height = 6)
plot(nmds_result_k2, type = "t")
# dev.off()

ordiplot(nmds_result_k2, display = "sites", type = "n")
points(nmds_result_k2$points, col = "blue", pch = 19)
text(nmds_result_k2$points, labels = rownames(nmds_result_k2$points), pos = 3, cex = 0.8)

# Merge NMDS results with microclimate classifications
nmds_data_k2 <- data.frame(location_id = rownames(nmds_result_k2$points), points = nmds_result_k2$points) %>%
  left_join(veg_matrix %>% filter(location_id %in% rownames(nmds_result_k2$points)), by = "location_id")

# Define microclimate classification
nmds_data_k2 <- nmds_data_k2 %>%
  mutate(microclimate = case_when(
    location_id %in% c("WARM1", "WARM2", "WARM3") ~ "Warm",
    location_id %in% c("MOD1", "MOD2", "MOD3") ~ "Moderate",
    location_id %in% c("COOL1", "COOL2", "COOL3", "COOL4") ~ "Cool"
  ))

# NMDS plot with microclimate classification
# pdf("./outputs/figures/NMDS_microclim_k2.pdf", width = 8.2, height = 6)
ordiplot(nmds_result_k2, display = "sites", type = "n")
points(nmds_result_k2$points, col = colors[microclimate_factor], pch = 19)
text(nmds_result_k2$points, labels = rownames(nmds_result_k2$points), pos = 3, cex = 0.8)
legend("topleft", legend = unique(microclimate_factor), col = colors, pch = 19, title = "Microclimate")
# dev.off()

ordiplot(nmds_result_k2, display = c("species", "sites"), type = "n")
points(nmds_result_k2$points, col = colors[microclimate_factor], pch = 19)
text(nmds_result_k2$points, labels = rownames(nmds_result_k2$points), pos = 3, cex = 0.8)
legend("topleft", legend = unique(microclimate_factor), col = colors, pch = 19, title = "Microclimate")


nmds_data_k2 <- nmds_data_k2 %>%
  left_join(mean_summer_temp, by = "location_id")

library(viridis)

# pdf("./outputs/figures/NMDS_summer_GDD0_k2.pdf", width = 8.2, height = 6)
ordiplot(nmds_result_k2, display = "sites", type = "n")
# Assign color based on summer_GDD0 using viridis_c
points(nmds_result_k2$points, col = viridis(100)[as.numeric(cut(nmds_data_k2$summer_GDD0, breaks = 100))], pch = 19)
text(nmds_result_k2$points, labels = rownames(nmds_result_k2$points), pos = 3, cex = 0.8)
# Add color legend
colorbar <- viridis(100)
legend("topleft", legend = round(seq(min(nmds_data_k2$summer_GDD0), max(nmds_data_k2$summer_GDD0), length.out = 5), 2), 
       fill = colorbar[seq(1, 100, length.out = 5)], title = "Summer GDD0")
# dev.off()




#### NMDS ORDINATION AT PLOT LEVEL ----
# Prepare data for NMDS
veg_matrix_plot <- vegetation_plot_summary %>%
  ungroup() %>%
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = 0) %>%
  mutate(location_plot = paste(location_id, plot, sep = "_"))

veg_comm_matrix_plot <- veg_matrix_plot %>%
  select(-c(location_id, plot, location_plot)) %>%
  as.matrix()
rownames(veg_comm_matrix_plot) <- veg_matrix_plot$location_plot

# Calculate a dissimilarity matrix using bray-curtis distance
dist_plot <- vegdist(veg_comm_matrix_plot,  method = "bray")

# Choose the optimal nr of dimensions
# pdf("./outputs/figures/NMDS_scree_plot.pdf", width = 7, height = 6)
NMDS.scree(dist_plot)
# dev.off()
# k = 3-4 could be okay

# Run NMDS ordination
nmds_result_plot_k3 <- metaMDS(veg_comm_matrix_plot, distance = "bray", k = 3, trymax = 200)

# Check stress and plot results
print(nmds_result_plot_k3$stress)
# pdf("./outputs/figures/NMDS_stress_plot_k3.pdf", width = 7, height = 6)
stressplot(nmds_result_plot_k3)
# dev.off()

# Create colour vector
plot_colors <- rainbow(length(unique(veg_matrix_plot$location_id)))[as.factor(veg_matrix_plot$location_id)]

# Create 3D NMDS plot with color coding by site
plot3d(nmds_result_plot_k3$points[,1], nmds_result_plot_k3$points[,2], nmds_result_plot_k3$points[,3], col = plot_colors, size = 3, xlab = "NMDS1", ylab = "NMDS2", zlab = "NMDS3")

# Add labels at the points (you can modify the labels as needed)
text3d(nmds_result_plot_k3$points[,1], nmds_result_plot_k3$points[,2], nmds_result_plot_k3$points[,3] + 0.05, 
       texts = rownames(nmds_result_plot_k3$points), cex = 0.7, col = "black")
text3d(nmds_result_plot_k3$species[,1], nmds_result_plot_k3$species[,2], nmds_result_plot_k3$species[,3], 
       texts = rownames(nmds_result_plot_k3$species), cex = 0.7, col = "grey")




#### NMDS ORDINATION AT PLOT LEVEL IN 2D FOR EASIER VISUALIZATION ----
# stress at k = 2 still around 0.2, not great

# Run NMDS ordination
nmds_result_plot_k2 <- metaMDS(veg_comm_matrix_plot, distance = "bray", k = 2, trymax = 200)

# Check stress and plot results
print(nmds_result_plot_k2$stress)
# pdf("./outputs/figures/NMDS_stress_plot_k2.pdf", width = 7, height = 6)
stressplot(nmds_result_plot_k2)
# dev.off()

# Create 2D NMDS plot with color coding by site
plot(nmds_result_plot_k2, type = "t")

# pdf("./outputs/figures/NMDS_plot_k2.pdf", width = 7, height = 6)
ordiplot(nmds_result_plot_k2, display = "sites", type = "n")
points(nmds_result_plot_k2$points, col = plot_colors, pch = 19)
text(nmds_result_plot_k2$points, labels = rownames(nmds_result_plot_k2$points), pos = 3, cex = 0.8)
# dev.off()




#### SCALING ANALYSIS --> COMBINING CORE PLOTS AND AVG PLOTS ----

# Filtered matrix for core sites
core_data <- veg_matrix_plot %>% 
  filter(plot == "Core") %>%
  mutate(site_id =paste(location_id, plot, sep = "_")) %>%
  ungroup() 
veg_matrix_core <- core_data %>%
  select(-c(location_id, plot, location_plot))

# Matrix for average sites
averaged_data <- veg_matrix %>% 
  mutate(site_id =paste(location_id)) %>%
  ungroup() 
veg_matrix_averaged <- averaged_data %>%
  select(-c(location_id))

# Tidy veg_matrix_core
veg_matrix_core <- veg_matrix_core[, !is.na(colnames(veg_matrix_core))]
veg_matrix_core <- veg_matrix_core[, colnames(veg_matrix_averaged)]

# Combine the two matrices by stacking them
veg_matrix_combined <- bind_rows(veg_matrix_core, veg_matrix_averaged)  
veg_comm_matrix_combined <- veg_matrix_combined %>%
  select(-site_id) %>%
  as.data.frame() %>% 
  na.omit()
rownames(veg_comm_matrix_combined) <- veg_matrix_combined$site_id 

# Match core and averaged points by 'location_id' (split 'site_id' to get 'location_id')
core_data$location_id <- gsub("_Core", "", core_data$site_id)
averaged_data$location_id <- averaged_data$site_id

# Run NMDS ordination
nmds_result_combined <- metaMDS(veg_comm_matrix_combined, distance = "bray", k = 2, trymax = 200)

# Check stress and plot results
print(nmds_result_combined$stress)
# pdf("./outputs/figures/NMDS_stress_combined_k2.pdf", width = 7, height = 6)
stressplot(nmds_result_combined)
# dev.off()

# Extract the MDS coordinates from the NMDS result
nmds_coords_combined <- as.data.frame(nmds_result_combined$points)
nmds_coords_combined$location_id <- rownames(nmds_coords_combined)  # Make sure location_id is in the same order
nmds_coords_combined <- nmds_coords_combined %>% mutate(site_id = location_id) %>%
  select(-c(location_id))

# Merge the MDS coordinates with the core and averaged data based on 'location_id'
core_data <- left_join(core_data, nmds_coords_combined, by = "site_id")
averaged_data <- left_join(averaged_data, nmds_coords_combined, by = "site_id")

# Ensure both data sets are ordered the same way
core_data <- core_data %>%
  select(site_id, location_id, MDS1, MDS2)
averaged_data <- averaged_data %>%
  select(site_id, location_id, MDS1, MDS2)

# Create the plot with adjusted xlim and ylim
# pdf("./outputs/figures/NMDS_combined_scaling.pdf", width = 7, height = 6)
plot(nmds_coords_combined[, 1], nmds_coords_combined[, 2], type = "n", xlim = c(min(nmds_coords_combined[, 1]) - 0.25, max(nmds_coords_combined[, 1]) + 0.25), 
     ylim = c(min(nmds_coords_combined[, 2]) - 0.25, max(nmds_coords_combined[, 2]) + 0.25))
# Plot core and averaged sites
points(core_data$MDS1, core_data$MDS2, col = "red", pch = 19)  # Core points in red
points(averaged_data$MDS1, averaged_data$MDS2, col = "blue", pch = 19)  # Averaged points in blue
# Add arrows between corresponding core and averaged points
for(i in 1:nrow(core_data)) {
  arrows(averaged_data$MDS1[i], averaged_data$MDS2[i], core_data$MDS1[i], core_data$MDS2[i], length = 0.1, angle = 15, col = "black", lwd = 1)
}
# Add a legend
legend("topright", legend = c("Core", "Averaged"), col = c("red", "blue"), pch = 19, cex = 0.8)
# Add labels for each site based on location_id 
text(nmds_coords_combined[, 1], nmds_coords_combined[, 2], labels = rownames(nmds_coords_combined), pos = 3, cex = 0.8, col = "black")
# dev.off()


# Combine and prepare your community matrix with group labels
combined_matrix <- rbind(veg_matrix_core, veg_matrix_averaged)
combined_matrix_numeric <- combined_matrix %>%
  select(!site_id)
group <- factor(c(rep("core", nrow(veg_matrix_core)), rep("average", nrow(veg_matrix_averaged))))


# Run PERMANOVA
adonis2_result <- adonis2(combined_matrix_numeric ~ group, method = "bray", permutations = 999)
print(adonis2_result)

# 1. Calculate Bray-Curtis distances
bray_dist <- vegdist(combined_matrix_numeric, method = "bray")

# 2. Create group factor
group <- factor(c(rep("core", nrow(veg_matrix_core)), rep("average", nrow(veg_matrix_averaged))))

# 3. Run the dispersion test
dispersion <- betadisper(bray_dist, group)

# 4. Test for significance
dispersion_test <- permutest(dispersion)
print(dispersion_test)

# 5. Optional: visualize the dispersion
plot(dispersion, hull = FALSE)
boxplot(dispersion, main = "Multivariate Dispersion by Group")


# Convert adonis2 result to a data frame
adonis_df <- as.data.frame(adonis2_result)

# Write to CSV
write.csv(adonis_df, "./outputs/permanova_results_corevssite.csv", row.names = TRUE)




#### PERMANOVA ----
# Running PERMANOVA to test if microclimate gradient significantly explains NMDS axes
library(vegan)

# Assuming 'nmds_result$points' contains the NMDS coordinates
# Make sure the rows are in the same order as nmds_data_k2
community_matrix <- veg_matrix %>%
  filter(location_id %in% nmds_data_k2$location_id) %>%
  column_to_rownames("location_id")  # Drop location_id into rownames

# Double-check order consistency
community_matrix <- community_matrix[match(nmds_data_k2$location_id, rownames(community_matrix)), ]

# Run PERMANOVA (using Bray-Curtis by default)
permanova_result <- adonis2(community_matrix ~ microclimate, data = nmds_data_k2, permutations = 999)

# View results
print(permanova_result)

dispersion <- betadisper(vegdist(community_matrix), nmds_data_k2$microclimate)
anova(dispersion)

ordiplot(nmds_result_k2, display = "sites", type = "n")
points(nmds_result_k2$points, col = colors[microclimate_factor], pch = 19)
ordiellipse(nmds_result_k2, nmds_data_k2$microclimate, kind = "sd", label = TRUE, col = colors)
legend("topright", legend = levels(nmds_data_k2$microclimate), col = colors, pch = 19)


# Basic NMDS plot
ordiplot(nmds_result_k2, display = c("sites"), type = "n")

# Plot points using your custom colors
points(nmds_result_k2$points,
       col = viridis(100)[as.numeric(cut(nmds_data_k2$summer_GDD0, breaks = 100))],
       pch = 19)

text(nmds_result_k2$points, labels = rownames(nmds_result_k2$points), pos = 3, cex = 0.8)

# Convex hull function using custom colors
draw_hull <- function(group) {
  group_points <- nmds_result_k2$points[nmds_data_k2$microclimate == group, ]
  hull_indices <- chull(group_points)
  hull_indices <- c(hull_indices, hull_indices[1])  # close the hull
  lines(group_points[hull_indices, ], col = colors[group], lwd = 2)
}

# Draw hulls for each microclimate group
for (group in names(colors)) {
  draw_hull(group)
}

# Add legend
legend("topleft", legend = names(colors),
       col = colors, pch = 19, title = "Microclimate")


# Test for pariwise differences between core and averaged okits
microclimates <- unique(nmds_data_k2$microclimate)
pairwise_results <- combn(microclimates, 2, simplify = FALSE) %>%
  purrr::map_df(function(pair) {
    subset_data <- nmds_data_k2 %>% filter(microclimate %in% pair)
    subset_matrix <- community_matrix[subset_data$location_id, ]
    
    result <- adonis2(subset_matrix ~ microclimate, data = subset_data, permutations = 999)
    
    tibble(
      group1 = pair[1],
      group2 = pair[2],
      F_value = result$F[1],
      R2 = result$R2[1],
      p_value = result$`Pr(>F)`[1]
    )
  })

write_csv(pairwise_results, "./outputs/pairwise_permanova_results.csv")

# Test for homogeneity of dispersion
dispersion <- betadisper(vegdist(community_matrix), nmds_data_k2$microclimate)

# Test statistically
anova(dispersion)

# Visualize
plot(dispersion)
boxplot(dispersion, main = "Multivariate Dispersion by Microclimate")
