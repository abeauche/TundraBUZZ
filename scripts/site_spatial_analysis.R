# ====================================================
# Script Name: site_spatial_analysis.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-04-15
# Last Modified: 2025-04-22
# Description: This script does an exploratory analysis of distances between site locations as well as carrying out a hierarchical clustering analysis.
# Dependencies: R package: geosphere
# ====================================================

# Load packages
library(geosphere)


#### Define site coordinates ----
# Define site coordinates
coords_cool <- data.frame(
  site = c("COOL1", "COOL2", "COOL3", "COOL4"),
  lat = c(69.56863, 69.56984, 69.57541, 69.57161),
  lon = c(-138.91122, -138.90289, -138.89679, -138.90336)
)

coords_mod <- data.frame(
  site = c("MOD1", "MOD2", "MOD3"),
  lat = c(69.57969, 69.57637, 69.58023),
  lon = c(-138.91010, -138.88717, -138.86055)
)

coords_warm <- data.frame(
  site = c("WARM1", "WARM2", "WARM3"),
  lat = c(69.57367, 69.57176, 69.57268),
  lon = c(-138.89902, -138.87789, -138.88519)
)

# Add temp_group column to each
coords_cool$temp_group <- "COOL"
coords_mod$temp_group  <- "MOD"
coords_warm$temp_group <- "WARM"

# Combine all site coordinate data frames
coords_all <- rbind(coords_cool, coords_mod, coords_warm)

# Compute all pairwise distances (in meters)
dist_matrix <- distm(coords_all[, c("lon", "lat")], fun = distHaversine)

# Extract upper triangle of matrix
dists <- dist_matrix[upper.tri(dist_matrix)]

# Average distance in meters
mean_distance_m <- mean(dists)

# Convert to km if needed
mean_distance_km <- mean_distance_m / 1000
mean_distance_km




#### Calculate pairwise distances ----

# Create a function to calculate average pairwise distance between two groups
average_group_distance <- function(group1, group2, data) {
  # Subset coordinates
  coords1 <- data[data$temp_group == group1, c("lon", "lat")]
  coords2 <- data[data$temp_group == group2, c("lon", "lat")]
  
  # Create all pairwise combinations between the two groups
  pairs <- expand.grid(1:nrow(coords1), 1:nrow(coords2))
  
  # Calculate distances for all pairs
  dists <- mapply(function(i, j) {
    distHaversine(coords1[i, ], coords2[j, ])
  }, pairs[, 1], pairs[, 2])
  
  # Return mean distance (in meters)
  mean(dists)
}

# Calculate average pairwise distances between each group pair
avg_cool_mod  <- average_group_distance("COOL", "MOD", coords_all)
avg_cool_warm <- average_group_distance("COOL", "WARM", coords_all)
avg_mod_warm  <- average_group_distance("MOD", "WARM", coords_all)

# Print the results in km
cat("Average distance COOL to MOD:", round(avg_cool_mod / 1000, 2), "km\n")
cat("Average distance COOL to WARM:", round(avg_cool_warm / 1000, 2), "km\n")
cat("Average distance MOD to WARM:", round(avg_mod_warm / 1000, 2), "km\n")

# Get distance matrix (in meters)
dist_matrix <- distm(coords_all[, c("lon", "lat")], fun = distHaversine)
rownames(dist_matrix) <- coords_all$site
colnames(dist_matrix) <- coords_all$site

# Convert to dist object (required for hclust)
geo_dist <- as.dist(dist_matrix)

# Perform hierarchical clustering
hc_geo <- hclust(geo_dist, method = "average")  # or "complete", "ward.D", etc.

plot(hc_geo, main = "Hierarchical Clustering of Sites by Geographic Distance",
     xlab = "Site", sub = "", ylab = "Distance (meters)")

clusters <- cutree(hc_geo, k = 3)
coords_all$cluster <- clusters

# View cluster assignments
coords_all
