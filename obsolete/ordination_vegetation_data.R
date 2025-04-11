library(tidyr)
library(dplyr)
library(vegan)
library(ggplot2)
library(lubridate)

# Read csv
vegetation_raw <- read.csv("/Users/alexandrebeauchemin/Desktop/ARU_veg_survey_compiled.csv", stringsAsFactors = TRUE)

# Correct data classes
vegetation_raw$date <- as.Date(vegetation_raw$date)
vegetation_raw$notes <- as.character(vegetation_raw$notes)
vegetation_raw$all.flower.count <- as.integer(vegetation_raw$all.flower.count)
vegetation_raw$canopy_height_top <- as.numeric(vegetation_raw$canopy_height_top)

summary(vegetation_raw$class)

# Correct class labels 
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Moss"] <- "xxxmoss"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Polygonum viv "] <- "Polygonum viviparum"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Polygonum viviparum "] <- "Polygonum viviparum"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Polygonum bis "] <- "Polygonum bistorta"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Poly bistorta"] <- "Polygonum bistorta"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Lichen "] <- "Lichen"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Other Forb "] <- "otherforb"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "otherforb "] <- "otherforb"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Castilleja "] <- "Castilleja"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Cassiope tet"] <- "Cassiope tetragona"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Cassiope tetragona "] <- "Cassiope tetragona"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Fungus"] <- "xxxfungus"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Mushroom"] <- "xxxfungus"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Petasites"] <- "Petasites frigidus"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Petasities frigidus"] <- "Petasites frigidus"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Salix Richarsonii "] <- "Salix Richarsonii"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Salix Polaris "] <- "Salix Polaris"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Litter"] <- "xxxlitter"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Graminoid"] <- "Graminoid/horsetails"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Bare ground"] <- "xxxbareground"
levels(vegetation_raw$class)[levels(vegetation_raw$class) == "Salix spp."] <- "Salix Other"
levels(vegetation_raw$aru_id)[levels(vegetation_raw$aru_id) == "ARU10"] <- "ARUQ10"

# Convert empty strings to NA
vegetation_raw[vegetation_raw == ""] <- NA

#write.csv(vegetation_raw, file = "/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/QHI_vegetation_2024.csv")

# Group by ARU, plot, and species, summarize mean percent cover for both observers
vegetation_plot_summary <- vegetation_raw %>%
  group_by(aru_id, plot, class) %>%
  summarise(mean_percent_cover = mean(percent.cover, na.rm = TRUE))

vegetation_aru_summary <- vegetation_raw %>%
  group_by(aru_id, class) %>%
  summarise(mean_percent_cover = mean(percent.cover, na.rm = TRUE))


### NMDS Plot Level
# Transform the data into a wide format (species-by-plot matrix)
veg_matrix <- vegetation_aru_summary %>%
  select(aru_id, class, mean_percent_cover) %>% 
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = list(percent.cover = 0))

# Ensure NA values are replaced with 0
veg_matrix[is.na(veg_matrix)] <- 0
veg_matrix[veg_matrix == "NaN"] <- 0
veg_matrix <- veg_matrix %>% select(-`NA`)

# Convert to matrix (excluding the first column which is 'plot')
veg_comm_matrix <- as.matrix(veg_matrix[, -1])
rownames(veg_comm_matrix) <- veg_matrix$aru_id  # Set plot names as rownames

# Run NMDS ordination
set.seed(123)  # For reproducibility
nmds_result <- metaMDS(veg_comm_matrix, distance = "bray", k = 2, trymax = 100)


# Basic NMDS plot
plot(nmds_result, type = "t")  # 't' = text labels

# Add plot points
ordiplot(nmds_result, display = "sites", type = "n")
points(nmds_result$points, col = "blue", pch = 19)
text(nmds_result$points, labels = rownames(nmds_result$points), pos = 3, cex = 0.8)



### CODING CLUB

dist <- vegdist(veg_comm_matrix,  method = "bray")


# Increase trymax to 200 for more iterations
nmds_result <- metaMDS(veg_comm_matrix, k = 2, trymax = 200, distance = "bray")

# Check stress and plot results
print(nmds_result$stress)
stressplot(nmds_result)

# In this part, we define a function NMDS.scree() that automatically 
# performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(dist)


# Because the final result depends on the initial 
# random placement of the points 
# we`ll set a seed to make the results reproducible
set.seed(123)

# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 4, trymax = 100, trace = F)
# Do you know what the trymax = 100 and trace = F means?
# Let's check the results
NMDS1

# If you don`t provide a dissimilarity matrix, metaMDS automatically applies Bray-Curtis. So in our case, the results would have to be the same
NMDS2 <- metaMDS(veg_comm_matrix, k = 2, trymax = 100, trace = F)
NMDS2

stressplot(NMDS1)
plot(NMDS1, type = "t")

NMDS3 <- metaMDS(veg_comm_matrix, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "t")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)





### ARU_ID

# Assuming you already have the 'vegetation_aru_summary' data frame that contains 'aru_id'
# First, add the 'microclimate' classification
vegetation_aru_summary2 <- vegetation_aru_summary %>%
  mutate(microclimate = case_when(
    aru_id %in% c("ARUQ5", "ARUQ9", "ARUQ7") ~ "Warm",
    aru_id %in% c("ARUQ3", "ARUQ8", "ARUQ10") ~ "Intermediate",
    aru_id %in% c("ARUQ6", "ARUQ1", "ARUQ2", "ARUQ4") ~ "Cool"
  ))

veg_matrix <- vegetation_aru_summary2 %>%
  select(aru_id, class, mean_percent_cover) %>% 
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = list(percent.cover = 0))

veg_matrix2 <- vegetation_aru_summary2 %>%
  select(aru_id, microclimate, class, mean_percent_cover) %>% 
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = list(percent.cover = 0))


# Ensure NA values are replaced with 0
veg_matrix[is.na(veg_matrix)] <- 0
veg_matrix[veg_matrix == "NaN"] <- 0
veg_matrix <- veg_matrix %>% select(-`NA`)

# Convert to matrix (excluding the first column which is 'plot')
veg_comm_matrix <- as.matrix(veg_matrix[, -1])
rownames(veg_comm_matrix) <- veg_matrix$aru_id  # Set plot names as rownames

# Run NMDS ordination
set.seed(123)  # For reproducibility
nmds_result <- metaMDS(veg_comm_matrix, distance = "bray", k = 2, trymax = 100)

# Basic NMDS plot
plot(nmds_result, type = "t")  # 't' = text labels



### TEST ###
library(vegan)

plot(nmds_result, type = "n")  # No labels initially
points(nmds_result, display = "sites", col = "blue", pch = 19)
points(nmds_result, display = "species", col = "red", pch = 17)

# Prevent overlapping species labels
orditorp(nmds_result, display = "species", col = "red", pch = 17, air = 1.2)  # Adjust `air`
orditorp(nmds_result, display = "sites", col = "blue", pch = 19, air = 1.2)
########

# Add plot points
ordiplot(nmds_result, display = "sites", type = "n")
points(nmds_result$points, col = "", pch = 19)
text(nmds_result$points, labels = rownames(nmds_result$points), pos = 3, cex = 0.8)


# Now, merge the 'microclimate' classification with the NMDS result for coloring
# Ensure that the classification matches the number of sites/rows in the NMDS data
# Ensure that we are only using the 'aru_id' values that are present in the NMDS result
nmds_data <- data.frame(aru_id = rownames(nmds_result$points), points = nmds_result$points) %>%
  left_join(veg_matrix2 %>% filter(aru_id %in% rownames(nmds_result$points)), by = "aru_id")


# Color palette for the microclimate groups
library(RColorBrewer)
color_palette <- brewer.pal(length(unique(nmds_data$microclimate)), "Set1")

# Assign colors based on the 'microclimate' classification
classification_colors <- color_palette[as.factor(nmds_data$microclimate)]

# Basic NMDS plot with color-coded microclimate groups
ordiplot(nmds_result, display = "sites", type = "n")  # 'n' = no points initially
points(nmds_result$points, col = classification_colors, pch = 19)  # Add plot points with colors
text(nmds_result$points, labels = rownames(nmds_result$points), pos = 3, cex = 0.8)  # Add text labels






### NMDS Plot Level
# Transform the data into a wide format (species-by-plot matrix)
veg_matrix_plot <- vegetation_plot_summary %>%
  select(aru_id, plot, class, mean_percent_cover) %>% 
  pivot_wider(names_from = class, values_from = mean_percent_cover, values_fill = list(percent.cover = 0))

veg_matrix_plot <- veg_matrix_plot %>%
  mutate(aru_plot = paste(aru_id, plot, sep = "_"))

# Ensure NA values are replaced with 0
veg_matrix_plot[is.na(veg_matrix_plot)] <- 0
veg_matrix_plot[veg_matrix_plot == "NaN"] <- 0
veg_matrix_plot <- veg_matrix_plot %>% select(-`NA`)

# Convert to matrix (excluding the first column which is 'plot')
veg_comm_matrix_plot <- veg_matrix_plot %>%
  ungroup() %>%  # Remove grouping%>%
  select(-c(aru_id, plot, aru_plot)) %>%
  as.data.frame()
rownames(veg_comm_matrix_plot) <- veg_matrix_plot$aru_plot  # Set plot names as rownames


# Run NMDS ordination
set.seed(123)  # For reproducibility
nmds_result_plot <- metaMDS(veg_comm_matrix_plot, distance = "bray", k = 2, trymax = 100)

# Basic NMDS plot
plot(nmds_result_plot, type = "t")  # 't' = text labels

# Add plot points
ordiplot(nmds_result_plot, display = "sites", type = "n")
points(nmds_result_plot$points, col = "blue", pch = 19)
text(nmds_result_plot$points, labels = rownames(nmds_result_plot$points), pos = 3, cex = 0.8)



# Define colors for each aru_id
aru_colors <- rainbow(length(unique(veg_matrix_plot$aru_id)))[as.factor(veg_matrix_plot$aru_id)]

# Plot NMDS
ordiplot(nmds_result_plot, display = "sites", type = "t")  # Empty plot
points(nmds_result_plot$points, col = aru_colors, pch = 19)  # Color by aru_id
legend("topright", legend = unique(nmds_data$aru_id), col = unique(aru_colors), pch = 19, cex = 0.8)


# Plot NMDS
ordiplot(nmds_result_plot, display = "sites", type = "n")  # Empty plot
points(nmds_result_plot$points, col = classification_colors, pch = 19)  # Color by aru_id
legend("topright", legend = unique(nmds_data$aru_id), col = unique(classification_colors), pch = 19, cex = 0.8)






veg_matrix_core <- veg_matrix_plot %>% 
  filter(plot == "Core") %>%
  mutate(site_id =paste(aru_id, plot, sep = "_")) %>%
  ungroup() %>%
  select(-c(aru_id, plot, aru_plot))

veg_matrix_averaged <- veg_matrix %>% 
  mutate(site_id =paste(aru_id)) %>%
  ungroup() %>%
  select(-c(aru_id))

# Combine the two matrices by stacking them (rows)
veg_matrix_combined <- bind_rows(veg_matrix_core, veg_matrix_averaged)  
veg_comm_matrix_combined <- veg_matrix_combined %>%
  select(-site_id) %>%
  as.data.frame()
rownames(veg_comm_matrix_combined) <- veg_matrix_combined$site_id 

# Run NMDS ordination
set.seed(123)  # For reproducibility
nmds_result_combined <- metaMDS(veg_comm_matrix_combined, distance = "bray", k = 2, trymax = 100)

# Basic NMDS plot
plot(nmds_result_combined, type = "t")  # 't' = text labels

# Add plot points
ordiplot(nmds_result_combined, display = "sites", type = "n")
points(nmds_result_combined$points, col = "blue", pch = 19)
text(nmds_result_combined$points, labels = rownames(nmds_result_combined$points), pos = 3, cex = 0.8)







### TEST LINKING CORE AND AVERAGE POINTS ###

# Split the data into 'core' and 'averaged' based on 'site_id'
core_data <- veg_matrix_combined %>%
  filter(grepl("_Core", site_id)) %>%
  mutate(site_id = site_id)

averaged_data <- veg_matrix_combined %>%
  filter(!grepl("_Core", site_id)) %>%
  mutate(site_id = site_id)

# Match core and averaged points by 'aru_id' (split 'site_id' to get 'aru_id')
core_data$aru_id <- gsub("_Core", "", core_data$site_id)
averaged_data$aru_id <- averaged_data$site_id


# Extract the MDS coordinates from the NMDS result
nmds_coords <- as.data.frame(nmds_result_combined$points)
nmds_coords$aru_id <- rownames(nmds_coords)  # Make sure aru_id is in the same order
nmds_coords <- nmds_coords %>% mutate(site_id = aru_id) %>%
  select(-c(aru_id))

# Merge the MDS coordinates with the core and averaged data based on 'aru_id'
core_data <- left_join(core_data, nmds_coords, by = "site_id")
averaged_data <- left_join(averaged_data, nmds_coords, by = "site_id")

# Now core_data and averaged_data have MDS1 and MDS2 columns for plotting


# Ensure both data sets are ordered the same way
core_data <- core_data %>%
  select(site_id, aru_id, MDS1, MDS2)

averaged_data <- averaged_data %>%
  select(site_id, aru_id, MDS1, MDS2)


# Create the base NMDS plot with no points initially
ordiplot(nmds_result, display = "sites", type = "n")  # Empty plot


# Create the plot with adjusted xlim and ylim
plot(nmds_coords[, 1], nmds_coords[, 2], type = "n", xlim = c(min(nmds_coords[, 1]) - 0.25, max(nmds_coords[, 1]) + 0.25), 
     ylim = c(min(nmds_coords[, 2]) - 0.25, max(nmds_coords[, 2]) + 0.25))  # Adjust limits with a buffer


# Plot core and averaged sites
points(core_data$MDS1, core_data$MDS2, col = "red", pch = 19)  # Core points in red
points(averaged_data$MDS1, averaged_data$MDS2, col = "blue", pch = 19)  # Averaged points in blue

# Add arrows between corresponding core and averaged points
for(i in 1:nrow(core_data)) {
  arrows(averaged_data$MDS1[i], averaged_data$MDS2[i], core_data$MDS1[i], core_data$MDS2[i], length = 0.1, angle = 15, col = "black", lwd = 1)
}

# Optionally add a legend
legend("topright", legend = c("Core", "Averaged"), col = c("red", "blue"), pch = 19, cex = 0.8)

# Add labels for each site based on aru_id (assuming rownames of nmds_coords are aru_id)
text(nmds_coords[, 1], nmds_coords[, 2], labels = rownames(nmds_coords), pos = 3, cex = 0.8, col = "black")


