library(ggplot2)
library(tidyr)
library(dplyr)

#install.packages("cowplot")
#install.packages("ggridges")
#install.packages("ggnewscale")
library(ggridges)
library(cowplot)
library(ggnewscale)
library(viridis)


setwd("~/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/")

POLCAM_daily <- read.csv("POLCAM_daily.csv")

# Pivot columns into long format

POLCAM_daily_long <- pivot_longer(POLCAM_daily, "sal_arc_fem":"bis_viv", names_to = "species", values_to = "flower_count")
POLCAM_daily_long$date <- as.Date(POLCAM_daily_long$date, format = "%Y/%m/%d")



# Set the desired order of plots
plot_order <- c("POLCAM1", "POLCAM2", "POLCAM3", "POLCAM4", "POLCAM5", "POLCAM6", "POLCAM7", "POLCAM8", "POLCAM9", "POLCAM10")  # Specify the order here

# Reorder the 'plot' factor in the data
POLCAM_daily_long$plot <- factor(POLCAM_daily_long$plot, levels = plot_order)


# Set the desired order of plots
microclim_order <- c("POLCAM3", "POLCAM7", "POLCAM10", "POLCAM9", "POLCAM5", "POLCAM8", "POLCAM6", "POLCAM4", "POLCAM1", "POLCAM2")  # Specify the order here
microclim_order_2 <- c("POLCAM5", "POLCAM9", "POLCAM7", "POLCAM3", "POLCAM8", "POLCAM10", "POLCAM6", "POLCAM1", "POLCAM2", "POLCAM4")


# Reorder the 'plot' factor in the data
POLCAM_daily_long$plot <- factor(POLCAM_daily_long$plot, levels = microclim_order_2)




ggplot(POLCAM_daily_long, aes(x = date, y = plot, height = flower_count, fill = species)) +
  geom_density_ridges(stat = "identity", scale = 10, alpha = 0.5) +
  #facet_wrap(~ plot) +
  labs(
    title = "Flower Counts Over Time per Species for Each Plot",
    x = "Date",
    y = "Species"
  ) +
  theme_ridges() +
  #scale_y_discrete(limits = c(0, 20), breaks = seq(0, 20, by = 5))
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

POLCAM_daily_long_no_castet <- POLCAM_daily_long %>%
  filter(species != "cas_tet")

ggplot(POLCAM_daily_long_no_castet, aes(x = date, y = plot, height = flower_count, fill = species)) +
  geom_density_ridges(stat = "identity", scale = 3, alpha = 0.5) +
  #facet_wrap(~ plot) +
  labs(
    title = "Flower Counts Over Time per Species for Each Plot",
    x = "Date",
    y = "Species"
  ) +
  scale_fill_manual(values = species_colors) +
  theme_ridges() +
  #scale_y_discrete(limits = c(0, 20), breaks = seq(0, 20, by = 5))
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(POLCAM_daily_long_no_castet, aes(x = date, y = flower_count, height = flower_count, fill = species)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 1) + 
  facet_wrap(~ plot, scales = "free_y") +  # Allow free y-scale per plot
  labs(
    title = "Flower Counts Over Time per Species for Each Plot",
    x = "Date",
    y = "Flower Count"
  ) +
 # theme_ridges() +
  #scale_fill_brewer() +  # Optional for color enhancement
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)  # Adjust facet title font size
  )


species_colors <- c(
  "sal_arc_fem" = "#d3c171",  # Blue
  "sal_arc_male" = "#ffd966",  # Orange
  "sal_rich_fem" = "#c1981d",  # Green
  "sal_rich_male" = "#f1c132",  # Red
  "sal_ret_open" = "#8c564b",  # Brown
  "sal_pul_fem" = "#dbb658",  # Brown
  "sal_pul_male" = "#f8d000",  # Pink
  "lup_arc" = "#90caef",  # Gray
  "dry_int" = "#fff2cc",  # Yellow-Green
  "astra" = "#eca3ba", # Cyan
  "oxy" = "#67528a", # Light Blue
  "stel_long" = "#ededed", # Light Orange
  "ped_all" = "#dc79c2", # Light Green
  "ped_cap" = "#fceea0", # Light Red
  "cast" = "#ff76a3", # Light Purple
  "bis_viv" = "#b7e36a", # Light Brown
  "sen" = "#ffeb3b", # Light Pink
  "pyr_gra" = "#f7b6d2"  # Light Pink
)


POLCAM_daily_aggregated <- POLCAM_daily_long_no_castet %>%
  group_by(date, plot) %>%
  summarise(total_flower_count = sum(flower_count, na.rm = TRUE)) %>%
  ungroup()

POLCAM_daily_microclimate <- POLCAM_daily_long_no_castet %>%
  mutate(microclimate = case_when(
    plot %in% c("POLCAM5", "POLCAM9", "POLCAM7") ~ "Warm",
    plot %in% c("POLCAM3", "POLCAM8", "POLCAM10") ~ "Intermediate",
    plot %in% c("POLCAM6", "POLCAM1", "POLCAM2", "POLCAM4") ~ "Cool",
    TRUE ~ plot  # Keep other species as they are
  ))

plot <- c("POLCAM5", "POLCAM9", "POLCAM7", "POLCAM3", "POLCAM8", "POLCAM10", "POLCAM6", "POLCAM1", "POLCAM2", "POLCAM4")
summer_temp <- c("12.51938", "12.44199", "12.15929", "11.54562", "11.52226", "11.50299", "11.28128", "11.14114", "11.14114", "10.68225")
summer_temp_df <- data.frame(plot, summer_temp)

POLCAM_daily_aggregated_2 <- POLCAM_daily_microclimate %>%
  group_by(date, plot) %>%
  summarise(total_flower_count = sum(flower_count, na.rm = TRUE)) %>%
  ungroup()

POLCAM_daily_microclimate_aggregated <- POLCAM_daily_aggregated_2 %>%
  right_join(summer_temp_df, by = "plot")

POLCAM_daily_aggregated_3 <- POLCAM_daily_microclimate %>%
  group_by(date, plot, microclimate) %>%
  summarise(total_flower_count = sum(flower_count, na.rm = TRUE)) %>%
  ungroup()

plot_colours <- rev(viridis(10))
plot_colours_2 <- viridis(3)

# Plot the aggregated data
ggplot(POLCAM_daily_aggregated, aes(x = date, y = plot, height = total_flower_count, fill = plot)) +
  geom_density_ridges(stat = "identity", scale = 2.5, alpha = 0.5) +
  labs(
    x = "Date",
    y = "Total flower count per plot",
    fill = "Plot"
  ) +
  scale_fill_manual(values = plot_colours) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

POLCAM_daily_microclimate_aggregated$plot <- factor(POLCAM_daily_microclimate_aggregated$plot, levels = microclim_order_2)
levels(POLCAM_daily_microclimate_aggregated$plot) <- c("WARM1", "WARM2", "WARM3", "MOD1", "MOD2", "MOD3", "COOL1", "COOL2", "COOL3", "COOL4")

ggplot(POLCAM_daily_microclimate_aggregated, aes(x = date, y = plot, height = total_flower_count, fill = as.numeric(summer_temp))) +
  geom_density_ridges(stat = "identity", scale = 2.5, alpha = 0.5) +
  labs(
    x = "Date",
    y = "Total flower count per plot",
    fill = "Mean summer temp. (C)"
  ) +
  scale_fill_viridis() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Create a new 'group' variable for the species
POLCAM_daily_long_no_castet_grouped <- POLCAM_daily_long_no_castet %>%
  mutate(species_group = case_when(
    species %in% c("sal_arc_fem", "sal_arc_male") ~ "sal_arc",
    species %in% c("sal_rich_fem", "sal_rich_male") ~ "sal_rich",
    species %in% c("sal_pul_fem", "sal_pul_male") ~ "sal_pul",
    species %in% c("pyr_gra", "bis_viv", "cast") ~ "others",
    TRUE ~ species  # Keep other species as they are
  ))

# Aggregate data by the new 'species_group'
POLCAM_daily_aggregated_grouped <- POLCAM_daily_long_no_castet_grouped %>%
  group_by(date, plot, species_group) %>%
  summarise(total_flower_count = sum(flower_count, na.rm = TRUE)) %>%
  ungroup()

species_group_colors <- c(
  "sal_arc" = "#d3c171",  
  "sal_rich" = "#c1981d",
  "sal_pul" = "#dbb658",
  "sal_ret_open" = "#8c564b",
  "others" = "darkgrey",  # Light Pink for others
  "lup_arc" = "#90caef",  
  "dry_int" = "#fff2cc",  
  "astra" = "#eca3ba", 
  "oxy" = "#67528a", 
  "stel_long" = "#ededed", 
  "ped_all" = "#dc79c2", 
  "ped_cap" = "#fceea0", 
  "sen" = "#ffeb3b"
)

ggplot(POLCAM_daily_aggregated_grouped, aes(x = date, y = plot, height = total_flower_count, fill = species_group)) +
  geom_density_ridges(stat = "identity", scale = 3, alpha = 0.7) +
  #facet_wrap(~ plot) +
  labs(
    x = "Date",
    y = "Flower Count",
    fill = "Species"
  ) +
  scale_fill_manual(values = species_group_colors, 
                    labels = c(
    "sal_arc" = "Salix arctica",  # Label for the sal_arc group
    "sal_rich" = "Salix richardsonii",  # Label for the sal_rich group
    "sal_pul" = "Salix pulchra",  # Label for the sal_pul group
    "sal_ret_open" = "Salix reticulata",
    "lup_arc" = "Lupinus arcticus",
    "dry_int" = "Dryas integrifolia",
    "astra" = "Astragalus spp.",
    "oxy" = "Oxytropis spp.",
    "stel_long" = "Stellaria longipes",
    "ped_all" = "Pedicularis spp.",
    "ped_cap" = "Pedicularis capitata",
    "sen" = "Senecio spp.",
    "others" = "Other species"  # Label for the 'others' group
  )) +
  theme_ridges() +
  #scale_y_discrete(limits = c(0, 20), breaks = seq(0, 20, by = 5))
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()



# Filter the data for a single species (e.g., "sal_arc_fem")
salix_species_data <- POLCAM_daily_aggregated_grouped %>%
  filter(species_group %in% c("sal_arc", "sal_rich", "sal_pul", "sal_ret_open"))  # Change to the species you want to graph

# Plot the flower counts over time for the selected species
ggplot(salix_species_data, aes(x = date, y = plot, height = total_flower_count, fill = species_group)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.7) +
  scale_fill_manual(values = species_group_colors, 
                    labels = c("sal_arc" = "Salix arctica",
                               "sal_rich" = "Salix richardsonii",
                               "sal_pul" = "Salix pulchra",
                               "sal_ret_open" = "Salix reticulata")) +  # Apply color for the selected species
  labs(
    x = "Date",
    y = "Flower density",
    fill = "Species"
  ) +
  theme_ridges() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Filter the data for a single species (e.g., "sal_arc_fem")
dryas_species_data <- POLCAM_daily_aggregated_grouped %>%
  filter(species_group == c("dry_int"))  # Change to the species you want to graph

# Plot the flower counts over time for the selected species
ggplot(dryas_species_data, aes(x = date, y = plot, height = total_flower_count, fill = species_group)) +
  geom_density_ridges(stat = "identity", scale = 1.5, alpha = 0.7) +
  scale_fill_manual(values = species_group_colors, 
                    labels = c("dry_int" = "Dryas integrifolia")) +  # Apply color for the selected species
  labs(
    x = "Date",
    y = "Flower density",
    fill = "Species"
  ) +
  theme_ridges() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Filter the data for a single species (e.g., "sal_arc_fem")
lupin_species_data <- POLCAM_daily_aggregated_grouped %>%
  filter(species_group == c("lup_arc"))  # Change to the species you want to graph

# Plot the flower counts over time for the selected species
ggplot(lupin_species_data, aes(x = date, y = plot, height = total_flower_count, fill = species_group)) +
  geom_density_ridges(stat = "identity", scale = 1.5, alpha = 0.7) +
  scale_fill_manual(values = species_group_colors, 
                    labels = c("lup_arc" = "Lupinus arctica")) +  # Apply color for the selected species
  labs(
    x = "Date",
    y = "Flower density",
    fill = "Species"
  ) +
  theme_ridges() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Filter the data for a single species (e.g., "sal_arc_fem")
astra_oxy_species_data <- POLCAM_daily_aggregated_grouped %>%
  filter(species_group %in% c("astra", "oxy"))  # Change to the species you want to graph

# Plot the flower counts over time for the selected species
ggplot(astra_oxy_species_data, aes(x = date, y = plot, height = total_flower_count, fill = species_group)) +
  geom_density_ridges(stat = "identity", scale = 1.5, alpha = 0.7) +
  scale_fill_manual(values = species_group_colors, 
                    labels = c("astra" = "Astragalus spp.",
                               "oxy" = "Oxytropis spp.")) +  # Apply color for the selected species
  labs(
    x = "Date",
    y = "Flower density",
    fill = "Species"
  ) +
  theme_ridges() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Identify flowering periods with flowering intensity
phenology_periods <- POLCAM_daily_aggregated_grouped %>%
  filter(total_flower_count > 0) %>%
  group_by(plot, species_group) %>%
  arrange(date) %>%
  mutate(gap = c(0, diff(date) > 1)) %>%  # Identify gaps longer than 1 day
  mutate(period_id = cumsum(gap)) %>%  # Assign period IDs for each gap
  group_by(species_group, plot, period_id) %>%
  summarise(
    Start = min(date),
    End = max(date),
    Avg_Flower_Count = mean(total_flower_count),  # Use mean as a summary measure
    .groups = 'drop'
  )

phenology_periods <- phenology_periods %>%
  mutate(species_group = factor(species_group, 
                                levels = c("sal_pul", "sal_rich", "sal_arc", "sal_ret_open", "lup_arc", "dry_int", "stel_long", "astra", "ped_cap", "oxy", "ped_all", "sen", "others"),
                                labels = c("Salix pulchra", "Salix richardsonii", "Salix arctica", "Salix reticulata", "Lupinus arcticus", "Dryas integrifolia", "Stellaria longipes", "Astragalus spp.", "Pedicularis capitata", "Oxytropis spp.", "Pedicularis spp.", "Senecio spp.", "Other species"
                                          )))  # Custom order


# Plot with color gradient based on flower count
ggplot(phenology_periods, aes(y = species_group, x = Start, xend = End)) +
  geom_segment(aes(xend = End, color = Avg_Flower_Count), size = 6) +
  labs(
    x = "Date",
    y = "Species Group",
    color = "Flower Count",
  ) +
  theme_minimal() +
  #scale_color_gradient(low = "#cbe5a3", high = "darkgreen") +  # Adjust colors as needed
  #scale_color_gradient(low = "lightblue", high = "darkblue") +
  scale_color_gradient(low = "forestgreen", high = "gold") +
  theme(legend.position = "right") +
  theme_classic()




species_gradients <- list(
  "sal_arc" = scale_color_gradient(low = "#fceea0", high = "#c1981d"), 
  "sal_rich" = scale_color_gradient(low = "#fceea0", high = "#c1981d"),
  "sal_pul" = scale_color_gradient(low = "#fceea0", high = "#c1981d"),
  "sal_ret_open" = scale_color_gradient(low = "#fceea0", high = "#c1981d"),
  "others" = scale_color_gradient(low = "lightgrey", high = "darkgrey"),  # Light Pink for others
  "lup_arc" = scale_color_gradient(low = "#90caef", high = "darkblue"),  
  "dry_int" = scale_color_gradient(low = "#fff2cc", high = "bisque3"), 
  "astra" = scale_color_gradient(low = "lightpink", high = "lightpink4"), 
  "oxy" = scale_color_gradient(low = "plum4", high = "#67528a"), 
  "stel_long" = scale_color_gradient(low = "#ededed", high = "azure3"), 
  "ped_all" = scale_color_gradient(low = "hotpink1", high = "hotpink4"), 
  "ped_cap" = scale_color_gradient(low = "hotpink1", high = "hotpink4"), 
  "sen" = scale_color_gradient(low = "khaki", high = "gold")
)




# Split data by species
species_data <- split(POLCAM_daily_aggregated_grouped, POLCAM_daily_aggregated_grouped$species_group)

# Initialize ggplot object
p <- ggplot()

# Function to find flowering periods
identify_flowering_periods <- function(df) {
  # Group by species_group
  df <- df %>%
    group_by(species_group) %>%
    arrange(date) %>%
    mutate(
      flowering_period = cumsum(total_flower_count == 0) + 1
    )
  return(df)
}

# Apply the function to split into flowering periods
data_with_periods <- identify_flowering_periods(POLCAM_daily_aggregated_grouped)

data_with_periods <- data_with_periods %>%
  mutate(species_group = factor(species_group, levels = c("sal_pul", "sal_rich", "sal_arc", "sal_ret_open", "lup_arc", "dry_int", "stel_long", "astra", "ped_cap", "oxy", "ped_all", "sen", "others")))  # Custom order



# Initialize ggplot object
p <- ggplot()

# Iterate over species and their flowering periods
for (species in names(species_gradients)) {
  # Filter data for the current species
  species_data <- filter(data_with_periods, species_group == species, total_flower_count > 0)
  
  if (nrow(species_data) > 0) {
    # Add segments for each flowering period
    p <- p + ggnewscale::new_scale_color() + 
      geom_segment(data = species_data, 
                   aes(x = date, xend = lead(date, order_by = date), 
                       y = species_group, yend = species_group, color = total_flower_count), 
                   size = 6) +
      species_gradients[[species]]
  }
}

# Customize the plot
p <- p + 
  labs(title = "Flowering Phenology with Distinct Flowering Periods & Species-Specific Gradients", 
       x = "Date", 
       y = "Species Group") +
  theme_minimal() +
  theme(legend.position = "right")

# Display the plot
print(p)
