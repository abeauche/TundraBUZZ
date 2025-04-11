# ====================================================
# Script Name: flight_buzzes_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-21
# Last Modified: 2025-04-10
# Description: This script TBD.
# Dependencies: summary_flightbuzzes_ARUQ_2024.csv, daily_summary_flightbuzzes_ARUQ_2024.csv, environmental_variables_hourly.csv, environmental_variables_daily.csv, QHI_location_temperature_hourly.csv, QHI_location_temperature_daily.csv, mean_summer_temp_TundraBUZZ.csv, location_mapping_TundraBUZZ.csv, R packages: tidyverse, lubridate, suncalc, hms, lme4, lmerTest, mgcv, visreg, patchwork, cowplot, viridis.
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(lubridate)
library(suncalc)
library(hms)
library(lme4)
library(lmerTest) 
library(mgcv)
library(visreg)
library(patchwork)
library(cowplot)
library(viridis)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

# Set latitude and longitude for Qikiqtaruk - Herschel Island
lat <- 69.57
lon <- -138.91


#### Using tidy datasets ----
# Load data
summary_flightbuzzes_ARUQ_2024 <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/summary_flightbuzzes_ARUQ_2024.csv")
daily_summary_flightbuzzes_ARUQ_2024 <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/daily_summary_flightbuzzes_ARUQ_2024.csv")
environmental_variables_hourly <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/clean/environmental_variables_hourly.csv")
environmental_variables_daily <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/clean/environmental_variables_daily.csv")
QHI_temp_hourly <- read.csv("/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_hourly.csv")
QHI_temp_daily <- read.csv("/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_daily.csv")
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")
mean_summer_temp <- read_csv("/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")



#### Format datasets ----
# Change location_id and microclimate to factor
summary_flightbuzzes_ARUQ_2024$location_id <- as.factor(summary_flightbuzzes_ARUQ_2024$location_id) 
summary_flightbuzzes_ARUQ_2024$microclimate <- as.factor(summary_flightbuzzes_ARUQ_2024$microclimate2)
daily_summary_flightbuzzes_ARUQ_2024$location_id <- as.factor(daily_summary_flightbuzzes_ARUQ_2024$location_id) 
daily_summary_flightbuzzes_ARUQ_2024$microclimate <- as.factor(daily_summary_flightbuzzes_ARUQ_2024$microclimate2)


# Check levels and table
levels(summary_flightbuzzes_ARUQ_2024$location_id)
table(summary_flightbuzzes_ARUQ_2024$location_id)
levels(summary_flightbuzzes_ARUQ_2024$microclimate)
table(summary_flightbuzzes_ARUQ_2024$microclimate)

# Check timezone
attr(summary_flightbuzzes_ARUQ_2024$datetime, "tzone")

# Ensure datetime is in "America/Whitehorse" 
summary_flightbuzzes_ARUQ_2024 <- summary_flightbuzzes_ARUQ_2024 %>%
  mutate(datetime = with_tz(datetime, tzone = "America/Whitehorse"))

# Aggregate flight_buzz data to hourly
hourly_summary_flightbuzzes_ARUQ_2024 <- summary_flightbuzzes_ARUQ_2024 %>%
  mutate(datetime = floor_date(datetime, unit = "hour")) %>%
  group_by(datetime, location_id, microclimate) %>%
  summarize(total_duration_above_threshold = sum(total_duration_above_threshold, na.rm = TRUE),
            .groups = "drop")

# Convert temp data to POSIXct (UTC timezone)
QHI_temp_hourly$datetime <- ymd_hms(QHI_temp_hourly$datetime, tz = "UTC")

# Ensure datetime is in "America/Whitehorse"
QHI_temp_hourly$datetime <- with_tz(QHI_temp_hourly$datetime, tzone = "America/Whitehorse")

# Select columns for merging
QHI_temp_hourly <- QHI_temp_hourly %>%
  mutate(mean_temp = value) %>%
  select(location_id, datetime, mean_temp)

# Properly name columns
QHI_temp_daily <- QHI_temp_daily %>%
  mutate(mean_temp = value) %>%
  mutate(date = as.POSIXct(datetime, tz = "UTC")) %>%  # or use "America/Whitehorse"
  select(location_id, date, mean_temp)






#### Merge datasets ----
## Hourly datasets
flight_buzz_hourly <- hourly_summary_flightbuzzes_ARUQ_2024 %>%
  left_join(environmental_variables_hourly, by = "datetime") %>%
  left_join(QHI_temp_hourly, by = c("datetime", "location_id"))

flight_buzz_hourly <- flight_buzz_hourly %>%
  mutate(time_hour = hms::as_hms(floor_date(datetime, unit = "hour")))

write_csv(flight_buzz_hourly, "/Volumes/TundraBUZZ/data/clean/flight_buzz_hourly.csv")

## Daily datasets
flight_buzz_daily <- daily_summary_flightbuzzes_ARUQ_2024 %>%
  left_join(environmental_variables_daily, by = "date") %>%
  left_join(QHI_temp_daily, by = c("date", "location_id"))

write_csv(flight_buzz_daily, "/Volumes/TundraBUZZ/data/clean/flight_buzz_daily.csv")




#### Data visualization ----
flight_buzz_daily_sites <- flight_buzz_daily %>%
  filter(!location_id == "BEEBOX")
flight_buzz_beebox <- flight_buzz_daily %>%
  filter(location_id == "BEEBOX")

# Plot flight buzzes over time
ggplot(flight_buzz_daily_sites, aes(x = date, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="loess", aes(colour = microclimate)) +
  labs(x = "2024 Growing Season", 
       y = "Daily Bumblebee Flight Buzz Detections (s)",
       colour = "Microclimate") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) +
  scale_colour_manual(values = c("#440154", "forestgreen","gold"))

# Plot flight buzzes at BEEBOX over time
ggplot(flight_buzz_beebox, aes(x = date, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="loess", colour = "grey44") +
  labs(x = "2024 Growing Season", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 






#### Testing POLCAM data ----

polcam_data_long <- read_csv("/Volumes/TundraBUZZ/data/clean/polcam_data_long.csv")

flower_counts <- polcam_data_long %>%
  group_by(date, location_id) %>%
  summarize(total_flower_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  filter(total_flower_count > 0)

ggplot(flower_counts, aes(x = date, y = total_flower_count)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.7, color = "darkolivegreen") +
  #geom_line(color = "darkgreen") +
  labs(title = "Total Flower Count Over Time",
       x = "Date",
       y = "Total Flower Count") +
  theme_classic() +
  facet_wrap(~location_id, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Merge the two datasets
combined_data <- left_join(flight_buzz_daily, flower_counts, by = c("date", "location_id"))

scale_factor <- 10

combined_data_filtered <- combined_data %>%
  filter(!location_id == "BEEBOX")

# Plot with two y-axes using ggplot2 and ggplot2-secondary-axis functionality
ggplot(combined_data_filtered, aes(x = date)) +
  geom_point(aes(y = daily_duration_above_threshold)) +
  geom_point(aes(y = total_flower_count*5), colour = "darkolivegreen") +
  geom_smooth(method = "loess", aes(y = daily_duration_above_threshold, color = microclimate), se = TRUE) +
  #geom_smooth(method = "loess", aes(y = total_flower_count*5), colour = "darkolivegreen", se = FALSE) +
  scale_y_continuous(
    name = "Total Flight Buzz Duration (s)",
    sec.axis = sec_axis(~./5, name = "Total Flower Count")
  ) +
  facet_wrap(~location_id) +
  labs(title = "Flight Buzzes and Flower Counts Over Time") +
  scale_colour_manual(values = c("#440154", "forestgreen","gold")) +
  ylim(0,600) +
  theme_classic() +
  theme(legend.title = element_blank()) 
  

p1 <- ggplot(combined_data_filtered, aes(x = date)) +
  geom_point(aes(y = daily_duration_above_threshold)) +
  geom_smooth(method = "loess", aes(y = daily_duration_above_threshold, color = microclimate), se = TRUE) +
  scale_y_continuous(name = "Total Flight Buzz Duration (s)") +
  facet_wrap(~location_id) +
  labs(title = "Flight Buzzes and Flower Counts Over Time") +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  ylim(0, 600) +
  theme_classic() +
  theme(legend.title = element_blank())


filtered_data <- combined_data_filtered %>%
  filter(!is.na(total_flower_count))

p2 <- ggplot(filtered_data, aes(x = date, y = 1, fill = total_flower_count)) +
  geom_tile() +
  scale_fill_gradient(low = "forestgreen", high = "gold", name = "Flower Count") +
  facet_wrap(~location_id) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_blank()
  )

p1 / p2  + plot_layout(heights = c(6, 1))




#### Test ----
# Get unique location IDs
locations <- unique(combined_data_filtered$location_id)

# Create a list to store combined plots per location
plot_list <- list()

# Loop through each location
for (loc in locations) {
  df_loc <- combined_data_filtered %>% filter(location_id == loc)
  flower_df <- df_loc %>% filter(!is.na(total_flower_count))
  
  # Flower tile strip with viridis gradient
  p2 <- ggplot(flower_df, aes(x = date, y = 0.5, fill = total_flower_count)) +
    geom_tile() +
    scale_fill_viridis(option = "C", name = "Flower Count") +  # Using viridis palette
    scale_x_date(limits = c(date_min, date_max)) +
    theme_void() +
    theme(legend.position = "none")
  
  # Buzz plot
  p1 <- ggplot(df_loc, aes(x = date)) +
    geom_point(aes(y = daily_duration_above_threshold)) +
    geom_smooth(method = "loess", aes(y = daily_duration_above_threshold, color = microclimate), se = TRUE) +
    scale_y_continuous(name = "Buzz Duration (s)") +
    labs(title = paste("Location:", loc)) +
    scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
    ylim(0, 600) +
    theme_classic() +
    theme(legend.position = "none")
  
  # Combine tile strip + buzz plot for this location
  combined <- p1 / p2  + plot_layout(heights = c(6, 1))
  
  # Store in list
  plot_list[[loc]] <- combined
}

# Split the plot list into rows of 3
row1 <- cowplot::plot_grid(plotlist = plot_list[1:3], nrow = 1)
row2 <- cowplot::plot_grid(plotlist = plot_list[4:6], nrow = 1)
row3 <- cowplot::plot_grid(plotlist = plot_list[7:9], nrow = 1)

# Combine all rows vertically
final_plot <- cowplot::plot_grid(row1, row2, row3, ncol = 1)

# Show it!
final_plot






# 1. Ensure your full dataset has the date column in Date format
combined_data_filtered <- combined_data_filtered %>%
  mutate(
    date = as.Date(date),
    microclimate = factor(microclimate, levels = c("Cool", "Moderate", "Warm"))
  )

# 2. Get shared x-axis limits
date_min <- min(combined_data_filtered$date, na.rm = TRUE)
date_max <- max(combined_data_filtered$date, na.rm = TRUE)

# 3. Get unique location IDs
locations <- unique(combined_data_filtered$location_id)
locations <- sort(unique(combined_data_filtered$location_id))

# 4. Loop and build plots
plot_list <- list()

for (loc in locations) {
  
  # These must be *inside* the loop so you get location-specific data each time
  df_loc <- combined_data_filtered %>%
    filter(location_id == loc)
  
  flower_df <- df_loc %>%
    filter(!is.na(total_flower_count))
  
  # Flower tile strip with viridis gradient
  p2 <- ggplot(flower_df, aes(x = date, y = 0.5, fill = total_flower_count)) +
    geom_tile() +
    scale_fill_gradient(low = "forestgreen", high = "gold", name = "Flower Count") +
    scale_x_date(limits = c(date_min, date_max)) +
    theme_void() +
    theme(legend.position = "none")
  
  # Buzz plot
  p1 <- ggplot(df_loc, aes(x = date)) +
    geom_point(aes(y = daily_duration_above_threshold)) +
    geom_smooth(method = "loess", aes(y = daily_duration_above_threshold, color = microclimate, fill = microclimate), se = TRUE, alpha = 0.3) +
    scale_y_continuous(name = "Buzz Duration (s)") +
    scale_x_date(limits = c(date_min, date_max)) +
    scale_colour_manual(
      values = c("Cool" = "#440154", "Moderate" = "forestgreen", "Warm" = "gold")
    ) +
    scale_fill_manual(
      values = c("Cool" = "#440154", "Moderate" = "forestgreen", "Warm" = "gold")
    ) +
    ylim(0, 600) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title = element_blank()
          ) +
    facet_wrap(~location_id)
  
  # Combine and store
  combined <- p1 / p2 + plot_layout(heights = c(6, 0.5))
  plot_list[[loc]] <- combined
}

# Optional: Combine into rows and final plot
row1 <- cowplot::plot_grid(plotlist = plot_list[1:3], nrow = 1)
row2 <- cowplot::plot_grid(plotlist = plot_list[4:6], nrow = 1)
row3 <- cowplot::plot_grid(plotlist = plot_list[7:9], nrow = 1)

# Combine your rows (assuming row1, row2, row3 already created)
final_plot <- cowplot::plot_grid(row1, row2, row3, ncol = 1, rel_heights = c(1, 1, 1), align = "v", axis = "tblr", vjust = -1, hjust = -2) 

# Wrap with ggdraw() to apply margin spacing
final_plot_with_margin <- ggdraw(final_plot) +
  theme(plot.margin = margin(t = 10, r = 20, b = 30, l = 20))

# Then add labels with ggdraw
(final_with_labels <- ggdraw(final_plot_with_margin) +
  draw_label("2024 Growing Season", x = 0.5, y = 0.04, vjust = 1, angle = 0) +
  draw_label("Daily Bumblebee Flight Buzz Detections (s)", x = 0.02, y = 0.5, angle = 90, vjust = 1))


#####




# Plot flight buzzes by time of day
ggplot(flight_buzz_hourly, aes(x = time_hour, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "loess", aes(colour = microclimate)) +
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # +
  # facet_wrap(~location_id)

flight_buzz_hourly_centered <- flight_buzz_hourly %>%
  mutate(time_hour_shifted = (as.numeric(time_hour) - 7200) %% 86400)

ggplot(flight_buzz_hourly_centered, aes(x = time_hour_shifted, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "loess", aes(colour = microclimate)) +
  labs(x = "Time of Day", 
       y = "Hourly Flight Buzz Detection (s)") +
  scale_x_continuous(
    breaks = seq(0, 86400, by = 3600),
    labels = function(x) format(as.POSIXct((x + 7200) %% 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M")
  ) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Prepare data

buzz_beebox <- flight_buzz_hourly_centered %>%
  filter(location_id == "BEEBOX") %>%
  mutate(
    date = as.Date(datetime),  # Replace with your actual column name
    week = floor_date(date, unit = "week")
  )

buzz_beebox <- buzz_beebox %>%
  filter(!week == "2024-08-11")

# Plot
ggplot(buzz_beebox, aes(x = time_hour_shifted, y = total_duration_above_threshold)) +
  geom_point(aes(colour = as.factor(week)), alpha = 0.6) +  
  geom_smooth(aes(colour = as.factor(week)), method = "loess", se = FALSE) +  # One overall trend line
  labs(
    x = "Time of Day", 
    y = "Hourly Flight Buzz Detection (s)",
    colour = "Week of"
  ) +
  scale_x_continuous(
    breaks = seq(0, 86400, by = 3600),
    labels = function(x) format(as.POSIXct((x + 7200) %% 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M")
  ) +
  scale_colour_viridis_d() +  # Apply discrete Viridis color scale
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot flight buzzes by time of day, week
ggplot(flight_buzz_hourly, aes(x = time_hour, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id + month)


# Plot flight buzzes by time of day, week with sunrise and sunset
ggplot(summary_flightbuzzes_ARUQ_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~week) +
  geom_vline(data = QHI_sunrise_sunset_filtered, aes(xintercept = avg_sunrise), 
             linetype = "dashed", color = "orange", size = 1) +
  geom_vline(data = QHI_sunrise_sunset_filtered, aes(xintercept = avg_sunset), 
             linetype = "dashed", color = "orange4", size = 1) +
  geom_text(data = QHI_sunrise_sunset_filtered, 
            aes(x = avg_sunrise, y = 45, label = "Sunrise"), 
            color = "orange", angle = 90, vjust = -0.5, size = 3) +
  geom_text(data = QHI_sunrise_sunset_filtered, 
            aes(x = avg_sunset, y = 45, label = "Sunset"), 
            color = "orange4", angle = 90, vjust = -0.5, size = 3)





# Plot total flight buzz duration vs. mean temperature
ggplot(flight_buzz_hourly, aes(x = mean_temp, y = total_duration_above_threshold)) +
  geom_point(aes(color = as.factor(microclimate)), alpha = 0.2) +
  geom_smooth(method = "lm", aes(color = as.factor(microclimate)), se = TRUE) +
  labs(x = "Hourly Mean Temperature (°C)",
       y = "Hourly Flight Buzz Duration (s)",
       colour = "Microclimate") +
  geom_vline(xintercept = c(6, 12.6), color = c("orange", "orange4"), linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = 70, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = 70, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  ylim(0,100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold"))





#### Exploratory ----

library(broom)

mean_summer_temp <- read_csv("/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")

slopes_flower_adj <- combined_data %>%
  filter(!is.na(mean_temp), !is.na(total_flower_count), !is.na(daily_duration_above_threshold)) %>%
  group_by(location_id, microclimate) %>%
  filter(n() > 2) %>%  # Only keep groups with at least 3 complete rows
  do(tidy(lm(daily_duration_above_threshold ~ mean_temp + total_flower_count, data = .))) %>%
  filter(term == "mean_temp") %>%
  rename(slope = estimate)

slope_temp_df <- slopes_flower_adj %>%
  left_join(mean_summer_temp, by = "location_id")

ggplot(slope_temp_df, aes(x = summer_temp, y = slope)) +
  geom_errorbar(aes(ymin = slope - std.error, ymax = slope + std.error, colour = microclimate), width = 0.2) +
  geom_point(aes(colour = microclimate), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "grey4", se = TRUE) +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Temperature Sensitivity Slope (adj. for Flower Count)",
    colour = "Microclimate"
  ) +
  theme_classic() +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold"))



#### Other plotting ----

# Plot total flight buzz duration vs. mean temperature
ggplot(flight_buzz_hourly, aes(x = mean_temp, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Hourly Mean Temperature (°C)",
       y = "Hourly Flight Buzz Duration (s)") +
  geom_vline(xintercept = c(6, 12.6), color = c("orange", "orange4"), linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = 80, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = 80, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  ylim(0,100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot flight buzzes over time
ggplot(flight_buzz_daily, aes(x = date, y = daily_duration_above_threshold)) +
  geom_point(aes(colour=microclimate), alpha = 0.4) +  
  geom_smooth(aes(colour=microclimate), method= "loess", se = FALSE) +
  labs(x = "2024 Growing Season", 
       y = "Daily Bumblebee Flight Buzz Detections (s)",
       colour = "Microclimate") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold"))





# Fit separate models for each location_id
models_daily <- flight_buzz_daily %>%
  group_by(microclimate) %>%
  do(model = lm(log(daily_duration_above_threshold) ~ log(mean_temp), data = .))

# Generate predictions for each location_id
predictions <- flight_buzz_daily %>%
  group_by(microclimate) %>%
  do({
    model <- lm(log(daily_duration_above_threshold) ~ log(mean_temp), data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      mean_temp = .$mean_temp,
      daily_duration_above_threshold = .$daily_duration_above_threshold,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })



##### SOON: MAKE THIS IN BAYESIAN FRAMEWORK

# Plot total flight buzz duration vs. mean temperature
ggplot(predictions, aes(x = mean_temp, y = daily_duration_above_threshold)) +
  geom_point() +
  geom_line(aes(y = predicted, colour = microclimate), size = 1) +  # Regression line
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, colour = microclimate), alpha = 0.1) +  # Confidence interval
  
  labs(x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)",
       colour = "Microclimate") +
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = max(predictions$daily_duration_above_threshold, na.rm = TRUE) * 0.3, 
           label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = max(predictions$daily_duration_above_threshold, na.rm = TRUE) * 0.3, 
           label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  ylim(0,400) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~microclimate) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold"))









######## IN PROGRESS ######### -----
#### MICROCLIMATE DATA ----

# Read in microclimate data
aru_temp_daily_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv") #### RENAMED QHI_temp_daily
aru_temp_hourly_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv") #### RENAMED QHI_temp_hourly

# Filter for ARUQ0
aru_temp_daily_micro <- aru_temp_daily_micro %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day))

aruq0_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ0") %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, sep="-"), 
                               format="%Y-%m-%d", tz="UTC"))









#### DEBUG
aru_temp_hourly_micro <- aru_temp_hourly_micro %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day),
         hour = as.numeric(hour))

aruq0_data_plot_h <- aru_temp_hourly_micro %>%
  filter(aru_name == "ARUQ0") %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, hour), format="%Y-%m-%d %H", tz="UTC"))

# Aggregate total duration above threshold per day
summary_pred_duration_agg_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with daily microclimate data
merged_data_aruq0 <- summary_pred_duration_agg_ARUQ0 %>%
  left_join(aruq0_data_plot, by = c("date" = "datetime")) %>%
  filter(!is.na(month))  # Remove NAs in month column

# Aggregate total duration above threshold per hour
summary_pred_duration_agg_hourly_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date_time_hour = floor_date(datetime, "hour")) %>%
  group_by(date_time_hour) %>%
  summarise(total_duration_hour = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with hourly microclimate data
merged_data_aruq0_hourly <- summary_pred_duration_agg_hourly_ARUQ0 %>%
  left_join(aruq0_data_plot_h, by = c("date_time_hour" = "datetime"))

# Plot total flight buzz duration vs. mean temperature
ggplot(merged_data_aruq0, aes(x = mean_value, y = total_duration_day)) +
  geom_point(aes(color = as.factor(month))) +
  geom_smooth(method = "lm", aes(color = as.factor(month)), se = FALSE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature - ARUQ0",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  geom_vline(xintercept = c(6, 12.6), color = c("orange", "orange4"), linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Log-transformed plot
ggplot(merged_data_aruq0, aes(x = mean_value, y = log(total_duration_day))) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "skyblue4", se = TRUE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Log Total Flight Buzz Duration (s)") +
  theme_classic()



#### Work with ARUQ456 data ----
# Define threshold and filter data
threshold <- 8  
ARUQ456_2024_above_threshold <- ARUQ456_2024_pred_mapped %>%
  filter(BUZZ > threshold) %>%
  mutate(duration_above_threshold = 0.15)  # Each segment is 0.3s, so each overlap segment is 0.15s

# Save csv
write.csv(ARUQ456_2024_above_threshold, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ456_2024_pred_above_threshold.csv", row.names = FALSE)

# Summarize total duration above threshold per datetime
summary_pred_duration_ARUQ456_2024 <- ARUQ456_2024_above_threshold %>%
  group_by(datetime, location_id, microclimate) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold), .groups = "drop")

# Convert datetime to UTC-7
summary_pred_duration_ARUQ456_2024 <- summary_pred_duration_ARUQ456_2024 %>%
  mutate(time_of_day = hms::as_hms(format(datetime, "%H:%M:%S")))

# Save csv
write.csv(summary_pred_duration_ARUQ456_2024, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/summary_ARUQ456_2024_pred.csv", row.names = FALSE)


# Plot flight buzzes over time
ggplot(summary_pred_duration_ARUQ456_2024, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  
  labs(title = "Flight Buzzes Over Time (Threshold = 8)",
       x = "Datetime", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  #ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)

# Group by week for aggregation
summary_pred_duration_ARUQ456_2024 <- summary_pred_duration_ARUQ456_2024 %>%
  mutate(week = floor_date(datetime, "week"))

# Get sunrise and sunset times
lat <- 69.57
lon <- -138.91
# summary_pred_duration_ARUQ0_2024_sun <- summary_pred_duration_ARUQ0_2024_month %>%
#  mutate(date = as.Date(datetime))

# sun_times <- getSunlightTimes(data = data.frame(date = unique(summary_pred_duration_ARUQ0_2024_sun$date), lat = lat, lon = lon),
#                              keep = c("sunrise", "sunset"))

# Plot flight buzzes by time of day
ggplot(summary_pred_duration_ARUQ456_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~week + location_id)

# Plot flight buzzes by time of day
ggplot(summary_pred_duration_ARUQ456_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = loess) +
  labs(title = "Flight Buzzes Over a 24-hour Period (Threshold = 8)",
       x = "Time of Day", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
  #facet_wrap(~location_id)


#### Work with ARUQ4 data ----
# Define threshold and filter data
threshold <- 8  
ARUQ4_2024_above_threshold <- ARUQ4_2024_pred_mapped %>%
  filter(BUZZ > threshold) %>%
  mutate(duration_above_threshold = 0.15)  # Each segment is 0.3s, so each overlap segment is 0.15s

# Save csv
write.csv(ARUQ4_2024_above_threshold, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ4_2024_pred_above_threshold.csv", row.names = FALSE)

# Summarize total duration above threshold per datetime
summary_pred_duration_ARUQ4_2024 <- ARUQ4_2024_above_threshold %>%
  group_by(datetime, location_id, microclimate) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold), .groups = "drop")

# Convert datetime to UTC-7
summary_pred_duration_ARUQ4_2024 <- summary_pred_duration_ARUQ4_2024 %>%
  mutate(time_of_day = hms::as_hms(format(datetime, "%H:%M:%S")))

# Save csv
write.csv(summary_pred_duration_ARUQ4_2024, "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/summary_ARUQ4_2024_pred.csv", row.names = FALSE)


# Plot flight buzzes over time
ggplot(summary_pred_duration_ARUQ4_2024, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  
  labs(title = "Flight Buzzes Over Time (Threshold = 8)",
       x = "Datetime", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  #ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Plot flight buzzes over time
ggplot(summary_pred_duration_ARUQ4_2024, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point() +  
  labs(title = "Flight Buzzes Over Time (Threshold = 8)",
       x = "Datetime", 
       y = "Total Predicted Flight Buzz Duration (s)") +
  #ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#### Microclimate data ----
# Read in microclimate data
aru_temp_daily_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv")
aru_temp_hourly_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv")

# Clean aru_temp
aru_temp_daily_micro <- aru_temp_daily_micro %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day))

# Filter for ARUQ0
aruq0_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ0") %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, sep="-"), 
                               format="%Y-%m-%d", tz="UTC"))

# Filter for ARUQ56
aruq56_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name %in% c("ARUQ5", "ARUQ6")) %>%
  mutate(location_id = recode(aru_name, "ARUQ5" = "WARM1", "ARUQ6" = "COOL1")) %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, sep="-"), 
                               format="%Y-%m-%d", tz="UTC"))

# Filter for ARUQ4
aruq4_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ4") %>%
  mutate(location_id = recode(aru_name, "ARUQ4" = "COOL4")) %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, sep="-"), 
                               format="%Y-%m-%d", tz="UTC"))

# Aggregate total duration above threshold per day
summary_pred_duration_agg_ARUQ456_2024 <- summary_pred_duration_ARUQ456_2024 %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, location_id) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with daily microclimate data
merged_data_ARUQ56 <- summary_pred_duration_agg_ARUQ456_2024 %>%
  filter(location_id %in% c("WARM1", "COOL1")) %>%
  left_join(aruq56_data_plot, by = c("date" = "datetime", "location_id")) %>%
  filter(!is.na(month))  # Remove NAs in month column

hist(log(merged_data_ARUQ56$mean_value))
hist(log(merged_data_ARUQ56$total_duration_day))


# Fit separate models for each location_id
models_ARUQ56 <- merged_data_ARUQ56 %>%
  group_by(location_id) %>%
  do(model = lm(log(total_duration_day) ~ log(mean_value), data = .))

# Generate predictions for each location_id
predictions_ARUQ56 <- merged_data_ARUQ56 %>%
  group_by(location_id) %>%
  do({
    model <- lm(log(total_duration_day) ~ log(mean_value), data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      mean_value = .$mean_value,
      total_duration_day = .$total_duration_day,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })

# Plot total flight buzz duration vs. mean temperature
ggplot(predictions_ARUQ56, aes(x = mean_value, y = total_duration_day)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "skyblue4", size = 1) +  # Regression line
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +  # Confidence interval
  
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = max(predictions_ARUQ56$total_duration_day, na.rm = TRUE) * 0.8, 
           label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = max(predictions_ARUQ56$total_duration_day, na.rm = TRUE) * 0.8, 
           label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  ylim(0,500) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)



class(aruq56_data_plot$datetime)
aruq56_data_plot$date <- as.POSIXct(aruq56_data_plot$datetime)
class(summary_pred_duration_agg_ARUQ456_2024$date)




# Convert date columns to Date or POSIXct
summary_pred_duration_agg_ARUQ456_2024$date <- as.Date(summary_pred_duration_agg_ARUQ456_2024$date)
aruq56_data_plot$date <- as.Date(aruq56_data_plot$date)

summary_pred_duration_agg_ARUQ456_2024 <- summary_pred_duration_agg_ARUQ456_2024 %>%
  filter(!location_id == "WARM2")

ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_agg_ARUQ456_2024, aes(x = date, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Temperature curve
  geom_smooth(data = aruq56_data_plot, aes(x = date, y = mean_value), color = "orange3", method = "loess", alpha = 0.7, se = FALSE) +
  geom_smooth(data = summary_pred_duration_agg_ARUQ456_2024, aes(x = date, y = total_duration_day/24), color = "skyblue4", method = "loess", se = TRUE) +
  geom_line(data = aruq56_data_plot, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq56_data_plot, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Customizing the plot
  labs(
    title = "Flight Buzzes and Temperature Over Time",
    x = "Datetime", 
    y = "Mean Flight Buzz Duration per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())
  ) +
  
  # Ensure the x-axis is properly formatted and limited
  scale_x_date(limits = c(as.Date("2024-06-20"), max(summary_pred_duration_agg_ARUQ456_2024$date, na.rm = TRUE))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") + 
  
  facet_wrap(~location_id)



#### Plotting ARUQ4 ----
# Aggregate total duration above threshold per day
summary_pred_duration_agg_ARUQ4_2024 <- summary_pred_duration_ARUQ4_2024 %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, location_id) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with daily microclimate data
merged_data_ARUQ4 <- summary_pred_duration_agg_ARUQ4_2024 %>%
  filter(location_id == "COOL4") %>%
  left_join(aruq4_data_plot, by = c("date" = "datetime", "location_id")) %>%
  filter(!is.na(month))  # Remove NAs in month column

hist(log(merged_data_ARUQ4$mean_value))
hist(log(merged_data_ARUQ4$total_duration_day))


# Fit separate models for each location_id
models_ARUQ4 <- merged_data_ARUQ4 %>%
  group_by(location_id) %>%
  do(model = lm(log(total_duration_day) ~ log(mean_value), data = .))

# Generate predictions for each location_id
predictions_ARUQ4 <- merged_data_ARUQ4 %>%
  group_by(location_id) %>%
  do({
    model <- lm(log(total_duration_day) ~ log(mean_value), data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      mean_value = .$mean_value,
      total_duration_day = .$total_duration_day,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })

# Plot total flight buzz duration vs. mean temperature
ggplot(predictions_ARUQ4, aes(x = mean_value, y = total_duration_day)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "skyblue4", size = 1) +  # Regression line
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +  # Confidence interval
  
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = max(predictions_ARUQ4$total_duration_day, na.rm = TRUE) * 0.8, 
           label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = max(predictions_ARUQ4$total_duration_day, na.rm = TRUE) * 0.8, 
           label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  ylim(0,200) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id)



class(aruq4_data_plot$datetime)
aruq4_data_plot$date <- as.POSIXct(aruq4_data_plot$datetime)
class(summary_pred_duration_agg_ARUQ4_2024$date)



# Convert date columns to Date or POSIXct
summary_pred_duration_agg_ARUQ4_2024$date <- as.Date(summary_pred_duration_agg_ARUQ4_2024$date)
aruq4_data_plot$date <- as.Date(aruq4_data_plot$date)

ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_agg_ARUQ4_2024, aes(x = date, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Temperature curve
  geom_smooth(data = aruq4_data_plot, aes(x = date, y = mean_value), color = "orange3", method = "loess", alpha = 0.7, se = FALSE) +
  geom_smooth(data = summary_pred_duration_agg_ARUQ4_2024, aes(x = date, y = total_duration_day/24), color = "skyblue4", method = "loess", se = TRUE) +
  geom_line(data = aruq4_data_plot, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq4_data_plot, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Customizing the plot
  labs(
    title = "Flight Buzzes and Temperature Over Time - ARUQ4",
    x = "Datetime", 
    y = "Mean Flight Buzz Duration per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())
  ) +
  
  # Ensure the x-axis is properly formatted and limited
  scale_x_date(limits = c(as.Date("2024-06-20"), max(summary_pred_duration_agg_ARUQ4_2024$date, na.rm = TRUE))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom")




#### POLCAM DATA ----

polcam <- read.csv("/Volumes/TundraBUZZ/data/raw/POLCAM_daily_quick_raw.csv")


# Merge to replace aru_id with location_id
polcam_mapped <- polcam %>%
  select(-c(site,year)) %>%
  left_join(location_mapping, by = c("plot" = "polcam_id")) %>%
  select(-c(aru_id, plot,tomst_id,site,year,notes,observer))  # Remove aru_id, now using location_id

str(polcam_mapped)

# Summing the flower counts per day and location_id
polcam_mapped$flower_count <- rowSums(polcam_mapped[, c("sal_arc_fem", "sal_arc_male", "sal_rich_fem", 
                                                        "sal_rich_male", "sal_ret_open", "sal_pul_fem", 
                                                        "sal_pul_male", "lup_arc", "ped_all", "ped_cap", 
                                                        "dry_int", "astra", "oxy", "stel_long", "sen", 
                                                        "cast", "cas_tet", "pyr_gra", "bis_viv")], na.rm = TRUE)

# Summarize the total flower count by day and location
flower_summary <- polcam_mapped %>%
  group_by(date, location_id) %>%
  summarise(total_flower_count = sum(flower_count), .groups = "drop")



#### ARUQ56 ----

# Merge the flower count data with the other datasets
merged_data <- merge(summary_pred_duration_agg_ARUQ456_2024, flower_summary, by = c("date", "location_id"))
merged_data <- merge(merged_data, aruq56_data_plot, by = c("date", "location_id"))


# Filter out rows where total_flower_count is zero
merged_data_filtered <- merged_data %>%
  filter(total_flower_count > 0)

# Plot
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = merged_data, aes(x = date, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Plot the temperature curve
  geom_smooth(data = merged_data, aes(x = date, y = mean_value), color = "orange3", method = "loess", alpha = 0.7, se = FALSE) +
  geom_smooth(data = merged_data, aes(x = date, y = total_duration_day/24), color = "skyblue4", method = "loess", se = FALSE) +
  geom_line(data = merged_data, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = merged_data, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Add the total flower count as a new series
  geom_smooth(data = merged_data_filtered, aes(x = date, y = total_flower_count), color = "darkolivegreen", size = 1.2, se = FALSE) +
  geom_point(data = merged_data_filtered, aes(x = date, y = total_flower_count), color = "darkolivegreen", size = 2, alpha = 0.7, se = FALSE) +
  
  # Customizing the plot
  labs(
    title = "Flight Buzzes, Temperature, and Flower Count Over Time",
    x = "Datetime", 
    y = "Mean Flight Buzz Duration per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature | Green: Flower Count"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())
  ) +
  
  # Ensure the x-axis is properly formatted and limited
  scale_x_date(limits = c(as.Date("2024-06-20"), max(merged_data$date, na.rm = TRUE))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") + 
  
  facet_wrap(~location_id)




# Filter out rows with NA, NaN, or Inf values
merged_data_clean <- merged_data %>%
  filter(!is.na(total_duration_day), !is.na(total_flower_count)) %>%
  filter(!is.nan(total_duration_day), !is.nan(total_flower_count)) %>%
  filter(total_duration_day != Inf, total_flower_count != Inf)


hist(log(merged_data_clean$total_flower_count))

# Fit separate models for each location_id
models_ARUQ56_flowering <- merged_data_clean %>%
  group_by(location_id) %>%
  do(model = lm(log(total_duration_day) ~ total_flower_count, data = .))

# Generate predictions for each location_id
predictions_ARUQ56_flowering <- merged_data_clean %>%
  group_by(location_id) %>%
  do({
    model <- lm(log(total_duration_day) ~ total_flower_count, data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      total_flower_count = .$total_flower_count,  # Ensure this column is included
      total_duration_day = .$total_duration_day,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })


# Plotting
ggplot(merged_data_clean, aes(x = total_flower_count, y = total_duration_day)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_line(data = predictions_ARUQ56_flowering, aes(x = total_flower_count, y = predicted), color = "skyblue4", size = 1) +
  geom_ribbon(data = predictions_ARUQ56_flowering, aes(x = total_flower_count, ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +
  # Add model coefficients and p-values as text annotations
  # geom_text(data = predictions_ARUQ56_flowering %>%
  #            group_by(location_id) %>%
  #            slice(1), 
  #          aes(x = 1, y = 1600, label = paste("Intercept =", round(coef(models_ARUQ56_flowering)[1], 2), "\np-value =", round(summary(models_ARUQ56_flowering)$coefficients[1,4], 4)),
  #              color = "black", size = 5)) +  # Intercept annotation with p-value
  #geom_text(data = predictions_ARUQ56_flowering %>%
  #            group_by(location_id) %>%
  #            slice(1),
  #          aes(x = 1, y = 1400, label = paste("Slope =", round(coef(models_ARUQ56_flowering)[2], 2), "\np-value =", round(summary(models_ARUQ56_flowering)$coefficients[2,4], 4)),
  #              color = "black", size = 5)) +  # Slope annotation with p-value
  labs(
    title = "Total Flight Buzz Duration vs Flowering Count",
    x = "Daily Flowering Count",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  theme_classic() +
  facet_wrap(~location_id)





#### TEST ARUQ56 ----
# Fit linear model
model_merged_data_clean <- lm(total_duration_day ~ total_flower_count + mean_value, 
                         data = merged_data_clean)

summary(model_merged_data_clean)
plot(model_merged_data_clean)  # Residuals vs. fitted values
qqnorm(resid(model_merged_data_clean))
qqline(resid(model_merged_data_clean))


merged_data_clean$datetime <- as.POSIXct(merged_data_clean$datetime)  # Ensure datetime is POSIXct
merged_data_clean$datetime_num <- as.numeric(merged_data_clean$datetime) / (60*60*24)  # Convert to days


# Fit GAMM model
model_merged_gam <- gam(total_duration_day ~ 
                         s(datetime_num, bs = "cs") +  
                         total_flower_count + mean_value + location_id,                
                       data = merged_data_clean, 
                       method = "REML")

summary(model_merged_gam)


# Plot effect of date on activity
plot(model_merged_gam, pages = 1, shade = TRUE)

# Check residuals
gam.check(model_merged_gam)


# Fit interaction GAMM model
model_merged_gam_interaction <- gam(total_duration_day ~ 
                                     s(datetime_num, bs = "cs") +  
                                     total_flower_count * mean_value + location_id,  # Interaction term
                                   data = merged_data_clean, 
                                   method = "REML")

summary(model_merged_gam_interaction)


# Visualize interaction
visreg(model_merged_gam_interaction, "total_flower_count", by = "mean_value", overlay = TRUE)









#### ARUQ4 ----
# Merge the flower count data with the other datasets
merged_data_aruq4 <- merge(summary_pred_duration_agg_ARUQ4_2024, flower_summary, by = c("date", "location_id"))
merged_data_aruq4 <- merge(merged_data_aruq4, aruq4_data_plot, by = c("date", "location_id"))


# Filter out rows where total_flower_count is zero
merged_data_filtered_aruq4 <- merged_data_aruq4 %>%
  filter(total_flower_count > 0)

# Plot
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = merged_data_aruq4, aes(x = date, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Plot the temperature curve
  geom_smooth(data = merged_data_aruq4, aes(x = date, y = mean_value), color = "orange3", method = "loess", alpha = 0.7, se = FALSE) +
  geom_smooth(data = merged_data_aruq4, aes(x = date, y = total_duration_day/24), color = "skyblue4", method = "loess", se = FALSE) +
  geom_line(data = merged_data_aruq4, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = merged_data_aruq4, aes(x = date, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Add the total flower count as a new series
  geom_smooth(data = merged_data_filtered_aruq4, aes(x = date, y = total_flower_count), color = "darkolivegreen", size = 1.2, se = FALSE) +
  geom_point(data = merged_data_filtered_aruq4, aes(x = date, y = total_flower_count), color = "darkolivegreen", size = 2, alpha = 0.7, se = FALSE) +
  
  # Customizing the plot
  labs(
    title = "Flight Buzzes, Temperature, and Flower Count Over Time - ARUQ4",
    x = "Datetime", 
    y = "Mean Flight Buzz Duration per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature | Green: Flower Count"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 25),
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())
  ) +
  
  # Ensure the x-axis is properly formatted and limited
  scale_x_date(limits = c(as.Date("2024-06-20"), max(merged_data_aruq4$date, na.rm = TRUE))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom")




# Filter out rows with NA, NaN, or Inf values
merged_data_clean_aruq4 <- merged_data_aruq4 %>%
  filter(!is.na(total_duration_day), !is.na(total_flower_count)) %>%
  filter(!is.nan(total_duration_day), !is.nan(total_flower_count)) %>%
  filter(total_duration_day != Inf, total_flower_count != Inf)


hist(log(merged_data_clean_aruq4$total_flower_count))

# Fit separate models for each location_id
models_ARUQ4_flowering <- merged_data_clean_aruq4 %>%
  group_by(location_id) %>%
  do(model = lm(log(total_duration_day) ~ total_flower_count, data = .))

# Generate predictions for each location_id
predictions_ARUQ4_flowering <- merged_data_clean_aruq4 %>%
  group_by(location_id) %>%
  do({
    model <- lm(log(total_duration_day) ~ total_flower_count, data = .)
    pred <- predict(model, newdata = ., se.fit = TRUE)
    tibble(
      total_flower_count = .$total_flower_count,  # Ensure this column is included
      total_duration_day = .$total_duration_day,
      predicted = exp(pred$fit),  # Convert back from log scale
      lower_bound = exp(pred$fit - 1.96 * pred$se.fit),
      upper_bound = exp(pred$fit + 1.96 * pred$se.fit)
    )
  })


# Plotting
ggplot(merged_data_clean_aruq4, aes(x = total_flower_count, y = total_duration_day)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_line(data = predictions_ARUQ4_flowering, aes(x = total_flower_count, y = predicted), color = "skyblue4", size = 1) +
  geom_ribbon(data = predictions_ARUQ4_flowering, aes(x = total_flower_count, ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +
  # Add model coefficients and p-values as text annotations
  # geom_text(data = predictions_ARUQ56_flowering %>%
  #            group_by(location_id) %>%
  #            slice(1), 
  #          aes(x = 1, y = 1600, label = paste("Intercept =", round(coef(models_ARUQ56_flowering)[1], 2), "\np-value =", round(summary(models_ARUQ56_flowering)$coefficients[1,4], 4)),
  #              color = "black", size = 5)) +  # Intercept annotation with p-value
  #geom_text(data = predictions_ARUQ56_flowering %>%
  #            group_by(location_id) %>%
  #            slice(1),
  #          aes(x = 1, y = 1400, label = paste("Slope =", round(coef(models_ARUQ56_flowering)[2], 2), "\np-value =", round(summary(models_ARUQ56_flowering)$coefficients[2,4], 4)),
  #              color = "black", size = 5)) +  # Slope annotation with p-value
  labs(
    title = "Total Flight Buzz Duration vs Flowering Count - ARUQ4",
    x = "Daily Flowering Count",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  theme_classic() 



#### Test ARUQ4 ----
# Fit linear model
model_aruq4_merged <- lm(total_duration_day ~ total_flower_count + mean_value, 
              data = merged_data_aruq4)

summary(model_aruq4_merged)
plot(model_aruq4_merged)  # Residuals vs. fitted values
qqnorm(resid(model_aruq4_merged))
qqline(resid(model_aruq4_merged))


merged_data_aruq4$datetime <- as.POSIXct(merged_data_aruq4$datetime)  # Ensure datetime is POSIXct
merged_data_aruq4$datetime_num <- as.numeric(merged_data_aruq4$datetime) / (60*60*24)  # Convert to days


# Fit GAMM model
model_aruq4_gam <- gam(total_duration_day ~ 
                         s(datetime_num, bs = "cs") +  
                         total_flower_count +       
                         mean_value,                
                       data = merged_data_aruq4, 
                       method = "REML")

summary(model_aruq4_gam)


# Plot effect of date on activity
plot(model_aruq4_gam, pages = 1, shade = TRUE)

# Check residuals
gam.check(model_aruq4_gam)


# Fit interaction GAMM model
model_aruq4_gam_interaction <- gam(total_duration_day ~ 
                                     s(datetime_num, bs = "cs") +  
                                     total_flower_count * mean_value,  # Interaction term
                                   data = merged_data_aruq4, 
                                   method = "REML")

summary(model_aruq4_gam_interaction)


# Visualize interaction
visreg(model_aruq4_gam_interaction, "total_flower_count", by = "mean_value", overlay = TRUE)






#### DEBUG ----
aru_temp_hourly_micro <- aru_temp_hourly_micro %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day),
         hour = as.numeric(hour))

aruq0_data_plot_h <- aru_temp_hourly_micro %>%
  filter(aru_name == "ARUQ0") %>%
  mutate(datetime = as.POSIXct(paste(2024, month, day, hour), format="%Y-%m-%d %H", tz="UTC"))

# Aggregate total duration above threshold per day
summary_pred_duration_agg_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with daily microclimate data
merged_data_aruq0 <- summary_pred_duration_agg_ARUQ0 %>%
  left_join(aruq0_data_plot, by = c("date" = "datetime")) %>%
  filter(!is.na(month))  # Remove NAs in month column

# Aggregate total duration above threshold per hour
summary_pred_duration_agg_hourly_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date_time_hour = floor_date(datetime, "hour")) %>%
  group_by(date_time_hour) %>%
  summarise(total_duration_hour = sum(total_duration_above_threshold, na.rm = TRUE), .groups = "drop")

# Merge with hourly microclimate data
merged_data_aruq0_hourly <- summary_pred_duration_agg_hourly_ARUQ0 %>%
  left_join(aruq0_data_plot_h, by = c("date_time_hour" = "datetime"))

# Plot total flight buzz duration vs. mean temperature
ggplot(merged_data_aruq0, aes(x = mean_value, y = total_duration_day)) +
  geom_point(aes(color = as.factor(month))) +
  geom_smooth(method = "lm", aes(color = as.factor(month)), se = FALSE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature - ARUQ0",
       x = "Daily Mean Temperature (°C)",
       y = "Total Flight Buzz Duration (s)") +
  geom_vline(xintercept = c(6, 12.6), color = c("orange", "orange4"), linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Log-transformed plot
ggplot(merged_data_aruq0, aes(x = mean_value, y = log(total_duration_day))) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "skyblue4", se = TRUE) +
  labs(title = "Total Flight Buzz Duration vs. Mean Temperature",
       x = "Daily Mean Temperature (°C)",
       y = "Log Total Flight Buzz Duration (s)") +
  theme_classic()
