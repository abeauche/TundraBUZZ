# ====================================================
# Script Name: flowering_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-04-08
# Last Modified: 2025-04-11
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
library(patchwork)
library(brms)
library(tidybayes)
library(ggeffects)
library(broom.mixed)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

# Load data
polcam_data <- read_csv("/Volumes/TundraBUZZ/data/raw/POLCAM_data.csv", skip = 1)
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")
mean_summer_temperature <- read_csv("/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")
FloRes_raw <- read_csv("/Volumes/TundraBUZZ/FloRes_database/doi_10_5061_dryad_djh9w0w29__v20220825/Data/5_FloRes_no_corolla.csv")


#### Tidy data ----
# Select and format columns of interest
polcam_data <- polcam_data %>%
  mutate(date = as.POSIXct(`Date (year-month-day)`),
         plot = Plot)
polcam_data <- polcam_data %>%
  select(-c(Plot, Site, `Date (year-month-day)`, Time, Observer, Notes))

# Rename plot to location_id
location_mapping <- location_mapping %>%
  select(polcam_id, location_id, microclimate)
polcam_data <- polcam_data %>%
  left_join(location_mapping, by = c("plot"="polcam_id")) %>%
  select(-plot)

# Pivot longer
polcam_data_long <- polcam_data %>%
  pivot_longer(
    cols = -c(date, location_id, microclimate),  # keep these columns as-is
    names_to = "flower_type",                   # new column for former column names
    values_to = "count"                     # new column for values
  )
polcam_data_long <- polcam_data_long %>%
  mutate(species = sub("^([^_]+_[^_]+)_.*", "\\1", flower_type))

write_csv(polcam_data_long, "/Volumes/TundraBUZZ/data/clean/polcam_data_long.csv")

# Group per species
polcam_data_species <- polcam_data_long %>%
  group_by(species, location_id, date, microclimate) %>%
  summarize(count = sum(count))



#### Summarize flowering data ----
# Calculate phenology metrics
flowering_metrics <- polcam_data_species %>%
  filter(count > 0) %>%
  group_by(location_id, microclimate) %>%
  summarize(
    first_flowering = min(date),
    last_flowering = max(date),
    duration_days = as.numeric(difftime(last_flowering, first_flowering, units = "days")),
    .groups = "drop"
  )

# Calculate peak flowering (in case of ties, take middle date)
peak_flowering <- polcam_data_species %>%
  group_by(location_id) %>%
  filter(count == max(count, na.rm = TRUE), count > 0) %>%
  arrange(date) %>%
  summarize(
    peak_flowering = {
      n <- n()
      if (n %% 2 == 1) {
        date[ceiling(n / 2)]
      } else {
        # for even number of tied dates, take the later of the two middle ones
        date[n / 2 + 1]
      }
    },
    .groups = "drop"
  )

# Combine flowering metrics and peak date
flowering_summary <- flowering_metrics %>%
  left_join(peak_flowering, by = c("location_id")) %>%
  left_join(mean_summer_temperature, by = "location_id")


flowering_summary <- flowering_summary %>%
  mutate(first_flowering = as.Date(first_flowering)) %>%
  mutate(last_flowering = as.Date(last_flowering)) %>%
  mutate(peak_flowering = as.Date(peak_flowering))


# Plot first flowering date
ggplot(flowering_summary, aes(x = summer_temp, y = last_flowering)) +
  geom_point(aes(colour = microclimate), size = 3) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey44") +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "First Flowering Date",
    colour = "Microclimate"
  ) +
  theme_classic() +
  scale_colour_manual(values = c("#440154", "forestgreen","gold"))


# Define common theme and color scale
microclimate_colors <- c("#440154", "forestgreen", "gold")

# Define origin date (e.g., first date in your dataset)
origin_date <- as.Date("2024-06-23")

# Mutate date objects to numeric
flowering_bayes <- flowering_summary %>%
  mutate(
    first_flowering_num = as.numeric(first_flowering - origin_date),
    last_flowering_num = as.numeric(last_flowering - origin_date),
    peak_flowering_num = as.numeric(peak_flowering - origin_date)
  )



#### FIT MODELS

# First flowering model
mod_first <- brm(first_flowering_num ~ summer_temp, 
                 data = flowering_bayes, family = gaussian(), cores = 4)
summary(mod_first)
plot(mod_first)
pp_check(mod_first)

# Last flowering model
mod_last <- brm(last_flowering_num ~ summer_temp, 
                data = flowering_bayes, family = gaussian(), cores = 4)
summary(mod_last)
plot(mod_last)
pp_check(mod_last)

# Peak flowering model
mod_peak <- brm(peak_flowering_num ~ summer_temp, 
                data = flowering_bayes, family = gaussian(), cores = 4)
summary(mod_peak)
plot(mod_peak)
pp_check(mod_peak)

# Duration (numeric so no conversion needed)
mod_duration <- brm(duration_days ~ summer_temp, 
                    data = flowering_bayes, family = gaussian(), cores = 4)
summary(mod_duration)
plot(mod_duration)
pp_check(mod_duration)


# Save each model as an RDS file
saveRDS(mod_first, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/mod_first.rds")
saveRDS(mod_last, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/mod_last.rds")
saveRDS(mod_peak, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/mod_peak.rds")
saveRDS(mod_duration, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/mod_duration.rds")


#### Coding club code
# mod_first_fit <- flowering_bayes %>%
#   add_predicted_draws(mod_first) %>%  # adding the posterior distribution
#   ggplot(aes(x = summer_temp, y = first_flowering_num)) +  
#   stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
#                   alpha = 0.5, colour = "black") +
#   scale_fill_brewer(palette = "Greys") +
#   geom_point(data = flowering_bayes, aes(colour = microclimate), size = 3) +
#   scale_colour_manual(values = c("#440154", "forestgreen","gold"))+
#   theme_classic()


### First flowering date
# Get predicted values from Bayesian model
preds_first <- ggpredict(mod_first, terms = "summer_temp [all]")  # or use pretty() to control breaks

# Convert predictions to Date format if the response was numeric date
preds_first$predicted_date <- as.Date(preds_first$predicted, origin = "2024-06-23")
preds_first$conf.low_date <- as.Date(preds_first$conf.low, origin = "2024-06-23")
preds_first$conf.high_date <- as.Date(preds_first$conf.high, origin = "2024-06-23")

# Plot
mod_first_plot <- ggplot(preds_first, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes, aes(x = summer_temp, y = as.Date(first_flowering_num, origin = "2024-06-23"), colour = microclimate), size = 3) +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "First Flowering Date",
    colour = "Microclimate"
  ) +
  theme_classic() 



### Last flowering date
# Get predicted values from Bayesian model
preds_last <- ggpredict(mod_last, terms = "summer_temp [all]")  # or use pretty() to control breaks

# Convert predictions to Date format if the response was numeric date
preds_last$predicted_date <- as.Date(preds_last$predicted, origin = "2024-06-23")
preds_last$conf.low_date <- as.Date(preds_last$conf.low, origin = "2024-06-23")
preds_last$conf.high_date <- as.Date(preds_last$conf.high, origin = "2024-06-23")

# Plot
mod_last_plot <- ggplot(preds_last, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes, aes(x = summer_temp, y = as.Date(last_flowering_num, origin = "2024-06-23"), colour = microclimate), size = 3) +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Last Flowering Date",
    colour = "Microclimate"
  ) +
  theme_classic() 


### Peak flowering date
# Get predicted values from Bayesian model
preds_peak <- ggpredict(mod_peak, terms = "summer_temp [all]")  # or use pretty() to control breaks

# Convert predictions to Date format if the response was numeric date
preds_peak$predicted_date <- as.Date(preds_peak$predicted, origin = "2024-06-23")
preds_peak$conf.low_date <- as.Date(preds_peak$conf.low, origin = "2024-06-23")
preds_peak$conf.high_date <- as.Date(preds_peak$conf.high, origin = "2024-06-23")

# Plot
mod_peak_plot <- ggplot(preds_peak, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes, aes(x = summer_temp, y = as.Date(peak_flowering_num, origin = "2024-06-23"), colour = microclimate), size = 3) +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Peak Flowering Date",
    colour = "Microclimate"
  ) +
  theme_classic() 


### Duration
# Get predicted values from Bayesian model
preds_duration <- ggpredict(mod_duration, terms = "summer_temp [all]")  # or use pretty() to control breaks

# Plot
mod_duration_plot <- ggplot(preds_duration, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes, aes(x = summer_temp, y = duration_days, colour = microclimate), size = 3) +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Flowering Duration",
    colour = "Microclimate"
  ) +
  theme_classic()


# Combine patchwork layout
flowering_season_traits_plot <- (mod_first_plot + mod_last_plot) / 
  (mod_peak_plot + mod_duration_plot) + 
  plot_layout(guides = "collect")

# Save the combined plot
ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/flowering_season_traits_plot.pdf",
  plot = flowering_season_traits_plot,
  width = 10,       # adjust based on layout
  height = 8
)



# Extract and label slopes
slopes <- bind_rows(
  tidy(mod_first, effects = "fixed", parameters = "summer_temp") %>% mutate(trait = "First flowering date"),
  tidy(mod_last, effects = "fixed", parameters = "summer_temp") %>% mutate(trait = "Last flowering date"),
  tidy(mod_peak, effects = "fixed", parameters = "summer_temp") %>% mutate(trait = "Peak flowering date"),
  tidy(mod_duration, effects = "fixed", parameters = "summer_temp") %>% mutate(trait = "Flowering duration")
) %>%
  select(trait, estimate, std.error, conf.low, conf.high)

# Set the order of traits
slopes <- slopes %>%
  mutate(trait = factor(trait, levels = c(
    "First flowering date",
    "Last flowering date",
    "Peak flowering date",
    "Flowering duration"
  )))


# Plot
flowering_slope_effect_size <- ggplot(slopes, aes(x = estimate, y = trait)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    x = "Effect Size and Credible Interval of Mean Summer Temperature",
    y = "Flowering Season Trait"
  ) +
  theme_classic()

# Save plot
ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/flowering_slope_effect_size.pdf",
  plot = flowering_slope_effect_size,
  width = 8,
  height = 3.5
)





#### Summarize flowering data ----
# Calculate phenology metrics
flowering_metrics_per_species <- polcam_data_species %>%
  filter(count > 0) %>%
  group_by(location_id, species) %>%
  summarize(
    first_flowering = min(date),
    last_flowering = max(date),
    duration_days = as.numeric(difftime(last_flowering, first_flowering, units = "days")),
    .groups = "drop"
  )

# Calculate peak flowering (in case of ties, take middle date)
peak_flowering_per_species <- polcam_data_species %>%
  group_by(location_id, species) %>%
  filter(any(!is.na(count) & count > 0)) %>%  # keep only groups with at least one non-NA, non-zero count
  filter(count == max(count, na.rm = TRUE)) %>%
  arrange(date) %>%
  summarize(
    peak_flowering = {
      n <- n()
      if (n %% 2 == 1) {
        date[ceiling(n / 2)]
      } else {
        date[n / 2 + 1]  # take later of two middle dates
      }
    },
    .groups = "drop"
  )

# Combine flowering metrics and peak date
flowering_summary_per_species <- flowering_metrics_per_species %>%
  left_join(peak_flowering_per_species, by = c("location_id", "species")) %>%
  left_join(mean_summer_temperature, by = "location_id")

flowering_summary_per_species <- flowering_summary_per_species %>%
  mutate(first_flowering = as.Date(first_flowering)) %>%
  mutate(last_flowering = as.Date(last_flowering)) %>%
  mutate(peak_flowering = as.Date(peak_flowering))

flowering_summary_per_species_filtered <- flowering_summary_per_species %>%
  filter(species %in% c("sal_arc", "dry_int", "lup_arc"))

# Plot first flowering date
ggplot(flowering_summary_per_species_filtered, aes(x = summer_temp, color = species)) +
  geom_point(aes(y = first_flowering), size = 3, alpha = 0.8) +
  geom_point(aes(y = last_flowering), size = 3, alpha = 0.8) +
  geom_smooth(aes(y = first_flowering), method = "lm", se = FALSE) +
  geom_smooth(aes(y = last_flowering), method = "lm", se = FALSE) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "First Flowering Date",
    colour = "Species"
  ) +
  theme_classic() 


# Plot first flowering date
ggplot(flowering_summary_per_species_filtered, aes(x = summer_temp, y = first_flowering, color = species)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "First Flowering Date",
    colour = "Species"
  ) +
  theme_classic() 



# Mutate date objects to numeric
flowering_bayes_species <- flowering_summary_per_species_filtered %>%
  mutate(
    first_flowering_num = as.numeric(first_flowering - origin_date),
    last_flowering_num = as.numeric(last_flowering - origin_date),
    peak_flowering_num = as.numeric(peak_flowering - origin_date)
  )

# Model first flowering date per species
mod_first_species <- brm(first_flowering_num ~ summer_temp + (1|species), 
                 data = flowering_bayes_species, family = gaussian(), cores = 4,  control = list(adapt_delta = 0.95)  # default is 0.8
)

summary(mod_first_species)
plot(mod_first_species)
pp_check(mod_first_species)

# Model last flowering date per species
mod_last_species <- brm(last_flowering_num ~ summer_temp + (1|species), 
                         data = flowering_bayes_species, family = gaussian(), cores = 4,  control = list(adapt_delta = 0.95)  # default is 0.8
)

summary(mod_last_species)
plot(mod_last_species)
pp_check(mod_last_species)

# Model peak flowering date per species
mod_peak_species <- brm(peak_flowering_num ~ summer_temp + (1|species), 
                         data = flowering_bayes_species, family = gaussian(), cores = 4,  control = list(adapt_delta = 0.95)  # default is 0.8
)
summary(mod_peak_species)
plot(mod_peak_species)
pp_check(mod_peak_species)

# Model duration per species
mod_duration_species <- brm(duration_days ~ summer_temp + (1|species), 
                        data = flowering_bayes_species, family = gaussian(), cores = 4,  control = list(adapt_delta = 0.99)
)
summary(mod_duration_species)
plot(mod_duration_species)
pp_check(mod_duration_species)


# Save each model as an RDS file
saveRDS(mod_first_species, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/mod_first_species.rds")
saveRDS(mod_last_species, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/mod_last_species.rds")
saveRDS(mod_peak_species, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/mod_peak_species.rds")
saveRDS(mod_duration_species, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/mod_duration_species.rds")


### First flowering date per species
# Get predicted values from Bayesian model
preds_first_species <- ggpredict(mod_first_species, terms = "summer_temp [all]")  # or use pretty() to control breaks

# Convert predictions to Date format if the response was numeric date
preds_first_species$predicted_date <- as.Date(preds_first_species$predicted, origin = "2024-06-23")
preds_first_species$conf.low_date <- as.Date(preds_first_species$conf.low, origin = "2024-06-23")
preds_first_species$conf.high_date <- as.Date(preds_first_species$conf.high, origin = "2024-06-23")

# Plot
mod_first_species_plot <- ggplot(preds_first_species, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes_species, aes(x = summer_temp, y = as.Date(first_flowering_num, origin = "2024-06-23"), colour = species), size = 3) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "First Flowering Date",
    colour = "Species"
  ) +
  theme_classic() 


### Last flowering date per species
# Get predicted values from Bayesian model
preds_last_species <- ggpredict(mod_last_species, terms = "summer_temp [all]")  # or use pretty() to control breaks

# Convert predictions to Date format if the response was numeric date
preds_last_species$predicted_date <- as.Date(preds_last_species$predicted, origin = "2024-06-23")
preds_last_species$conf.low_date <- as.Date(preds_last_species$conf.low, origin = "2024-06-23")
preds_last_species$conf.high_date <- as.Date(preds_last_species$conf.high, origin = "2024-06-23")

# Plot
mod_last_species_plot <- ggplot(preds_last_species, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes_species, aes(x = summer_temp, y = as.Date(last_flowering_num, origin = "2024-06-23"), colour = species), size = 3) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Last Flowering Date",
    colour = "Species"
  ) +
  theme_classic() 


### Peak flowering date per species
# Get predicted values from Bayesian model
preds_peak_species <- ggpredict(mod_peak_species, terms = "summer_temp [all]")  # or use pretty() to control breaks

# Convert predictions to Date format if the response was numeric date
preds_peak_species$predicted_date <- as.Date(preds_peak_species$predicted, origin = "2024-06-23")
preds_peak_species$conf.low_date <- as.Date(preds_peak_species$conf.low, origin = "2024-06-23")
preds_peak_species$conf.high_date <- as.Date(preds_peak_species$conf.high, origin = "2024-06-23")

# Plot
mod_peak_species_plot <- ggplot(preds_peak_species, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes_species, aes(x = summer_temp, y = as.Date(peak_flowering_num, origin = "2024-06-23"), colour = species), size = 3) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Peak Flowering Date",
    colour = "Species"
  ) +
  theme_classic() 


### Duration per species
# Get predicted values from Bayesian model
preds_duration_species <- ggpredict(mod_duration_species, terms = "summer_temp [all]")  # or use pretty() to control breaks

# Plot
mod_duration_species_plot <- ggplot(preds_duration_species, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes_species, aes(x = summer_temp, y = duration_days, colour = species), size = 3) +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Duration of Flowering",
    colour = "Species"
  ) +
  theme_classic() 


# Combine with patchwork
flowering_species_traits_plot <- ((mod_first_species_plot + mod_last_species_plot) / (mod_peak_species_plot + mod_duration_species_plot) + plot_layout(guides = "collect"))

# Save the combined plot
ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/flowering_species_traits_plot.pdf",
  plot = flowering_species_traits_plot,
  width = 10,       # adjust based on layout
  height = 8
)


#### Test


# Extract and label slopes
slopes_species <- bind_rows(
  tidy(mod_first_species, effects = "fixed", parameters = "summer_temp") %>% mutate(trait = "First flowering date"),
  tidy(mod_last_species, effects = "fixed", parameters = "summer_temp") %>% mutate(trait = "Last flowering date"),
  tidy(mod_peak_species, effects = "fixed", parameters = "summer_temp") %>% mutate(trait = "Peak flowering date"),
  tidy(mod_duration_species, effects = "fixed", parameters = "summer_temp") %>% mutate(trait = "Flowering duration")
) %>%
  select(trait, estimate, std.error, conf.low, conf.high)

# Set the order of traits
slopes_species <- slopes_species %>%
  mutate(trait = factor(trait, levels = c(
    "First flowering date",
    "Last flowering date",
    "Peak flowering date",
    "Flowering duration"
  )))


# Plot
slope_species_effect_size <- ggplot(slopes_species, aes(x = estimate, y = trait)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    x = "Effect Size and Credible Interval of Mean Summer Temperature per Species",
    y = "Flowering Season Trait"
  ) +
  theme_classic()

# Save plot
ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/flowering_slope_species_effect_size.pdf",
  plot = slope_species_effect_size,
  width = 8,
  height = 3.5
)


#### Plot flowering metrics ----

# Group by location and date to get total flower count
polcam_data_grouped <- polcam_data_species %>%
  filter(!species == "cas_tet") %>%
  group_by(location_id, date) %>%
  summarise(total_flower_count = sum(count, na.rm = TRUE), .groups = "drop")

# Join with a lookup table that has microclimate or temperature info per location
polcam_data_grouped <- polcam_data_grouped %>%
  left_join(mean_summer_temperature, by = "location_id")


# Reverse order by mean_summer_temp
polcam_data_grouped$location_id <- factor(polcam_data_grouped$location_id,
                                          levels = rev(unique(polcam_data_grouped$location_id[order(polcam_data_grouped$summer_temp)])))


# Ridge plot
flowering_temperature_ridgeplot <- ggplot(polcam_data_grouped, aes(x = date, y = location_id, height = total_flower_count, fill = summer_temp)) +
  geom_density_ridges(stat = "identity", scale = 2.5, alpha = 0.7) +
  scale_fill_viridis_c(name = "Mean Summer Temp (°C)") +
  labs(
    x = "2024 Growing Season",
    y = "Total Flower Count per Site"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/flowering_temperature_ridgeplot.pdf",
  plot = flowering_temperature_ridgeplot,
  width = 8,
  height = 6
)
