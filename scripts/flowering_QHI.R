# ====================================================
# Script Name: flowering_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-04-08
# Last Modified: 2025-04-22
# Description: This script wrangles flowering data and prepares it for plotting and modelling as per RQ3. 
# Dependencies: POLCAM_data.csv, location_mapping_TundraBUZZ.csv, mean_summer_temp_TundraBUZZ.csv, 5_FloRes_raw.csv, 3_Aggregate_species.csv, flower_list_POLCAM.csv, QHI_location_temperature_daily.csv; R packages: tidyverse, lubridate, hms, ggridges, cowplot, ggnewscale, viridis, patchwork, brms, tidybayes, ggeffects, broom.mixed
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
FloRes_raw <- read_csv("/Volumes/TundraBUZZ/FloRes_database/doi_10_5061_dryad_djh9w0w29__v20220825/Data/5_FloRes_raw.csv")
FloRes_aggregate_species <- read_csv("/Volumes/TundraBUZZ/FloRes_database/doi_10_5061_dryad_djh9w0w29__v20220825/Data/3_Aggregate_species.csv")
flower_list <- read_csv("/Volumes/TundraBUZZ/data/raw/flower_list_POLCAM.csv")
QHI_temp_daily <- read.csv("/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_daily.csv")

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

# write_csv(polcam_data_long, "/Volumes/TundraBUZZ/data/clean/polcam_data_long.csv")

# Group per species
polcam_data_species <- polcam_data_long %>%
  group_by(species, location_id, date, microclimate) %>%
  summarize(count = sum(count), .groups = "drop") %>%
  left_join(flower_list %>% select(species, sugar_production_daily_flower),
            by = "species")

# Fliter FloRes for species in dataset
FloRes_filtered <- FloRes_raw %>%
  filter(taxon %in% flower_list$taxon)

# Fliter FloRes for species in dataset
FloRes_aggregate_species_filtered <- FloRes_aggregate_species %>%
  filter(species %in% flower_list$taxon)

# Calculate sugar per observation and summarize per site per day
daily_nectar_per_site <- polcam_data_species %>%
  mutate(daily_nectar_sugar_mg = count * sugar_production_daily_flower) %>%
  group_by(location_id, date, microclimate) %>%
  summarize(daily_nectar_sugar_mg = sum(daily_nectar_sugar_mg, na.rm = TRUE), .groups = "drop")

# write_csv(daily_nectar_per_site, "/Volumes/TundraBUZZ/data/clean/nectar_sugar_daily.csv")



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

QHI_temp_daily <- QHI_temp_daily %>%
  mutate(datetime = as.Date(datetime))

# Join first flowering dates to GDD data
flowering_summary <- flowering_summary %>%
  left_join(QHI_temp_daily %>%
              select(location_id, datetime, cumulative_GDD0, cumulative_GDD5),
            by = c("location_id" = "location_id", "first_flowering" = "datetime")) %>%
  rename(
    GDD0_first = cumulative_GDD0,
    GDD5_first = cumulative_GDD5
  )

# Join last flowering dates to GDD data
flowering_summary <- flowering_summary %>%
  left_join(QHI_temp_daily %>%
              select(location_id, datetime, cumulative_GDD0, cumulative_GDD5),
            by = c("location_id" = "location_id", "last_flowering" = "datetime")) %>%
  rename(
    GDD0_last = cumulative_GDD0,
    GDD5_last = cumulative_GDD5
  )

# Join peak flowering dates to GDD data
flowering_summary <- flowering_summary %>%
  left_join(QHI_temp_daily %>%
              select(location_id, datetime, cumulative_GDD0, cumulative_GDD5),
            by = c("location_id" = "location_id", "peak_flowering" = "datetime")) %>%
  rename(
    GDD0_peak = cumulative_GDD0,
    GDD5_peak = cumulative_GDD5
  )

# COOL1 has last flowering date on last date of recording, which is beyond last TOMST reading --> manually assign cumulative summmer GDD values
flowering_summary <- flowering_summary %>%
  mutate(
    GDD0_last = if_else(is.na(GDD0_last), 586.919, GDD0_last),
    GDD5_last = if_else(is.na(GDD5_last), 297.8602, GDD5_last)
  )
  

# write_csv(flowering_summary, "./data/clean/QHI_flowering_season_2024.csv")


# Pivot longer for plotting
flowering_long_GDD0 <- flowering_summary %>%
  select(location_id, microclimate, 
         GDD0_first, GDD0_peak, GDD0_last, summer_GDD0) %>%
  pivot_longer(
    cols = starts_with("GDD0"),
    names_to = "phase",
    values_to = "GDD0"
  )


# Clean and order phase labels
flowering_long_GDD0$phase <- recode(flowering_long_GDD0$phase,
                                    "GDD0_first" = "First Flowering",
                                    "GDD0_peak"  = "Peak Flowering",
                                    "GDD0_last"  = "Last Flowering")

# Convert to ordered factor
flowering_long_GDD0$phase <- factor(flowering_long_GDD0$phase,
                                    levels = c("First Flowering", "Peak Flowering", "Last Flowering"))

# Plot
ggplot(flowering_long_GDD0, aes(x = microclimate, y = GDD0, fill = phase)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_point(aes(color = phase), 
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             alpha = 0.7) +
  labs(
    x = "Microclimate",
    y = "Cumulative GDD0",
    title = "GDD Accumulation at Flowering Phases",
    fill = "Phenology Phase",
    color = "Phenology Phase"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

# Plot
ggplot(flowering_long_GDD0, aes(x = summer_GDD0, y = GDD0, fill = phase)) +
  geom_point(aes(color = phase), 
             alpha = 0.9) +
  geom_smooth(aes(color = phase), method ="lm") +
  labs(
    x = "Summer Total Growing Degree Days (T = 0°C)",
    y = "Cumulative Growing Degree Days (T = 0°C)",
    fill = "Phenology Phase",
    color = "Phenology Phase"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

# Pivot longer for plotting
flowering_long_GDD5 <- flowering_summary %>%
  select(location_id, microclimate, 
         GDD5_first, GDD5_peak, GDD5_last, summer_GDD5) %>%
  pivot_longer(
    cols = starts_with("GDD5"),
    names_to = "phase",
    values_to = "GDD5"
  )


# Clean and order phase labels
flowering_long_GDD5$phase <- recode(flowering_long_GDD5$phase,
                                    "GDD5_first" = "First Flowering",
                                    "GDD5_peak"  = "Peak Flowering",
                                    "GDD5_last"  = "Last Flowering")

# Convert to ordered factor
flowering_long_GDD5$phase <- factor(flowering_long_GDD5$phase,
                                    levels = c("First Flowering", "Peak Flowering", "Last Flowering"))

# Plot
ggplot(flowering_long_GDD5, aes(x = microclimate, y = GDD5, fill = phase)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_point(aes(color = phase), 
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             alpha = 0.7) +
  labs(
    x = "Microclimate",
    y = "Cumulative GDD5",
    title = "GDD Accumulation at Flowering Phases",
    fill = "Phenology Phase",
    color = "Phenology Phase"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

# Plot
ggplot(flowering_long_GDD5, aes(x = summer_GDD5, y = GDD5, fill = phase)) +
  geom_point(aes(color = phase), 
             alpha = 0.9) +
  geom_smooth(aes(color = phase), method ="lm") +
  labs(
    x = "Summer Total Growing Degree Days (T = 5°C)",
    y = "Cumulative Growing Degree Days (T = 5°C)",
    fill = "Phenology Phase",
    color = "Phenology Phase"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

# Plot last flowering date
ggplot(flowering_summary, aes(x = summer_GDD0, y = last_flowering)) +
  geom_point(aes(colour = microclimate), size = 3) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey44") +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Cumulative Summer Degree Growing Days (T = 0°C)",
    y = "First Flowering Date",
    colour = "Microclimate"
  ) +
  theme_classic() +
  scale_colour_manual(values = c("#440154", "forestgreen","gold"))


# Define common theme and color scale
microclimate_colors <- c("#440154", "forestgreen", "gold")

# Define origin date
origin_date <- as.Date("2024-06-23")

# Mutate date objects to numeric
flowering_bayes <- flowering_summary %>%
  mutate(
    first_flowering_num = as.numeric(first_flowering - origin_date),
    last_flowering_num = as.numeric(last_flowering - origin_date),
    peak_flowering_num = as.numeric(peak_flowering - origin_date)
  )

# Censoring dates with first flowering date before/= to installation date
flowering_bayes <- flowering_bayes %>%
  mutate(
    first_flowering_num = as.numeric(first_flowering - origin_date),
    censored = if_else(first_flowering == origin_date, "left", "none")
  )

flowering_bayes$censored <- factor(flowering_bayes$censored, levels = c("none", "left"))

flowering_bayes <- flowering_bayes %>%
  mutate(
    cens_duration = ifelse(first_flowering == as.Date("2024-06-23"), "right", "none")
  )

flowering_bayes$cens_duration <- factor(flowering_bayes$cens_duration, levels = c("none", "right"))

summary(flowering_bayes$duration_days)
table(flowering_bayes$cens_duration)
anyNA(flowering_bayes$duration_days)
head(flowering_bayes)



#### FIT MODELS ----
hist(flowering_bayes$first_flowering_num)

# First flowering model --> censored left given that there is uncertainty regarding start date prior to installation of camera
mod_first <- brm(
  first_flowering_num | cens(censored) ~ summer_GDD0,
  data = flowering_bayes,
  family = gaussian(),
  prior = c(
    prior(normal(3, 5), class = "Intercept"),
    prior(normal(-0.05, 0.15), class = "b", coef = "summer_GDD0"),
    prior(exponential(1), class = "sigma")
  ),
  cores = 4,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)
summary(mod_first)
plot(mod_first)
pp_check(mod_first)

hist(flowering_bayes$last_flowering_num)

# Last flowering model
mod_last <- brm(last_flowering_num ~ summer_GDD0, 
                data = flowering_bayes, 
                family = gaussian(),
                prior = c(
                  prior(normal(50, 10), class = "Intercept"),
                  prior(normal(-0.06, 0.15), class = "b", coef = "summer_GDD0"),
                  prior(exponential(1), class = "sigma")
                ),
                cores = 4,
                chains = 4,
                iter = 4000,
                warmup = 2000,
                control = list(adapt_delta = 0.95)
)
summary(mod_last)
plot(mod_last)
pp_check(mod_last)

# Peak flowering model
mod_peak <- brm(peak_flowering_num ~ summer_GDD0, 
                data = flowering_bayes, 
                family = gaussian(),
                prior = c(
                  prior(normal(20, 10), class = "Intercept"),
                  prior(normal(-0.05, 0.15), class = "b", coef = "summer_GDD0"),
                  prior(exponential(1), class = "sigma")
                ),
                cores = 4,
                chains = 4,
                iter = 4000,
                warmup = 2000,
                control = list(adapt_delta = 0.95)
)
summary(mod_peak)
plot(mod_peak)
pp_check(mod_peak)

# Duration (numeric)
mod_duration <- brm(
  duration_days ~ summer_GDD0, 
                data = flowering_bayes, family = gaussian(),
                prior = c(
                  prior(normal(40, 10), class = "Intercept"),
                  prior(normal(-0.02, 0.15), class = "b", coef = "summer_GDD0"),
                  prior(exponential(1), class = "sigma")
                ),
                cores = 4,
                chains = 4,
                iter = 4000,
                warmup = 2000,
                control = list(adapt_delta = 0.999)
                )
summary(mod_duration)
plot(mod_duration)
pp_check(mod_duration)




# Save each model as an RDS file
# saveRDS(mod_first, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayesian_mod_first.rds")
# saveRDS(mod_last, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayesian_mod_last.rds")
# saveRDS(mod_peak, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayesian_mod_peak.rds")
# saveRDS(mod_duration, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayesian_mod_duration.rds")

mod_first <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayesian_mod_first.rds")
mod_last <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayesian_mod_last.rds")
mod_peak <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayesian_mod_peak.rds")
mod_duration <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayesian_mod_duration.rds")



#### Extract model summaries ----

mod_first_summary_scaled <- summary(mod_first)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  ) %>%
  mutate(
    estimate = (estimate * 100),
    est_error = (est_error * 100),
    lower_95 = (lower_95 * 100),
    upper_95 = (upper_95 * 100)
  )


mod_last_summary_scaled <- summary(mod_last)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  ) %>%
  mutate(
    estimate = (estimate * 100),
    est_error = (est_error * 100),
    lower_95 = (lower_95 * 100),
    upper_95 = (upper_95 * 100)
  )

mod_peak_summary_scaled <- summary(mod_peak)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  ) %>%
  mutate(
    estimate = (estimate * 100),
    est_error = (est_error * 100),
    lower_95 = (lower_95 * 100),
    upper_95 = (upper_95 * 100)
  )

mod_duration_summary_scaled <- summary(mod_duration)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  ) %>%
  mutate(
    estimate = (estimate * 100),
    est_error = (est_error * 100),
    lower_95 = (lower_95 * 100),
    upper_95 = (upper_95 * 100)
  )


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



#### First flowering date
# Get predicted values from Bayesian model
preds_first <- ggpredict(mod_first, terms = "summer_GDD0 [all]") 

# Convert predictions to Date format if the response was numeric date
preds_first$predicted_date <- as.Date(preds_first$predicted, origin = "2024-06-23")
preds_first$conf.low_date <- as.Date(preds_first$conf.low, origin = "2024-06-23")
preds_first$conf.high_date <- as.Date(preds_first$conf.high, origin = "2024-06-23")

# Plot
mod_first_plot <- ggplot(preds_first, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(linewidth = 1.2, color = "black") +
  geom_hline(yintercept = as.Date("2024-06-23")-0.5, linetype = "dashed", color = "grey44") +  
  annotate("text", x = Inf, y = as.Date("2024-06-23") - 1.5, label = "Camera Deployment", 
           hjust = 1, vjust = 0, color = "grey44", size = 4, fontface = "italic") +
  geom_point(data = flowering_bayes, aes(x = summer_GDD0, y = as.Date(first_flowering_num, origin = "2024-06-23"), colour = microclimate), size = 3) +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Total Growing Degree Days (T = 0°C)",
    y = "Onset of Flowering",
    colour = "Microclimate"
  ) +
  theme_classic()



#### Last flowering date
# Get predicted values from Bayesian model
preds_last <- ggpredict(mod_last, terms = "summer_GDD0 [all]") 

# Convert predictions to Date format if the response was numeric date
preds_last$predicted_date <- as.Date(preds_last$predicted, origin = "2024-06-23")
preds_last$conf.low_date <- as.Date(preds_last$conf.low, origin = "2024-06-23")
preds_last$conf.high_date <- as.Date(preds_last$conf.high, origin = "2024-06-23")

# Plot
mod_last_plot <- ggplot(preds_last, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes, aes(x = summer_GDD0, y = as.Date(last_flowering_num, origin = "2024-06-23"), colour = microclimate), size = 3) +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Total Growing Degree Days (T = 0°C)",
    y = "End of Flowering",
    colour = "Microclimate"
  ) +
  theme_classic() 


#### Peak flowering date
# Get predicted values from Bayesian model
preds_peak <- ggpredict(mod_peak, terms = "summer_GDD0 [all]")  

# Convert predictions to Date format if the response was numeric date
preds_peak$predicted_date <- as.Date(preds_peak$predicted, origin = "2024-06-23")
preds_peak$conf.low_date <- as.Date(preds_peak$conf.low, origin = "2024-06-23")
preds_peak$conf.high_date <- as.Date(preds_peak$conf.high, origin = "2024-06-23")

# Plot
mod_peak_plot <- ggplot(preds_peak, aes(x = x, y = predicted_date)) +
  geom_ribbon(aes(ymin = conf.low_date, ymax = conf.high_date), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes, aes(x = summer_GDD0, y = as.Date(peak_flowering_num, origin = "2024-06-23"), colour = microclimate), size = 3) +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Total Growing Degree Days (T = 0°C)",
    y = "Peak Flowering Date",
    colour = "Microclimate"
  ) +
  theme_classic() 


#### Duration
# Get predicted values from Bayesian model
preds_duration <- ggpredict(mod_duration, terms = "summer_GDD0 [all]")  

# Plot
mod_duration_plot <- ggplot(preds_duration, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey60") +
  geom_line(size = 1.2, color = "black") +
  geom_point(data = flowering_bayes, aes(x = summer_GDD0, y = duration_days, colour = microclimate), size = 3) +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold")) +
  labs(
    x = "Total Growing Degree Days (T = 0°C)",
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

#### Effect size plot 
# Extract and label slopes
slopes <- bind_rows(
  tidy(mod_first, effects = "fixed", parameters = "summer_GDD0") %>% mutate(trait = "Onset of flowering"),
  tidy(mod_last, effects = "fixed", parameters = "summer_GDD0") %>% mutate(trait = "End of flowering"),
  tidy(mod_peak, effects = "fixed", parameters = "summer_GDD0") %>% mutate(trait = "Peak flowering date"),
  tidy(mod_duration, effects = "fixed", parameters = "summer_GDD0") %>% mutate(trait = "Flowering duration")
) %>%
  select(trait, estimate, std.error, conf.low, conf.high)

# Set the order of traits
slopes <- slopes %>%
  mutate(trait = factor(trait, levels = rev(c(
    "Onset of flowering",
    "End of flowering",
    "Peak flowering date",
    "Flowering duration"
  ))))

# Plot
flowering_slope_effect_size <- ggplot(slopes, aes(x = estimate*100, y = (trait))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low*100, xmax = conf.high*100), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    x = "Effect Size and Credible Intervals of 100-GDD Increase on Phenological Timing (T = 0°C)",
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

# Plot last flowering date
ggplot(flowering_summary_per_species_filtered, aes(x = summer_temp, y = last_flowering, color = species)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_date(date_labels = "%b %d") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "First Flowering Date",
    colour = "Species"
  ) +
  theme_classic() 

# Plot peak flowering date
ggplot(flowering_summary_per_species_filtered, aes(x = summer_temp, y = peak_flowering, color = species)) +
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
                 data = flowering_bayes_species, family = gaussian(), cores = 4,  control = list(adapt_delta = 0.95)
)

summary(mod_first_species)
plot(mod_first_species)
pp_check(mod_first_species)

# Model last flowering date per species
mod_last_species <- brm(last_flowering_num ~ summer_temp + (1|species), 
                         data = flowering_bayes_species, family = gaussian(), cores = 4,  control = list(adapt_delta = 0.95) 
)
summary(mod_last_species)
plot(mod_last_species)
pp_check(mod_last_species)

# Model peak flowering date per species
mod_peak_species <- brm(peak_flowering_num ~ summer_temp + (1|species), 
                         data = flowering_bayes_species, family = gaussian(), cores = 4,  control = list(adapt_delta = 0.95)
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


#### First flowering date per species
# Get predicted values from Bayesian model
preds_first_species <- ggpredict(mod_first_species, terms = "summer_temp [all]")  

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


#### Last flowering date per species
# Get predicted values from Bayesian model
preds_last_species <- ggpredict(mod_last_species, terms = "summer_temp [all]") 

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


#### Peak flowering date per species
# Get predicted values from Bayesian model
preds_peak_species <- ggpredict(mod_peak_species, terms = "summer_temp [all]")  

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


#### Duration per species
# Get predicted values from Bayesian model
preds_duration_species <- ggpredict(mod_duration_species, terms = "summer_temp [all]")

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


#### Effect size per species

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

# Join with a table that has microclimate/temperature info per location
polcam_data_grouped <- polcam_data_grouped %>%
  left_join(mean_summer_temperature, by = "location_id")

# Reverse order by mean_summer_temp
polcam_data_grouped$location_id <- factor(polcam_data_grouped$location_id,
                                          levels = rev(unique(polcam_data_grouped$location_id[order(polcam_data_grouped$summer_temp)])))


# Ridge plot
flowering_temperature_ridgeplot_GDD0 <- ggplot(polcam_data_grouped, aes(x = date, y = location_id, height = total_flower_count, fill = summer_GDD0)) +
  geom_density_ridges(stat = "identity", scale = 2.5, alpha = 0.7) +
  scale_fill_viridis_c(name = "Total GDD\n(T = 0°C)") +
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
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/flowering_temperature_ridgeplot_GDD0.pdf",
  plot = flowering_temperature_ridgeplot_GDD0,
  width = 8,
  height = 6
)
