# ====================================================
# Script Name: flight_buzz_analysis_QHI.R
# Project: TundraBUZZ 2024-25
# Author: Alex Beauchemin
# Date Created: 2025-03-21
# Last Modified: 2025-04-22
# Description: This script prepares data, analyzes it in relation to research questions, and outputs figures and tables.
# Dependencies: summary_flightbuzzes_ARUQ_2024.csv, daily_summary_flightbuzzes_ARUQ_2024.csv, environmental_variables_hourly.csv, environmental_variables_daily.csv, QHI_location_temperature_hourly.csv, QHI_location_temperature_daily.csv, mean_summer_temp_TundraBUZZ.csv, location_mapping_TundraBUZZ.csv, R packages: tidyverse, lubridate, suncalc, hms, lme4, lmerTest, mgcv, visreg, patchwork, cowplot, viridis, brms, tidybayes, slider, ggeffects, corrplot, ggppubr, car, lme4, broom, splines.
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(dplyr)
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
library(brms)
library(tidybayes)
library(slider)
library(ggeffects)
library(corrplot)
library(ggpubr)
library(car)
library(lme4)
library(broom)
library(splines)

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

# Set latitude and longitude for Qikiqtaruk - Herschel Island
lat <- 69.57
lon <- -138.91

#### ----



#### Using tidy datasets ----
# Load data
summary_flightbuzzes_ARUQ_2024 <- read_csv("./data/clean/flight_buzzes_complete_TundraBUZZ.csv")
daily_summary_flightbuzzes_ARUQ_2024 <- read_csv("./data/clean/daily_flight_buzzes_complete_TundraBUZZ.csv")
environmental_variables_hourly <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/clean/environmental_variables_hourly.csv")
environmental_variables_daily <- read_csv("/Users/alexandrebeauchemin/TundraBUZZ_github/data/clean/environmental_variables_daily.csv")
QHI_temp_hourly <- read.csv("/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_hourly.csv")
QHI_temp_daily <- read.csv("/Volumes/TundraBUZZ/data/clean/QHI_location_temperature_daily.csv")
location_mapping <- read_csv("./data/raw/location_mapping_TundraBUZZ.csv")
mean_summer_temp <- read_csv("/Volumes/TundraBUZZ/data/clean/mean_summer_temp_TundraBUZZ.csv")
flowering_summary <- read_csv("./data/clean/QHI_flowering_season_2024.csv")
polcam_data_long <- read_csv("/Volumes/TundraBUZZ/data/clean/polcam_data_long.csv")
daily_nectar_per_site <- read_csv("/Volumes/TundraBUZZ/data/clean/nectar_sugar_daily.csv")
#### ----


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
table(daily_summary_flightbuzzes_ARUQ_2024$location_id)

# Check timezone
attr(summary_flightbuzzes_ARUQ_2024$datetime, "tzone")

# Ensure datetime is in "America/Whitehorse" 
summary_flightbuzzes_ARUQ_2024 <- summary_flightbuzzes_ARUQ_2024 %>%
  mutate(datetime = with_tz(datetime, tzone = "America/Whitehorse"))

# Aggregate flight_buzz data to hourly
hourly_summary_flightbuzzes_ARUQ_2024 <- summary_flightbuzzes_ARUQ_2024 %>%
  mutate(datetime = floor_date(datetime, unit = "hour")) %>%
  group_by(datetime, location_id, microclimate) %>%
  summarize(total_duration_above_threshold = mean(total_duration_above_threshold, na.rm = TRUE)*2,
            .groups = "drop")

# Convert temp data to POSIXct (UTC timezone)
QHI_temp_hourly$datetime <- ymd_hms(QHI_temp_hourly$datetime, tz = "UTC")

# Ensure datetime is in "America/Whitehorse"
QHI_temp_hourly$datetime <- with_tz(QHI_temp_hourly$datetime, tzone = "America/Whitehorse")

# Select columns for merging
QHI_temp_hourly <- QHI_temp_hourly %>%
  mutate(mean_temp = value) %>%
  select(location_id, datetime, mean_temp, cumulative_GDD0, cumulative_GDD5)

# Properly name columns
QHI_temp_daily <- QHI_temp_daily %>%
  mutate(mean_temp = value) %>%
  mutate(date = as.POSIXct(datetime, tz = "UTC")) %>%  
  select(location_id, date, mean_temp, cumulative_GDD0, cumulative_GDD5)

#### ----

#### Merge datasets ----
## Hourly datasets
flight_buzz_hourly <- hourly_summary_flightbuzzes_ARUQ_2024 %>%
  left_join(environmental_variables_hourly, by = "datetime") %>%
  left_join(QHI_temp_hourly, by = c("datetime", "location_id"))

flight_buzz_hourly <- flight_buzz_hourly %>%
  mutate(time_hour = hms::as_hms(floor_date(datetime, unit = "hour")))
table(flight_buzz_hourly$location_id)
# write_csv(flight_buzz_hourly, "/Volumes/TundraBUZZ/data/clean/flight_buzz_hourly.csv")

## Daily datasets
flight_buzz_daily <- daily_summary_flightbuzzes_ARUQ_2024 %>%
  left_join(environmental_variables_daily, by = "date") %>%
  left_join(QHI_temp_daily, by = c("date", "location_id"))
flight_buzz_daily <- flight_buzz_daily %>%
  mutate(location_id = as.factor(location_id))

# write_csv(flight_buzz_daily, "/Volumes/TundraBUZZ/data/clean/flight_buzz_daily.csv")

# Extract flower counts from polcam_data_long
flower_counts <- polcam_data_long %>%
  group_by(date, location_id) %>%
  summarize(total_flower_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  filter(total_flower_count > 0)

flower_counts <- flower_counts %>%
  left_join(daily_nectar_per_site, by = c("location_id", "date"))

# Merge the two datasets
combined_data <- left_join(flight_buzz_daily, flower_counts, by = c("date", "location_id", "microclimate"))

combined_data_filtered <- combined_data %>%
  filter(!location_id == "BEEBOX")


#### ----

#### PREPARE DATASETS ----
## From flowering_QHI.R
peak_flowering <- flowering_summary %>%
  select(location_id, peak_flowering)

peak_temp_flowering <- peak_temp_df %>%
  left_join(peak_flowering, by = "location_id")  # assuming env_summary has mean_summer_temp
peak_temp_flowering <- peak_temp_flowering %>%
  filter(!location_id == "BEEBOX") %>%
  mutate(peak_flowering = as.Date(peak_flowering)
  ) %>%
  mutate(peak_date = as.Date(peak_date)
  ) %>%
  mutate(difference_days = as.numeric(peak_date - peak_flowering)
  ) 

# Convert peak_date and peak_flowering to Date class if they are not already
peak_temp_flowering <- peak_temp_flowering %>%
  mutate(
    peak_date = as.Date(peak_date),  # Ensure peak_date is in Date format
    peak_flowering = as.Date(peak_flowering)  # Ensure peak_flowering is in Date format
  )

# Convert peak_date and peak_flowering to numeric (days since the first date)
peak_temp_flowering <- peak_temp_flowering %>%
  mutate(
    peak_date_numeric = as.numeric(peak_date),
    peak_flowering_numeric = as.numeric(peak_flowering)
  )

peak_temp_flowering <- peak_temp_flowering %>%
  mutate(
    peak_date_numeric = peak_date_numeric - 19913.5,
    peak_flowering_numeric = peak_flowering_numeric - 19913.5)

peak_temp_flowering <- peak_temp_flowering %>%
  mutate(
    summer_GDD0_100 = summer_GDD0 / 100,
    summer_temp_centered = summer_temp - mean(summer_temp, na.rm = TRUE)
  )
peak_temp_flowering <- peak_temp_flowering %>%
  mutate(summer_GDD0_100_c = summer_GDD0_100 - mean(summer_GDD0_100))


# Create a data frame with only the numeric columns that need to be scaled
numeric_data <- peak_temp_flowering %>%
  select(peak_date_numeric, peak_flowering_numeric, summer_temp)

# Create a data frame with only the numeric columns that need to be scaled
numeric_data_GDD0 <- peak_temp_flowering %>%
  select(peak_date_numeric, peak_flowering_numeric, summer_GDD0)

# Add new columns to flight_buzz_daily
flight_buzz_daily <- flight_buzz_daily %>%
  mutate(duration_rounded = round(daily_duration_above_threshold))
flight_buzz_daily <- flight_buzz_daily %>%
  left_join(mean_summer_temp, by = "location_id")
flight_buzz_daily <- flight_buzz_daily %>%
  arrange(date) %>%
  mutate(date_num = as.numeric(((date - min(date))/86400) +1)) %>%
  ungroup()
flight_buzz_daily <- flight_buzz_daily %>%
  mutate(mean_temp_center = scale(mean_temp, center = TRUE, scale = FALSE))  # Center only
flight_buzz_daily <- flight_buzz_daily %>%
  mutate(
    summer_GDD0_100s_c = (summer_GDD0 / 100) - mean(summer_GDD0 / 100, na.rm = TRUE)
  )
mean_temp_raw_mean <- mean(flight_buzz_daily$mean_temp, na.rm = TRUE)


#### ----




#### RESEARCH QUESTION 1: OVERLAP METRICS ----

#### RQ1.1. IMPACT OF SUMMER GDD ON PHENOLOGY + MISMATCH ----
# Set prior for peak bumblebee activity date
prior_peak_date <- c(
  prior(normal(0, 5), class = "Intercept"),  
  prior(normal(-0.04, 0.15), class = "b", coef = "summer_GDD0")  
)

# Set prior for peak flowering date
prior_peak_flowering <- c(
  prior(normal(0, 5), class = "Intercept"),  
  prior(normal(-0.05, 0.15), class = "b", coef = "summer_GDD0")  # Stronger negative slope for peak_flowering (expect greater sensitivity to warming in flowering phenology)
)

# Set prior for mismatch (difference in days)
prior_difference_days <- c(
  prior(normal(0, 5), class = "Intercept"), 
  prior(normal(0.02, 0.3), class = "b", coef = "summer_GDD0") 
)

# Fit Bayesian model with informed priors for peak bumblebee activity date
model_peak_date_bayesian <- brm(
  peak_date_numeric ~ summer_GDD0,
  data = peak_temp_flowering,
  prior = prior_peak_date,
  chains = 4, cores = 4, iter = 2000
)

# Fit Bayesian model with informed priors for peak flowering date
model_peak_flowering_bayesian <- brm(
  peak_flowering_numeric ~ summer_GDD0,
  data = peak_temp_flowering,
  prior = prior_peak_flowering,
  chains = 4, cores = 4, iter = 2000
)

# Fit Bayesian model with informed priors for mismatch
model_difference_days_bayesian <- brm(
  difference_days ~ summer_GDD0,
  data = peak_temp_flowering,
  prior = prior_difference_days,
  chains = 4, cores = 4, iter = 2000
)


summary(model_peak_date_bayesian)
plot(model_peak_date_bayesian)
pp_check(model_peak_date_bayesian)
# saveRDS(model_peak_date_bayesian, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_peak_activitydate_GDD0_bayesian.rds")
model_peak_date_bayesian <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_peak_activitydate_GDD0_bayesian.rds")

summary(model_peak_flowering_bayesian)
plot(model_peak_flowering_bayesian)
pp_check(model_peak_flowering_bayesian)
# saveRDS(model_peak_flowering_bayesian, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_peak_floweringdate_GDD0_bayesian.rds")
model_peak_flowering_bayesian <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_peak_floweringdate_GDD0_bayesian.rds")

summary(model_difference_days_bayesian)
plot(model_difference_days_bayesian)
pp_check(model_difference_days_bayesian)
# saveRDS(model_difference_days_bayesian, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_diff_peak_activity_flowering_GDD0_bayesian.rds")
model_difference_days_bayesian <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_diff_peak_activity_flowering_GDD0_bayesian.rds")


## Extract summary tables, and scale for interpretability
model_peak_date_bayesian_summary <- summary(model_peak_date_bayesian)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  )

model_peak_date_bayesian_summary_scaled <- summary(model_peak_date_bayesian)$fixed %>%
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

# Export to CSV
write.csv(model_peak_date_bayesian_summary, "outputs/model_peak_date_bayesian_summary.csv", row.names = FALSE)
# Export to CSV
write.csv(model_peak_date_bayesian_summary_scaled, "outputs/model_peak_date_bayesian_summary_scaled.csv", row.names = FALSE)


model_peak_flowering_bayesian_summary <- summary(model_peak_flowering_bayesian)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  )

model_peak_flowering_bayesian_summary_scaled <- summary(model_peak_flowering_bayesian)$fixed %>%
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

# Export to CSV
write.csv(model_peak_flowering_bayesian_summary, "outputs/model_peak_flowering_bayesian_summary.csv", row.names = FALSE)
write.csv(model_peak_flowering_bayesian_summary_scaled, "outputs/model_peak_flowering_bayesian_summary_scaled.csv", row.names = FALSE)


model_difference_days_bayesian_summary <- summary(model_difference_days_bayesian)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  )

model_difference_days_bayesian_summary_scaled <- summary(model_difference_days_bayesian)$fixed %>%
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

# Export to CSV
write.csv(model_difference_days_bayesian_summary, "outputs/model_difference_days_bayesian_summary.csv", row.names = FALSE)
write.csv(model_difference_days_bayesian_summary_scaled, "outputs/model_difference_days_bayesian_summary_scaled.csv", row.names = FALSE)


### Plotting
# Generate predictions for each model
pred_peak_date <- ggpredict(model_peak_date_bayesian, terms = "summer_GDD0")
pred_peak_flowering <- ggpredict(model_peak_flowering_bayesian, terms = "summer_GDD0")
pred_difference_days <- ggpredict(model_difference_days_bayesian, terms = "summer_GDD0")

# Plot the predictions with confidence intervals using ggplot
bayes_mismatch_plot <- ggplot() +
  # Plot predictions for Peak Date
  geom_line(data = pred_peak_date, aes(x = x, y = predicted, color = "Peak Bumblebee Activity"), size = 1.2) +
  geom_ribbon(data = pred_peak_date, aes(x = x, ymin = conf.low, ymax = conf.high, fill = "Peak Bumblebee Activity"), alpha = 0.1) +
  geom_point(data = peak_temp_flowering, aes(x = summer_GDD0, y = peak_date_numeric, color = "Peak Bumblebee Activity")) +
  
  # Plot predictions for Peak Flowering
  geom_line(data = pred_peak_flowering, aes(x = x, y = predicted, color = "Peak Flowering"), size = 1.2) +
  geom_ribbon(data = pred_peak_flowering, aes(x = x, ymin = conf.low, ymax = conf.high, fill = "Peak Flowering"), alpha = 0.1) +
  geom_point(data = peak_temp_flowering, aes(x = summer_GDD0, y = peak_flowering_numeric, color = "Peak Flowering")) +
  
  # Plot predictions for Mismatch
  geom_line(data = pred_difference_days, aes(x = x, y = predicted, color = "Mismatch"), size = 1.2) +
  geom_ribbon(data = pred_difference_days, aes(x = x, ymin = conf.low, ymax = conf.high, fill = "Mismatch"), alpha = 0.1) +
  geom_point(data = peak_temp_flowering, aes(x = summer_GDD0, y = difference_days, color = "Mismatch")) +
  
  # Customize the plot
  labs(
    x = "Cumulative Growing Degree Days (T = 0°C)",
    y = "Phenological Timing / Mismatch",
    color = "Phenological Metric",
    fill = "Phenological Metric"
  ) +
  scale_color_manual(values = c("Peak Bumblebee Activity" = "darkolivegreen3", "Peak Flowering" = "forestgreen", "Mismatch" = "orange2")) +
  scale_fill_manual(values = c("Peak Bumblebee Activity" = "darkolivegreen3", "Peak Flowering" = "forestgreen", "Mismatch" = "orange2")) +
  theme_classic() + 
  # Add horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  
  # Add arrows showing mismatch direction (example)
  geom_segment(data = data.frame(x = c(745, 750, 755), xend = c(745, 750, 755),
                                 y = 0, yend = c(-4.65, 4.15, -9.5)),
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.25, "cm")),
               color = c("darkolivegreen4", "orange3", "darkgreen"), size = 1) +
  theme(legend.position = "top")


# Save the combined plot
ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/bayes_mismatch_plot.pdf",
  plot = bayes_mismatch_plot,
  width = 10,       # adjust based on layout
  height = 8
)


# Extract posterior samples for the coefficient of summer_GDD0 (slope)
posterior_peak_date <- posterior_samples(model_peak_date_bayesian, pars = "b_summer_GDD0")
posterior_peak_flowering <- posterior_samples(model_peak_flowering_bayesian, pars = "b_summer_GDD0")
posterior_difference_days <- posterior_samples(model_difference_days_bayesian, pars = "b_summer_GDD0")

# Calculate the 95% credible intervals and the effect size (mean)
summary_peak_date <- data.frame(
  effect = "Peak Bumblebee Activity",
  mean = mean(posterior_peak_date$b_summer_GDD0)*100,
  lower = quantile(posterior_peak_date$b_summer_GDD0, 0.025)*100,
  upper = quantile(posterior_peak_date$b_summer_GDD0, 0.975)*100
)

summary_peak_flowering <- data.frame(
  effect = "Peak Flowering",
  mean = mean(posterior_peak_flowering$b_summer_GDD0)*100,
  lower = quantile(posterior_peak_flowering$b_summer_GDD0, 0.025)*100,
  upper = quantile(posterior_peak_flowering$b_summer_GDD0, 0.975)*100
)

summary_difference_days <- data.frame(
  effect = "Mismatch",
  mean = mean(posterior_difference_days$b_summer_GDD0)*100,
  lower = quantile(posterior_difference_days$b_summer_GDD0, 0.025)*100,
  upper = quantile(posterior_difference_days$b_summer_GDD0, 0.975)*100
)

# Combine all summaries into one dataframe
summary_df <- bind_rows(summary_peak_date, summary_peak_flowering, summary_difference_days)

# Plot the slopes with 95% credible intervals using ggplot
bayesian_slopes_mismatch <- ggplot(summary_df, aes(y = effect, x = mean, xmin = lower, xmax = upper, colour = effect, fill = effect)) +
  geom_point(size = 3) +  # Effect size point (mean slope)
  geom_errorbar(width = 0.1) +  # 95% credible intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  # Vertical dashed grey line
  labs(
    y = "Phenological Metric",
    x = "Effect Size and Credible Intervals of 100-GDD Increase on Phenological Timing (T = 0°C)"
  ) +
  scale_color_manual(values = c("Peak Bumblebee Activity" = "darkolivegreen4", "Peak Flowering" = "darkgreen", "Mismatch" = "orange3")) +
  scale_fill_manual(values = c("Peak Bumblebee Activity" = "darkolivegreen4", "Peak Flowering" = "darkgreen", "Mismatch" = "orange3")) +
  theme_classic() +
  theme(legend.position = "none")


ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/bayesian_slopes_mismatch.pdf",
  plot = bayesian_slopes_mismatch,
  width = 8,       # adjust based on layout
  height = 4
)

#### ----

#### RQ1.2. PEAK BUMBLEBEE ~ PEAK FLOWERING ----
## Define priors
priors_GDD0_informed <- c(
  prior(normal(1.1, 0.1), class = "b", coef = "peak_flowering_numeric"),  # strong positive
  prior(normal(-0.3, 0.3), class = "b", coef = "summer_GDD0_100_c"),      # weak negative
  prior(normal(0.3, 0.5), class = "b", coef = "peak_flowering_numeric:summer_GDD0_100_c"),  # weak positive
  prior(normal(0, 1), class = "Intercept"),
  prior(exponential(1), class = "sigma")
)

## Fit model
model_bayes_GDD0_interaction_informed <- brm(
  peak_date_numeric ~ peak_flowering_numeric + summer_GDD0_100_c + peak_flowering_numeric:summer_GDD0_100_c, 
  data = peak_temp_flowering,
  prior = priors_GDD0_informed,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123,
  cores = 4,
  control = list(adapt_delta = 0.999)
)

summary(model_bayes_GDD0_interaction_informed)
plot(model_bayes_GDD0_interaction_informed)
pp_check(model_bayes_GDD0_interaction_informed)

# write.csv(model_bayes_GDD0_interaction_informed, "outputs/effect_summary_bayes_floweringbuzz_GDD0inter_informed.csv", row.names = FALSE)
# saveRDS(model_bayes_GDD0_interaction_informed, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/buzz_to_floweringGDD0inter_informed.rds")


## PLOT --
bumblebee_activity_vs_flowering_plot <- ggplot(peak_temp_flowering, aes(x = peak_flowering, y = peak_date)) +
  geom_smooth(method="lm", colour = "grey20") +
  geom_point(aes(colour = summer_GDD0), size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "gray40") +  # 1:1 reference line
  labs(
    x = "Peak Flowering Date",
    y = "Peak Bumblebee Activity Date",
    colour = "Cumul. GDD\n(T = 0°C)"
  ) +
  scale_colour_viridis_c() +
  theme_classic()


# Save the combined plot
ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/bumblebee_activity_vs_flowering_plot.pdf",
  plot = bumblebee_activity_vs_flowering_plot,
  width = 10,       # adjust based on layout
  height = 8
)

#### ----

#### RQ1.3. PROPORTION OF OVERLAP ----
#### Define top days of activity 

# Extract top days of activity > SD
top_by_stdev_site <- combined_data_filtered %>%
  group_by(location_id) %>%
  mutate(
    mean_activity = mean(daily_duration_above_threshold, na.rm = TRUE),
    sd_activity = sd(daily_duration_above_threshold, na.rm = TRUE)
  ) %>%
  filter(daily_duration_above_threshold >= (mean_activity + 1 * sd_activity)) %>%  
  ungroup()

# View result
top_by_stdev_site

# Summarize by site
proportion_by_site <- top_by_stdev_site %>%
  group_by(location_id) %>%
  summarise(
    total_days = n(),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    days_with_flowers = sum(total_flower_count > 0, na.rm = TRUE),
    proportion_with_flowers = days_with_flowers / total_days
  )

proportion_by_site <- proportion_by_site %>%
  left_join(mean_summer_temp, by = "location_id")


## Test for binomial distribution assumptions
# Calculate observed variance
proportion_by_site <- proportion_by_site %>%
  mutate(observed_variance = (days_with_flowers / total_days) * (1 - days_with_flowers / total_days))

# Calculate expected variance under binomial assumption
proportion_by_site <- proportion_by_site %>%
  mutate(expected_variance = total_days * (days_with_flowers / total_days) * (1 - days_with_flowers / total_days))

# Compare observed variance to expected variance
proportion_by_site <- proportion_by_site %>%
  mutate(overdispersion_factor = observed_variance / expected_variance)

# Check if any site has overdispersion
summary(proportion_by_site$overdispersion_factor)

## Fit binomial model
model_binomial <- brm(
  formula = days_with_flowers | trials(total_days) ~ summer_GDD0_scaled,
  family = binomial(),
  data = proportion_by_site,
  prior = c(
    prior(normal(2, 2), class = "Intercept"),
    prior(normal(-0.5, 1), class = "b")  # weak negative slope expectation
  ),
  chains = 4, cores = 4, iter = 4000, warmup = 1000,
  seed = 123
)

summary(model_binomial)
plot(model_binomial)
pp_check(model_binomial)

# Get model summary
model_summary_binomial <- summary(model_binomial)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  )

# Export to CSV
# write.csv(model_summary_binomial, "outputs/model_summary_overlap.csv", row.names = FALSE)
# model_summary_binomial <- read_csv("outputs/model_summary_overlap.csv")


# Generate predicted probabilities
predicted <- ggpredict(model_binomial, terms = "summer_GDD0_scaled")
predicted <- predicted %>%
  mutate(predicted_probabilities = predicted / 6)
head(predicted)

# Predicted number of flowering days and their corresponding credible intervals
predicted_successes <- predicted$predicted
predicted_lower <- predicted$conf.low  # Lower bound of 95% CI
predicted_upper <- predicted$conf.high  # Upper bound of 95% CI

# Convert to probabilities (successes / total_days)
predicted_probabilities <- predicted_successes / 6
predicted_probabilities_lower <- predicted_lower / 6
predicted_probabilities_upper <- predicted_upper / 6

# Plot predicted probabilities with the credible intervals
ggplot(predicted, aes(x = x)) +
  geom_line(aes(y = predicted_probabilities), color = "grey20") +  # Predicted line
  geom_ribbon(aes(ymin = predicted_probabilities_lower, ymax = predicted_probabilities_upper), fill = "grey44", alpha = 0.2) +  # Credible intervals
  geom_point(data = proportion_by_site, aes(x = summer_GDD0_scaled, y = proportion_with_flowers), size = 3, color = "forestgreen") +  # Actual data points
  labs(x = "Scaled Cumul. GDD (1:100, T = 0°C)",
       y = "Overlap between Top 10% Activity Dates and Flowering") +
  theme_classic()


# saveRDS(model_binomial, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_proportion_flowering.rds")
# model_binomial <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_proportion_flowering.rds")

#### ----




#### RESEARCH QUESTION 2: DRIVERS OF BUMBLEBEE ACTIVITY ----

#### BAYESIAN GLMM for ENV PREDICTORS ----
#### View climate variables for Bayesian model 
combined_data_filtered 

?hist()
hist(combined_data_filtered$mean_temp) # normal ish
hist((combined_data_filtered$daily_nectar_sugar_mg), breaks = 20) # zero inflated
hist(combined_data_filtered$mean_wind_speed) # normal ish, right skew
hist(combined_data_filtered$mean_stn_press_k_pa) # normal
hist(combined_data_filtered$predominant_wind_dir, breaks = 20) # bimodal --> remove
hist(combined_data_filtered$avg_air_rh_1m, breaks = 20) # normal ish? left skewed
hist(combined_data_filtered$day_length_hours) # super 24 inflated, maybe do log(night length)?
hist(combined_data_filtered$cloud_cover_pct) # flat --> remove



#### PREPARE CORRELATION PLOT
numeric_vars <- combined_data_filtered %>%
  select(mean_temp, daily_nectar_sugar_mg, mean_wind_speed,
         mean_stn_press_k_pa, predominant_wind_dir, avg_air_rh_1m, day_length_hours,
         cloud_cover_pct)

corrplot(cor(na.omit(numeric_vars)), method = "color", type = "upper")

# Define the custom color gradient from orange2 to grey44 to darkgreen
my_colors <- colorRampPalette(c("orange2", "cornsilk", "darkgreen"))(200)

# You can specify your own custom labels here, for example:
custom_labels <- c("mean_temp" = "Mean Temperature (°C)", "daily_nectar_sugar_mg" = "Nectar Sugar Availability (mg)", "mean_wind_speed" = "Wind Speed", "mean_stn_press_k_pa" = "Atmospheric Pressure (kPa)", "predominant_wind_dir" = "Predominant Wind Dir.", "avg_air_rh_1m" = "Relative Humidity (%)", "day_length_hours" = "Day Length", "cloud_cover_pct" = "Cloud Cover (Pct)")

# Edit the correlation plot with custom labels
corrplot(cor(na.omit(numeric_vars)), 
         method = "color", 
         type = "lower", 
         col = my_colors,  # Apply Viridis color scale
         tl.col = "black",    # Text label color
         tl.srt = 45,         # Label text rotation
         tl.cex = 0.8,        # Label text size
         col.lab = "black",     # Label color (optional)
         # Use custom labels for x and y axes
         addCoef.col = "black", # Add correlation coefficient values (optional)
         tl.labels = custom_labels)  # Applying custom labels

# Export to PDF
pdf("outputs/figures/env_predictors_corrplot.pdf", width = 7, height = 6)

# Create the corrplot
corrplot(corr_matrix, 
         method = "color", 
         type = "lower", 
         col = my_colors,
         tl.col = "black",    
         tl.srt = 45,         
         tl.cex = 0.8,        
         addCoef.col = "black")  

dev.off()



#### PREPARE VARIABLES FOR MODELLING
# Scale/standardize predictors to help with convergence
combined_data_filtered_transformed2 <- combined_data_filtered %>%
  mutate(
    # Replace zeros with NA for nectar sugar
    daily_nectar_sugar_mg = ifelse(daily_nectar_sugar_mg == 0, NA, daily_nectar_sugar_mg),
    
    # Log-transform nectar sugar with +1 to avoid issues with zero values
    daily_nectar_sugar_mg = log(daily_nectar_sugar_mg + 1),
    
    # Apply other transformations and scaling to other variables
    mean_wind_speed = log(mean_wind_speed + 1),
    avg_air_rh_1m = log(avg_air_rh_1m + 1),
    day_length_hours = 24 - day_length_hours)

# Transform dataset
combined_data_filtered_transformed <- combined_data_filtered_transformed2 %>%
  mutate(
    # Scale and center continuous predictors
    across(
      c(mean_temp, daily_nectar_sugar_mg, mean_wind_speed, mean_stn_press_k_pa, predominant_wind_dir, avg_air_rh_1m, day_length_hours, cloud_cover_pct),
      ~ as.numeric(scale(.)),
      .names = "{.col}_z"
    )) %>%
  mutate(daily_duration_above_threshold = daily_duration_above_threshold / 0.15) %>%
  select(
    location_id, date, microclimate, daily_duration_above_threshold,
    ends_with("_z")
  )

# View distribution of variables
combined_data_filtered_transformed$daily_duration_above_threshold <- 
  as.integer(combined_data_filtered_transformed$daily_duration_above_threshold)
hist(combined_data_filtered_transformed$mean_temp_z) 
hist(log(combined_data_filtered_transformed$daily_nectar_sugar_mg_z)) 
hist(combined_data_filtered_transformed$mean_wind_speed_z) 
hist(combined_data_filtered_transformed$mean_stn_press_k_pa_z) 
hist(combined_data_filtered_transformed$predominant_wind_dir_z) 
hist(combined_data_filtered_transformed$avg_air_rh_1m_z) 
hist(combined_data_filtered_transformed$day_length_hours_z) 
hist(combined_data_filtered_transformed$cloud_cover_pct_z) 

# Convert date to numeric 
combined_data_filtered_transformed <- combined_data_filtered_transformed %>%
  mutate(date_numeric = as.numeric(difftime(date, min(date), units = "days")))
acf(combined_data_filtered_transformed$daily_duration_above_threshold)

# Check the proportion of zeros
mean(combined_data_filtered$daily_duration_above_threshold == 0)



#### FIT A NEGATIVE BINOMIAL MODEL
# Define the formula correctly using brmsformula()
my_formula <- brmsformula(
  daily_duration_above_threshold ~ 
    mean_temp_z + 
    daily_nectar_sugar_mg_z + 
    mean_wind_speed_z + 
    mean_stn_press_k_pa_z + 
    avg_air_rh_1m_z + 
    day_length_hours_z + 
    cloud_cover_pct_z + 
    (mean_temp_z + daily_nectar_sugar_mg_z | location_id) + ar(gr = location_id, cov = TRUE)
)

# Pass the AR(1) separately with grouping via the main brm() call
nb_model <- brm(
  formula = my_formula,
  data = combined_data_filtered_transformed,
  family = negbinomial(),
  prior = c(
    # Priors for fixed effects
    prior(normal(1, 0.5), class = "b", coef = "mean_temp_z"),  
    prior(normal(1, 0.5), class = "b", coef = "daily_nectar_sugar_mg_z"),  
    prior(normal(-1, 0.5), class = "b", coef = "mean_wind_speed_z"),  
    prior(normal(0, 1), class = "b", coef = "mean_stn_press_k_pa_z"),  
    prior(normal(-1, 0.5), class = "b", coef = "avg_air_rh_1m_z"),  
    prior(normal(-1, 0.5), class = "b", coef = "day_length_hours_z"),  
    prior(normal(-1, 0.5), class = "b", coef = "cloud_cover_pct_z"),  
    # Intercept
    prior(normal(0, 1), class = "Intercept"),
    # Random effects
    prior(exponential(1), class = "sd", group = "location_id")
  ),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  file = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_flight_buzzes_env_pred.rds"
)

# Check model summary
summary(nb_model)
plot(nb_model)
pp_check(nb_model)

# Get model summary
model_summary <- summary(nb_model)$fixed %>%
  as_tibble(rownames = "term") %>%
  rename(
    estimate = Estimate,
    est_error = Est.Error,
    lower_95 = 'l-95% CI',
    upper_95 = 'u-95% CI',
    rhat = Rhat,
    Bulk_ESS = Bulk_ESS,
    Tail_ESS = Tail_ESS
  )

# Export to CSV
write.csv(model_summary, "outputs/effect_summary_bayes_flight_buzzes_env_pred.csv", row.names = FALSE)

# saveRDS(nb_model, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_flight_buzzes_env_pred.rds")
nb_model <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_flight_buzzes_env_pred.rds")



#### Extract summary stats
model_summary_clean <- model_summary_clean %>%
  mutate(significant = ifelse(lower_95 > 0 | upper_95 < 0, "Yes", "No"))
model_summary_clean <- model_summary_clean %>%
  mutate(direction = case_when(
    lower_95 > 0 ~ "Positive",
    upper_95 < 0 ~ "Negative",
    TRUE ~ "Uncertain"
  ))
model_summary_clean <- model_summary_clean %>%
  mutate(direction = factor(direction, levels = c("Positive", "Negative", "Uncertain")))

## Plot
environmental_summary_plot <- ggplot(model_summary_clean, aes(x = estimate, y = reorder(term_label, estimate))) +
  geom_point(aes(color = direction), size = 2) +
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95, color = direction), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(
    "Positive" = "darkgreen",
    "Negative" = "orange2",
    "Uncertain" = "black"
  )) +
  labs(
    x = "Effect Size and Credible Intervals",
    y = "Predictor",
    color = "Effect Direction"
  ) +
  theme_classic() +
  theme(legend.position = "right")

ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/bayesian_slopes_environmental_pred.pdf",
  plot = environmental_summary_plot,
  width = 8,       # adjust based on layout
  height = 4
)


#### ----

#### VISUALIZE FLIGHT BUZZES ----
#### PANEL FIGURE FLIGHT BUZZES VS FLOWER AND NECTAR ----
# Get unique location IDs
locations <- unique(combined_data_filtered$location_id)

# Format date column in Date format
combined_data_filtered <- combined_data_filtered %>%
  mutate(date = as.Date(date)) %>%
  group_by(location_id) %>%
  mutate(
    flower_count_std = total_flower_count / max(total_flower_count, na.rm = TRUE),
    nectar_sugar_std = daily_nectar_sugar_mg / max(daily_nectar_sugar_mg, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    microclimate = factor(microclimate, levels = c("Cool", "Moderate", "Warm"))
  )

# Get shared x-axis limits
date_min <- min(combined_data_filtered$date, na.rm = TRUE)
date_max <- max(combined_data_filtered$date, na.rm = TRUE)

# Get unique location IDs
locations <- unique(combined_data_filtered$location_id)
locations <- sort(unique(combined_data_filtered$location_id))

# Loop and build plots
plot_list <- list()

for (loc in locations) {
  
  # Keep these inside plot
  df_loc <- combined_data_filtered %>%
    filter(location_id == loc)
  
  flower_df <- df_loc %>%
    filter(!is.na(total_flower_count))
  
  # Flower tile strip
  p2 <- ggplot(flower_df, aes(x = date)) +
    geom_tile(aes(y = 2, fill = flower_count_std)) +
    scale_fill_gradient(low = "forestgreen", high = "gold", name = "Flower Count") +
    new_scale_fill() +
    geom_tile(aes(y = 0.5, fill = nectar_sugar_std)) +
    scale_fill_gradient(low = "#A0522D", high = "#FFA500", name = "Nectar Sugar") +
    scale_x_date(limits = c(date_min, date_max)) +
    theme_void() +
    theme(legend.position = "none") +
    annotate("text", x = date_min, y = 2, label = "F", hjust = 1, size = 3.5) +
    annotate("text", x = date_min, y = 0.5, label = "N", hjust = 1, size = 3.5)
  
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
  combined <- p1 / p2 + plot_layout(heights = c(6, 1.5))
  plot_list[[loc]] <- combined
}

# Combine into rows and final plot
row1 <- cowplot::plot_grid(plotlist = plot_list[1:3], nrow = 1)
row2 <- cowplot::plot_grid(plotlist = plot_list[4:6], nrow = 1)
row3 <- cowplot::plot_grid(plotlist = plot_list[7:9], nrow = 1)

# Combine rows
final_plot <- cowplot::plot_grid(row1, row2, row3, ncol = 1, rel_heights = c(1, 1, 1), align = "v", axis = "tblr", vjust = -1, hjust = -2) 

# Wrap with ggdraw() to apply margin spacing
final_plot_with_margin <- ggdraw(final_plot) +
  theme(plot.margin = margin(t = 10, r = 20, b = 30, l = 20))

# Add labels with ggdraw
final_with_labels <- ggdraw(final_plot_with_margin) +
  draw_label("2024 Growing Season", x = 0.5, y = 0.04, vjust = 1, angle = 0) +
  draw_label("Daily Bumblebee Flight Buzz Detections (s)", x = 0.02, y = 0.5, angle = 90, vjust = 1)


# Dummy plot for microclimate
legend_microclimate <- ggplot(combined_data_filtered, aes(x = date, y = 1)) +
  geom_point() +
  geom_smooth(method = "loess", aes(fill = microclimate, colour = microclimate), se = TRUE, alpha = 0.3) +
  scale_fill_manual(
    name = "Microclimate",
    values = c("Cool" = "#440154", "Moderate" = "forestgreen", "Warm" = "gold")
  ) +
  scale_colour_manual(
    name = "Microclimate",
    values = c("Cool" = "#440154", "Moderate" = "forestgreen", "Warm" = "gold")
  ) +
  theme_void() +
  theme(legend.position = "right")

# Dummy plot for flower count
legend_flower <- ggplot(combined_data_filtered, aes(x = date, y = 1, fill = flower_count_std)) +
  geom_tile() +
  scale_fill_gradient(low = "forestgreen", high = "gold", name = "Relative\nFlower Count") +
  theme_void() +
  theme(legend.position = "right")

# Dummy plot for nectar sugar
legend_nectar <- ggplot(combined_data_filtered, aes(x = date, y = 1, fill = nectar_sugar_std)) +
  geom_tile() +
  scale_fill_gradient(low = "#A0522D", high = "#FFA500", name = "Rel. Nectar Sugar\nAvailability") +
  theme_void() +
  theme(legend.position = "right")

# Extract legends using cowplot
legend_microclimate_only <- get_legend(legend_microclimate)
legend_flower_only <- get_legend(legend_flower)
legend_nectar_only <- get_legend(legend_nectar)

# Stack legends
legend_stack <- cowplot::plot_grid(
  legend_microclimate_only,
  legend_flower_only,
  legend_nectar_only,
  ncol = 1,
  align = "v"
)

# Plot with legends
final_with_all_legends <- plot_grid(
  final_with_labels,
  legend_stack,
  rel_widths = c(4, 0.6)
)

# Save figure
ggsave(
  filename = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/figures/flight_buzzes_flowering_nectar_panels.pdf",
  plot = final_with_all_legends,
  width = 11,       # adjust based on layout
  height = 8
)
#####
#### Visualize flight buzzes across a 24-hour window ----
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~location_id)


flight_buzz_hourly_centered_beebox <- flight_buzz_hourly_centered %>%
  filter(location_id == "BEEBOX")

ggplot(flight_buzz_hourly_centered_beebox, aes(x = time_hour_shifted, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "loess", colour = "grey44") +
  labs(x = "Time of Day", 
       y = "Hourly Detections (s)") +
  scale_x_continuous(
    breaks = seq(0, 86400, by = 7200),
    labels = function(x) format(as.POSIXct((x + 7200) %% 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M")
  ) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~location_id)


# Convert 2:18 PM to seconds of the day (2:18 PM (shifted) is 12 * 3600 + 18 * 60 = 50400 + 1080 = 44480 seconds)
solar_noon_seconds <- 44480

ggplot(flight_buzz_hourly_centered_beebox, aes(x = time_hour_shifted, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "loess", colour = "grey44") +
  labs(x = "Time of Day", 
       y = "Hourly Flight Buzz Detection (s)") +
  scale_x_continuous(
    breaks = seq(0, 86400, by = 3600),
    labels = function(x) format(as.POSIXct((x + 7200) %% 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M")
  ) +
  theme_classic() +
  ylim(0, 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) +
  # Add vertical line for solar noon (2:18 PM UTC)
  geom_vline(xintercept = solar_noon_seconds, linetype = "dashed", color = "orange2", size = 1) +
  annotate("text", x = solar_noon_seconds, y = 40, label = "Solar Noon", angle = 90, color = "orange2", vjust = - 0.7)

#### ----


#### ACTIVITY PER MICROCLIMATE ----

## Summarize
microclimate_summary <- flight_buzz_daily %>%
  group_by(microclimate, location_id, date) %>%
  summarise(
    mean_daily_buzz = mean(daily_duration_above_threshold, na.rm = TRUE)
  )

microclimate_avg_activity <- microclimate_summary %>%
  group_by(microclimate) %>%
  summarise(
    avg_daily_buzz = mean(mean_daily_buzz, na.rm = TRUE),
    sd_activity = sd(mean_daily_buzz, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_daily_buzz))

## Run tests
kruskal.test(mean_daily_buzz ~ microclimate, data = microclimate_summary)
library(FSA)
dunnTest(mean_daily_buzz ~ microclimate, data = microclimate_summary, method = "bh")

## Display results
sig_results <- data.frame(
  group1 = c("BEEBOX", "BEEBOX", "BEEBOX"),
  group2 = c("Cool", "Moderate", "Warm"),
  p.adj = c(1.107502e-04, 6.769086e-04, 2.969826e-05),
  y.position = c(750, 780, 810),  # adjust based on your y-axis range
  label = c("***", "**", "***") # You can use stars or paste0("p = ", format(p.adj, ...))
)

## Plot results
ggplot(microclimate_summary, aes(x = microclimate, y = mean_daily_buzz)) +
  geom_boxplot(aes(fill = microclimate)) +
  labs(
    x = "Microclimate",
    y = "Daily Flight Buzz Detections (s)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  stat_pvalue_manual(sig_results, 
                     label = "label", 
                     tip.length = 0.01, 
                     bracket.size = 0.5, 
                     size = 4)


#### ----


#### NECTAR PER MICROCLIMATE ----
# Replace NAs to 0s
combined_data_filtered_noNA <- combined_data_filtered %>%
  mutate(
    daily_nectar_sugar_mg = ifelse(is.na(daily_nectar_sugar_mg), 0, daily_nectar_sugar_mg)
  ) 

# Aggregate by microclimate
microclimate_nectar <- combined_data_filtered_noNA %>%
  group_by(microclimate) %>%
  summarise(
    avg_daily_nectar = mean(daily_nectar_sugar_mg),
    sd_nectar = sd(daily_nectar_sugar_mg),
    n = n(),
    se_nectar = sd_nectar / sqrt(n)
  )

# Run Levene test for homogeneity of variances
leveneTest(daily_nectar_sugar_mg ~ as.factor(microclimate), data = combined_data_filtered_noNA)

# Run kruskal-wallis test
kruskal.test(daily_nectar_sugar_mg ~ as.factor(microclimate), data = combined_data_filtered_noNA)

#### ----


#### TEMPERATURE ON ACTIVITY ----
## PREPARATION
# Remove rows with NA, NaN, or Inf in the relevant columns
flight_buzz_daily_clean <- flight_buzz_daily %>%
  filter(!is.na(mean_temp_center) & !is.na(daily_duration_above_threshold)) %>%
  filter(!is.infinite(mean_temp_center) & !is.infinite(daily_duration_above_threshold))

# Set range of degrees of freedom to test
df_values <- 2:10

# Create list to store models and AIC values
models <- list()
aic_values <- numeric(length(df_values))

# Loop over each df value and fit the model
for (i in seq_along(df_values)) {
  df <- df_values[i]
  
  model <- lmer(duration_rounded ~ ns(mean_temp_center, df = df) + (1 | location_id),
                data = flight_buzz_daily_clean,
                REML = FALSE)  # REML = FALSE for model comparison
  
  models[[paste0("df_", df)]] <- model
  aic_values[i] <- AIC(model)
}

# Combine results
df_comparison <- data.frame(df = df_values, AIC = aic_values)

# Print AIC comparison
print(df_comparison)

# Fit mixed effects model with a natural spline term for mean_temp_center
model_lmer_spline <- lmer(duration_rounded ~ ns(mean_temp_center, df = 4) + (1 | location_id), data = flight_buzz_daily_clean)

# Summary of fitted model
summary(model_lmer_spline)

# Make predictions from model with standard errors
predictions <- predict(model_lmer_spline, 
                       newdata = flight_buzz_daily_clean, 
                       re.form = NULL, 
                       se.fit = TRUE)

# Add predictions and errors to dataset
flight_buzz_daily_clean$pred <- predictions$fit
flight_buzz_daily_clean$se_fit <- predictions$se.fit
flight_buzz_daily_clean$ci_lower <- flight_buzz_daily_clean$pred - 1.96 * flight_buzz_daily_clean$se_fit
flight_buzz_daily_clean$ci_upper <- flight_buzz_daily_clean$pred + 1.96 * flight_buzz_daily_clean$se_fit

# Plot data with error lines (CI)
ggplot(flight_buzz_daily_clean, aes(x = mean_temp_center + mean_temp_raw_mean, y = duration_rounded)) +
  geom_point(alpha = 0.6) +  # raw data points
  geom_line(aes(y = pred, colour = microclimate, group = location_id), size = 1.2) +  # fitted line
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = microclimate, group = location_id), alpha = 0.3) +  # CI ribbon
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = max(flight_buzz_daily_clean$duration_rounded, na.rm = TRUE) * 0.5, 
           label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = max(flight_buzz_daily_clean$duration_rounded, na.rm = TRUE) * 0.5, 
           label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  labs(
    x = "Daily Mean Temperature (°C)",
    y = "Daily Bumblebee Flight Buzz Detections (s)",
    colour = "Microclimate",
    fill = "Microclimate"
  ) +
  ylim(0,750) +
  theme_classic() +
  facet_wrap(~microclimate) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold"))+
  scale_fill_manual(values = c("grey44", "#440154", "forestgreen", "gold"))+
  theme(legend.position = "none")



#### ESTIMATE TEMPERATURE THRESHOLD
# Create a sequence of mean_temp_center values for predictions
temp_seq <- seq(min(flight_buzz_daily_clean$mean_temp_center), 
                max(flight_buzz_daily_clean$mean_temp_center), 
                length.out = 100)

# Create an empty data frame to store the results
results <- data.frame(location_id = character(0), 
                      closest_to_zero = numeric(0))

# Loop over each unique location_id in your dataset
for (loc in unique(flight_buzz_daily_clean$location_id)) {
  
  # Create new data frame with the sequence of mean_temp_center values and the current location_id
  newdata <- data.frame(mean_temp_center = temp_seq, 
                        location_id = loc)
  # Make predictions from the model (including random effects for location_id)
  predictions <- predict(model_lmer_spline, newdata = newdata, re.form = ~(1 | location_id), se.fit = FALSE)
  # Find the value of mean_temp_center where the prediction is closest to zero
  closest_to_zero <- temp_seq[which.min(abs(predictions))]
  # Store the result in the results data frame
  results <- rbind(results, data.frame(location_id = loc, temp_threshold = closest_to_zero + mean_temp_raw_mean))
}

# Print the results
print(results)

# Join results with location data
results_joined <- results %>%
  left_join(mean_summer_temp, by = "location_id") %>%
  left_join(location_mapping, by = "location_id")

results_joined %>%
  group_by(microclimate) %>%
  summarize(mean_temp_threshold = mean(temp_threshold))

results_joined %>%
  summarize(
    mean_temp_threshold = mean(temp_threshold),
    sd_temp_threshold = sd(temp_threshold),
    n = n(),
    se_temp_threshold = sd_temp_threshold / sqrt(n)
  )

# Plot threshold per cumulative GDD
ggplot(results_joined, aes(x = summer_GDD0, y = temp_threshold)) +
  geom_smooth(method = "lm", size = 1.2, colour = "grey44") +  # fitted line
  geom_point(aes(colour = microclimate), size = 3) +  # raw data points
  labs(
    x = "Cumul. GDD (T = 0°C)",
    y = "Estimated Threshold Temperature (°C)",
    colour = "Microclimate"
  ) +
  theme_classic() +
  scale_colour_manual(values = c("#440154", "forestgreen", "gold"))























