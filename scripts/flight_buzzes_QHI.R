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

# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Set seed for repeatability
set.seed(123)

# Set latitude and longitude for Qikiqtaruk - Herschel Island
lat <- 69.57
lon <- -138.91


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
  mutate(date = as.POSIXct(datetime, tz = "UTC")) %>%  # or use "America/Whitehorse"
  select(location_id, date, mean_temp, cumulative_GDD0, cumulative_GDD5)






#### Merge datasets ----
## Hourly datasets
flight_buzz_hourly <- hourly_summary_flightbuzzes_ARUQ_2024 %>%
  left_join(environmental_variables_hourly, by = "datetime") %>%
  left_join(QHI_temp_hourly, by = c("datetime", "location_id"))

flight_buzz_hourly <- flight_buzz_hourly %>%
  mutate(time_hour = hms::as_hms(floor_date(datetime, unit = "hour")))

# write_csv(flight_buzz_hourly, "/Volumes/TundraBUZZ/data/clean/flight_buzz_hourly.csv")

## Daily datasets
flight_buzz_daily <- daily_summary_flightbuzzes_ARUQ_2024 %>%
  left_join(environmental_variables_daily, by = "date") %>%
  left_join(QHI_temp_daily, by = c("date", "location_id"))
flight_buzz_daily <- flight_buzz_daily %>%
  mutate(location_id = as.factor(location_id))

# write_csv(flight_buzz_daily, "/Volumes/TundraBUZZ/data/clean/flight_buzz_daily.csv")

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



#### Obtain 3-day moving average with the highest bumblebee activity per site
# Calculate centered 3-day moving average and assign to middle date
moving_avg_df <- flight_buzz_daily %>%
  arrange(location_id, date) %>%
  group_by(location_id) %>%
  mutate(
    ma_3day = slide_dbl(
      daily_duration_above_threshold,
      mean,
      .before = 1,
      .after = 1,
      .complete = TRUE
    )
  ) %>%
  ungroup()

# Extract the date where the centered 3-day average is highest
peak_dates <- moving_avg_df %>%
  group_by(location_id) %>%
  filter(ma_3day == max(ma_3day, na.rm = TRUE)) %>%
  slice(1) %>%  # in case of ties
  select(location_id, peak_date = date, peak_ma_3day = ma_3day)

# View result
peak_dates %>%
  select(location_id, peak_date, peak_ma_3day)

# Join with summer temp
peak_temp_df <- peak_dates %>%
  left_join(mean_summer_temp, by = "location_id")  # assuming env_summary has mean_summer_temp


ggplot(peak_temp_df, aes(x = summer_temp, y = peak_date)) +
  geom_point(size = 3, color = "darkorange") +
  geom_smooth(method="lm") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Date of Peak Bumblebee Activity (3-day avg)"
  ) +
  theme_classic()

ggplot(peak_temp_df, aes(x = summer_GDD0, y = peak_date)) +
  geom_point(size = 3, color = "darkorange") +
  geom_smooth(method="lm") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Date of Peak Bumblebee Activity (3-day avg)"
  ) +
  theme_classic()



#### From flowering_QHI.R
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
  


ggplot(peak_temp_flowering, aes(x = summer_temp, y = difference_days)) +
  geom_point(size = 3, color = "darkorange") +
  geom_smooth(method="lm") +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Phenological Mismatch"
  ) +
  theme_classic()

ggplot(peak_temp_flowering, aes(x = summer_GDD0, y = difference_days)) +
  geom_point(size = 3, color = "darkorange") +
  geom_smooth(method="lm") +
  labs(
    x = "Cumulative Summer Growing Degree Days (T = 0°C)",
    y = "Phenological Mismatch"
  ) +
  theme_classic()

ggplot(peak_temp_flowering, aes(x = summer_GDD5, y = difference_days)) +
  geom_point(size = 3, color = "darkorange") +
  geom_smooth(method="lm") +
  labs(
    x = "Cumulative Summer Growing Degree Days (T = 5°C)",
    y = "Phenological Mismatch"
  ) +
  theme_classic()




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


# Fit the model
model_peak <- lm(peak_date_numeric ~ peak_flowering_numeric + summer_temp, data = peak_temp_flowering)

# Interaction model
model_peak_inter <- lm(peak_date_numeric ~ peak_flowering_numeric * summer_temp, data = peak_temp_flowering)


# Summary of model
summary(model_peak)
summary(model_peak_inter)

peak_temp_flowering_clean <- peak_temp_flowering %>%
  na.omit()  # Removes rows with any NA

head(peak_temp_flowering_clean)

# Scale
library(caret)

# Create a data frame with only the numeric columns that need to be scaled
numeric_data <- peak_temp_flowering %>%
  select(peak_date_numeric, peak_flowering_numeric, summer_temp)

# Create a data frame with only the numeric columns that need to be scaled
numeric_data_GDD0 <- peak_temp_flowering %>%
  select(peak_date_numeric, peak_flowering_numeric, summer_GDD0)

# Apply centering and scaling
preprocessed_data <- preProcess(numeric_data, method = c("center", "scale"))
preprocessed_data_GDD0 <- preProcess(numeric_data_GDD0, method = c("center", "scale"))

# Extract the scaled values
scaled_values <- predict(preprocessed_data, numeric_data)
scaled_values_GDD0 <- predict(preprocessed_data_GDD0, numeric_data_GDD0)

priors <- c(
  prior(normal(0, 1), class = "b"),  # Main effects (coefficients for predictors)
  prior(normal(0, 1), class = "b", coef = "peak_flowering_numeric:summer_temp"),  # Interaction
  prior(normal(0, 5), class = "Intercept"),  # Prior for the intercept
  prior(exponential(1), class = "sigma")  # Prior for the residual SD
)


model_bayes_scaled <- brm(
  peak_date_numeric ~ peak_flowering_numeric * summer_temp,
  data = scaled_values,
  prior = priors,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123,
  cores = 4,
  control = list(adapt_delta = 0.999)
)

summary(model_bayes_scaled)
plot(model_bayes_scaled)
pp_check(model_bayes_scaled)


priors_2 <- c(
  # Main effect priors (flowering and temperature)
  prior(normal(1, 0.1), class = "b"),  # Expect a 1:1 baseline relationship of peak bumblebee activity according to peak flowering date
  prior(normal(0, 0.25), class = "b", coef = "peak_flowering_numeric:summer_temp"),  # Expect summer temperature to shift this relationship
  prior(normal(0, 1), class = "Intercept"), # Intercept prior: data scaled and centered to 0, stdev of 1
  prior(exponential(1), class = "sigma")  # Residual SD: assuming a positive residual variability but with a rate constraint
)



model_bayes_scaled2 <- brm(
  peak_date_numeric ~ peak_flowering_numeric * summer_temp,
  data = scaled_values,
  prior = priors_2,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123,
  cores = 4,
  control = list(adapt_delta = 0.999)
)

summary(model_bayes_scaled2)
plot(model_bayes_scaled2)
pp_check(model_bayes_scaled2)

# saveRDS(model_bayes_scaled2, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/buzz_to_floweringtempinter.rds")
model_bayes_scaled2 <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/buzz_to_floweringtempinter.rds")


priors_GDD0 <- c(
  prior(normal(1, 0.1), class = "b"),  # Main effects (coefficients for predictors)
  prior(normal(0, 3), class = "b", coef = "peak_flowering_numeric:summer_GDD0"),  # Interaction
  prior(normal(0, 1), class = "Intercept"),  # Prior for the intercept
  prior(exponential(1), class = "sigma")  # Prior for the residual SD
)


model_bayes_GDD0 <- brm(
  peak_date_numeric ~ peak_flowering_numeric * summer_GDD0,
  data = scaled_values_GDD0,
  prior = priors_GDD0,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123,
  cores = 4,
  control = list(adapt_delta = 0.999)
)

summary(model_bayes_GDD0)
plot(model_bayes_GDD0)
pp_check(model_bayes_GDD0)


conditional_effects(model_bayes_GDD0)


# Get model summary
model_bayes_GDD0_summary <- summary(model_bayes_GDD0)$fixed %>%
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

# View or export to CSV
# write.csv(model_bayes_GDD0_summary, "outputs/effect_summary_bayes_floweringbuzz_GDD0.csv", row.names = FALSE)
# saveRDS(model_bayes_GDD0_summary, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/buzz_to_floweringGDD0inter.rds")




library(GGally)

# Create a data frame with the variables of interest
plot_data <- scaled_values[, c("peak_date_numeric", "peak_flowering_numeric", "summer_temp")]

# Create pairwise scatter plots
ggpairs(plot_data, 
        upper = list(continuous = "cor"), 
        lower = list(continuous = "smooth"), 
        diag = list(continuous = "barDiag")) +
  theme_minimal()


#### LINEAR MODELS ----

# Now let's plot using ggplot with a separate y-axis for difference_days
ggplot(peak_temp_flowering, aes(x = summer_GDD0)) +
  # Plot Peak Date with distinct color
  geom_point(aes(y = peak_date_numeric), size = 3, color = "goldenrod", alpha = 0.6) +
  # Plot Peak Flowering Date with distinct color
  geom_point(aes(y = peak_flowering_numeric), size = 3, color = "pink", alpha = 0.6) +
  # Plot Difference in Days with distinct color on a separate axis
  geom_point(aes(y = difference_days), size = 3, color = "steelblue", alpha = 0.6) +
  # Add linear regression lines for peak_date, peak_flowering, and difference_days
  geom_smooth(aes(y = peak_date_numeric), method = "lm", color = "goldenrod", se = FALSE) +
  geom_smooth(aes(y = peak_flowering_numeric), method = "lm", color = "pink", se = FALSE) +
  geom_smooth(aes(y = difference_days), method = "lm", color = "steelblue", se = FALSE) +
  # Add a secondary y-axis for difference_days
  scale_y_continuous(
    name = "Phenological Timing (Days)",
    sec.axis = sec_axis(~ ., name = "Phenological Mismatch (Days)")
  ) +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Phenological Timing / Mismatch"
  ) +
  theme_classic()

# Fit the linear models
model_peak_date <- lm(peak_date_numeric ~ summer_GDD0, data = peak_temp_flowering)
model_peak_flowering <- lm(peak_flowering_numeric ~ summer_GDD0, data = peak_temp_flowering)
model_difference_days <- lm(difference_days ~ summer_GDD0, data = peak_temp_flowering)

summary(model_peak_date)
summary(model_peak_flowering)
summary(model_difference_days)

# Extract slopes (coefficients) and standard errors
slopes <- tibble(
  variable = c("Peak Bumblebee Activity", "Peak Flowering", "Difference in Days"),
  slope = c(coef(model_peak_date)[2], coef(model_peak_flowering)[2], coef(model_difference_days)[2]),
  se = c(summary(model_peak_date)$coefficients[2, 2],
         summary(model_peak_flowering)$coefficients[2, 2],
         summary(model_difference_days)$coefficients[2, 2])
)

# Plot slopes with error lines (standard errors)
ggplot(slopes, aes(x = slope, y = variable)) +
  geom_point(size = 3) +  # Points for the slopes
  geom_errorbar(aes(xmin = slope - se, xmax = slope + se), width = 0.2) +  # Error bars for standard errors
  labs(
    x = "Effect Size and Credible Interval of Cumulative GDD5",
    y = "Phenological Trait"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_classic() 



#### NOW IN BAYESIAN ----

prior_peak_date <- c(
  prior(normal(0, 5), class = "Intercept"),  # Intercept with a wide prior
  prior(normal(-0.04, 0.15), class = "b", coef = "summer_GDD0")  # Negative slope for peak_date
)

prior_peak_flowering <- c(
  prior(normal(0, 5), class = "Intercept"),  # Intercept with a wide prior
  prior(normal(-0.05, 0.15), class = "b", coef = "summer_GDD0")  # Stronger negative slope for peak_flowering (expect greater sensitivity to warming in flowering phenology)
)

prior_difference_days <- c(
  prior(normal(0, 5), class = "Intercept"),  # Intercept with a wide prior
  prior(normal(0.02, 0.3), class = "b", coef = "summer_GDD0")  # Positive slope for difference_days
)


# Fit Bayesian models with informed priors (from your earlier code)
model_peak_date_bayesian <- brm(
  peak_date_numeric ~ summer_GDD0,
  data = peak_temp_flowering,
  prior = prior_peak_date,
  chains = 4, cores = 4, iter = 2000
)

model_peak_flowering_bayesian <- brm(
  peak_flowering_numeric ~ summer_GDD0,
  data = peak_temp_flowering,
  prior = prior_peak_flowering,
  chains = 4, cores = 4, iter = 2000
)

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
# model_peak_date_bayesian <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_peak_activitydate_GDD0_bayesian.rds")

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

# View or export to CSV
write.csv(model_peak_date_bayesian_summary, "outputs/model_peak_date_bayesian_summary.csv", row.names = FALSE)

summary(model_peak_flowering_bayesian)
plot(model_peak_flowering_bayesian)
pp_check(model_peak_flowering_bayesian)
# saveRDS(model_peak_flowering_bayesian, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_peak_floweringdate_GDD0_bayesian.rds")
# model_peak_flowering_bayesian <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_peak_floweringdate_GDD0_bayesian.rds")

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

# View or export to CSV
write.csv(model_peak_flowering_bayesian_summary, "outputs/model_peak_flowering_bayesian_summary.csv", row.names = FALSE)

summary(model_difference_days_bayesian)
plot(model_difference_days_bayesian)
pp_check(model_difference_days_bayesian)
# saveRDS(model_difference_days_bayesian, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_diff_peak_activity_flowering_GDD0_bayesian.rds")
# model_difference_days_bayesian <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/model_diff_peak_activity_flowering_GDD0_bayesian.rds")

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

# View or export to CSV
write.csv(model_difference_days_bayesian_summary, "outputs/model_difference_days_bayesian_summary.csv", row.names = FALSE)


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
  mean = mean(posterior_peak_date$b_summer_GDD0),
  lower = quantile(posterior_peak_date$b_summer_GDD0, 0.025),
  upper = quantile(posterior_peak_date$b_summer_GDD0, 0.975)
)

summary_peak_flowering <- data.frame(
  effect = "Peak Flowering",
  mean = mean(posterior_peak_flowering$b_summer_GDD0),
  lower = quantile(posterior_peak_flowering$b_summer_GDD0, 0.025),
  upper = quantile(posterior_peak_flowering$b_summer_GDD0, 0.975)
)

summary_difference_days <- data.frame(
  effect = "Mismatch",
  mean = mean(posterior_difference_days$b_summer_GDD0),
  lower = quantile(posterior_difference_days$b_summer_GDD0, 0.025),
  upper = quantile(posterior_difference_days$b_summer_GDD0, 0.975)
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
    x = "Effect Size and Credible Intervals of Cumul. GDD (T = 0°C)"
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







#### Data visualization ----
flight_buzz_daily_sites <- flight_buzz_daily %>%
  filter(!location_id == "BEEBOX")
flight_buzz_daily_beebox <- flight_buzz_daily %>%
  filter(location_id == "BEEBOX")

# Plot flight buzzes over time
ggplot(flight_buzz_daily_sites, aes(x = date, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="loess", aes(colour = microclimate, group = location_id), se = F) +
  labs(x = "2024 Growing Season", 
       y = "Daily Bumblebee Flight Buzz Detections (s)",
       colour = "Microclimate") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~microclimate, ncol = 1) +
  scale_colour_manual(values = c("#440154", "forestgreen","gold"))

# Plot flight buzzes at BEEBOX over time
ggplot(flight_buzz_daily_beebox, aes(x = date, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="loess", colour = "grey44") +
  labs(x = "2024 Growing Season", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 


# Fit LOESS on BEEBOX site
loess_beebox <- loess(daily_duration_above_threshold ~ as.numeric(date), data = flight_buzz_daily_beebox, span = 0.5)

# Add predictions from BEEBOX LOESS to all data
flight_buzz_daily_with_beebox_loess <- flight_buzz_daily %>%
  mutate(beebox_loess_pred = predict(loess_beebox, newdata = data.frame(date = as.numeric(date))))

# Difference from BEEBOX prediction
flight_buzz_daily_with_beebox_loess <- flight_buzz_daily_with_beebox_loess %>%
  mutate(diff_from_beebox = daily_duration_above_threshold - beebox_loess_pred)

ggplot(flight_buzz_daily_with_beebox_loess, aes(x = date, y = diff_from_beebox, color = microclimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 1.5, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE) +
  #facet_wrap(~microclimate, ncol = 1) +
  labs(
    x = "2024 Growing Season",
    y = "Difference from BEEBOX LOESS (s)",
    title = "Deviation in Flight Buzz Detections from BEEBOX LOESS Trend"
  ) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

flight_buzz_scaled <- flight_buzz_daily %>%
  group_by(location_id) %>%
  mutate(scaled_buzz = daily_duration_above_threshold / mean(daily_duration_above_threshold, na.rm = TRUE)) %>%
  ungroup()

ggplot(flight_buzz_scaled, aes(x = date, y = scaled_buzz, colour = location_id)) +
  geom_point(alpha = 0.8) +
  geom_smooth(se = FALSE) +
  theme_classic() +
  labs(x = "Date", y = "Relative Buzz Intensity (Scaled)",
       colour = "Site") +
  scale_colour_viridis_d()

# First filter BEEBOX and fit loess
beebox_loess <- flight_buzz_scaled %>%
  filter(location_id == "BEEBOX") %>%
  loess(scaled_buzz ~ as.numeric(date), data = ., span = 0.3)

# Predict LOESS values for all dates
loess_preds <- tibble(
  date = unique(flight_buzz_scaled$date),
  beebox_loess = predict(beebox_loess, newdata = data.frame(date = as.numeric(unique(flight_buzz_scaled$date))))
)

flight_buzz_with_diff <- flight_buzz_scaled %>%
  left_join(loess_preds, by = "date") %>%
  mutate(diff_from_beebox_loess = scaled_buzz - beebox_loess)

ggplot(flight_buzz_with_diff %>% filter(location_id != "BEEBOX"), 
       aes(x = date, y = diff_from_beebox_loess, colour = location_id)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  facet_wrap(~microclimate, ncol = 1) +
  labs(
    x = "Date",
    y = "Difference from BEEBOX LOESS (scaled buzz)",
    colour = "Site"
  ) +
  theme_classic() +
  scale_colour_viridis_d()

daily_summary <- flight_buzz_daily %>%
  group_by(date) %>%
  summarise(
    mean_daily_buzz = mean(daily_duration_above_threshold, na.rm = TRUE),
    sd_daily_buzz = sd(daily_duration_above_threshold, na.rm = TRUE),
    n_sites = n()
  )

site_avg_activity <- site_summary %>%
  group_by(location_id) %>%
  summarise(
    avg_daily_buzz = mean(mean_daily_buzz, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_daily_buzz)) %>%
  left_join(mean_summer_temp, by = "location_id")


site_avg_activity_nobeebox <- site_avg_activity %>%
  filter(!location_id == "BEEBOX")

# View the site with the highest average activity
ggplot(site_avg_activity_nobeebox, aes(x = summer_GDD0, y = avg_daily_buzz)) +
  geom_point(aes(colour = summer_GDD0)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Date",
    y = "Mean Daily Flight Buzz Duration (s)",
    color = "Site"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



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

ggplot(microclimate_summary, aes(x = microclimate, y = mean_daily_buzz)) +
  geom_boxplot(aes(fill = microclimate)) +
  labs(
    x = "Microclimate",
    y = "Daily Flight Buzz Duration (s)",
    title = "Comparison of Bumblebee Activity Across Microclimates"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### CONTINUE IN BAYESIAN FRAMEWORK ####
#### CLIMATE VARIABLES

ggplot(flight_buzz_daily_beebox, aes(x = mean_wind_speed, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Mean Wind Speed (km/h)", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_daily_beebox, aes(x = mean_stn_press_k_pa, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Mean Atmospheric Pressure (kPa)", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_daily_beebox, aes(x = predominant_wind_dir, y = daily_duration_above_threshold)) +
  geom_point() +  
  #geom_smooth(method="loess", colour = "grey44") +
  labs(x = "Daily Predominant Wind Direction", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_daily_beebox, aes(x = avg_air_rh_1m, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Relative Humidity", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_daily_beebox, aes(x = day_length_hours, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Day Length", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_daily_beebox, aes(x = cloud_cover_pct, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Cloud Cover", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_daily_beebox, aes(x = mean_temp, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Mean Temperature (°C)", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

#### ----

#### ACROSS ALL SITES ----
ggplot(combined_data_filtered, aes(x = daily_nectar_sugar_mg, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Nectar Sugar Availability (mg)", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(flight_buzz_daily, aes(x = mean_wind_speed, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Mean Wind Speed (km/h)", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(flight_buzz_daily, aes(x = mean_stn_press_k_pa, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Mean Atmospheric Pressure (kPa)", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(flight_buzz_daily, aes(x = predominant_wind_dir, y = daily_duration_above_threshold)) +
  geom_point() +  
  #geom_smooth(method="loess", colour = "grey44") +
  labs(x = "Daily Predominant Wind Direction", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(flight_buzz_daily, aes(x = avg_air_rh_1m, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Relative Humidity", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(flight_buzz_daily, aes(x = day_length_hours, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Day Length", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(flight_buzz_daily, aes(x = cloud_cover_pct, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Cloud Cover", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(flight_buzz_daily, aes(x = mean_temp, y = daily_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Daily Mean Temperature (°C)", 
       y = "Daily Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 600) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### ----

#### Preparing climate variables for Bayesian model ----
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



numeric_vars <- combined_data_filtered %>%
  select(mean_temp, daily_nectar_sugar_mg, mean_wind_speed,
         mean_stn_press_k_pa, predominant_wind_dir, avg_air_rh_1m, day_length_hours,
         cloud_cover_pct)

numeric_vars_short <- combined_data_filtered %>%
  select(mean_temp, daily_nectar_sugar_mg, mean_wind_speed,
         mean_stn_press_k_pa, avg_air_rh_1m, day_length_hours)

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


# Plot using Viridis color palette
corrplot(cor(na.omit(numeric_vars)), method = "color", type = "upper", col = viridis(200))

corrplot(cor(na.omit(numeric_vars_short)), method = "color", type = "lower")

cor_matrix <- cor(numeric_vars, use = "complete.obs")

cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%                        # remove self-correlations
  filter(abs(Freq) > 0.7) %>%                     # filter for high correlations
  arrange(desc(abs(Freq)))                        # sort by correlation strength

# 4. View the results
cor_df


# Scale/standardize predictors if needed to help with convergence
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

combined_data_filtered_transformed$daily_duration_above_threshold <- 
  as.integer(combined_data_filtered_transformed$daily_duration_above_threshold)
hist(combined_data_filtered_transformed$mean_temp_z) # normal ish
hist(log(combined_data_filtered_transformed$daily_nectar_sugar_mg_z)) # zero inflated
hist(combined_data_filtered_transformed$mean_wind_speed_z) # normal ish, right skew
hist(combined_data_filtered_transformed$mean_stn_press_k_pa_z) # normal
hist(combined_data_filtered_transformed$predominant_wind_dir_z) # bimodal
hist(combined_data_filtered_transformed$avg_air_rh_1m_z) # normal ish? left skewed
hist(combined_data_filtered_transformed$day_length_hours_z) # super 24 inflated, maybe do log(night length)?
hist(combined_data_filtered_transformed$cloud_cover_pct_z) # flat
 

# Convert date to numeric (e.g., number of days since the start)
combined_data_filtered_transformed <- combined_data_filtered_transformed %>%
  mutate(date_numeric = as.numeric(difftime(date, min(date), units = "days")))
acf(combined_data_filtered_transformed$daily_duration_above_threshold)

# Check the proportion of zeros in your data
mean(combined_data_filtered$daily_duration_above_threshold == 0)

# Fit a negative binomial model

# Step 1: Define the formula correctly using brmsformula()
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

# Then pass the AR(1) separately with grouping via the main brm() call:
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

# Check the summary of the model
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

# View or export to CSV
write.csv(model_summary, "outputs/effect_summary_bayes_flight_buzzes_env_pred.csv", row.names = FALSE)

# saveRDS(nb_model, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_flight_buzzes_env_pred.rds")
# nb_model <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_flight_buzzes_env_pred.rds")

# Use ggpredict to generate predictions for each predictor

pred_temp <- ggpredict(nb_model, terms = "mean_temp_z", bias_correction = TRUE)
pred_nectar <- ggpredict(nb_model, terms = "daily_nectar_sugar_mg_z", bias_correction = TRUE)
pred_nectar <- ggpredict(nb_model, terms = "mean_wind_speed_z", bias_correction = TRUE)



# 1. List of variables that were z-transformed
vars_to_detransform <- c("mean_temp", "daily_nectar_sugar_mg", 
                         "mean_wind_speed", "mean_stn_press_k_pa", 
                         "avg_air_rh_1m", "day_length_hours", 
                         "cloud_cover_pct")

# 2. Calculate the means and standard deviations for each variable
means <- sapply(vars_to_detransform, function(var) mean(combined_data_filtered_transformed2[[var]], na.rm = TRUE))
sds <- sapply(vars_to_detransform, function(var) sd(combined_data_filtered_transformed2[[var]], na.rm = TRUE))

# 3. Detransform the z-scored variables using the calculated means and sds
vars_to_detransform_z <- paste0(vars_to_detransform, "_z")  # Create corresponding z-scored variable names

for (i in seq_along(vars_to_detransform_z)) {
  var_name <- vars_to_detransform[i]  # Original variable name
  var_z_name <- vars_to_detransform_z[i]  # Z-scored variable name
  
  # Detransform the z-scored variable using its mean and standard deviation
  combined_data_filtered_transformed[[var_name]] <- 
    combined_data_filtered_transformed[[var_z_name]] * sds[var_name] + means[var_name]
}

### UNDO THESE TRANSFORMATIONS
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



# Create nicer labels for plotting
model_summary_clean <- model_summary %>%
  filter(term != "Intercept") %>%
  mutate(
    term_label = recode(term,
                        "mean_temp_z" = "Mean Temperature (°C)",
                        "daily_nectar_sugar_mg_z" = "Nectar Sugar (mg)",
                        "mean_wind_speed_z" = "Wind Speed (m/s)",
                        "mean_stn_press_k_pa_z" = "Air Pressure (kPa)",
                        "avg_air_rh_1m_z" = "Relative Humidity (%)",
                        "day_length_hours_z" = "Night Length (hrs)",
                        "cloud_cover_pct_z" = "Cloud Cover (%)"
    )
  )

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

environmental_summary_plot <- ggplot(model_summary_clean, aes(x = estimate, y = reorder(term_label, estimate))) +
  geom_point(aes(color = direction), size = 2) +
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95, color = direction), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(
    "Positive" = "darkgreen",
    "Negative" = "orange2",
    "Uncertain" = "grey44"
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

# Plot (excluding Intercept for clarity if desired)
model_summary %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  labs(
    x = "Effect Size (Estimate ± 95% CI)",
    y = "Predictor"
  ) +
  theme_classic()



##########

### MODEL ITERATION 2
updated_combined_transformed <- combined_data_filtered %>%
  mutate(
    # Create column for presence/absence of flowering
    flowering_present = if_else(total_flower_count > 0, 1, 0),
    flowering_present = replace_na(flowering_present, 0),
    
    daily_nectar_sugar_mg = replace_na(daily_nectar_sugar_mg, 0),
    # Log-transform nectar sugar with +1 to avoid issues with zero values
    daily_nectar_sugar_mg = log(daily_nectar_sugar_mg +1),
    
    # Apply other transformations and scaling to other variables
    mean_wind_speed = log(mean_wind_speed),
    avg_air_rh_1m = log(avg_air_rh_1m),
    night_length_hours = 24 - day_length_hours, 
    night_absent = if_else((24 - day_length_hours) == 0, 1, 0)
)

updated_combined_transformed <- updated_combined_transformed %>%
  left_join(mean_summer_temp, by = "location_id")

hist(updated_combined_transformed$mean_temp) # normal, centered on about 11, ranges from 4 to 22
hist(updated_combined_transformed$daily_nectar_sugar_mg) # log-transformed, zero inflated, right skewed
hist(updated_combined_transformed$flowering_present) # relatively even, 0 or 1
hist(updated_combined_transformed$mean_wind_speed) # quite normal, centered on 2.6, ranges from 1.5 to 3.7
hist(updated_combined_transformed$mean_stn_press_k_pa) # quite normal, centered on 101.0, ranges from 99.5 to 102
hist(updated_combined_transformed$avg_air_rh_1m) # decently normal, centered on 4.3
hist(updated_combined_transformed$night_length_hours) # zero inflated (maybe just remove to NA?)
hist(updated_combined_transformed$night_absent) # binary (presence absence)
hist(updated_combined_transformed$summer_GDD0, breaks = 20) # not a very continuous distribution, not very centered, ranges from 550 to 775

# Compute mean and standard deviation for continuous variables
summary_stats <- updated_combined_transformed %>%
  summarise(
    mean_temp_mean = mean(mean_temp, na.rm = TRUE),
    mean_temp_sd = sd(mean_temp, na.rm = TRUE),
    daily_nectar_sugar_mean = mean(daily_nectar_sugar_mg, na.rm = TRUE),
    daily_nectar_sugar_sd = sd(daily_nectar_sugar_mg, na.rm = TRUE),
    mean_wind_speed_mean = mean(mean_wind_speed, na.rm = TRUE),
    mean_wind_speed_sd = sd(mean_wind_speed, na.rm = TRUE),
    mean_stn_press_mean = mean(mean_stn_press_k_pa, na.rm = TRUE),
    mean_stn_press_sd = sd(mean_stn_press_k_pa, na.rm = TRUE),
    avg_air_rh_1m_mean = mean(avg_air_rh_1m, na.rm = TRUE),
    avg_air_rh_1m_sd = sd(avg_air_rh_1m, na.rm = TRUE),
    night_length_hours_mean = mean(night_length_hours, na.rm = TRUE),
    night_length_hours_sd = sd(night_length_hours, na.rm = TRUE),
    summer_GDD0_mean = mean(summer_GDD0, na.rm = TRUE),
    summer_GDD0_sd = sd(summer_GDD0, na.rm = TRUE)
  )

updated_combined_transformed <- updated_combined_transformed %>%
  mutate(
    mean_temp_z = (mean_temp - summary_stats$mean_temp_mean) / summary_stats$mean_temp_sd,
    daily_duration_above_threshold = as.integer(daily_duration_above_threshold/0.15),
    daily_nectar_sugar_mg_z = (daily_nectar_sugar_mg - summary_stats$daily_nectar_sugar_mean) / summary_stats$daily_nectar_sugar_sd,
    mean_wind_speed_z = (mean_wind_speed - summary_stats$mean_wind_speed_mean) / summary_stats$mean_wind_speed_sd,
    mean_stn_press_k_pa_z = (mean_stn_press_k_pa - summary_stats$mean_stn_press_mean) / summary_stats$mean_stn_press_sd,
    avg_air_rh_1m_z = (avg_air_rh_1m - summary_stats$avg_air_rh_1m_mean) / summary_stats$avg_air_rh_1m_sd,
    night_length_hours_z = (night_length_hours - summary_stats$night_length_hours_mean) / summary_stats$night_length_hours_sd,
    summer_GDD0_z = (summer_GDD0 - summary_stats$summer_GDD0_mean) / summary_stats$summer_GDD0_sd
  )

updated_combined_transformed <- updated_combined_transformed %>%
  arrange(date) %>%
  mutate(date_num = as.numeric(((date - min(date))/86400) +1)) %>%
  ungroup()


# Define the model formula with interaction terms
model_interaction_term <- brmsformula(
  daily_duration_above_threshold ~ 
    mean_temp_z * summer_GDD0_z +  # Interaction between mean_temp_z and summer_GDD0_z
    daily_nectar_sugar_mg_z * summer_GDD0_z +  # Interaction between nectar and summer_GDD0_z
    mean_wind_speed_z +  # Main effect of wind speed
    mean_stn_press_k_pa_z +  # Main effect of pressure
    avg_air_rh_1m_z +  # Main effect of RH
    night_length_hours_z +  # Main effect of night length
    flowering_present * summer_GDD0_z +  # Main effect of flowering presence
    night_absent +  # Main effect of night absence
    (1 | location_id) + ar(time = date_num, gr = location_id, cov = TRUE) # Random intercept for location_id and temporal autocorrelation
)

summary(updated_combined_transformed$flowering_present)
summary(updated_combined_transformed$daily_nectar_sugar_mg_z)
summary(updated_combined_transformed$summer_GDD0_z)

get_prior(
  formula = model_interaction_term,
  data = updated_combined_transformed,
  family = negbinomial()
)

env_model2 <- brm(
  formula = model_interaction_term,
  data = updated_combined_transformed,
  family = negbinomial(),
  prior = c(
    # Priors for fixed effects
    prior(normal(0.5, 0.5), class = "b", coef = "mean_temp_z"),  
    prior(normal(0.5, 0.5), class = "b", coef = "daily_nectar_sugar_mg_z"),  
    prior(normal(-0.5, 0.5), class = "b", coef = "mean_wind_speed_z"),  
    prior(normal(0, 1), class = "b", coef = "mean_stn_press_k_pa_z"),  
    prior(normal(-0.5, 0.5), class = "b", coef = "avg_air_rh_1m_z"),  
    prior(normal(-0.5, 0.5), class = "b", coef = "night_length_hours_z"), 
    prior(normal(0.5, 0.5), class = "b", coef = "flowering_present"),
    prior(normal(0.5, 1), class = "b", coef = "night_absent"),
    prior(normal(-0.3, 0.5), class = "b", coef = "mean_temp_z:summer_GDD0_z"),
    prior(normal(-0.3, 0.5), class = "b", coef = "summer_GDD0_z:daily_nectar_sugar_mg_z"),
    prior(normal(-0.3, 0.5), class = "b", coef = "summer_GDD0_z:flowering_present"),
    
    
    
    # Intercept
    prior(normal(0, 5), class = "Intercept"),
    
    # Random effects
    prior(exponential(1), class = "sd", group = "location_id")
  ),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  file = "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_env_model_updated2.rds"
)

summary(env_model2)
plot(env_model2)
pp_check(env_model2)


summary(env_model)
pp_check(env_model)


# Run loo for both models
loo_env <- loo(env_model)
loo_env2 <- loo(env_model2)
loo_nb_model <- loo(nb_model)

nrow(loo_env$data)  # Check the number of rows for the data used in loo_env
nrow(loo_env2$data) # Check the number of rows for the data used in loo_env2
nrow(loo_nb_model$data) # Check the number of rows for the data used in loo_nb_model

# Compare the LOO results
library(loo)
loo_compare(loo_env, loo_env2)




# Plot for 'mean_temp_z'
pred_temp <- ggpredict(nb_model, terms = "mean_temp_z")
plot(pred_temp) +
  labs(title = "Predicted Relationship: Mean Temperature vs. Duration Above Threshold",
       x = "Mean Temperature (z-scored)",
       y = "Predicted Duration Above Threshold") +
  theme_minimal()

# Plot for 'daily_nectar_sugar_mg_z'
pred_nectar <- ggpredict(nb_model, terms = "daily_nectar_sugar_mg_z")
plot(pred_nectar) +
  labs(title = "Predicted Relationship: Nectar Sugar vs. Duration Above Threshold",
       x = "Daily Nectar Sugar (z-scored)",
       y = "Predicted Duration Above Threshold") +
  theme_minimal()





# Logistic regression model for flower presence
logistic_model <- brm(
  formula = flower_present ~ daily_nectar_sugar_mg + mean_temp + mean_wind_speed + 
    mean_stn_press_k_pa + avg_air_rh_1m + day_length_hours + cloud_cover_pct,
  family = bernoulli(),  # Logistic regression for binary outcome (presence/absence)
  data = combined_data_filtered,
  prior = c(
    prior(normal(0, 5), class = "b"),  # Priors for coefficients
    prior(normal(0, 5), class = "Intercept")
  ),
  chains = 4, 
  iter = 2000, 
  warmup = 1000
)

# Check the summary of the logistic model
summary(logistic_model)


# Negative Binomial model for activity given flower presence
count_model <- brm(
  formula = daily_duration_above_threshold ~ daily_nectar_sugar_mg + mean_temp + mean_wind_speed + 
    mean_stn_press_k_pa + avg_air_rh_1m + day_length_hours + cloud_cover_pct + 
    (1 | location_id),
  family = negbinomial(),  # Negative Binomial distribution for count data
  data = combined_data_filtered,
  prior = c(
    prior(normal(0, 5), class = "b"),  # Priors for coefficients
    prior(normal(0, 5), class = "Intercept"),
    prior(exponential(1), class = "sd")  # Prior for random effects
  ),
  chains = 4, 
  iter = 2000, 
  warmup = 1000
)

# Check the summary of the count model
summary(count_model)



#### BAYESIAN
# Bayesian Linear Model using brms
model_bumblebee_activity <- brm(
  daily_duration_above_threshold ~ mean_temp + daily_nectar_sugar_mg + 
    mean_wind_speed + 
    mean_stn_press_k_pa + 
    predominant_wind_dir + 
    avg_air_rh_1m + 
    day_length_hours + 
    cloud_cover_pct, 
  data = flight_buzz_daily,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = "b"),  # Priors for coefficients
    prior(normal(0, 10), class = "sigma")  # Prior for residual error
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Warmup period
  chains = 4,  # Number of chains
  cores = 4,  # Number of cores
  control = list(adapt_delta = 0.95)  # Control for convergence
)


# Check the model summary
summary(model_bumblebee_activity)

# Plot the diagnostics (trace plots)
plot(model_bumblebee_activity)


















#### Testing POLCAM data ----


ggplot(flower_counts, aes(x = date, y = total_flower_count)) +
  geom_point(color = "pink") +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.7, color = "magenta") +
  #geom_line(color = "darkgreen") +
  labs(title = "Total Flower Count Over Time",
       x = "Date",
       y = "Total Flower Count") +
  theme_classic() +
  facet_wrap(~location_id, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(flower_counts, aes(x = date, y = daily_nectar_sugar_mg)) +
  geom_point(color = "gold") +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.7, color = "orange") +
  #geom_line(color = "darkgreen") +
  labs(title = "Total Nectar Sugar Availability Over Time",
       x = "Date",
       y = "Nectar Sugar Availability (mg)") +
  theme_classic() +
  facet_wrap(~location_id, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(flower_counts, aes(x = date)) +
  geom_point(aes(y = total_flower_count), color = "pink") +
  geom_smooth(aes(y = total_flower_count), method = "loess", se = FALSE, alpha = 0.7, color = "magenta") +
  geom_point(aes(y = daily_nectar_sugar_mg), color = "gold") +
  geom_smooth(aes(y = daily_nectar_sugar_mg), method = "loess", se = FALSE, alpha = 0.7, color = "orange") +
  #geom_line(color = "darkgreen") +
  labs(title = "Flower Count and Nectar Sugar Over Time",
       x = "Date",
       y = "Total Flower and Nectar Availability") +
  theme_classic() +
  facet_wrap(~location_id, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

scale_factor <- 10

ggplot(combined_data_filtered, aes(x = cumulative_GDD0)) +
  geom_point(aes(y = daily_duration_above_threshold)) +
  geom_point(aes(y = total_flower_count*5), colour = "magenta") +
  geom_point(aes(y = daily_nectar_sugar_mg*5), colour = "orange") 

# Plot with two y-axes using ggplot2 and ggplot2-secondary-axis functionality
ggplot(combined_data_filtered, aes(x = date)) +
  geom_point(aes(y = daily_duration_above_threshold)) +
  geom_point(aes(y = total_flower_count*5), colour = "magenta") +
  geom_point(aes(y = daily_nectar_sugar_mg*5), colour = "orange") +
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





# get top 10% dates of activity per site
top_10_by_site <- combined_data_filtered %>%
group_by(location_id) %>%
  filter(daily_duration_above_threshold >= quantile(daily_duration_above_threshold, 0.9, na.rm = TRUE)) %>%
  ungroup()

# View the result
top_10_by_site 

# Step 1: Calculate mean and standard deviation of daily_duration_above_threshold for each site
top_by_stdev_site <- combined_data_filtered %>%
  group_by(location_id) %>%
  mutate(
    mean_activity = mean(daily_duration_above_threshold, na.rm = TRUE),
    sd_activity = sd(daily_duration_above_threshold, na.rm = TRUE)
  ) %>%
  filter(daily_duration_above_threshold >= (mean_activity + 1 * sd_activity)) %>%  # Adjust the multiplier for the number of standard deviations
  ungroup()

# View the result
top_by_stdev_site


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

ggplot(proportion_by_site, aes(x = summer_GDD0, y = proportion_with_flowers)) +
  geom_point(size = 3, color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "grey30") +
  labs(
    x = "Cumulative Summer Growing Degree Days (T = 0°C)",
    y = "Overlap Between Peak Bumblebee Activity and Flowering"
  ) +
  theme_classic()

proportion_by_site <- proportion_by_site %>%
  mutate(
    summer_GDD0_scaled = (summer_GDD0 - mean(summer_GDD0, na.rm = TRUE)) / 100
  )

# Step 1: Calculate observed variance
proportion_by_site <- proportion_by_site %>%
  mutate(observed_variance = (days_with_flowers / total_days) * (1 - days_with_flowers / total_days))

# Step 2: Calculate expected variance under binomial assumption
proportion_by_site <- proportion_by_site %>%
  mutate(expected_variance = total_days * (days_with_flowers / total_days) * (1 - days_with_flowers / total_days))

# Step 3: Compare observed variance to expected variance
proportion_by_site <- proportion_by_site %>%
  mutate(overdispersion_factor = observed_variance / expected_variance)

# Step 4: Check if any site has overdispersion
summary(proportion_by_site$overdispersion_factor)

# Fit binomial model
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


# Check the summary of the model
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

# View or export to CSV
# write.csv(model_summary_binomial, "outputs/model_summary_overlap.csv", row.names = FALSE)

         
# Load the necessary library
library(ggeffects)

# Use ggpredict to generate predicted probabilities
predicted <- ggpredict(model_binomial, terms = "summer_GDD0_scaled")
predicted <- predicted %>%
  mutate(predicted_probabilities = predicted / 6)

# View the predicted values
head(predicted)


# Predicted number of flowering days and their corresponding credible intervals
predicted_successes <- predicted$predicted
predicted_lower <- predicted$conf.low  # Lower bound of 95% CI
predicted_upper <- predicted$conf.high  # Upper bound of 95% CI

# Convert to probabilities (successes / total_days)
predicted_probabilities <- predicted_successes / 6
predicted_probabilities_lower <- predicted_lower / 6
predicted_probabilities_upper <- predicted_upper / 6

# Now plot the predicted probabilities with the credible intervals
ggplot(predicted, aes(x = x)) +
  geom_line(aes(y = predicted_probabilities), color = "grey20") +  # Predicted line
  geom_ribbon(aes(ymin = predicted_probabilities_lower, ymax = predicted_probabilities_upper), fill = "grey44", alpha = 0.2) +  # Credible intervals
  geom_point(data = proportion_by_site, aes(x = summer_GDD0_scaled, y = proportion_with_flowers), size = 3, color = "forestgreen") +  # Actual data points
  labs(x = "Scaled Cumul. GDD (1:100, T = 0°C)",
       y = "Overlap between Top 10% Activity Dates and Flowering") +
  theme_classic()


# saveRDS(model_binomial, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/bayes_proportion_flowering.rds")








library(zoo)

# Ensure date is in Date format
combined_data <- combined_data %>%
  arrange(location_id, date)

# Compute 5-day rolling sums of activity
rolling_activity <- combined_data %>%
  group_by(location_id) %>%
  arrange(date) %>%
  mutate(rolling_sum = zoo::rollapply(
    daily_duration_above_threshold, 
    width = 10, 
    FUN = sum, 
    align = "left", 
    fill = NA
  )) %>%
  ungroup()

top_5day_window <- rolling_activity %>%
  group_by(location_id) %>%
  filter(rolling_sum == max(rolling_sum, na.rm = TRUE)) %>%
  slice(1) %>%  # In case of ties, pick the first occurrence
  ungroup()

# For each selected window, get the 5 dates starting from the start date
top_5day_periods <- top_5day_window %>%
  select(location_id, start_date = date) %>%
  rowwise() %>%
  mutate(date_seq = list(seq(start_date, by = "day", length.out = 10))) %>%
  unnest(cols = c(date_seq)) %>%
  rename(date = date_seq)

top_5day_with_flowers <- left_join(top_5day_periods, combined_data, by = c("location_id", "date"))
top_5day_with_flowers <- left_join(top_5day_with_flowers, mean_summer_temp, by = "location_id")

overlap_summary <- top_5day_with_flowers %>%
  group_by(location_id) %>%
  summarise(
    mean_temp = mean(mean_temp, na.rm = TRUE),
    summer_temp = mean(summer_temp, na.rm = TRUE),
    summer_GDD0 = mean(summer_GDD0, na.rm = TRUE),
    flowering_days = sum(total_flower_count > 0, na.rm = TRUE),
    overlap_proportion = flowering_days / 10
  )

ggplot(overlap_summary, aes(x = summer_temp, y = overlap_proportion)) +
  geom_point(size = 3, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "grey40") +
  labs(
    title = "Flowering Overlap During Peak 5-Day Bumblebee Activity",
    x = "Mean Summer Temperature (°C)",
    y = "Proportion of Peak Activity Period with Flowers"
  ) +
  theme_classic()







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


# Prepare data
buzz_beebox <- flight_buzz_hourly_centered %>%
  filter(location_id == "BEEBOX") %>%
  mutate(
    date = as.Date(datetime),  # Replace with your actual column name
    week = floor_date(date, unit = "week")
  )

buzz_beebox <- buzz_beebox %>%
  filter(!week == "2024-08-11")

# Get the full sequence of dates and hours in the range
all_times <- expand.grid(
  date = seq(min(buzz_beebox$date), max(buzz_beebox$date), by = "day"),
  time_hour = 0:23)

# Convert time_hour from integer to "HH:00:00" format
all_times$time_hour <- sprintf("%02d:00:00", all_times$time_hour)

# Convert time_hour to time class
all_times$time_hour <- hms::as_hms(all_times$time_hour)

# Shift_time_hour
all_times <- all_times %>%
  mutate(time_hour_shifted = (as.numeric(time_hour) - 7200) %% 86400)


# Join and fill in missing values
buzz_beebox_complete <- buzz_beebox %>%
  right_join(all_times, by = c("date", "time_hour_shifted", "time_hour")) %>%
  mutate(
    location_id = "BEEBOX",
    week = floor_date(date, unit = "week"),
    total_duration_above_threshold = replace_na(total_duration_above_threshold, 0)
  )

# Join and fill in missing values
buzz_beebox_complete <- buzz_beebox_complete %>%
  select(location_id, total_duration_above_threshold, time_hour, time_hour_shifted, date)

# Combine date and time_hour into a datetime column
buzz_beebox_merge <- buzz_beebox_complete %>%
  mutate(datetime = as.POSIXct(paste(date, time_hour), format = "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_hour)



flight_buzz_hourly_beebox_complete <- buzz_beebox_merge %>%
  left_join(environmental_variables_hourly, by = c("datetime")) %>%
  left_join(QHI_temp_hourly, by = c("datetime", "location_id"))


flight_buzz_hourly_beebox_complete <- flight_buzz_hourly_beebox_complete %>%
  mutate(datetime = with_tz(datetime, tz = "America/Whitehorse")) %>%
  mutate(time_hour = hms::as_hms(floor_date(datetime, unit = "hour")))


#### CONTINUE IN BAYESIAN FRAMEWORK ####
#### CLIMATE VARIABLES

ggplot(flight_buzz_hourly_beebox_complete, aes(x = wind_spd_km_h, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Hourly Mean Wind Speed (km/h)", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  #ylim(0, 90) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_hourly_beebox_complete, aes(x = stn_press_k_pa, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Hourly Atmospheric Pressure (kPa)", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  #ylim(0, 90) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_hourly_beebox_complete, aes(x = wind_dir_deg, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="loess", colour = "grey44") +
  labs(x = "Wind Direction (in degrees)", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  #ylim(0, 90) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_hourly_beebox_complete, aes(x = avg_air_rh_1m, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Relative Humidity", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  #ylim(0, 90) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_hourly_beebox_complete, aes(x = altitude, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Sun altitude", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  #ylim(0, 90) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_hourly_beebox_complete, aes(x = azimuth, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="loess", colour = "grey44") +
  labs(x = "Sun azimuth", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  #ylim(0, 90) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 

ggplot(flight_buzz_hourly_beebox_complete, aes(x = mean_temp, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Air Temperature", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  #ylim(0, 90) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~location_id) 







#### CONTINUE IN BAYESIAN FRAMEWORK ####
#### CLIMATE VARIABLES ----

hist(flight_buzz_hourly$wind_spd_km_h)
ggplot(flight_buzz_hourly, aes(x = wind_spd_km_h, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Hourly Mean Wind Speed (km/h)", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 120) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hist(flight_buzz_hourly$stn_press_k_pa)
ggplot(flight_buzz_hourly, aes(x = stn_press_k_pa, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Hourly Atmospheric Pressure (kPa)", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 120) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

hist(flight_buzz_hourly$avg_air_rh_1m)
ggplot(flight_buzz_hourly, aes(x = avg_air_rh_1m, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Relative Humidity", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 120) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

hist(flight_buzz_hourly$altitude)
ggplot(flight_buzz_hourly, aes(x = altitude, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Sun altitude", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 120) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# Fit a linear mixed-effects model
library(lme4)
model <- lmer(total_duration_above_threshold ~ altitude + (1|day), data = flight_buzz_hourly)
summary(model)

hist(flight_buzz_hourly$azimuth)
ggplot(flight_buzz_hourly, aes(x = abs(azimuth), y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Sun azimuth", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 120) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hist(flight_buzz_hourly$mean_temp)
ggplot(flight_buzz_hourly, aes(x = mean_temp, y = total_duration_above_threshold)) +
  geom_point() +  
  geom_smooth(method="lm", colour = "grey44") +
  labs(x = "Air Temperature", 
       y = "Hourly Bumblebee Flight Buzz Detections (s)") +
  ylim(0, 120) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_temp <- lm(total_duration_above_threshold ~ mean_temp, data = flight_buzz_hourly)
summary(model_temp)


# Plot
ggplot(buzz_beebox_complete, aes(x = time_hour_shifted, y = total_duration_above_threshold)) +
  geom_point(aes(colour = as.factor(week)), alpha = 0.4, size = 3) +  
  geom_smooth(aes(colour = as.factor(week)), method = "loess", se = F) +  # One overall trend line
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
  ylim(0, 90) +
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


hist(flight_buzz_hourly$mean_temp)
hist(log(flight_buzz_hourly$total_duration_above_threshold))

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



hist(log(flight_buzz_daily$daily_duration_above_threshold))

# Fit separate models for each location_id
# models_daily <- flight_buzz_daily %>%
#  group_by(microclimate) %>%
#  do(model = lm(log(daily_duration_above_threshold) ~ log(mean_temp), data = .))


# Fit models per microclimate using quasipoisson GLM
models_daily <- flight_buzz_daily %>%
  filter(!is.na(daily_duration_above_threshold), !is.na(mean_temp)) %>%
  group_by(microclimate) %>%
  nest() %>%
  mutate(model = map(data, ~ glm(daily_duration_above_threshold ~ log(mean_temp), 
                                 family = quasipoisson(), data = .)))
# Generate predictions from each model
predictions <- models_daily %>%
  mutate(pred_data = map2(data, model, ~ {
    preds <- predict(.y, newdata = .x, se.fit = TRUE, type = "link")
    tibble(
      mean_temp = .x$mean_temp,
      daily_duration_above_threshold = .x$daily_duration_above_threshold,
      predicted = exp(preds$fit),
      lower_bound = exp(preds$fit - 1.96 * preds$se.fit),
      upper_bound = exp(preds$fit + 1.96 * preds$se.fit)
    )
  })) %>%
  select(microclimate, pred_data) %>%
  unnest(pred_data)




# Use GLM with quasipoisson family instead of LM
slopes_flower_adj <- combined_data %>%
  filter(!is.na(mean_temp), !is.na(total_flower_count), !is.na(daily_duration_above_threshold)) %>%
  group_by(location_id, microclimate) %>%
  filter(n() > 2) %>%  # Only keep groups with at least 3 complete rows
  do(tidy(glm(daily_duration_above_threshold ~ mean_temp + total_flower_count, 
              family = quasipoisson(), data = .))) %>%
  filter(term == "mean_temp") %>%
  rename(slope = estimate)

# Join with mean summer temperature
slope_temp_df <- slopes_flower_adj %>%
  left_join(mean_summer_temp, by = "location_id")

# Plot
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




# Fit Bayesian model with group-specific slopes and intercepts for microclimate
flight_buzz_daily <- flight_buzz_daily %>%
  mutate(duration_rounded = round(daily_duration_above_threshold))

hist(flight_buzz_daily$duration_rounded)
hist(flight_buzz_daily$mean_temp)

flight_buzz_daily %>%
  summarise(
    mean_buzz = mean(duration_rounded, na.rm = TRUE),
    var_buzz = var(duration_rounded, na.rm = TRUE)
  )

# Bayesian Negative Binomial model
temp_flight_buzz_bayes <- brm(
  formula = duration_rounded ~ mean_temp + (1 + mean_temp | location_id),
  data = flight_buzz_daily,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000,
  control = list(adapt_delta = 0.999)
)

# Bayesian Negative Binomial model with reparametrization
temp_flight_buzz_bayes2 <- brm(
  formula = duration_rounded ~ mean_temp + 
    (1 | location_id) + (0 + mean_temp | location_id),
  data = flight_buzz_daily,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000,
  control = list(adapt_delta = 0.999)
)

summary(temp_flight_buzz_bayes2)
plot(temp_flight_buzz_bayes2)
pp_check(temp_flight_buzz_bayes2)

flight_buzz_daily <- flight_buzz_daily %>%
  left_join(mean_summer_temp, by = "location_id")
  

flight_buzz_daily <- flight_buzz_daily %>%
  arrange(date) %>%
  mutate(date_num = as.numeric(((date - min(date))/86400) +1)) %>%
  ungroup()
flight_buzz_daily <- flight_buzz_daily %>%
  mutate(mean_temp_center = scale(mean_temp, center = TRUE, scale = FALSE))  # Center only

hist(flight_buzz_daily$duration_rounded)
hist(flight_buzz_daily$mean_temp)
hist(flight_buzz_daily$mean_temp_center)

flight_buzz_daily <- flight_buzz_daily %>%
  mutate(
    summer_GDD0_100s_c = (summer_GDD0 / 100) - mean(summer_GDD0 / 100, na.rm = TRUE)
  )

hist(flight_buzz_daily$summer_GDD0_100s_c)

prior <- c(
  # Fixed effects
  prior(normal(0.5, 0.3), class = "b", coef = "mean_temp_center"),
  prior(normal(0, 0.3), class = "b", coef = "summer_GDD0_100s_c"),
  prior(normal(-0.3, 0.3), class = "b", coef = "mean_temp_center:summer_GDD0_100s_c"),
  
  # Intercept
  prior(normal(4, 3), class = "Intercept"),  # log-scale; covers durations ~50–400
  
  # Random effects
  prior(exponential(1), class = "sd", group = "location_id"),
  prior(lkj(2), class = "cor"),
  
  # AR(1) term
  prior(lkj(2), class = "ar")
)

temp_flight_buzz_bayes3 <- brm(
  formula = duration_rounded ~ mean_temp_center * summer_GDD0_100s_c +
    (1 + mean_temp_center | location_id) + ar(time = date_num, gr = location_id, cov = TRUE),
  data = flight_buzz_daily,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000,
  control = list(adapt_delta = 0.999)
)

summary(temp_flight_buzz_bayes3)
plot(temp_flight_buzz_bayes3)
pp_check(temp_flight_buzz_bayes3)

# sites are the real sampling unit, microclimate is a post-hoc label

prior_microclim <- c(
  # Fixed effects
  prior(normal(0.5, 0.3), class = "b", coef = "mean_temp_center"),
  prior(normal(0, 0.3), class = "b", coef = "summer_GDD0_100s_c"),
  prior(normal(-0.3, 0.3), class = "b", coef = "mean_temp_center:summer_GDD0_100s_c"),
  
  # Intercept
  prior(normal(4, 3), class = "Intercept"),  # log-scale; covers durations ~50–400
  
  # Random effects
  prior(exponential(1), class = "sd", group = "location_id")
)

temp_flight_buzz_bayes_microclim <- brm(
  formula = duration_rounded ~ mean_temp_center * summer_GDD0_100s_c +
    (1 | location_id),
  data = flight_buzz_daily,
  family = negbinomial(),
  prior = prior_microclim, 
  chains = 4,
  cores = 4,
  iter = 2000,
  control = list(adapt_delta = 0.999)
)

summary(temp_flight_buzz_bayes_microclim)
plot(temp_flight_buzz_bayes_microclim)
pp_check(temp_flight_buzz_bayes_microclim)




# Save last model as an RDS file
saveRDS(temp_flight_buzz_bayes2, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/temp_activity_negbinomial_model.rds")
temp_flight_buzz_bayes2 <- readRDS("/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/temp_activity_negbinomial_model.rds")

saveRDS(temp_flight_buzz_bayes3, "/Users/alexandrebeauchemin/TundraBUZZ_github/outputs/brms_models/temp_activity_negbinomial_model_3.rds")


# Plot conditional effects per site
conditional_effects(temp_flight_buzz_bayes_microclim, 
                    effects = "mean_temp_center", 
                    re_formula = NULL,  # include random effects
                    conditions = data.frame(location_id = unique(flight_buzz_daily$location_id)))

conditional_effects(temp_flight_buzz_bayes_microclim, 
                    effects = "mean_temp_center")



library(ggeffects)
# Use ggeffects to generate predictions from your model
predictions <- ggpredict(temp_flight_buzz_bayes_microclim,
                         terms = "mean_temp_center",
                         bias_correction = TRUE,
                         interval = "prediction")

predictions_by_site <- ggpredict(
  temp_flight_buzz_bayes2,
  terms = c("mean_temp", "location_id"),
  type = "random",  # includes random effects
  bias_correction = TRUE,
  interval = "prediction"
)






# Get predicted values (marginal effects) on response scale with credible intervals

preds <- fitted(temp_flight_buzz_bayes_microclim,
                newdata = flight_buzz_daily,
                summary = TRUE,
                re_formula = NULL) 

str(preds)

mean_temp_raw_mean <- mean(flight_buzz_daily$mean_temp, na.rm = TRUE)

# Combine predictions with original data
flight_buzz_preds <- flight_buzz_daily %>%
  bind_cols(as.data.frame(preds)) %>%
  rename(
    predicted = Estimate,
    ci_lower = Q2.5,
    ci_upper = Q97.5
  )

ggplot(flight_buzz_preds, aes(x = mean_temp_center + mean_temp_raw_mean, y = duration_rounded)) +
  geom_point() +  # raw data points
  geom_line(aes(y = predicted, group = location_id, color = microclimate), size = 1.2) +  # fitted line
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = location_id, fill = microclimate), alpha = 0.3) +  # CI ribbon
  facet_wrap(~ microclimate) +  # facet by microclimate
  labs(
    x = "Mean Daily Temperature (°C)",
    y = "Flight Buzz Duration (s)"
  ) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  scale_fill_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")


hist((flight_buzz_daily$duration_rounded)) # somewhat normal? centered around 3.5-4, peters off in both directions between 0 and 7

# Fit the model with Gaussian distribution (since we are working with log-transformed data)
temp_flight_buzz_bayes_log <- brm(
  formula = log(duration_rounded + 1) ~ mean_temp_center * summer_GDD0_100s_c + 
    (1 | location_id),
  data = flight_buzz_daily,
  family = gaussian(),  # Change to Gaussian distribution
  chains = 4,
  cores = 4,
  iter = 2000,
  control = list(adapt_delta = 0.999)
)

summary(temp_flight_buzz_bayes_log)
plot(temp_flight_buzz_bayes_log)
pp_check(temp_flight_buzz_bayes_log)


preds_log <- fitted(temp_flight_buzz_bayes_log,
                newdata = flight_buzz_daily,
                summary = TRUE,
                re_formula = NULL) 


# Combine predictions with original data
flight_buzz_preds_log <- flight_buzz_daily %>%
  bind_cols(as.data.frame(preds_log)) %>%
  rename(
    predicted = Estimate,
    ci_lower = Q2.5,
    ci_upper = Q97.5
  )

ggplot(flight_buzz_preds_log, aes(x = mean_temp_center + mean_temp_raw_mean, y = duration_rounded)) +
  geom_point() +  # raw data points
  geom_line(aes(y = exp(predicted), group = location_id, color = microclimate), size = 1.2) +  # fitted line
  geom_ribbon(aes(ymin = exp(ci_lower), ymax = exp(ci_upper), group = location_id, fill = microclimate), alpha = 0.3) +  # CI ribbon
  facet_wrap(~ microclimate) +  # facet by microclimate
  labs(
    x = "Mean Daily Temperature (°C)",
    y = "Flight Buzz Duration (s)"
  ) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  scale_fill_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")


flight_buzz_daily_poly <- flight_buzz_daily %>%
  filter(!mean_temp_center == "NA")

poly_terms <- poly(flight_buzz_daily_poly$mean_temp_center, degree = 2)
flight_buzz_daily_poly <- flight_buzz_daily_poly %>%
  mutate(
    polymean_temp_center1 = poly_terms[, 1],
    polymean_temp_center2 = poly_terms[, 2]
  )
poly_coefs <- attr(poly_terms, "coefs")

temp_flight_buzz_bayes_poly2 <- brm(
  formula = log(duration_rounded + 1) ~ polymean_temp_center1 + polymean_temp_center2 + summer_GDD0_100s_c + (1 | location_id),
  data = flight_buzz_daily_poly,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 2000,
  control = list(adapt_delta = 0.999)
)

summary(temp_flight_buzz_bayes_poly2)
plot(temp_flight_buzz_bayes_poly2)
pp_check(temp_flight_buzz_bayes_poly2)


flight_buzz_daily_poly_narrow <- flight_buzz_daily_poly %>%
  select(location_id,duration_rounded,summer_GDD0_100s_c,mean_temp_center,polymean_temp_center1,polymean_temp_center2)

# Run the fitted function again using the updated data with the polynomial terms
preds_poly <- fitted(temp_flight_buzz_bayes_poly2,
                     newdata = flight_buzz_daily_poly_narrow,
                     summary = TRUE,
                     re_formula = NULL)

# Check the results to ensure the fitted predictions were successful
head(preds_poly)


flight_buzz_preds_poly <- flight_buzz_daily_poly %>%
  bind_cols(as.data.frame(preds_poly)) %>%
  rename(
    predicted = Estimate,
    ci_lower = Q2.5,
    ci_upper = Q97.5
  )
flight_buzz_preds_poly <- flight_buzz_preds_poly %>%
  mutate(mean_temp_actual = mean_temp_center + mean_temp_raw_mean)


ggplot(flight_buzz_preds_poly, aes(x = mean_temp_actual, y = duration_rounded)) +
  geom_point(alpha = 0.6) +  # raw data points
  geom_line(aes(y = exp(predicted), group = location_id, color = microclimate), size = 1.2) +  # fitted line
  geom_ribbon(aes(ymin = exp(ci_lower), ymax = exp(ci_upper), group = location_id, fill = microclimate), alpha = 0.3) +  # CI ribbon
  facet_wrap(~ microclimate) +
  labs(
    x = "Mean Daily Temperature (°C)",
    y = "Flight Buzz Duration (s)"
  ) +
  ylim(0,750) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  scale_fill_manual(values = c("grey44", "#440154", "forestgreen", "gold")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")


# Remove rows with NA, NaN, or Inf in the relevant columns
flight_buzz_daily_clean <- flight_buzz_daily %>%
  filter(!is.na(mean_temp_center) & !is.na(daily_duration_above_threshold)) %>%
  filter(!is.infinite(mean_temp_center) & !is.infinite(daily_duration_above_threshold))

# Fit the mixed effects model
model_lmer <- lmer((duration_rounded) ~ (mean_temp_center) + (1 | location_id), 
              data = flight_buzz_daily_clean)

# Load the splines package for spline terms
library(splines)

# Fit the mixed effects model with a natural spline term for mean_temp_center
model_lmer_spline <- lmer(duration_rounded ~ ns(mean_temp_center, df = 4) + (1 | location_id), 
                          data = flight_buzz_daily_clean)


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
                REML = FALSE)  # Use REML = FALSE for model comparison
  
  models[[paste0("df_", df)]] <- model
  aic_values[i] <- AIC(model)
}

# Combine results
df_comparison <- data.frame(df = df_values, AIC = aic_values)

# Print AIC comparison
print(df_comparison)


# Fit the mixed effects model with a natural spline term for mean_temp_center
model_lmer_spline <- lmer(duration_rounded ~ ns(mean_temp_center, df = 4) + (1 | location_id), 
                          data = flight_buzz_daily_clean)

# Summary of the fitted model
summary(model_lmer_spline)

# Make predictions from the model
flight_buzz_daily_clean$pred <- predict(model_lmer_spline, newdata = flight_buzz_daily_clean)


# Plot data with model predictions
ggplot(flight_buzz_daily_clean, aes(x = mean_temp_center + mean_temp_raw_mean, y = duration_rounded)) +
  geom_point(alpha = 0.6) +  # raw data points
  geom_line(aes(y = pred, group = location_id), size = 1.2) +  # fitted line
  labs(
    x = "Mean Temperature (°C)",
    y = "Flight Buzz Duration (s)"
  ) +
  theme_classic(base_size = 14) +
  facet_wrap(~microclimate)

# Make predictions from the model with standard errors
predictions <- predict(model_lmer_spline, 
                       newdata = flight_buzz_daily_clean, 
                       re.form = NULL, 
                       se.fit = TRUE)

# Add predictions and errors to the dataset
flight_buzz_daily_clean$pred <- predictions$fit
flight_buzz_daily_clean$se_fit <- predictions$se.fit
flight_buzz_daily_clean$ci_lower <- flight_buzz_daily_clean$pred - 1.96 * flight_buzz_daily_clean$se_fit
flight_buzz_daily_clean$ci_upper <- flight_buzz_daily_clean$pred + 1.96 * flight_buzz_daily_clean$se_fit

# Plot data with error lines (CI)
ggplot(flight_buzz_daily_clean, aes(x = mean_temp_center + mean_temp_raw_mean, y = duration_rounded)) +
  geom_point(alpha = 0.6) +  # raw data points
  geom_line(aes(y = pred, colour = microclimate, group = location_id), size = 1.2) +  # fitted line
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, colour = microclimate, fill = microclimate, group = location_id), alpha = 0.3) +  # CI ribbon
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +
  annotate("text", x = 6, y = max(flight_buzz_daily_clean$duration_rounded, na.rm = TRUE) * 0.5, 
           label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  annotate("text", x = 12.6, y = max(flight_buzz_daily_clean$duration_rounded, na.rm = TRUE) * 0.5, 
           label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +
  labs(
    x = "Mean Temperature (°C)",
    y = "Flight Buzz Duration (s)",
    colour = "Microclimate",
    fill = "Microclimate"
  ) +
  ylim(0,750) +
  theme_classic() +
  facet_wrap(~microclimate) +
  scale_colour_manual(values = c("grey44", "#440154", "forestgreen", "gold"))+
  scale_fill_manual(values = c("grey44", "#440154", "forestgreen", "gold"))+
  theme(legend.position = "none")



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
                        location_id = loc)  # Replace with the actual location_id
  
  # Make predictions from the model (including random effects for location_id)
  predictions <- predict(model_lmer_spline, newdata = newdata, re.form = ~(1 | location_id), se.fit = FALSE)
  
  # Find the value of mean_temp_center where the prediction is closest to zero
  closest_to_zero <- temp_seq[which.min(abs(predictions))]
  
  # Store the result in the results data frame
  results <- rbind(results, data.frame(location_id = loc, temp_threshold = closest_to_zero + mean_temp_raw_mean))
}

# Print the results
print(results)

results_joined <- results %>%
  left_join(mean_summer_temp, by = "location_id") %>%
  left_join(location_mapping, by = "location_id")

results_joined %>%
  group_by(microclimate) %>%
  summarize(mean_temp_threshold = mean(temp_threshold))

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







# Generate predictions from the fitted model
flight_buzz_daily_clean$predicted <- predict(model_lmer, newdata = flight_buzz_daily_clean, re.form = NA)

# Create a plot using ggplot
ggplot(flight_buzz_daily_clean, aes(x = mean_temp_center + mean_temp_raw_mean, y = (daily_duration_above_threshold))) +
  geom_point(aes(color = location_id), alpha = 0.6) +  # raw data points with colors by location_id
  geom_line(aes(y = (predicted)), size = 1.2, color = "blue") +  # fitted line (predictions)
  facet_wrap(~ location_id) +  # facet by location_id
  labs(
    x = "Log(Daily Duration Above Threshold)",
    y = "Mean Temperature Center (°C)",
    title = "Predicted Mean Temperature Center vs Log of Daily Duration Above Threshold"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")





summary(temp_flight_buzz_bayes_microclim)
summary(temp_flight_buzz_bayes_log)
summary(temp_flight_buzz_bayes_poly2)
summary(model_lmer)

#### TO TROUBLESHOOT

temp_flight_buzz_bayes_spline <- brm(
  log(duration_rounded + 1) ~ 
    s(mean_temp_center, k = 4) + 
    summer_GDD0_100s_c + 
    t2(mean_temp_center, summer_GDD0_100s_c, k = c(4, 4)) + 
    (1 | location_id),
  data = flight_buzz_daily,
  family = gaussian(),
  cores = 4, chains = 4
)

### Not good ---> 4000 divergent transitions


microclim_avg_preds <- flight_buzz_preds %>%
  group_by(microclimate, mean_temp_center) %>%
  summarise(
    predicted_mean = mean(predicted),
    ci_lower_mean = mean(ci_lower),
    ci_upper_mean = mean(ci_upper),
    .groups = "drop"
  )

microclim_avg_preds <- microclim_avg_preds %>%
  mutate(mean_temp_pred = mean_temp_center + mean_temp_raw_mean)

ggplot() +
  # Raw data
  geom_point(data = flight_buzz_daily, 
             aes(x = mean_temp, y = duration_rounded), 
             color = "darkgreen", alpha = 0.4) +
  
  # Predicted line
  geom_line(data = microclim_avg_preds, 
            aes(x = mean_temp_pred, y = predicted_mean, group = location_id), 
            color = "steelblue", linewidth = 1) +
  
  # Credible interval ribbon
  geom_ribbon(data = microclim_avg_preds,
              aes(x = mean_temp_pred, ymin = ci_lower_mean, ymax = ci_upper_mean, group = location_id),
              fill = "steelblue", alpha = 0.2) +
  
  facet_wrap(~ microclimate) +
  labs(x = "Mean Daily Temperature (°C)",
       y = "Flight Buzz Duration (rounded)",
       title = "Predicted Flight Buzz Duration by Temperature and Microclimate") +
  theme_classic()


ggplot() +
  # Raw data
  geom_point(data = flight_buzz_daily, 
             aes(x = mean_temp, y = duration_rounded), 
             color = "darkgreen", alpha = 0.4) +
  
  # Smoothed predicted line
  geom_smooth(data = microclim_avg_preds, 
              aes(x = mean_temp_bin, y = predicted_mean), 
              method = "loess", color = "steelblue", linewidth = 1) +
  
  # Smoothed credible interval ribbon (both lower and upper)
  geom_smooth(data = microclim_avg_preds,
              aes(x = mean_temp_bin, y = ci_lower_mean), 
              method = "loess", color = "steelblue", linewidth = 1, linetype = "dashed") +
  
  geom_smooth(data = microclim_avg_preds,
              aes(x = mean_temp_bin, y = ci_upper_mean), 
              method = "loess", color = "steelblue", linewidth = 1, linetype = "dashed") +
  facet_wrap(~ microclimate) +
  labs(x = "Mean Daily Temperature (°C)",
       y = "Flight Buzz Duration (rounded)",
       title = "Predicted Flight Buzz Duration by Temperature and Microclimate") +
  theme_classic()



ggplot(flight_buzz_daily, aes(x = mean_temp)) +
  geom_point(aes(y = daily_duration_above_threshold), alpha = 0.4, colour = "grey") +
  geom_line(data = predictions_by_site, aes(x = x, y = predicted, colour = group), size = 1) +
  geom_ribbon(data = predictions_by_site, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15) +
  labs(
    x = "Daily Mean Temperature (°C)",
    y = "Flight Buzz Duration (s)",
    colour = "Site",
    fill = "Site"
  ) +
  theme_classic() +
  ylim(0,600)

# Plotting raw data and predictions
ggplot(flight_buzz_daily, aes(x = mean_temp)) +
  geom_point(aes(y = daily_duration_above_threshold), alpha = 0.4, colour = "grey") +  # Raw data points
  geom_line(data = predictions, aes(x = x, y = predicted), colour = "steelblue", size = 1) +  # Predicted line
  geom_ribbon(data = predictions, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "steelblue", alpha = 0.1) +  # Confidence interval
  labs(x = "Daily Mean Temperature (°C)",
       y = "Flight Buzz Duration (s)") +
  theme_classic() 


# Plotting raw data and predictions
ggplot(flight_buzz_daily, aes(x = mean_temp)) +
  geom_point(aes(y = daily_duration_above_threshold), alpha = 0.7, colour = "grey") +  # Raw data points
  geom_line(data = predictions, aes(x = x, y = predicted), colour = "steelblue", size = 1) +  # Predicted line
  geom_ribbon(data = predictions, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "steelblue", alpha = 0.1) +  # Confidence interval
  labs(x = "Daily Mean Temperature (°C)",
       y = "Flight Buzz Duration (s)") +
  theme_classic() +
  ylim(0,600)





# Add predictions (on response scale)
flight_buzz_daily_pred <- flight_buzz_daily %>%
  add_fitted_draws(temp_flight_buzz_bayes2, re_formula = NULL)  # Includes random effects

# Summarize predictions, ignoring NA values in 'mean_temp'
aggregated_predictions <- flight_buzz_daily_pred %>%
  group_by(mean_temp) %>%
  summarise(
    predicted_median = exp(median(.value, na.rm = TRUE)),
    lower_95 = exp(quantile(.value, probs = 0.025, na.rm = TRUE)),
    upper_95 = exp(quantile(.value, probs = 0.975, na.rm = TRUE))
  )

# Plot: predicted lines with uncertainty per site
ggplot(flight_buzz_daily_pred, aes(x = mean_temp, y = exp(.value), group = .draw)) +
  geom_line(alpha = 0.1, colour = "grey40") +  # Posterior draws
  stat_summary(fun = median, geom = "line", colour = "steelblue", size = 1) +  # Median prediction
  geom_point(data = flight_buzz_daily, aes(x = mean_temp, y = daily_duration_above_threshold), inherit.aes = FALSE, alpha = 0.4) +  # Raw data
  labs(x = "Daily Mean Temperature (°C)",
       y = "Flight Buzz Duration (s)") +
  theme_classic()

# Plot the predicted median and credible intervals with the raw data
ggplot(aggregated_predictions, aes(x = mean_temp, y = predicted_median)) +
  geom_line(colour = "steelblue", size = 1) +  # Median prediction
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "steelblue", alpha = 0.2) +  # 95% CI
  geom_point(data = flight_buzz_daily, aes(x = mean_temp, y = daily_duration_above_threshold), 
             inherit.aes = FALSE, alpha = 0.4, colour = "grey") +  # Raw data
  labs(x = "Daily Mean Temperature (°C)",
       y = "Flight Buzz Duration (s)") +
  theme_classic()



library(ggridges)

ggplot(flight_buzz_daily_pred, aes(x = exp(.value), y = mean_temp, fill = ..x..)) +
  geom_density_ridges(scale = 0.9, alpha = 0.6) +
  labs(x = "Predicted Buzz Duration (s)",
       y = "Daily Mean Temperature (°C)",
       title = "Density of Predicted Buzz Duration vs. Temperature (All Sites)") +
  theme_classic()













# Fit the Bayesian linear mixed-effects model
model_bayes_lme <- brm(
  formula = daily_duration_above_threshold ~ mean_temp + (1 | location_id),
  data = combined_data_filtered,
  family = gaussian(),
  chains = 4, cores = 4, iter = 4000, warmup = 1000,
  seed = 123
)

# Summarize the model
summary(model_bayes_lme)

# Plot the results with facetting by microclimate
ggplot(combined_data_filtered, aes(x = mean_temp, y = daily_duration_above_threshold)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "grey20", se = TRUE) +
  facet_wrap(~ microclimate) +  # Facet by microclimate
  labs(x = "Mean Temperature", y = "Daily Duration Above Threshold") +
  theme_classic()






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
