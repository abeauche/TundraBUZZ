# Load required libraries ----
library(tidyverse)
library(lubridate)
library(hms)
library(myClim) ## logger data reading
library(foreach) ## efficient loop
library(data.table) ## efficient data.frame  
library(gganimate)
library(ggplot2)
library(gganimate)


# Set working directory
setwd("/Users/alexandrebeauchemin/TundraBUZZ_github")

# Load data
# buzz_freq <- read_csv("/Volumes/IGUTCHAQ/data/QHI_ARU_Data_2025_Aug2025/buzz_freq_QHI_2025.csv")
buzz_freq <- read_csv("/Volumes/TundraBUZZ/data/clean/buzz_freq_QHI_2024.csv")


# plot
ggplot(buzz_freq, aes(x = date, y = buzz_freq, group = date)) +
  geom_boxplot() +
  geom_point(alpha = 0.3) +
  theme_classic()


# plot
ggplot(buzz_freq, aes(x = date, y = buzz_freq)) +
  geom_point(alpha = 0.5, colour = "darkgreen") +
  geom_smooth(colour = "darkgreen", span = 0.55, se = TRUE) +
  theme_classic() +
  labs(
    x = "2025 growing season",
    y = "Fundamental buzz frequency (Hz)"
  )

hist(buzz_freq$buzz_freq,
     breaks = 20)


library(ggridges)

ggplot(buzz_freq, aes(x = buzz_freq, y = factor(date))) +
  geom_density_ridges(scale = 1.5) +
  theme_classic() +
  labs(
    x = "Buzz frequency",
    y = "Date"
  )



