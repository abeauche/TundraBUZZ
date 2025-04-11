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

test_bumblebee <- read.csv("test_bumblebee_annotation_quick2.csv")

test_bumblebee <- test_bumblebee %>%
  select(aru_number:count)

ggplot(test_bumblebee, aes(x = time_hour, y = count, colour = micro_clim)) +
  geom_point() +
  theme_classic()

test_bumblebee_summary <- test_bumblebee %>%
  group_by(time_hour, micro_clim) %>%
  summarise(total_count = sum(count),
            n_samples = n()
            ) %>%
  mutate(frequency = total_count/n_samples)

test_bumblebee_summary_all <- test_bumblebee %>%
  group_by(time_hour) %>%
  summarise(total_count = sum(count),
            n_samples = n()
  ) %>%
  mutate(frequency = total_count/n_samples)

test_bumblebee_summary_all_floored <- test_bumblebee %>%
  mutate(time_hour_rounded = floor(time_hour)) %>%  # Round down to the nearest hour
  group_by(time_hour_rounded) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),  # Sum of counts per hour
    n_samples = n()  # Number of samples per hour
  ) %>%
  mutate(frequency = total_count / n_samples)  # Frequency per hour

ggplot(test_bumblebee_summary, aes(x = time_hour, y = frequency, colour = micro_clim)) +
  geom_point() +
  xlim(0,24) +
  ylim(0,30) +
  #geom_smooth(se = FALSE) +
  theme_classic()

ggplot(test_bumblebee_summary_all, aes(x = time_hour, y = frequency)) +
  geom_point() +
  xlim(5,24) +
  ylim(0,30) +
  #geom_smooth(se = FALSE) +
  theme_classic() +
  geom_smooth()


test_bumblebee <- test_bumblebee %>%
  mutate(
    time_hour_shifted = ifelse(time_hour < 2.5, time_hour + 24, time_hour) # Shift hours before 6 AM
  )

test_bumblebee_wc <- test_bumblebee %>%
  filter(!micro_clim == "mod")

wilcox.test(count ~ factor(micro_clim), data = test_bumblebee_wc)


test_bumblebee_stats <- test_bumblebee %>%
  

test_bumblebee_summary_shifted <- test_bumblebee %>%
  mutate(
    time_hour_shifted = ifelse(time_hour < 2.5, time_hour + 24, time_hour)
  ) %>%
  group_by(time_hour_shifted, micro_clim) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
    n_samples = n()
  ) %>%
  mutate(
    frequency = total_count / n_samples
  )

test_bumblebee_summary_shifted_rounded <- test_bumblebee %>%
  mutate(
    time_hour_shifted = ifelse(time_hour < 2.5, time_hour + 24, time_hour)
  ) %>%
  mutate(time_hour_shifted_rounded = floor(time_hour_shifted)) %>% 
  group_by(time_hour_shifted_rounded, micro_clim) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
    n_samples = n()
  ) %>%
  mutate(
    frequency = total_count / n_samples
  )

ggplot(test_bumblebee_summary_shifted, aes(x = time_hour_shifted, y = frequency)) +
  geom_point() +
  xlim(6,30) +
  ylim(0,30) +
  #y_line(x = 2.18) +
  #geom_smooth(se = FALSE) +
  theme_classic()

solar_noon <- 14 + 18/60  # Convert 2:18 PM to decimal hours (14.3)


test_bumblebee_summary_shifted_no_mod <- test_bumblebee_summary_shifted %>%
  filter(!micro_clim == "mod")

ggplot(test_bumblebee_summary_shifted_no_mod, aes(x = time_hour_shifted, y = frequency)) +
  geom_point(colour = "grey22", shape = 21, aes(fill = micro_clim), size = 2) +
  geom_vline(xintercept = solar_noon, linetype = "dashed", color = "grey44", size = 0.5) + # Add vertical line
  scale_x_continuous(
    breaks = c(4, 10, 16, 22, 28), # Adjust ticks to show desired times
    labels = c("4 AM", "10 AM", "4 PM", "10 PM", "4 AM (next day)") # Relabel axis
  ) +
  annotate(
    "text",
    x = solar_noon - 0.3, y = max(test_bumblebee_summary$frequency, na.rm = TRUE) * 0.5, # Adjust y-position
    label = "Solar Noon (2:18 PM)",
    angle = 90, # Rotate text vertically
    vjust = -0.5, # Adjust vertical alignment
    colour = "grey22"
  ) +
  labs(
    x = "Time of day",
    y = "Flight buzz frequency per recording",
    fill = "Microclimate"
  ) +
  scale_fill_manual(values = c("darkblue","gold"),
                    labels = c(
                      "cool" = "Cool", 
                      "warm" ="Warm")
  ) +
  theme_classic()


ggplot(test_bumblebee, aes(x = time_hour_shifted, y = count/10)) +
  geom_point(colour = "grey22", shape = 21, aes(fill = micro_clim), size = 3) +
  geom_vline(xintercept = solar_noon, linetype = "dashed", color = "grey44", size = 0.5) + # Add vertical line
  scale_x_continuous(
    breaks = c(4, 10, 16, 22, 28), # Adjust ticks to show desired times
    labels = c("4 AM", "10 AM", "4 PM", "10 PM", "4 AM (next day)") # Relabel axis
  ) +
  ggtitle("Figure 1. Frequency of manually annotated bumblebee flight buzzes (N = 413)") +
  annotate(
    "text",
    x = solar_noon - 0.3, y = max(test_bumblebee_summary$frequency/10, na.rm = TRUE) * 0.5, # Adjust y-position
    label = "Solar Noon (2:18 PM)",
    angle = 90, # Rotate text vertically
    vjust = -0.5, # Adjust vertical alignment
    colour = "grey22"
  ) +
  labs(
    x = "Time of day",
    y = "Flight buzz frequency per recording (count/min)",
    fill = "Microclimate"
  ) +
  scale_fill_manual(values = c("darkblue","forestgreen", "gold"),
                    labels = c(
                      "cool" = "Cool",
                      "mod" = "Moderate",
                      "warm" ="Warm")
  ) +
 # geom_smooth(se=FALSE) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold")
  ) 




