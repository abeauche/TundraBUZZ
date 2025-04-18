library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(purrr)
# install.packages("janitor")
library(janitor)
library(patchwork)
library(lubridate)
library(segmented)


### PREDICTIONS 2024-07-25 ### ----
ARUQ0_20240725_pred <- read_csv("/Users/alexandrebeauchemin/Desktop/ARUQ0/ARUQ0_20240725/predict_score_20240725.csv")

# Adjust the regular expression to capture the date (yyyymmdd) and time (hhmmss)
ARUQ0_20240725_pred <- ARUQ0_20240725_pred %>%
  mutate(datetime_str = gsub(".*_(\\d{8})_(\\d{6})\\.wav$", "\\1 \\2", file))

# Convert to POSIXct (date and time format)
ARUQ0_20240725_pred <- ARUQ0_20240725_pred %>%
  mutate(datetime = as.POSIXct(datetime_str, format="%Y%m%d %H%M%S"))

# Check the result
head(ARUQ0_20240725_pred)


# Define the threshold
threshold <- 7  # Adjust this threshold to your desired value

# Filter the dataset for BUZZ above the threshold
ARUQ0_20240725_pred_above_threshold <- ARUQ0_20240725_pred %>%
  filter(BUZZ > threshold)

# Calculate the duration above the threshold for each row
# Since each time segment is 0.3 seconds, we can multiply the count of segments by 0.3 to get the duration in seconds
ARUQ0_20240725_pred_above_threshold <- ARUQ0_20240725_pred_above_threshold %>%
  mutate(duration_above_threshold = 0.15)  # each segment is 0.3 seconds

# Summarize the total duration above the threshold for each datetime period
summary_pred_duration_0725 <- ARUQ0_20240725_pred_above_threshold %>%
  group_by(datetime) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold))

# View the summarized durations
head(summary_pred_duration_0725)


# Plot the total duration above the threshold over time
ggplot(summary_pred_duration_0725, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  # Line plot to show the duration over time
  labs(
    title = "Total Duration Above Threshold Over Time",
    x = "Datetime", 
    y = "Total Duration Above Threshold (seconds)"
  ) +
  geom_smooth(method = loess, color = "skyblue4", se = FALSE) +
  ylim(0,100) +
  theme_classic() +  # Optional: for a clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotates x-axis labels for better readability







### PREDICTIONS 2024-07-02 ### ----
ARUQ0_20240702_pred <- read_csv("/Volumes/TundraBUZZ/outputs/predict_score_20240702.csv")

# Adjust the regular expression to capture the date (yyyymmdd) and time (hhmmss)
ARUQ0_20240702_pred <- ARUQ0_20240702_pred %>%
  mutate(datetime_str = gsub(".*_(\\d{8})_(\\d{6})\\.wav$", "\\1 \\2", file))

# Convert to POSIXct (date and time format)
ARUQ0_20240702_pred <- ARUQ0_20240702_pred %>%
  mutate(datetime = as.POSIXct(datetime_str, format="%Y%m%d %H%M%S"))

# Check the result
head(ARUQ0_20240702_pred)


# Define the threshold
threshold <- 7  # Adjust this threshold to your desired value

# Filter the dataset for BUZZ above the threshold
ARUQ0_20240702_pred_above_threshold <- ARUQ0_20240702_pred %>%
  filter(BUZZ > threshold)

# Calculate the duration above the threshold for each row
# Since each time segment is 0.3 seconds, we can multiply the count of segments by 0.3 to get the duration in seconds
ARUQ0_20240702_pred_above_threshold <- ARUQ0_20240702_pred_above_threshold %>%
  mutate(duration_above_threshold = 0.15)  # each segment is 0.3 seconds

# Summarize the total duration above the threshold for each datetime period
summary_pred_duration <- ARUQ0_20240702_pred_above_threshold %>%
  group_by(datetime) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold))

# View the summarized durations
head(summary_pred_duration)


# Plot the total duration above the threshold over time
ggplot(summary_pred_duration, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  # Line plot to show the duration over time
  labs(
    title = "Total Duration Above Threshold Over Time",
    x = "Datetime", 
    y = "Total Duration Above Threshold (seconds)"
  ) +
  geom_smooth(method = loess, color = "skyblue4", se = FALSE) +
  ylim(0,100) +
  theme_classic() +  # Optional: for a clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotates x-axis labels for better readability






### HANDLE SELECTION TABLES ### ----
# Define the path to the selection tables folder
selections_path <- "/Users/alexandrebeauchemin/Desktop/ARUQ0/ARUQ0_20240702/selection_tables"

# List all the selection table files
selections_files <- list.files(selections_path, full.names = TRUE, pattern = "\\.Table\\.1\\.selections\\.txt$")

annotation_data <- read_tsv(selections_files[1], show_col_types = FALSE)  # Read one file to check column types
spec(annotation_data_spec)

# View the column names to identify any discrepancies
colnames(annotation_data)


# Check the column names in the first file after cleaning
annotation_data <- read_tsv(selections_files[1], col_types = cols(
  `Begin Time (s)` = col_double(),
  `End Time (s)` = col_double()
)) %>%
  clean_names()

# View the column names
colnames(annotation_data)


# Process each annotation file and summarize total time
total_times <- selections_files %>%
  map_dfr(~ {
    # Read and clean each selection table
    annotation_data <- read_tsv(.x, show_col_types = FALSE) %>%
      clean_names()  # Standardize column names
    
    # Convert to numeric
    annotation_data <- annotation_data %>%
      mutate(
        begin_time_s = as.numeric(begin_time_s),
        end_time_s = as.numeric(end_time_s)
      )
    
    # Calculate duration and summarize total annotation time
    annotation_data %>%
      mutate(duration = end_time_s - begin_time_s) %>%  # Use cleaned column names
      summarize(total_annotation_time = sum(duration, na.rm = TRUE)) %>%
      mutate(file = basename(.x))  # Add the file name for reference
  })

# View the total time for each file
head(total_times)


# Extract datetime from the file name and add it as a new column
total_times <- total_times %>%
  mutate(datetime = sub(".*_(\\d{8})_(\\d{6}).*", "\\1 \\2", file)) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y%m%d %H%M%S"))

# Check the data with the datetime column
head(total_times)



# Plot total annotation time over time
ggplot(total_times, aes(x = datetime, y = total_annotation_time)) +
  geom_point() +
  labs(x = "Date and Time", y = "Total Annotation Time (s)", title = "Total Annotation Time Over Time") +
  theme_classic()



# Ensure that file names match in both datasets, then join them
combined_data <- total_times %>%
  left_join(summary_pred_duration, by = "datetime")

# View the combined data
head(combined_data)

# Replace NA values with 0 in the predicted_time column
combined_data <- combined_data %>%
  mutate(total_duration_above_threshold = ifelse(is.na(total_duration_above_threshold), 0, total_duration_above_threshold))


# Plot both total annotation time and predicted time
ggplot(combined_data, aes(x = datetime)) +
  geom_point(aes(y = total_annotation_time), color = "blue", size = 2) +
  geom_point(aes(y = total_duration_above_threshold), color = "red", size = 2) +  # Show points for predicted time
  labs(x = "File", y = "Time (s)", title = "Total Annotation Time vs Predicted Time") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate file names for better readability
  scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Predicted Time (s)"), limits = c(0, 50)) +  # Set y-axis limits from 0 to 50  # Secondary axis for predicted time
  theme_classic()




ggplot(combined_data, aes(x = datetime)) +
  # Plot actual total annotation time as points
  geom_point(aes(y = total_annotation_time), color = "orange3", size = 2, alpha = 0.5) +
  # Plot predicted time as points
  geom_point(aes(y = total_duration_above_threshold), color = "skyblue", size = 2, alpha = 0.5) +
  # Add smooth lines for total annotation time
  geom_smooth(aes(y = total_annotation_time), method = "loess", color = "orange3", linetype = "dashed", se = FALSE) +
  # Add smooth lines for predicted time
  geom_smooth(aes(y = total_duration_above_threshold), method = "loess", color = "skyblue", linetype = "solid", se = FALSE) +
  # Labels and title
  labs(x = "Datetime", y = "Time (s)", title = "Total Annotation Time vs Predicted Time") +
  # Rotate datetime labels for readability
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # Set y-axis limits from 0 to 50 and add secondary axis for predicted time
  scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Predicted Time (s)"), limits = c(0, 30)) +
  # Classic theme for a clean look
  theme_classic()


p1 <- ggplot(combined_data, aes(x = datetime)) +
  # Plot actual total annotation time as points
  geom_point(aes(y = total_annotation_time, color = "Manual Annotation"), size = 2, alpha = 0.7) +
  # Plot predicted time as points
  geom_point(aes(y = total_duration_above_threshold, color = "Recognizer Prediction"), size = 2, alpha = 0.7) +
  # Add smooth lines for total annotation time
  geom_smooth(aes(y = total_annotation_time, color = "Manual Annotation"), method = "loess", linetype = "dashed", se = FALSE) +
  # Add smooth lines for predicted time
  geom_smooth(aes(y = total_duration_above_threshold, color = "Recognizer Prediction"), method = "loess", linetype = "solid", se = FALSE) +
  # Labels and title
  labs(x = "Date", y = "Flight Duration (s)", title = "Flight Buzz Duration vs Predicted Duration (Threshold = 7)") +
  # Rotate datetime labels for readability
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(face = "bold")) +  # Make the main title bold)
  # Set y-axis limits from 0 to 50 and add secondary axis for predicted time
  ylim(0,30) + 
  # Classic theme for a clean look
  theme_classic() +
  # Add legend title
  scale_color_manual(name = "Legend", values = c("Manual Annotation" = "orange3", "Recognizer Prediction" = "skyblue"))



### PLOTTING DIFFERENCE ###
combined_data <- combined_data %>%
  mutate(difference = total_duration_above_threshold - total_annotation_time)

p2 <- ggplot(combined_data, aes(x = datetime, y = difference)) +
  geom_point(color = "skyblue4") +
  #geom_line(y=0, size=1, alpha=0.3) +
  labs(x = "Time", y = "Difference (Predicted - Actual) (s)", title = "Difference Between Predicted and Actual Flight Times Over Time") +
  theme_classic() +
  geom_smooth(method = lm, color = "skyblue4") +
  ylim(-25, 25) 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels if needed


cor(combined_data$total_duration_above_threshold, combined_data$total_annotation_time, method = "pearson")


# Overlay p2 as an inset on p1
p1 + inset_element(p2, left = 0.6, bottom = 0.5, right = 1, top = 1)







### PREDICTIONS ARUQ5 ### ----
ARUQ5_2024_pred <- read_csv("/Volumes/TundraBUZZ/recognizer_outputs/predict_score_ARUQ5_20Aug2024.csv")

# Adjust the regular expression to capture the date (yyyymmdd) and time (hhmmss)
ARUQ5_2024_pred <- ARUQ5_2024_pred %>%
  mutate(datetime_str = gsub(".*_(\\d{8})_(\\d{6})\\.wav$", "\\1 \\2", file))

# Convert to POSIXct (date and time format)
ARUQ5_2024_pred <- ARUQ5_2024_pred %>%
  mutate(datetime = as.POSIXct(datetime_str, format="%Y%m%d %H%M%S"))

# Check the result
head(ARUQ5_2024_pred)


# Define the threshold
threshold <- 8  # Adjust this threshold to your desired value

# Filter the dataset for BUZZ above the threshold
ARUQ5_2024_pred_above_threshold <- ARUQ5_2024_pred %>%
  filter(BUZZ > threshold)

# Calculate the duration above the threshold for each row
# Since each time segment is 0.3 seconds, we can multiply the count of segments by 0.3 to get the duration in seconds
ARUQ5_2024_pred_above_threshold <- ARUQ5_2024_pred_above_threshold %>%
  mutate(duration_above_threshold = 0.15)  # each segment is 0.3 seconds

# Summarize the total duration above the threshold for each datetime period
summary_pred_duration_ARUQ5_2024 <- ARUQ5_2024_pred_above_threshold %>%
  group_by(datetime) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold))

# View the summarized durations
head(summary_pred_duration_ARUQ5_2024)


# Plot the total duration above the threshold over time
ggplot(summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  # Line plot to show the duration over time
  labs(
    title = "Flight buzzes across the growing season - ARUQ5 (Threshold = 8)",
    x = "Datetime", 
    y = "Total predicted flight buzz duration (seconds)"
  ) +
  #geom_smooth(method = loess, color = "skyblue4", se = FALSE) +
  ylim(0,100) +
  theme_classic() +  # Optional: for a clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotates x-axis labels for better readability


# Ensure datetime is in POSIXct format and adjust to UTC-7 timezone
summary_pred_duration_ARUQ5_2024_tz <- summary_pred_duration_ARUQ5_2024 %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
         datetime_utc7 = with_tz(datetime, tzone = "Etc/GMT+7"),  # Use a valid time zone
         time_of_day = format(datetime_utc7, "%H:%M:%S"))  # Extract time of day after conversion

# Convert time_of_day to proper time format for plotting
summary_pred_duration_ARUQ5_2024_tz <- summary_pred_duration_ARUQ5_2024_tz %>%
  mutate(time_of_day = hms::as_hms(time_of_day))  # Convert to hms for plotting

# Plot
ggplot(summary_pred_duration_ARUQ5_2024_tz, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5, color = "blue") +  # Points for visibility
  #geom_line(color = "skyblue4") +  # Line to show trends
  labs(
    title = "Flight buzzes across a 24-hour period - ARUQ5 (Threshold = 8)",
    x = "Time of Day", 
    y = "Total predicted flight buzz duration (seconds)"
  ) +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +  # Ensure readable time breaks
  geom_smooth(method = loess, color = "skyblue4", se = TRUE) +
  theme_classic() +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


aru_temp_daily_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv")

aruq5 <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ5")

aru_temp_hourly_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv")

aruq5_h <- aru_temp_hourly_micro %>%
  filter(aru_name == "ARUQ5")

# Assuming your dataset includes columns for 'day', 'month', and 'mean_value'

aruq5_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ5") %>%
  # Combine 'day' and 'month' to create a full date (assuming day is a day number and month is numeric)
  mutate(datetime = as.Date(paste(2024, month, day, sep = "-"), format = "%Y-%m-%d"))

aruq5_data_plot <- aruq5_data_plot %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))

aruq5_data_plot_h <- aru_temp_hourly_micro %>%
  filter(aru_name == "ARUQ5") %>%
  # Combine 'month' and 'day' to create a full date, assuming year is 2024
  mutate(datetime = as.Date(paste(2024, month, day, sep = "-"), format = "%Y-%m-%d")) %>%
  # Combine 'day' and 'month' to create a full date (assuming day is a day number and month is numeric)
  mutate(datetime = as.POSIXct(paste(datetime, hour), format="%Y-%m-%d %H", tz="UTC"))

aruq5_data_plot_h <- aruq5_data_plot_h %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))


# Aggregate total_duration_above_threshold by day
summary_pred_duration_agg <- summary_pred_duration_ARUQ5_2024_tz %>%
  mutate(date = as.Date(datetime)) %>%  # Convert datetime to Date (to group by day)
  group_by(date) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE))  # Sum the duration for each day

# Merge with aru_temp_daily_micro by datetime (ensure the datetime is also in Date format)
merged_data_aruq5 <- summary_pred_duration_agg %>%
  left_join(aruq5_data_plot, by = c("date" = "datetime"))

# Check the merged data
head(merged_data_aruq5)




# Aggregate total_duration_above_threshold by hour
summary_pred_duration_agg_hourly <- summary_pred_duration_ARUQ5_2024_tz %>%
  mutate(datetime = as.POSIXct(datetime)) %>%  # Ensure datetime is in POSIXct format
  mutate(date_time_hour = format(datetime, "%Y-%m-%d %H:00:00")) %>%  # Create a new datetime column with hour precision
  group_by(date_time_hour) %>%
  summarise(total_duration_hour = sum(total_duration_above_threshold, na.rm = TRUE))  # Sum the duration for each hour

# Merge with aru_temp_hourly_micro by datetime (ensure the datetime in aru_temp_hourly_micro also matches hour precision)
merged_data_aruq5_hourly <- summary_pred_duration_agg_hourly %>%
  mutate(date_time_hour = as.POSIXct(date_time_hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  left_join(aruq5_data_plot_h, by = c("date_time_hour" = "datetime"))

# Check the merged data
head(merged_data_aruq5_hourly)

hist(merged_data_aruq5$total_duration_day)
hist(merged_data_aruq5$mean_value)



# Create the plot
ggplot(merged_data_aruq5, aes(x = mean_value, y = total_duration_day)) +
  geom_point(aes(color = as.factor(month))) +  # Scatter plot with blue points
  geom_smooth(method = "lm", aes(color = as.factor(month)), se = FALSE) +  # Add a linear regression line (optional)
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature - ARUQ5",
    x = "Daily Mean Temperature (°C)",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 750)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotate x-axis labels for readability


# Create the plot with direct transformation in aes()
ggplot(merged_data_aruq5, aes(x = mean_value, y = log(total_duration_day))) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", formula = y ~ x, color = "skyblue4", se = TRUE) +  # Linear regression line (apply log transformation inside aes())
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature",
    x = "Daily Mean Temperature (°C)",
    y = "Log of Total Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 5, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 5, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 7)) +  # Adjusted to fit log-transformed values
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotate x-axis labels for readability



# Fit the model using log-transformed total_duration_day
model <- lm(log(total_duration_day) ~ log(mean_value), data = merged_data_aruq5)
summary(model)







#### HERE ####
# Remove rows with NA or Inf values
clean_data <- merged_data_polcam5[!is.na(merged_data_polcam5$mean_value) & 
                                    !is.na(merged_data_polcam5$total_flowering_count) &
                                    !is.infinite(merged_data_polcam5$mean_value) &
                                    !is.infinite(merged_data_polcam5$total_flowering_count), ]

model_2 <- lm(log(total_duration_day) ~ log(mean_value)+log(total_flowering_count), data = clean_data)
summary(model_2)


#######





# Calculate fitted values and confidence intervals from the model
predictions_log <- predict(model, newdata = merged_data_aruq5, se.fit = TRUE)

# Exponentiate to return to the original scale
predictions <- exp(predictions_log$fit)
lower_bound <- exp(predictions_log$fit - 1.96 * predictions_log$se.fit)  # Lower bound (95% CI)
upper_bound <- exp(predictions_log$fit + 1.96 * predictions_log$se.fit)  # Upper bound (95% CI)

aruq5_summer_daily <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ5") %>%
  filter(month %in% c("6","7","8"))

hist(aruq5_summer_daily$mean_value, xlim = c(0, 25), breaks = 11)
?hist()

# Create the plot with original scale data and predictions
ggplot(merged_data_aruq5, aes(x = mean_value, y = total_duration_day)) +
  geom_histogram(data = aruq5_summer_daily, aes(x = mean_value, y = ..density..*6000), 
                 fill = "lightgray", color = "lightgray", alpha = 0.1, bins = 11) +  # Temperature histogram
  geom_point(color = "skyblue") +  # Scatter plot with original data points
  geom_line(aes(x = mean_value, y = predictions), color = "skyblue4", size = 1) +  # Regression line
  geom_ribbon(aes(x = mean_value, ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +  # Confidence intervals
  
  labs(
    title = "Flight Buzz Duration vs Daily Mean Temperature",
    x = "Daily Mean Temperature (°C)",
    y = "Daily Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  #scale_x_continuous(limits = c(0, 29)) +
  scale_y_continuous(
    limits = c(0, 750),  # Adjust y-axis for the total_duration_day data
    sec.axis = sec_axis(trans = ~ . * 0.02, name = "Temperature Distribution")  # Secondary y-axis for histogram
  ) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Create the plot with original scale data and predictions
ggplot(merged_data_aruq5, aes(x = mean_value, y = total_duration_day)) +
  geom_point(color = "skyblue") +  # Scatter plot with original data points
  geom_line(aes(x = mean_value, y = predictions), color = "skyblue4", size = 1) +  # Regression line
  geom_ribbon(aes(x = mean_value, ymin = lower_bound, ymax = upper_bound), alpha = 0.1) +  # Confidence intervals
  
  labs(
    title = "Flight Buzz Duration vs Daily Mean Temperature",
    x = "Daily Mean Temperature (°C)",
    y = "Daily Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(
    limits = c(0, 750)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability



# Create the plot
ggplot(merged_data_aruq5_hourly, aes(x = mean_value, y = total_duration_hour)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", color = "skyblue4", se = TRUE) +  # Add a linear regression line (optional)
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature",
    x = "Hourly Mean Temperature (°C)",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_x_continuous(limits = c(-5, 30)) +
  scale_y_continuous(limits = c(0, 60)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotate x-axis labels for readability



# Calculate temperature density (i.e., how often each temperature occurs)
merged_data_aruq5_hourly_clean <- merged_data_aruq5_hourly %>%
  filter(!is.na(mean_value))

merged_data_aruq5_hourly_clean <- merged_data_aruq5_hourly_clean %>%
  mutate(temp_density = dnorm(mean_value, mean = mean(mean_value), sd = sd(mean_value)))

hist(log(merged_data_aruq5_hourly_clean$total_duration_hour))
hist(merged_data_aruq5_hourly_clean$mean_value)


# Remove rows with missing or infinite values in either predictor or response variables
clean_data <- merged_data_aruq5_hourly_clean %>%
  filter(!is.na(mean_value) & !is.na(total_duration_hour) & !is.na(temp_density) &
           !is.infinite(mean_value) & !is.infinite(total_duration_hour) & !is.infinite(temp_density))

# Check for consistency in the number of rows
nrow(clean_data)

# Create the plot with density as the weight for each point
ggplot(merged_data_aruq5_hourly_clean, aes(x = mean_value, y = total_duration_hour, size = temp_density), alpha = 0.5) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  #geom_smooth(method = "lm", formula = log(merged_data_aruq5_hourly_clean$total_duration_hour) ~ merged_data_aruq5_hourly_clean$mean_value, color = "skyblue4", se = FALSE) +  # Add a linear regression line (optional)
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature",
    x = "Hourly Mean Temperature (°C)",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 40, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 40, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_x_continuous(limits = c(-5, 30)) +
  #scale_y_continuous(limits = c(0, 60)) +
  scale_size_continuous(range = c(0.5, 3)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotate x-axis labels for readability




ggplot(merged_data_aruq5_hourly_clean, aes(x = mean_value, y = total_duration_hour, size = temp_density)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", formula = y ~ x, color = "skyblue4", se = FALSE, weights = merged_data_aruq5_hourly_clean$temp_density) +  # Weighted regression line
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature",
    x = "Hourly Mean Temperature (°C)",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  annotate("text", x = 6, y = 40, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 40, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_size_continuous(range = c(1, 5)) +  # Scale the size of the points
  scale_x_continuous(limits = c(-5, 30)) +
  scale_y_continuous(limits = c(0, 60)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




aruq5_plot <- aruq5_data_plot %>%
  ggplot(aes(x = date, y = mean_value)) +
  # Plot the points for temperature with color based on microclimate
  geom_point(aes(), size = 0.7, alpha = 0.7) +
  geom_line() +
  # Add a smoothed line for each microclimate
  geom_smooth(aes(), se = TRUE) +
  theme_classic()
  

aruq5_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ5") %>%
  ggplot(aes(x = day, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))


# Create the plot with dual axes
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue", alpha = 0.7) +
  #geom_line(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "blue") +
  # Plot temperature curve
  geom_smooth(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange3", method = loess, alpha = 0.7) +
  geom_smooth(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue4", method = loess) +
  geom_line(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  # Customizing the plot
  labs(
    title = "Flight Buzzes and Temperature Over Time",
    x = "Datetime", 
    y = "Total Duration Above Threshold (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature"
  ) +
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 100),  # Set the limit for total duration above threshold
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())  # Secondary axis for temperature
  ) +
  # Crop the x-axis to start from June 20
  scale_x_datetime(limits = c(as.POSIXct("2024-06-20"), max(summary_pred_duration_ARUQ5_2024$datetime))) +
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme(legend.position = "bottom")  # Optional: move legend



# Create the plot with dual axes
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue", alpha = 0.7) +
  #geom_line(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "blue") +
  # Plot temperature curve
  geom_smooth(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange3", method = loess, alpha = 0.7) +
  geom_smooth(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue4", method = loess) +
  geom_line(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  # Customizing the plot
  labs(
    title = "Flight Buzzes and Temperature Over Time",
    x = "Datetime", 
    y = "Total Duration Above Threshold (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature"
  ) +
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),  # Set the limit for total duration above threshold
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())  # Secondary axis for temperature
  ) +
  # Crop the x-axis to start from June 20
  scale_x_datetime(limits = c(as.POSIXct("2024-06-20"), max(summary_pred_duration_ARUQ5_2024$datetime))) +
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme(legend.position = "bottom")  # Optional: move legend



polcam5 <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/polcam5.csv")

# Assign the first row as column names
colnames(polcam5) <- as.character(polcam5[1,])
polcam5 <- polcam5[-1,]  # Remove the first row after assigning it as column names
# Make column names unique by appending a suffix
colnames(polcam5) <- make.unique(colnames(polcam5))

# Remove columns where any NA values are present
polcam5_clean <- polcam5 %>%
  dplyr::select(3,5,6,10,11,31)

# View the cleaned dataset
head(polcam5_clean)

# Convert 'Date (year-month-day)' to Date format and 'Time' to POSIXct
polcam5_clean$datetime <- as.POSIXct(paste(polcam5_clean$`Date (year-month-day)`), 
                                     format = "%Y-%m-%d", tz = "UTC")

# Drop the original date and time columns (if no longer needed)
polcam5_clean <- polcam5_clean %>%
  dplyr::select(-`Date (year-month-day)`, -Time, -Plot)

# Convert columns to numeric if necessary
polcam5_clean$`Salix arc. open female catkin` <- as.numeric(polcam5_clean$`Salix arc. open female catkin`)
polcam5_clean$`Salix arc. open male catkin` <- as.numeric(polcam5_clean$`Salix arc. open male catkin`)
polcam5_clean$`Lupine open flower` <- as.numeric(polcam5_clean$`Lupine open flower`)

# Create a new variable for the total flowering count
polcam5_clean$total_flowering_count <- 
  polcam5_clean$`Salix arc. open female catkin` +
  polcam5_clean$`Salix arc. open male catkin` +
  polcam5_clean$`Lupine open flower`

# Filter out rows where total_flowering_count is 0
polcam5_clean_filtered <- polcam5_clean %>%
  dplyr::filter(total_flowering_count != 0)

# Convert 'Date (year-month-day)' to Date format and 'Time' to POSIXct
polcam5_clean <- polcam5_clean %>%
  mutate(date = datetime)


# Create the plot with dual axes and the new variables
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue", alpha = 0.7) +
  
  # Plot temperature curve
  geom_smooth(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange3", method = loess, alpha = 0.7) +
  geom_smooth(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue4", method = loess) +
  geom_line(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Plot the new variables
  geom_point(data = polcam5_clean_filtered, aes(x = datetime, y = total_flowering_count), color = "darkolivegreen", alpha = 0.7) +
  geom_smooth(data = polcam5_clean_filtered, aes(x = datetime, y = total_flowering_count), color = "darkolivegreen", method = loess) +
  # Customizing the plot
  labs(
    title = "Flight Buzzes, Temperature, and Flowering Over Time",
    x = "Date", 
    y = "Total Flight Buzz Duration (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature | Green: Total Flowering Count"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),  # Set the limit for total duration above threshold
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())  # Secondary axis for temperature
  ) +
  
  # Crop the x-axis to start from June 20
  scale_x_datetime(limits = c(as.POSIXct("2024-06-20"), max(summary_pred_duration_ARUQ5_2024$datetime))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme(legend.position = "bottom")  # Optional: move legend



# Merge polcam5_clean with merged_data_aruq5 by datetime
merged_data_polcam5 <- polcam5_clean %>%
  dplyr::left_join(merged_data_aruq5, by = "date")

# Check the merged data
head(merged_data_polcam5)

hist(log(merged_data_polcam5$total_flowering_count))
hist(log(merged_data_polcam5$total_duration_day))

# Fit the model using log-transformed total_duration_day
model_polcam <- lm(log(total_duration_day) ~ total_flowering_count, data = merged_data_polcam5)
summary(model_polcam)

# Extract coefficients and p-values
model_polcam_summary <- summary(model_polcam)
intercept_polcam <- coef(model_polcam_summary)[1, "Estimate"]
slope_polcam <- coef(model_polcam_summary)[2, "Estimate"]
p_intercept_polcam <- coef(model_polcam_summary)[1, "Pr(>|t|)"]
p_slope_polcam <- coef(model_polcam_summary)[2, "Pr(>|t|)"]

# Calculate fitted values and confidence intervals from the model
predictions_log_polcam <- predict(model_polcam, newdata = merged_data_polcam5, se.fit = TRUE)

# Exponentiate to return to the original scale
predictions_polcam <- exp(predictions_log_polcam$fit)
lower_bound_polcam <- exp(predictions_log_polcam$fit - 1.96 * predictions_log_polcam$se.fit)  # Lower bound (95% CI)
upper_bound_polcam <- exp(predictions_log_polcam$fit + 1.96 * predictions_log_polcam$se.fit)  # Upper bound (95% CI)


ggplot(merged_data_polcam5, aes(x = total_flowering_count, y = total_duration_day)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_line(aes(x = total_flowering_count, y = predictions_polcam), color = "skyblue4", size = 1) +
  geom_ribbon(aes(x = total_flowering_count, ymin = lower_bound_polcam, ymax = upper_bound_polcam), alpha = 0.1) +
  # Add model coefficients and p-values as text annotations
  annotate("text", x = 1, y = 1600, label = paste("Intercept =", round(intercept_polcam, 2), "\np-value =", round(p_intercept_polcam, 4)),
           color = "black", size = 5) +  # Intercept annotation with p-value
  annotate("text", x = 1, y = 1400, label = paste("Slope =", round(slope_polcam, 2), "\np-value =", round(p_slope_polcam, 4)),
           color = "black", size = 5) +  # Slope annotation with p-value
  #geom_smooth(method = "lm", color = "skyblue4", se = TRUE) +  # Add a linear regression line (optional)
  labs(
    title = "Total Flight Buzz Duration vs Flowering Count",
    x = "Daily Flowering Count",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  #scale_y_continuous(limits = c(0, 750)) +
  theme_classic()  




ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = merged_data_polcam5, aes(x = datetime, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Plot temperature curve
  geom_smooth(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange3", method = loess, alpha = 0.7, se = FALSE) +
  geom_smooth(data = merged_data_polcam5, aes(x = datetime, y = total_duration_day/24), color = "skyblue4", method = loess, se = FALSE) +
  geom_line(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Plot the new variables
  geom_point(data = polcam5_clean_filtered, aes(x = datetime, y = total_flowering_count), color = "darkolivegreen", alpha = 0.7) +
  geom_smooth(data = polcam5_clean_filtered, aes(x = datetime, y = total_flowering_count), color = "darkolivegreen", method = loess, se = FALSE) +
  # Customizing the plot
  labs(
    title = "Flight Buzzes, Temperature, and Flowering at ARUQ5",
    x = "Date", 
    y = "Mean Flight Buzz Duration Per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes Per Hour | Orange: Temperature | Green: Total Flowering Availability"
  ) +
  
  
  # Set the primary y-axis for flight buzz duration and secondary axis for temperature
  scale_y_continuous(
    name = "Mean Flight Buzz Duration Per Hour (s)",
    limits = c(0, 30),
    sec.axis = sec_axis(
      trans = ~ .,  # Scale the temperature by a factor to make it more comparable
      name = "Temperature (°C)",
      labels = scales::label_number()
    )
  ) +
  
  # Crop the x-axis to start from June 20
  scale_x_datetime(limits = c(as.POSIXct("2024-06-20"), max(summary_pred_duration_ARUQ5_2024$datetime))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme(legend.position = "bottom")  # Optional: move legend













### PREDICTIONS ARUQ0 ### -----
ARUQ0_2024_pred <- read_csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/raw/predictions_ARUQ0_raw.csv")

# Adjust the regular expression to capture the date (yyyymmdd) and time (hhmmss)
ARUQ0_2024_pred <- ARUQ0_2024_pred %>%
  mutate(datetime_str = gsub(".*_(\\d{8})_(\\d{6})\\.wav$", "\\1 \\2", file))

# Convert to POSIXct (date and time format)
ARUQ0_2024_pred <- ARUQ0_2024_pred %>%
  mutate(datetime = as.POSIXct(datetime_str, format="%Y%m%d %H%M%S"))

# Check the result
head(ARUQ0_2024_pred)


# Define the threshold
threshold <- 8  # Adjust this threshold to your desired value

# Filter the dataset for BUZZ above the threshold
ARUQ0_2024_pred_above_threshold <- ARUQ0_2024_pred %>%
  filter(BUZZ > threshold)

# Calculate the duration above the threshold for each row
# Since each time segment is 0.3 seconds, we can multiply the count of segments by 0.3 to get the duration in seconds
ARUQ0_2024_pred_above_threshold <- ARUQ0_2024_pred_above_threshold %>%
  mutate(duration_above_threshold = 0.15)  # each segment is 0.3 seconds

# Summarize the total duration above the threshold for each datetime period
summary_pred_duration_ARUQ0_2024 <- ARUQ0_2024_pred_above_threshold %>%
  group_by(datetime) %>%
  summarize(total_duration_above_threshold = sum(duration_above_threshold))

# View the summarized durations
head(summary_pred_duration_ARUQ0_2024)


# Plot the total duration above the threshold over time
ggplot(summary_pred_duration_ARUQ0_2024, aes(x = datetime, y = total_duration_above_threshold)) +
  geom_point() +  # Line plot to show the duration over time
  labs(
    title = "Flight buzzes across the growing season - ARUQ0 (Threshold = 8)",
    x = "Datetime", 
    y = "Total predicted flight buzz duration (seconds)"
  ) +
  #geom_smooth(method = loess, color = "skyblue4", se = FALSE) +
  ylim(0,100) +
  theme_classic() +  # Optional: for a clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotates x-axis labels for better readability


# Ensure datetime is in POSIXct format and adjust to UTC-7 timezone
summary_pred_duration_ARUQ0_2024_tz <- summary_pred_duration_ARUQ0_2024 %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
         datetime_utc7 = with_tz(datetime, tzone = "Etc/GMT+7"),  # Use a valid time zone
         time_of_day = format(datetime_utc7, "%H:%M:%S"))  # Extract time of day after conversion

# Convert time_of_day to proper time format for plotting
summary_pred_duration_ARUQ0_2024_tz <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(time_of_day = hms::as_hms(time_of_day))  # Convert to hms for plotting

# Plot
ggplot(summary_pred_duration_ARUQ0_2024_tz, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5, color = "blue") +  # Points for visibility
  #geom_line(color = "skyblue4") +  # Line to show trends
  labs(
    title = "Flight buzzes across a 24-hour period - ARUQ0 (Threshold = 8)",
    x = "Time of Day", 
    y = "Total predicted flight buzz duration (seconds)"
  ) +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +  # Ensure readable time breaks
  geom_smooth(method = loess, color = "skyblue4", se = TRUE) +
  theme_classic() +
  #ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


summary_pred_duration_ARUQ0_2024_month <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(week = floor_date(datetime, "week"))  # Groups by month




#### IN PROGRESS ----
#install.packages("suncalc")
library(suncalc)

# Example: Your study site coordinates
lat <- 69.57  # Replace with your actual latitude
lon <- -138.91  # Replace with your actual longitude

# Convert POSIXct to Date
summary_pred_duration_ARUQ0_2024_sun <- summary_pred_duration_ARUQ0_2024_month %>%
  mutate(date = as.Date(datetime_utc7))  # Extract only the date

# Generate sunrise and sunset times for each date
sun_times <- getSunlightTimes(
  data = data.frame(date = unique(summary_pred_duration_ARUQ0_2024_sun$date),
                    lat = lat, lon = lon),
  keep = c("sunrise", "sunset")
)






# Plot
ggplot(summary_pred_duration_ARUQ0_2024_month, aes(x = time_of_day, y = total_duration_above_threshold)) +
  geom_point(alpha = 0.5) +  # Points for visibility
  #geom_line(color = "skyblue4") +  # Line to show trends
  labs(
    title = "Flight buzzes across a 24-hour period - ARUQ0 (Threshold = 8)",
    x = "Time of Day", 
    y = "Total predicted flight buzz duration (seconds)"
  ) +
  scale_x_time(breaks = seq(0, 86400, by = 3600), labels = function(x) format(x, "%H:%M")) +  # Ensure readable time breaks
  #geom_smooth(method = loess, se = FALSE) +
  theme_classic() +
  ylim(0,50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
  facet_wrap(~week)





aru_temp_daily_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv")

aruq0 <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ0")

aru_temp_hourly_micro <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv")

aruq0_h <- aru_temp_hourly_micro %>%
  filter(aru_name == "ARUQ0")

# Assuming your dataset includes columns for 'day', 'month', and 'mean_value'

aruq0_data_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ0") %>%
  # Combine 'day' and 'month' to create a full date (assuming day is a day number and month is numeric)
  mutate(datetime = as.Date(paste(2024, month, day, sep = "-"), format = "%Y-%m-%d"))

aruq0_data_plot <- aruq0_data_plot %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))

aruq0_data_plot_h <- aru_temp_hourly_micro %>%
  filter(aru_name == "ARUQ0") %>%
  # Combine 'month' and 'day' to create a full date, assuming year is 2024
  mutate(datetime = as.Date(paste(2024, month, day, sep = "-"), format = "%Y-%m-%d")) %>%
  # Combine 'day' and 'month' to create a full date (assuming day is a day number and month is numeric)
  mutate(datetime = as.POSIXct(paste(datetime, hour), format="%Y-%m-%d %H", tz="UTC"))

aruq0_data_plot_h <- aruq0_data_plot_h %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))


# Aggregate total_duration_above_threshold by day
summary_pred_duration_agg_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(date = as.Date(datetime)) %>%  # Convert datetime to Date (to group by day)
  group_by(date) %>%
  summarise(total_duration_day = sum(total_duration_above_threshold, na.rm = TRUE))  # Sum the duration for each day

# Merge with aru_temp_daily_micro by datetime (ensure the datetime is also in Date format)
merged_data_aruq0 <- summary_pred_duration_agg_ARUQ0 %>%
  left_join(aruq0_data_plot, by = c("date" = "datetime"))

# Check the merged data
head(merged_data_aruq0)




# Aggregate total_duration_above_threshold by hour
summary_pred_duration_agg_hourly_ARUQ0 <- summary_pred_duration_ARUQ0_2024_tz %>%
  mutate(datetime = as.POSIXct(datetime)) %>%  # Ensure datetime is in POSIXct format
  mutate(date_time_hour = format(datetime, "%Y-%m-%d %H:00:00")) %>%  # Create a new datetime column with hour precision
  group_by(date_time_hour) %>%
  summarise(total_duration_hour = sum(total_duration_above_threshold, na.rm = TRUE))  # Sum the duration for each hour

# Merge with aru_temp_hourly_micro by datetime (ensure the datetime in aru_temp_hourly_micro also matches hour precision)
merged_data_aruq0_hourly <- summary_pred_duration_agg_hourly_ARUQ0 %>%
  mutate(date_time_hour = as.POSIXct(date_time_hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  left_join(aruq0_data_plot_h, by = c("date_time_hour" = "datetime"))

# Check the merged data
head(merged_data_aruq0_hourly)

hist(merged_data_aruq0$total_duration_day)
hist(merged_data_aruq0$mean_value)


merged_data_aruq0 <- merged_data_aruq0 %>%
  filter(!month == "NA")

# Create the plot
ggplot(merged_data_aruq0, aes(x = mean_value, y = total_duration_day)) +
  geom_point(aes(color = as.factor(month))) +  # Scatter plot with blue points
  geom_smooth(method = "lm", aes(color = as.factor(month)), se = FALSE) +  # Add a linear regression line (optional)
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature - ARUQ0",
    x = "Daily Mean Temperature (°C)",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  #scale_x_continuous(limits = c(0, 25)) +
  #scale_y_continuous(limits = c(0, 750)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotate x-axis labels for readability


# Create the plot with direct transformation in aes()
ggplot(merged_data_aruq0, aes(x = mean_value, y = log(total_duration_day))) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", formula = y ~ x, color = "skyblue4", se = TRUE) +  # Linear regression line (apply log transformation inside aes())
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature",
    x = "Daily Mean Temperature (°C)",
    y = "Log of Total Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 5, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 5, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 7)) +  # Adjusted to fit log-transformed values
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotate x-axis labels for readability



# Fit the model using log-transformed total_duration_day
model_ARUQ0 <- lm(log(total_duration_day) ~ log(mean_value), data = merged_data_aruq0)
summary(model_ARUQ0)



# Calculate fitted values and confidence intervals from the model
predictions_log_ARUQ0 <- predict(model_ARUQ0, newdata = merged_data_aruq0, se.fit = TRUE)

# Exponentiate to return to the original scale
predictions_ARUQ0 <- exp(predictions_log_ARUQ0$fit)
lower_bound_ARUQ0 <- exp(predictions_log_ARUQ0$fit - 1.96 * predictions_log_ARUQ0$se.fit)  # Lower bound (95% CI)
upper_bound_ARUQ0 <- exp(predictions_log_ARUQ0$fit + 1.96 * predictions_log_ARUQ0$se.fit)  # Upper bound (95% CI)

aruq0_summer_daily <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ0") %>%
  filter(month %in% c("6","7","8"))

hist(aruq0_summer_daily$mean_value, xlim = c(0, 25), breaks = 11)
?hist()

# Create the plot with original scale data and predictions
ggplot(merged_data_aruq0, aes(x = mean_value, y = total_duration_day)) +
  geom_histogram(data = aruq0_summer_daily, aes(x = mean_value, y = ..density..*6000), 
                 fill = "lightgray", color = "lightgray", alpha = 0.1, bins = 11) +  # Temperature histogram
  geom_point(color = "skyblue") +  # Scatter plot with original data points
  geom_line(aes(x = mean_value, y = predictions_ARUQ0), color = "skyblue4", size = 1) +  # Regression line
  geom_ribbon(aes(x = mean_value, ymin = lower_bound_ARUQ0, ymax = upper_bound_ARUQ0), alpha = 0.1) +  # Confidence intervals
  
  labs(
    title = "Flight Buzz Duration vs Daily Mean Temperature - ARUQ0",
    x = "Daily Mean Temperature (°C)",
    y = "Daily Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  #scale_x_continuous(limits = c(0, 29)) +
  scale_y_continuous(
    limits = c(0, 750),  # Adjust y-axis for the total_duration_day data
    sec.axis = sec_axis(trans = ~ . * 0.02, name = "Temperature Distribution")  # Secondary y-axis for histogram
  ) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Create the plot with original scale data and predictions
ggplot(merged_data_aruq0, aes(x = mean_value, y = total_duration_day)) +
  geom_point(color = "skyblue") +  # Scatter plot with original data points
  geom_line(aes(x = mean_value, y = predictions_ARUQ0), color = "skyblue4", size = 1) +  # Regression line
  geom_ribbon(aes(x = mean_value, ymin = lower_bound_ARUQ0, ymax = upper_bound_ARUQ0), alpha = 0.1) +  # Confidence intervals
  
  labs(
    title = "Flight Buzz Duration vs Daily Mean Temperature - ARUQ0",
    x = "Daily Mean Temperature (°C)",
    y = "Daily Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(
    limits = c(0, 750)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability



# Create the plot
ggplot(merged_data_aruq0_hourly, aes(x = mean_value, y = total_duration_hour)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", color = "skyblue4", se = TRUE) +  # Add a linear regression line (optional)
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature - ARUQ0",
    x = "Hourly Mean Temperature (°C)",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  #scale_x_continuous(limits = c(-5, 30)) +
  scale_y_continuous(limits = c(0, 60)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotate x-axis labels for readability



merged_data_aruq0_hourly <- merged_data_aruq0_hourly %>%
  filter(!month == "NA")

merged_data_aruq0_hourly_august <- merged_data_aruq0_hourly %>%
  filter(month == "8")

# Create the plot
ggplot(merged_data_aruq0_hourly_august, aes(x = mean_value, y = total_duration_hour)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", color = "skyblue4", se = TRUE) +  # Add a linear regression line (optional)
  labs(
    title = "Flight Buzz Duration vs Mean Temperature - ARUQ0",
    x = "Hourly Mean Temperature (°C)",
    y = "Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  #scale_x_continuous(limits = c(-5, 30)) +
  scale_y_continuous(limits = c(0, 60)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Optional: rotate x-axis labels for readability 
  facet_wrap(~day)



merged_data_aruq0_hourly_test <- merged_data_aruq0_hourly %>%
  mutate(week_start = floor_date(date_time_hour, "week"))

# Create the plot
ggplot(merged_data_aruq0_hourly_test, aes(x = mean_value, y = total_duration_hour)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", color = "skyblue4", se = TRUE) +  # Add a linear regression line (optional)
  labs(
    title = "Flight Buzz Duration vs Mean Temperature - ARUQ0",
    x = "Hourly Mean Temperature (°C)",
    y = "Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 500, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 500, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  #scale_x_continuous(limits = c(-5, 30)) +
  scale_y_continuous(limits = c(0, 60)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Optional: rotate x-axis labels for readability 
  facet_wrap(~week_start)





# Calculate temperature density (i.e., how often each temperature occurs)
merged_data_aruq0_hourly_clean <- merged_data_aruq0_hourly %>%
  filter(!is.na(mean_value))

merged_data_aruq0_hourly_clean <- merged_data_aruq0_hourly_clean %>%
  mutate(temp_density = dnorm(mean_value, mean = mean(mean_value), sd = sd(mean_value)))

hist(log(merged_data_aruq0_hourly_clean$total_duration_hour))
hist(merged_data_aruq0_hourly_clean$mean_value)


# Remove rows with missing or infinite values in either predictor or response variables
clean_data_ARUQ0 <- merged_data_aruq0_hourly_clean %>%
  filter(!is.na(mean_value) & !is.na(total_duration_hour) & !is.na(temp_density) &
           !is.infinite(mean_value) & !is.infinite(total_duration_hour) & !is.infinite(temp_density))

# Check for consistency in the number of rows
nrow(clean_data_ARUQ0)

# Create the plot with density as the weight for each point
ggplot(merged_data_aruq0_hourly_clean, aes(x = mean_value, y = total_duration_hour, size = temp_density), alpha = 0.5) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  #geom_smooth(method = "lm", formula = log(merged_data_aruq5_hourly_clean$total_duration_hour) ~ merged_data_aruq5_hourly_clean$mean_value, color = "skyblue4", se = FALSE) +  # Add a linear regression line (optional)
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature - ARUQ0",
    x = "Hourly Mean Temperature (°C)",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  # Add vertical lines at temperature thresholds (6°C and 12.6°C)
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  # Add text annotations for the vertical lines
  annotate("text", x = 6, y = 40, label = "B. frigidus (Q)", 
           color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 40, label = "B. frigidus (W)", 
           color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  #scale_x_continuous(limits = c(-5, 30)) +
  scale_y_continuous(limits = c(0, 60)) +
  scale_size_continuous(range = c(0.5, 3)) +
  theme_classic() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: rotate x-axis labels for readability




ggplot(merged_data_aruq0_hourly_clean, aes(x = mean_value, y = total_duration_hour, size = temp_density)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_smooth(method = "lm", formula = y ~ x, color = "skyblue4", se = FALSE, weights = merged_data_aruq0_hourly_clean$temp_density) +  # Weighted regression line
  labs(
    title = "Total Flight Buzz Duration vs Mean Temperature",
    x = "Hourly Mean Temperature (°C)",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  geom_vline(xintercept = 6, color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 6°C
  geom_vline(xintercept = 12.6, color = "orange4", linetype = "dashed", size = 1, alpha = 0.7) +  # Line at 12.6°C
  annotate("text", x = 6, y = 40, label = "B. frigidus (Q)", color = "orange", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 6°C
  annotate("text", x = 12.6, y = 40, label = "B. frigidus (W)", color = "orange4", angle = 90, hjust = 0.5, vjust = -1, fontface = "italic") +  # Label for 12.6°C
  scale_size_continuous(range = c(1, 5)) +  # Scale the size of the points
  scale_x_continuous(limits = c(-5, 30)) +
  scale_y_continuous(limits = c(0, 60)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




aruq0_plot <- aruq0_data_plot %>%
  ggplot(aes(x = date, y = mean_value)) +
  # Plot the points for temperature with color based on microclimate
  geom_point(aes(), size = 0.7, alpha = 0.7) +
  geom_line() +
  # Add a smoothed line for each microclimate
  geom_smooth(aes(), se = TRUE) +
  theme_classic()


aruq0_plot <- aru_temp_daily_micro %>%
  filter(aru_name == "ARUQ0") %>%
  ggplot(aes(x = day, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))


# Create the plot with dual axes
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_ARUQ0_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue", alpha = 0.7) +
  #geom_line(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "blue") +
  # Plot temperature curve
  geom_smooth(data = aruq0_data_plot, aes(x = datetime, y = mean_value), color = "orange3", method = loess, alpha = 0.7) +
  geom_smooth(data = summary_pred_duration_ARUQ0_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue4", method = loess) +
  geom_line(data = aruq0_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq0_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  # Customizing the plot
  labs(
    title = "Flight Buzzes and Temperature Over Time - ARUQ0",
    x = "Datetime", 
    y = "Total Duration Above Threshold (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature"
  ) +
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 100),  # Set the limit for total duration above threshold
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())  # Secondary axis for temperature
  ) +
  # Crop the x-axis to start from June 20
  scale_x_datetime(limits = c(as.POSIXct("2024-06-20"), max(summary_pred_duration_ARUQ0_2024$datetime))) +
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme(legend.position = "bottom")  # Optional: move legend



# Create the plot with dual axes
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_ARUQ0_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue", alpha = 0.7) +
  #geom_line(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "blue") +
  # Plot temperature curve
  geom_smooth(data = aruq0_data_plot, aes(x = datetime, y = mean_value), color = "orange3", method = loess, alpha = 0.7, se = FALSE) +
  geom_smooth(data = summary_pred_duration_ARUQ0_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue4", method = loess, se = FALSE) +
  geom_line(data = aruq0_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq0_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  # Customizing the plot
  labs(
    title = "Flight Buzzes and Temperature Over Time - ARUQ0",
    x = "Datetime", 
    y = "Total Duration Above Threshold (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature"
  ) +
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),  # Set the limit for total duration above threshold
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())  # Secondary axis for temperature
  ) +
  # Crop the x-axis to start from June 20
  scale_x_datetime(limits = c(as.POSIXct("2024-06-20"), max(summary_pred_duration_ARUQ0_2024$datetime))) +
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme(legend.position = "bottom")  # Optional: move legend




#### HERE #### ARUQ0 HAS NO POLCAM
polcam5 <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/polcam5.csv")

# Assign the first row as column names
colnames(polcam5) <- as.character(polcam5[1,])
polcam5 <- polcam5[-1,]  # Remove the first row after assigning it as column names
# Make column names unique by appending a suffix
colnames(polcam5) <- make.unique(colnames(polcam5))

# Remove columns where any NA values are present
polcam5_clean <- polcam5 %>%
  dplyr::select(3,5,6,10,11,31)

# View the cleaned dataset
head(polcam5_clean)

# Convert 'Date (year-month-day)' to Date format and 'Time' to POSIXct
polcam5_clean$datetime <- as.POSIXct(paste(polcam5_clean$`Date (year-month-day)`), 
                                     format = "%Y-%m-%d", tz = "UTC")

# Drop the original date and time columns (if no longer needed)
polcam5_clean <- polcam5_clean %>%
  dplyr::select(-`Date (year-month-day)`, -Time, -Plot)

# Convert columns to numeric if necessary
polcam5_clean$`Salix arc. open female catkin` <- as.numeric(polcam5_clean$`Salix arc. open female catkin`)
polcam5_clean$`Salix arc. open male catkin` <- as.numeric(polcam5_clean$`Salix arc. open male catkin`)
polcam5_clean$`Lupine open flower` <- as.numeric(polcam5_clean$`Lupine open flower`)

# Create a new variable for the total flowering count
polcam5_clean$total_flowering_count <- 
  polcam5_clean$`Salix arc. open female catkin` +
  polcam5_clean$`Salix arc. open male catkin` +
  polcam5_clean$`Lupine open flower`

# Filter out rows where total_flowering_count is 0
polcam5_clean_filtered <- polcam5_clean %>%
  dplyr::filter(total_flowering_count != 0)

# Convert 'Date (year-month-day)' to Date format and 'Time' to POSIXct
polcam5_clean <- polcam5_clean %>%
  mutate(date = datetime)


# Create the plot with dual axes and the new variables
ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue", alpha = 0.7) +
  
  # Plot temperature curve
  geom_smooth(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange3", method = loess, alpha = 0.7) +
  geom_smooth(data = summary_pred_duration_ARUQ5_2024, aes(x = datetime, y = total_duration_above_threshold), color = "skyblue4", method = loess) +
  geom_line(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Plot the new variables
  geom_point(data = polcam5_clean_filtered, aes(x = datetime, y = total_flowering_count), color = "darkolivegreen", alpha = 0.7) +
  geom_smooth(data = polcam5_clean_filtered, aes(x = datetime, y = total_flowering_count), color = "darkolivegreen", method = loess) +
  # Customizing the plot
  labs(
    title = "Flight Buzzes, Temperature, and Flowering Over Time",
    x = "Date", 
    y = "Total Flight Buzz Duration (seconds)",
    subtitle = "Blue: Flight Buzzes | Orange: Temperature | Green: Total Flowering Count"
  ) +
  
  # Secondary y-axis for temperature
  scale_y_continuous(
    name = "Total Duration Above Threshold (seconds)",
    limits = c(0, 30),  # Set the limit for total duration above threshold
    sec.axis = sec_axis(~ ., name = "Temperature (°C)", labels = scales::label_number())  # Secondary axis for temperature
  ) +
  
  # Crop the x-axis to start from June 20
  scale_x_datetime(limits = c(as.POSIXct("2024-06-20"), max(summary_pred_duration_ARUQ5_2024$datetime))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme(legend.position = "bottom")  # Optional: move legend



# Merge polcam5_clean with merged_data_aruq5 by datetime
merged_data_polcam5 <- polcam5_clean %>%
  dplyr::left_join(merged_data_aruq5, by = "date")

# Check the merged data
head(merged_data_polcam5)

hist(log(merged_data_polcam5$total_flowering_count))
hist(log(merged_data_polcam5$total_duration_day))

# Fit the model using log-transformed total_duration_day
model_polcam <- lm(log(total_duration_day) ~ total_flowering_count, data = merged_data_polcam5)
summary(model_polcam)

# Extract coefficients and p-values
model_polcam_summary <- summary(model_polcam)
intercept_polcam <- coef(model_polcam_summary)[1, "Estimate"]
slope_polcam <- coef(model_polcam_summary)[2, "Estimate"]
p_intercept_polcam <- coef(model_polcam_summary)[1, "Pr(>|t|)"]
p_slope_polcam <- coef(model_polcam_summary)[2, "Pr(>|t|)"]

# Calculate fitted values and confidence intervals from the model
predictions_log_polcam <- predict(model_polcam, newdata = merged_data_polcam5, se.fit = TRUE)

# Exponentiate to return to the original scale
predictions_polcam <- exp(predictions_log_polcam$fit)
lower_bound_polcam <- exp(predictions_log_polcam$fit - 1.96 * predictions_log_polcam$se.fit)  # Lower bound (95% CI)
upper_bound_polcam <- exp(predictions_log_polcam$fit + 1.96 * predictions_log_polcam$se.fit)  # Upper bound (95% CI)


ggplot(merged_data_polcam5, aes(x = total_flowering_count, y = total_duration_day)) +
  geom_point(color = "skyblue") +  # Scatter plot with blue points
  geom_line(aes(x = total_flowering_count, y = predictions_polcam), color = "skyblue4", size = 1) +
  geom_ribbon(aes(x = total_flowering_count, ymin = lower_bound_polcam, ymax = upper_bound_polcam), alpha = 0.1) +
  # Add model coefficients and p-values as text annotations
  annotate("text", x = 1, y = 1600, label = paste("Intercept =", round(intercept_polcam, 2), "\np-value =", round(p_intercept_polcam, 4)),
           color = "black", size = 5) +  # Intercept annotation with p-value
  annotate("text", x = 1, y = 1400, label = paste("Slope =", round(slope_polcam, 2), "\np-value =", round(p_slope_polcam, 4)),
           color = "black", size = 5) +  # Slope annotation with p-value
  #geom_smooth(method = "lm", color = "skyblue4", se = TRUE) +  # Add a linear regression line (optional)
  labs(
    title = "Total Flight Buzz Duration vs Flowering Count",
    x = "Daily Flowering Count",
    y = "Total Flight Buzz Duration (seconds)"
  ) +
  #scale_y_continuous(limits = c(0, 750)) +
  theme_classic()  




ggplot() +
  # Plot the total duration above the threshold over time
  geom_point(data = merged_data_polcam5, aes(x = datetime, y = total_duration_day/24), color = "skyblue", alpha = 0.7) +
  
  # Plot temperature curve
  geom_smooth(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange3", method = loess, alpha = 0.7, se = FALSE) +
  geom_smooth(data = merged_data_polcam5, aes(x = datetime, y = total_duration_day/24), color = "skyblue4", method = loess, se = FALSE) +
  geom_line(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  geom_point(data = aruq5_data_plot, aes(x = datetime, y = mean_value), color = "orange", alpha = 0.7) +
  
  # Plot the new variables
  geom_point(data = polcam5_clean_filtered, aes(x = datetime, y = total_flowering_count), color = "darkolivegreen", alpha = 0.7) +
  geom_smooth(data = polcam5_clean_filtered, aes(x = datetime, y = total_flowering_count), color = "darkolivegreen", method = loess, se = FALSE) +
  # Customizing the plot
  labs(
    title = "Flight Buzzes, Temperature, and Flowering at ARUQ5",
    x = "Date", 
    y = "Mean Flight Buzz Duration Per Hour (seconds)",
    subtitle = "Blue: Flight Buzzes Per Hour | Orange: Temperature | Green: Total Flowering Availability"
  ) +
  
  
  # Set the primary y-axis for flight buzz duration and secondary axis for temperature
  scale_y_continuous(
    name = "Mean Flight Buzz Duration Per Hour (s)",
    limits = c(0, 30),
    sec.axis = sec_axis(
      trans = ~ .,  # Scale the temperature by a factor to make it more comparable
      name = "Temperature (°C)",
      labels = scales::label_number()
    )
  ) +
  
  # Crop the x-axis to start from June 20
  scale_x_datetime(limits = c(as.POSIXct("2024-06-20"), max(summary_pred_duration_ARUQ5_2024$datetime))) +
  
  # Clean theme and formatting
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme(legend.position = "bottom")  # Optional: move legend

