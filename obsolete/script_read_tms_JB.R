#### reading TOMST data with MyClim ####

#### packages ####
library(myClim) ## logger data reading
library(foreach) ## efficient loop
library(data.table) ## efficient data.frame  
library(stringr) ## efficient character manipulation
library(lubridate) ## manipulate date_time
library(tidyr)
library(dplyr)
library(ggplot2)
## fixing file that lacks a columns
## replace "QHI_TOMST_2024" by the path or directory that contains the raw csv
## already ran for 2024, no need to run again
##this step is needed only if the MC package doesn't recognize the sensors
# foreach(file_to_fix = list.files("QHI_TOMST_2024",full.names = T),
#         file_name = list.files("QHI_TOMST_2024",full.names = F))%do%{
#   tmp <- fread(file_to_fix,header = F)
#   tmp[,tmz:=4]
#   tmp[,V7:=202]
#   write.table(tmp[,c(1,2,9,3,4,5,6,7,8)],
#               file.path("QHI_TOMST_2024_fixed",file_name),
#               row.names = F,
#               col.names = F,
#               sep = ";",
#               quote = F)
# }
tomst_file_path <- "/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/QHI_TOMST_2024_fixed"

#### Creating a meta_data file ####
## list.file(, full.names = T) get thew hole path 
list_path <- list.files(tomst_file_path,full.names = T)
list_files <- list.files(tomst_file_path,full.names = F)
locality_name <-  str_extract(list_files, "TOMST[:digit:]+_QHI") ## other way of naming it, if we have a metada file for example

files_table <- data.table(path = list_path,
                          locality_id = locality_name ,  
                          data_format = "TOMST")

locality_metadata <-  data.table(locality_id = locality_name ,  
                                 tz_offset  = - 60 * 7)
######################################## Can add gps coordinates as metadata

## Read pre-defined loggers without metadata
# read from Tomst files

tms.f <- mc_read_data(files_table,locality_metadata)



mc_info_count(tms.f) #which returns the number of localities, loggers and sensors in myClim object
mc_info(tms.f)# returning data frame with summary per sensor
mc_info_meta(tms.f)# returning the data frame with locality metadata
mc_info_clean(tms.f) #returning the data frame with cleaning log

#mc_plot_raster(tms.f)  # TMOST14_QHI and TOMST8_QHI seem unreliable
tms.f <- mc_filter(tms.f,localities = c("TOMST14_QHI","TOMST8_QHI"),reverse = T )

mc_plot_line(mc_filter(tms.f, localities = "TOMST9_QHI")) 

tms.f <- mc_prep_crop(tms.f,start = as.POSIXct("2022-07-31", tz="UTC")) # croping the TS to when they were installed (I'm guessing a date with time series here)

## calculate virtual sensor VWC from raw TMS moisture signal

## let's check the most appropriate  soiltype, default to universal now
tms.calc <- mc_calc_vwc(tms.f, soiltype = "universal")

## virtual sensor with growing and freezing degree days
tms.calc <- mc_calc_gdd(tms.calc, sensor = "TMS_T3",)
tms.calc <- mc_calc_fdd(tms.calc, sensor = "TMS_T3")

## virtual sensor to estimate snow presence from 2 cm air temperature 
tms.calc <- mc_calc_snow(tms.calc, sensor = "TMS_T2")

## aggregates all those sensors to monthly values # choose between minimu percentile
hourly.tms <- mc_agg(tms.calc,fun=c("mean","min","max"),period = "hour",min_coverage=1,use_utc = T)
#monthly.tms <- mc_agg(tms.calc,fun=c("mean","percentile"),percentiles = c(0.05,0.95),period = "day",min_coverage=1,use_utc = F)
hourly.tms[, datetime_local := with_tz(datetime, tz = "America/Edmonton")]  # Adjust to your local timezone

## aggregates all those sensors to monthly values # choose between minimu percentile
monthly.tms <- mc_agg(tms.calc,fun=c("mean","min","max"),period = "day",min_coverage=1,use_utc = F)
monthly.tms <- mc_agg(tms.calc,fun=c("mean","percentile"),percentiles = c(0.05,0.95),period = "day",min_coverage=1,use_utc = F)

## export the object out of the MC framework
export_dt <- data.table(mc_reshape_long(monthly.tms))
export_dt[, serial_number:=NULL] ##removing useless col
export_dt[, datetime := ymd(datetime)] ## :=  creates or update a column in data.table, here we swith to a lubridate format with ymd
export_dt[, month := month(datetime)] ## extracting the month
export_dt[, day := day(datetime)] ## extracting the day
export_dt[, week := week(datetime)] ## extracting the week
export_dt[, year := year(datetime)] ## extracting the year


#### WORKING ON THIS
export_dt_hourly <- data.table(mc_reshape_long(hourly.tms))
export_dt_hourly[, serial_number:=NULL] ##removing useless col
export_dt_hourly_T3 <- export_dt_hourly %>%
  filter(sensor_name == "TMS_T3_mean") ##only taking TMS_T3_mean


# filter for only 2024
export_dt <- export_dt[year == 2024,]

monthly_values <- export_dt[,.(mean_value = mean(value,na.rm=T)),
                              by=.(month, sensor_name, locality_id)] # na.rm=T remove incomplete months 

daily_values <- export_dt[,.(mean_value = mean(value,na.rm=T)),
                          by=.(month,day,sensor_name,height,week, locality_id)] # na.rm=T remove incomplete days 

weekly_values <- export_dt[,.(mean_value = mean(value,na.rm=T)),
                           by=.(month,sensor_name,height,week, locality_id)] # na.rm=T remove incomplete days 


monthly_airtemp <- monthly_values %>%
  filter(sensor_name == "TMS_T3_mean") %>%
  filter(month %in% c(5:8))

daily_airtemp <- daily_values %>%
  filter(sensor_name == "TMS_T3_mean") %>%
  filter(month %in% c(5:8))


aru_tomst_mapping <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_tomst_mapping.csv")

aru_tomst_list <- aru_tomst_mapping %>%
  mutate(locality_id = paste0("TOMST", tomst_num, "_QHI")) %>%
  filter(aru_name %in% c("ARUQ0", "ARUQ1", "ARUQ2", "ARUQ3", "ARUQ4", "ARUQ5", "ARUQ6", "ARUQ7", "ARUQ8", "ARUQ9", "ARUQ10"))

export_dt_hourly_T3 <- export_dt_hourly_T3 %>%
  filter(locality_id %in% aru_tomst_list$locality_id)

# Ensure there are extra column slots
setalloccol(export_dt_hourly_T3, n = ncol(export_dt_hourly_T3) + 1)

# Convert to "EAmerica/Whitehorse"
set(export_dt_hourly_T3, j = "datetime_local", value = with_tz(export_dt_hourly_T3$datetime, "America/Whitehorse"))

set(export_dt_hourly_T3, j = "month", value = month(export_dt_hourly_T3$datetime_local))
set(export_dt_hourly_T3, j = "day", value = day(export_dt_hourly_T3$datetime_local))
set(export_dt_hourly_T3, j = "week", value = week(export_dt_hourly_T3$datetime_local))
set(export_dt_hourly_T3, j = "year", value = year(export_dt_hourly_T3$datetime_local))
set(export_dt_hourly_T3, j = "hour", value = hour(export_dt_hourly_T3$datetime_local))
export_dt_hourly_T3 <- export_dt_hourly_T3[year == 2024,]



hourly_values <- export_dt_hourly_T3[,.(mean_value = mean(value,na.rm=T)),
                                  by=.(hour, day, month, sensor_name, locality_id)] # na.rm=T remove incomplete months 

hourly_airtemp <- hourly_values %>%
  filter(month %in% c(5:8)) %>%
  filter(sensor_name == "TMS_T3_mean")


aru_tomst_mapping_month_joined <- aru_tomst_mapping %>%
  mutate(locality_id = paste0("TOMST", tomst_num, "_QHI")) %>%
  left_join(monthly_airtemp, by = "locality_id")

aru_tomst_mapping_daily_joined <- aru_tomst_mapping %>%
  mutate(locality_id = paste0("TOMST", tomst_num, "_QHI")) %>%
  left_join(daily_airtemp, by = "locality_id")

aru_tomst_mapping_hourly_joined <- aru_tomst_mapping %>%
  mutate(locality_id = paste0("TOMST", tomst_num, "_QHI")) %>%
  left_join(hourly_airtemp, by = "locality_id")

aru_temp_monthly <- aru_tomst_mapping_month_joined %>%
  filter(aru_name %in% c("ARUQ0", "ARUQ1", "ARUQ2", "ARUQ3", "ARUQ4", "ARUQ5", "ARUQ6", "ARUQ7", "ARUQ8", "ARUQ9", "ARUQ10"))

aru_temp_daily <- aru_tomst_mapping_daily_joined %>%
  filter(aru_name %in% c("ARUQ0", "ARUQ1", "ARUQ2", "ARUQ3", "ARUQ4", "ARUQ5", "ARUQ6", "ARUQ7", "ARUQ8", "ARUQ9", "ARUQ10"))

aru_temp_hourly <- aru_tomst_mapping_hourly_joined %>%
  filter(aru_name %in% c("ARUQ0", "ARUQ1", "ARUQ2", "ARUQ3", "ARUQ4", "ARUQ5", "ARUQ6", "ARUQ7", "ARUQ8", "ARUQ9", "ARUQ10"))


ggplot(aru_temp_monthly, aes(x = aru_name, y = mean_value, colour = month)) +
  geom_point() +
  theme_classic()

summer_temp_average <- ggplot(aru_temp_monthly, aes(x = aru_name, y = mean_value)) +
  geom_boxplot() +
  theme_classic()

ordered_aru_temp_monthly <- arrange(aru_temp_monthly, desc(aru_temp_monthly$mean_value)) %>%
  filter(!month == "6")

ordered_aru_temp_summer <- ordered_aru_temp_monthly %>%
  group_by(aru_name) %>%
  summarize(summer_temp = mean(mean_value)) %>%
  arrange(desc(summer_temp))

microclim_order <- c("ARUQ0", "ARUQ5", "ARUQ9", "ARUQ7", "ARUQ3", "ARUQ8", "ARUQ10", "ARUQ6", "ARUQ1", "ARUQ2", "ARUQ4")  # Specify the order here
ordered_aru_temp_summer$aru_name <- factor(ordered_aru_temp_summer$aru_name, levels = microclim_order)

summer_temp_ordered <- ggplot(ordered_aru_temp_summer, aes(x = aru_name, y = summer_temp)) +
  geom_boxplot() +
  theme_classic()

clean_aru_summer_temp_ordered_avg <- ordered_aru_temp_summer %>%
  filter(!aru_name %in% c("ARUQ0"))


warm_arus <- clean_aru_summer_temp_ordered_avg %>%
  slice(1:3) %>%
  pull(aru_name)

intermediate_arus <- clean_aru_summer_temp_ordered_avg %>%
  slice(4:6) %>%
  pull(aru_name)

cool_arus <- clean_aru_summer_temp_ordered_avg %>%
  slice(7:10) %>%
  pull(aru_name)

clean_aru_summer_temp_ordered_avg <- clean_aru_summer_temp_ordered_avg %>%
  mutate(microclimate = case_when(
    aru_name %in% warm_arus ~ "Warm",
    aru_name %in% intermediate_arus ~ "Intermediate",
    aru_name %in% cool_arus ~ "Cool",
    TRUE ~ NA_character_  # Handle any extra cases
  ))

clean_summer_temp_per_aru <- ggplot(clean_aru_summer_temp_ordered_avg, aes(x = aru_name, y = summer_temp, colour = microclimate)) +
  geom_boxplot() +
  theme_classic() +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold"))

clean_summer_temp_per_microclimate <- ggplot(clean_aru_summer_temp_ordered_avg, aes(x = microclimate, y = summer_temp, fill = microclimate)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("darkblue", "forestgreen", "gold"))


daily_temp_average <- ggplot(aru_temp_daily, aes(x = aru_name, y = mean_value, fill = factor(month))) +
  geom_boxplot() +
  theme_classic() 

daily_temp_average_diff <- ggplot(aru_temp_daily, aes(x = factor(month), y = mean_value, fill = aru_name)) +
  geom_boxplot() +
  theme_classic()





aru_temp_daily$aru_name <- factor(aru_temp_daily$aru_name, levels = microclim_order)

aru_temp_daily_micro <- aru_temp_daily %>%
  mutate(microclimate = case_when(
    aru_name %in% warm_arus ~ "Warm",
    aru_name %in% intermediate_arus ~ "Intermediate",
    aru_name %in% cool_arus ~ "Cool",
    TRUE ~ NA_character_  # Handle any extra cases
  ))

aru_temp_hourly$aru_name <- factor(aru_temp_hourly$aru_name, levels = microclim_order)

aru_temp_hourly_micro <- aru_temp_hourly %>%
  mutate(microclimate = case_when(
    aru_name %in% warm_arus ~ "Warm",
    aru_name %in% intermediate_arus ~ "Intermediate",
    aru_name %in% cool_arus ~ "Cool",
    TRUE ~ NA_character_  # Handle any extra cases
  ))

# Save the dataset as a CSV file
# write.csv(aru_temp_daily_micro, "/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_daily_micro.csv", row.names = FALSE)
# write.csv(aru_temp_hourly_micro, "/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/aru_temp_hourly_micro.csv", row.names = FALSE)

temps_hourly_may <- aru_temp_hourly_micro %>%
  filter(month == "5") %>%
  filter(!aru_name == "ARUQ0") %>%
  ggplot(aes(x = hour, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  #geom_line(aes(colour = microclimate, linetype  = aru_name), alpha = 0.3) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))

temps_hourly_june <- aru_temp_hourly_micro %>%
  filter(month == "6") %>%
  filter(!aru_name == "ARUQ0") %>%
  ggplot(aes(x = hour, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  #geom_line(aes(colour = microclimate, linetype  = aru_name), alpha = 0.3) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))

temps_hourly_summer <- aru_temp_hourly_micro %>%
  filter(month %in% c("6", "7", "8")) %>%
  filter(!aru_name == "ARUQ0") %>%
  ggplot(aes(x = hour, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  #geom_line(aes(colour = microclimate, linetype  = aru_name), alpha = 0.3) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))

may_temps_hourly <- aru_temp_hourly_micro %>%
  filter(month == "5") %>%
  filter(!aru_name == "ARUQ0") %>%
  ggplot(aes(x = day, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  geom_line(aes(colour = microclimate, linetype  = aru_name), alpha = 0.3) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))

may_temps <- aru_temp_daily_micro %>%
  filter(month == "5") %>%
  filter(!aru_name == "ARUQ0") %>%
  ggplot(aes(x = day, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  geom_line(aes(colour = microclimate, linetype  = aru_name), alpha = 0.3) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))

june_temps <- aru_temp_daily_micro %>%
  filter(month == "6") %>%
  filter(!aru_name == "ARUQ0") %>%
  ggplot(aes(x = day, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  geom_line(aes(colour = microclimate, linetype  = aru_name), alpha = 0.3) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  ylim(-1, 20) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))


july_temps <- aru_temp_daily_micro %>%
  filter(month == "7") %>%
  filter(!aru_name == "ARUQ0") %>%
  ggplot(aes(x = day, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  geom_line(aes(colour = microclimate, linetype  = aru_name), alpha = 0.3) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  ylim(-1, 30) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))


august_temps <- aru_temp_daily_micro %>%
  filter(month == "8") %>%
  filter(!aru_name == "ARUQ0") %>%
  ggplot(aes(x = day, y = mean_value)) +
  geom_point(aes(colour = microclimate), size = 0.7, alpha = 0.7) +
  geom_smooth(aes(colour = microclimate), se = TRUE) +
  geom_line(aes(colour = microclimate, linetype  = aru_name), alpha = 0.3) +
  scale_colour_manual(values = c("darkblue", "forestgreen", "gold")) +
  ylim(-1, 30) +
  theme_classic() +
  geom_line(aes(y=0, linetype = "73"))




##############
#daily_values_summer <- daily_values[month %in% c(6,7,8),] # get october

#weekly_values_summer <- weekly_values[month %in% c(6,7,8),] # get october

#daily_values_summer <- dcast(daily_values,month+day ~ sensor_name,value.var = 'mean_value')
#weekly_values_summer <- dcast(weekly_values,month+week ~ sensor_name,value.var = 'mean_value')

#View(weekly_values_summer)





#### COMPUTING NUMBER OF DAYS WITH T ABOVE THRESHOLD

# Define temperature threshold
threshold_frig_q <- 6 
threshold_frig_w <- 12.6
threshold_sylv_q <- 12

# Compute number of days with temperature above the threshold for each site
active_frig_q <- aru_temp_daily_micro %>%
  filter(mean_value > threshold_frig_q) %>%
  group_by(aru_name) %>% 
  summarise(days_above_threshold = n())

active_frig_w <- aru_temp_daily_micro %>%
  filter(mean_value > threshold_frig_w) %>%
  group_by(aru_name) %>% 
  summarise(days_above_threshold = n())

active_sylv_q <- aru_temp_daily_micro %>%
  filter(mean_value > threshold_sylv_q) %>%
  group_by(aru_name) %>% 
  summarise(days_above_threshold = n())


active_frig_q <- active_frig_q %>%
  mutate(microclimate = case_when(
    aru_name %in% c("ARUQ0") ~ 'Beebox',
    aru_name %in% warm_arus ~ "Warm",
    aru_name %in% intermediate_arus ~ "Moderate",
    aru_name %in% cool_arus ~ "Cool",
    TRUE ~ NA_character_  # Handle any extra cases
  ))


active_frig_w <- active_frig_w %>%
  mutate(microclimate = case_when(
    aru_name %in% c("ARUQ0") ~ 'Beebox',
    aru_name %in% warm_arus ~ "Warm",
    aru_name %in% intermediate_arus ~ "Moderate",
    aru_name %in% cool_arus ~ "Cool",
    TRUE ~ NA_character_  # Handle any extra cases
  ))


active_sylv_q <- active_sylv_q %>%
  mutate(microclimate = case_when(
    aru_name %in% c("ARUQ0") ~ 'Beebox',
    aru_name %in% warm_arus ~ "Warm",
    aru_name %in% intermediate_arus ~ "Intermediate",
    aru_name %in% cool_arus ~ "Cool",
    TRUE ~ NA_character_  # Handle any extra cases
  ))


# Create the boxplot
ggplot(active_frig_q, aes(x = microclimate, y = days_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  labs(title = "Days Above B. frigidus (Q) Temperature Thresholds by Microclimate",
       x = "Microclimate",
       y = "Number of Days Above Threshold") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2")  # Change color palette if needed


ggplot(active_frig_w, aes(x = microclimate, y = days_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  labs(title = "Days Above B. frigidus (W) Temperature Thresholds by Microclimate",
       x = "Microclimate",
       y = "Number of Days Above Threshold") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") 


ggplot(active_sylv_q, aes(x = microclimate, y = days_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  labs(title = "Days Above B. sylvicola (Q) Temperature Thresholds by Microclimate",
       x = "Microclimate",
       y = "Number of Days Above Threshold") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") 



# Add an identifier column to each dataset
active_frig_q <- active_frig_q %>% mutate(species_threshold = "B. frigidus (Q)")
active_frig_w <- active_frig_w %>% mutate(species_threshold = "B. frigidus (W)")
active_sylv_q <- active_sylv_q %>% mutate(species_threshold = "B. sylvicola (Q)")

# Combine into one dataframe
active_days_combined <- bind_rows(active_frig_q, active_frig_w)

microclimate_colors <- c(
  "Beebox" = "grey60",  # Grayish for Beebox
  "Cool" = "#440154FF",  # Purple from viridis
  "Moderate" = "#22A884FF",  # Green from viridis
  "Warm" = "#FDE725FF"  # Yellow from viridis
)

# Create the faceted boxplot with custom colors
ggplot(active_days_combined, aes(x = microclimate, y = days_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  facet_wrap(~ species_threshold) +  # Creates a separate panel for each species/threshold
  labs(title = "Days Above Temperature Thresholds by Microclimate",
       x = "Microclimate",
       y = "Number of Days Above Physiological Threshold") +
  theme_classic() +
  scale_fill_manual(values = microclimate_colors) +  # Apply custom colors
  theme(strip.text = element_text(size = 12, face = "bold"))  # Adjust facet label size


active_days_filtered <- active_days_combined %>%
  filter(!microclimate == "Beebox") %>%
  filter(!species_threshold == "B. sylvicola (Q)")

ggplot(active_days_filtered, aes(x = microclimate, y = days_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  facet_wrap(~ species_threshold) +  # Creates a separate panel for each species/threshold
  labs(title = "Days Above Temperature Thresholds by Microclimate",
       x = "Microclimate",
       y = "Number of Days Above Physiological Threshold") +
  theme_classic() +
  scale_fill_manual(values = microclimate_colors) +  # Apply custom colors
  theme(strip.text = element_text(size = 12, face = "bold"))  # Adjust facet label size






### REDO WITH HOURS

# Compute number of days with temperature above the threshold for each site
active_frig_q_h <- aru_temp_hourly_micro %>%
  filter(mean_value > threshold_frig_q) %>%
  group_by(aru_name) %>% 
  summarise(hours_above_threshold = n())

active_frig_w_h <- aru_temp_hourly_micro %>%
  filter(mean_value > threshold_frig_w) %>%
  group_by(aru_name) %>% 
  summarise(hours_above_threshold = n())



active_frig_q_h <- active_frig_q_h %>%
  mutate(microclimate = case_when(
    aru_name %in% c("ARUQ0") ~ 'Beebox',
    aru_name %in% warm_arus ~ "Warm",
    aru_name %in% intermediate_arus ~ "Intermediate",
    aru_name %in% cool_arus ~ "Cool",
    TRUE ~ NA_character_  # Handle any extra cases
  ))


active_frig_w_h <- active_frig_w_h %>%
  mutate(microclimate = case_when(
    aru_name %in% c("ARUQ0") ~ 'Beebox',
    aru_name %in% warm_arus ~ "Warm",
    aru_name %in% intermediate_arus ~ "Intermediate",
    aru_name %in% cool_arus ~ "Cool",
    TRUE ~ NA_character_  # Handle any extra cases
  ))




# Create the boxplot
ggplot(active_frig_q_h, aes(x = microclimate, y = hours_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  labs(title = "Days Above B. frigidus (Q) Temperature Thresholds by Microclimate",
       x = "Microclimate",
       y = "Number of Hours Above Threshold") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2")  # Change color palette if needed


ggplot(active_frig_w_h, aes(x = microclimate, y = hours_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  labs(title = "Days Above B. frigidus (W) Temperature Thresholds by Microclimate",
       x = "Microclimate",
       y = "Number of Hours Above Threshold") +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") 





# Add an identifier column to each dataset
active_frig_q_h <- active_frig_q_h %>% mutate(species_threshold = "B. frigidus (Q)")
active_frig_w_h <- active_frig_w_h %>% mutate(species_threshold = "B. frigidus (W)")

# Combine into one dataframe
active_days_combined_h <- bind_rows(active_frig_q_h, active_frig_w_h)

microclimate_colors <- c(
  "Beebox" = "grey60",  # Grayish for Beebox
  "Cool" = "#440154FF",  # Purple from viridis
  "Intermediate" = "#22A884FF",  # Green from viridis
  "Warm" = "#FDE725FF"  # Yellow from viridis
)

# Create the faceted boxplot with custom colors
ggplot(active_days_combined_h, aes(x = microclimate, y = hours_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  facet_wrap(~ species_threshold) +  # Creates a separate panel for each species/threshold
  labs(title = "Hours Above Temperature Thresholds by Microclimate",
       x = "Microclimate",
       y = "Number of Hours Above Physiological Threshold") +
  theme_classic() +
  scale_fill_manual(values = microclimate_colors) +  # Apply custom colors
  theme(strip.text = element_text(size = 12, face = "bold"))  # Adjust facet label size






### WORKING WITH FLOWERING DATA

flowering_season <- read.csv("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/team_shrub_beauchemin_honours/flowering_season_QHI_2024.csv")

flowering_season_length_by_microclim <- ggplot(flowering_season, aes(x = microclimate, y = flowering_season_length, fill = microclimate)) +
  geom_boxplot() +
  labs(title = "Flowering Season Length by Microclimate",
       x = "Microclimate",
       y = "Flowering Season Length (Last-First Flowering Date)") +
  theme_classic() +
  scale_fill_manual(values = microclimate_colors) +  # Apply custom colors
  theme(strip.text = element_text(size = 12, face = "bold"))  # Adjust facet label size


levels(flowering_season$microclimate) 

flowering_season <- flowering_season %>%
  mutate(total_flowering_days = flowering_season_length - days_no_flowers)

# Check normality per microclimate using Shapiro-Wilk test
flowering_season %>%
  group_by(microclimate) %>%
  summarise(shapiro_p = shapiro.test(flowering_season_length)$p.value)

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(flowering_season_length ~ microclimate, data = flowering_season)
print(kruskal_result)

# Post-hoc test: Dunn’s test for pairwise comparisons
# install.packages("FSA")
library(FSA)
dunn_result <- dunnTest(flowering_season_length ~ microclimate, data = flowering_season, method = "bonferroni")
print(dunn_result)



# Run linear regressions
lm_first_flowering <- lm(first_flowering_doy ~ avg_summer_temp, data = flowering_season)
lm_last_flowering <- lm(last_flowering_doy ~ avg_summer_temp, data = flowering_season)
lm_flowering_length <- lm(flowering_season_length ~ avg_summer_temp, data = flowering_season)
lm_peak_flowering <- lm(peak_flowering_doy ~ avg_summer_temp, data = flowering_season)
lm_days_no_flowers <- lm(days_no_flowers ~ avg_summer_temp, data = flowering_season)
lm_total_flowering <- lm(total_flowering_days ~ avg_summer_temp, data = flowering_season)

# Summarize results
summary(lm_first_flowering)
summary(lm_last_flowering)
summary(lm_flowering_length)
summary(lm_peak_flowering)
summary(lm_days_no_flowers)
summary(lm_total_flowering)

# Define a function to create scatterplots with regression lines
plot_regression <- function(x_var, y_var, y_label) {
  ggplot(flowering_season, aes(x = avg_summer_temp, y = !!sym(y_var))) +
    geom_point(aes(color = microclimate), size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    labs(x = "Mean Summer Temperature (°C)", y = y_label) +
    theme_classic() +
    scale_color_manual(values = c("Cool" = "#440154FF", "Intermediate" = "#22A884FF", "Warm" = "#FDE725FF"))
}

# Create and display plots
plot_first_flowering <- plot_regression("avg_summer_temp", "first_flowering_doy", "First Flowering DOY")
plot_last_flowering <- plot_regression("avg_summer_temp", "last_flowering_doy", "Last Flowering DOY")
plot_flowering_length <- plot_regression("avg_summer_temp", "flowering_season_length", "Flowering Season Length")
plot_peak_flowering <- plot_regression("avg_summer_temp", "peak_flowering_doy", "Peak Flowering DOY")
plot_days_no_flowers <- plot_regression("avg_summer_temp", "days_no_flowers", "Days Without Flowers")
plot_total_flowering  <- plot_regression("avg_summer_temp", "total_flowering_days", "Days With Flowers")

# Print all plots
library(gridExtra)
grid.arrange(plot_first_flowering, plot_last_flowering, plot_flowering_length, plot_peak_flowering, plot_days_no_flowers, plot_total_flowering, ncol = 2)




#### PROPORTION OF FLOWERING SEASON WITH OKAY TEMPERATURES

# Create a new column with date ranges for each ARU
flowering_season_2 <- flowering_season

flowering_season_2$flowering_dates <- mapply(function(start, end) seq.Date(as.Date(start), as.Date(end), by = "day"), 
                                         flowering_season$first_flowering_date, 
                                         flowering_season$last_flowering_date)

expanded_flowering_data <- flowering_season_2 %>%
  unnest(flowering_dates) %>%
  rename(flowering_date = flowering_dates)

expanded_flowering_data <- as_tibble(expanded_flowering_data)


# Add a new column for year (you can adjust it to your study year)
aru_temp_daily_micro$year <- 2024  # Adjust the year as needed
aru_temp_hourly_micro$year <- 2024 

# Combine month, day, and year into a full Date column
aru_temp_daily_micro$date <- as.Date(paste(aru_temp_daily_micro$year, aru_temp_daily_micro$month, aru_temp_daily_micro$day, sep = "-"), format = "%Y-%m-%d")
aru_temp_hourly_micro$date <- as.Date(paste(aru_temp_hourly_micro$year, aru_temp_hourly_micro$month, aru_temp_hourly_micro$day, sep = "-"), format = "%Y-%m-%d")


# Filter temperature data based on each threshold
temperature_frig_q <- aru_temp_daily_micro[aru_temp_daily_micro$mean_value > threshold_frig_q, ]
temperature_frig_w <- aru_temp_daily_micro[aru_temp_daily_micro$mean_value > threshold_frig_w, ]

temperature_frig_q_h <- aru_temp_hourly_micro[aru_temp_hourly_micro$mean_value > threshold_frig_q, ]
temperature_frig_w_h <- aru_temp_hourly_micro[aru_temp_hourly_micro$mean_value > threshold_frig_w, ]

expanded_flowering_data <- expanded_flowering_data %>%
  mutate(date = flowering_date)

library(dplyr)

narrow_flowering_data <- expanded_flowering_data %>%
  dplyr::select(aru_id, avg_summer_temp, microclimate, date)

temperature_frig_q <- temperature_frig_q %>%
  mutate(aru_id = aru_name)
temperature_frig_w <- temperature_frig_w %>%
  mutate(aru_id = aru_name)

temperature_frig_q_h <- temperature_frig_q_h %>%
  mutate(aru_id = aru_name)
temperature_frig_w_h <- temperature_frig_w_h %>%
  mutate(aru_id = aru_name)

merged_narrow_frig_q <- narrow_flowering_data %>%
  left_join(temperature_frig_q, by = c("aru_id", "date"))
merged_narrow_frig_w <- narrow_flowering_data %>%
  left_join(temperature_frig_w, by = c("aru_id", "date"))

merged_narrow_frig_q_h <- narrow_flowering_data %>%
  left_join(temperature_frig_q_h, by = c("aru_id", "date"))
merged_narrow_frig_w_h <- narrow_flowering_data %>%
  left_join(temperature_frig_w_h, by = c("aru_id", "date"))

merged_frig_q_grouped <- merged_narrow_frig_q %>%
  filter(!aru_id == "NA") %>%
  filter(!mean_value == "NA") %>%
  group_by(aru_id) %>%
  summarize(days_above_temp = n())

merged_frig_w_grouped <- merged_narrow_frig_w %>%
  filter(!aru_id == "NA") %>%
  filter(!mean_value == "NA") %>%
  group_by(aru_id) %>%
  summarize(days_above_temp = n())


merged_frig_q_grouped_h <- merged_narrow_frig_q_h %>%
  filter(!aru_id == "NA") %>%
  filter(!mean_value == "NA") %>%
  group_by(aru_id) %>%
  summarize(hours_above_temp = n())

merged_frig_w_grouped_h <- merged_narrow_frig_w_h %>%
  filter(!aru_id == "NA") %>%
  filter(!mean_value == "NA") %>%
  group_by(aru_id) %>%
  summarize(hours_above_temp = n())
  
merged_total_data <- bind_rows(
  mutate(merged_frig_q_grouped, threshold = "Frigidus Q"),
  mutate(merged_frig_w_grouped, threshold = "Frigidus W"))

merged_total_data_h <- bind_rows(
  mutate(merged_frig_q_grouped_h, threshold = "Frigidus Q"),
  mutate(merged_frig_w_grouped_h, threshold = "Frigidus W"))

merged_total_data_temp <- left_join(merged_total_data, flowering_season_2 %>% 
                                      dplyr::select(aru_id, avg_summer_temp, microclimate), by = "aru_id")

merged_total_data_temp_h <- left_join(merged_total_data_h, flowering_season_2 %>% 
                                      dplyr::select(aru_id, avg_summer_temp, microclimate), by = "aru_id")

ggplot(merged_total_data_temp, aes(x = avg_summer_temp, y = days_above_temp, color = threshold)) +
  geom_point(size = 3) +  # Plot the points
  geom_smooth(method = "lm", se = TRUE, aes(color = threshold), linetype = "solid") +  # Add regression lines
  labs(title = "Foraging Window Length",
       x = "Average Summer Temperature (°C)",
       y = "Number of Flowering Days Above Physiological Threshold") +
  scale_color_manual(values = c("Frigidus Q" = "skyblue4", "Frigidus W" = "skyblue")) +
  theme_classic() +
  theme(legend.title = element_blank())  # Hide the legend title if preferred


ggplot(merged_total_data_temp_h, aes(x = avg_summer_temp, y = hours_above_temp, color = threshold)) +
  geom_point(size = 3) +  # Plot the points
  geom_smooth(method = "lm", se = TRUE, aes(color = threshold), linetype = "solid") +  # Add regression lines
  labs(title = "Foraging Window Length",
       x = "Average Summer Temperature (°C)",
       y = "Number of Flowering Hours Above Physiological Threshold") +
  scale_color_manual(values = c("Frigidus Q" = "skyblue4", "Frigidus W" = "skyblue")) +
  theme_classic() +
  theme(legend.title = element_blank())  # Hide the legend title if preferred



# Merge for each threshold
merged_frig_q <- merge(expanded_flowering_data, temperature_frig_q, by.x = "flowering_date", by.y = "date", all.x = TRUE)
merged_frig_w <- merge(expanded_flowering_data, temperature_frig_w, by.x = "flowering_date", by.y = "date", all.x = TRUE)
merged_sylv_q <- merge(expanded_flowering_data, temperature_sylv_q, by.x = "flowering_date", by.y = "date", all.x = TRUE)




### CALCULATIONS

# Define a function to calculate the proportion of flowering days with temperatures above the threshold
calculate_proportion <- function(merged_data, threshold_column) {
  merged_data %>%
    group_by(aru_id) %>%
    summarise(proportion_above_threshold = mean(!is.na(get(threshold_column))))  # Counts non-NA (i.e., temperature above threshold)
}

# Calculate the proportion for each threshold
prop_frig_q <- calculate_proportion(merged_frig_q, "mean_value")  # Temperature above threshold_frig_q
prop_frig_w <- calculate_proportion(merged_frig_w, "mean_value")  # Temperature above threshold_frig_w
prop_sylv_q <- calculate_proportion(merged_sylv_q, "mean_value")  # Temperature above threshold_sylv_q

# Combine all proportions into one dataframe for easy plotting
proportion_data <- bind_rows(
  mutate(prop_frig_q, threshold = "Frigidus Q"),
  mutate(prop_frig_w, threshold = "Frigidus W"),
  mutate(prop_sylv_q, threshold = "Sylvicola Q")
)

# Plot the proportion of flowering days above threshold for each ARU and threshold
ggplot(proportion_data, aes(x = aru_id, y = proportion_above_threshold, fill = threshold)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Proportion of Flowering Days with Above Threshold Temperatures",
       x = "ARU ID",
       y = "Proportion of Flowering Days Above Threshold") +
  scale_fill_manual(values = c("Frigidus Q" = "skyblue4", "Frigidus W" = "skyblue", "Sylvicola Q" = "orange4")) +
  theme_classic()

# Join the proportion data with avg_summer_temp for each ARU
merged_proportion_temp <- left_join(proportion_data, flowering_season_2 %>% 
                                      select(aru_id, avg_summer_temp, microclimate), by = "aru_id")

merged_proportion_temp_filtered <- merged_proportion_temp %>%
  filter(!threshold == "Sylvicola Q")

# Scatter plot with regression lines
ggplot(merged_proportion_temp, aes(x = avg_summer_temp, y = proportion_above_threshold, color = threshold)) +
  geom_point(size = 3) +  # Plot the points
  geom_smooth(method = "lm", se = TRUE, aes(color = threshold), linetype = "solid") +  # Add regression lines
  labs(title = "Proportion of Flowering Days Above Threshold vs. Avg Summer Temperature",
       x = "Average Summer Temperature (°C)",
       y = "Proportion of Flowering Days Above Threshold") +
  scale_color_manual(values = c("Frigidus Q" = "skyblue4", "Frigidus W" = "skyblue", "Sylvicola Q" = "orange4")) +
  theme_classic() +
  theme(legend.title = element_blank())  # Hide the legend title if preferred

ggplot(merged_proportion_temp_filtered, aes(x = avg_summer_temp, y = proportion_above_threshold, color = threshold)) +
  geom_point(size = 3) +  # Plot the points
  geom_smooth(method = "lm", se = TRUE, aes(color = threshold), linetype = "solid") +  # Add regression lines
  labs(title = "Proportion of Flowering Days Above Threshold vs. Avg Summer Temperature",
       x = "Average Summer Temperature (°C)",
       y = "Proportion of Flowering Days Above Threshold") +
  scale_color_manual(values = c("Frigidus Q" = "skyblue4", "Frigidus W" = "skyblue", "Sylvicola Q" = "orange4")) +
  theme_classic() +
  theme(legend.title = element_blank())  # Hide the legend title if preferred


library(broom)

# Compute linear models and extract p-values
lm_results <- merged_proportion_temp %>%
  group_by(threshold) %>%
  do(tidy(lm(proportion_above_threshold ~ avg_summer_temp, data = .))) %>%
  filter(term == "avg_summer_temp") 

# View the p-values
print(lm_results)

# Boxplot of proportion of flowering days above threshold by microclimate
ggplot(merged_proportion_temp, aes(x = microclimate, y = proportion_above_threshold, fill = threshold)) +
  geom_boxplot() +  # Create boxplot
  labs(title = "Proportion of Flowering Days Above Threshold by Microclimate",
       x = "Microclimate",
       y = "Proportion of Flowering Days Above Threshold") +
  scale_fill_manual(values = c("Frigidus Q" = "skyblue4", "Frigidus W" = "skyblue", "Sylvicola Q" = "orange4")) +
  theme_classic() +
  theme(legend.title = element_blank())  # Hide the legend title if preferred

ggplot(merged_proportion_temp, aes(x = threshold, y = proportion_above_threshold, fill = microclimate)) +
  geom_boxplot() +  # Create boxplot
  labs(title = "Proportion of Flowering Days Above Threshold by Species and Microclimate",
       x = "Species and Caste",
       y = "Proportion of Flowering Days Above Threshold") +
  scale_fill_manual(values = c("Cool" = "#440154FF", "Intermediate" = "#22A884FF", "Warm" = "#FDE725FF")) +
  theme_classic() +
  theme(legend.title = element_blank())  # Hide the legend title if preferred



# Create a new column to indicate if the day is above the threshold (1 for True, 0 for False)
merged_frig_q$days_above_frig_q <- ifelse(!is.na(merged_frig_q$mean_value), 1, 0)
merged_frig_w$days_above_frig_w <- ifelse(!is.na(merged_frig_w$mean_value), 1, 0)
merged_sylv_q$days_above_sylv_q <- ifelse(!is.na(merged_sylv_q$mean_value), 1, 0)

# Summarize by ARU and microclimate to get the total number of days above each threshold within the flowering period
summarized_frig_q <- merged_frig_q %>%
  group_by(aru_id, microclimate.x) %>%
  summarise(days_above_frig_q_total = sum(days_above_frig_q, na.rm = TRUE)) 

summarized_frig_w <- merged_frig_w %>%
  group_by(aru_id, microclimate.x) %>%
  summarise(days_above_frig_w_total = sum(days_above_frig_w, na.rm = TRUE))

summarized_sylv_q <- merged_sylv_q %>%
  group_by(aru_id, microclimate.x) %>%
  summarise(days_above_sylv_q_total = sum(days_above_sylv_q, na.rm = TRUE))

# Combine all the summarized data into one long-format data frame
summarized_data <- bind_rows(
  summarized_frig_q %>% select(aru_id, microclimate.x, days_above_frig_q_total) %>% 
    rename(days_above_threshold = days_above_frig_q_total) %>%
    mutate(threshold = "frig_q") %>%
    group_by(aru_id, microclimate.x, threshold) %>%
    summarise(mean_days_above_threshold = mean(days_above_threshold, na.rm = TRUE), .groups = 'drop'),  # Add the threshold label
  
  summarized_frig_w %>% select(aru_id, microclimate.x, days_above_frig_w_total) %>% 
    rename(days_above_threshold = days_above_frig_w_total) %>%
    mutate(threshold = "frig_w") %>%
    group_by(aru_id, microclimate.x, threshold) %>%
    summarise(mean_days_above_threshold = mean(days_above_threshold, na.rm = TRUE), .groups = 'drop'),  # Add the threshold label
  
  summarized_sylv_q %>% select(aru_id, microclimate.x, days_above_sylv_q_total) %>% 
    rename(days_above_threshold = days_above_sylv_q_total) %>%
    mutate(threshold = "sylv_q") %>%
    group_by(aru_id, microclimate.x, threshold) %>%
    summarise(mean_days_above_threshold = mean(days_above_threshold, na.rm = TRUE), .groups = 'drop')  # Add the threshold label
)

# Rename the column from microclimate.x to microclimate
summarized_data <- summarized_data %>%
  rename(microclimate = microclimate.x)

# Boxplot for number of days above each threshold by microclimate and threshold type
ggplot(summarized_data, aes(x = microclimate, y = mean_days_above_threshold, fill = microclimate)) +
  geom_boxplot() +
  facet_wrap(~ threshold, scales = "free_y") +  # Facet by each threshold
  labs(title = "Absolute Number of Flowering Days Above Temperature Threshold by Microclimate",
       x = "Microclimate",
       y = "Number of Flowering Days Above Temperature Threshold") +
  scale_fill_manual(values = c("Cool" = "#440154FF", "Intermediate" = "#22A884FF", "Warm" = "#FDE725FF")) +
  theme_classic() +
  theme(legend.title = element_blank())  # Hide the legend title if preferred


# Assuming avg_summer_temp_data contains the avg_summer_temp for each ARU
summarized_data_with_temp <- left_join(summarized_data, flowering_season_2, by = c("aru_id", "microclimate"))


# Compute linear models and extract p-values for avg_summer_temp by threshold
lm_results2 <- summarized_data_with_temp %>%
  group_by(threshold) %>%
  do(tidy(lm(mean_days_above_threshold ~ avg_summer_temp, data = .))) %>%
  filter(term == "avg_summer_temp")  # Extract only the p-value for avg_summer_temp

# View the p-values and coefficients
print(lm_results2)

# Scatter plot with regression line
ggplot(summarized_data_with_temp, aes(x = avg_summer_temp, y = mean_days_above_threshold, color = threshold)) +
  geom_point(size = 3) +  # Plot the points
  geom_smooth(method = "lm", se = TRUE, aes(color = threshold), linetype = "solid") +  # Add regression lines
  labs(title = "Mean Number of Flowering Days Above Threshold vs. Avg Summer Temperature",
       x = "Average Summer Temperature (°C)",
       y = "Mean Number of Flowering Days Above Threshold") +
  scale_color_manual(values = c("frig_q" = "skyblue4", "frig_w" = "skyblue", "sylv_q" = "orange4")) +
  theme_classic() +
  theme(legend.title = element_blank())  # Hide the legend title if preferred
