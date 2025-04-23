# TundraBUZZ
GitHub Repository for Alex Beauchemin's ecology honours (B.Sc. Hons) research project with Team Shrub in the Faculty of Forestry at the University of British Columbia

This project contains several key analyses, contained in separate scripts as follows:

- **aru_dates_tidy_TundraBUZZ.R:** This script extracts all recording dates and creates a completed dataframe with temperature readings.
- **environmental_variables_QHI.R:** This script compiles and tidies all environmental data into a single dataset per date and location.
- **flight_buzz_analysis_QHI.R:** This script prepares data, analyzes it in relation to research questions, and outputs figures and tables.
- **flight_data_tidying.R:** This script wrangles raw recognizer outputs, combines them into a single organized dataframe, sets a threshold and filters observations with a score above the confidence threshold, and exports the dataset. 
- **microclimate_TundraBUZZ_QHI.R:** This script reads and prepares TOMST data with the myclim package.
- **phenocam_image_sorting.R:** This script sorts through pollinator camera outputs and separates timelapse camera photos from the motion activated photos and videos.
- **site_spatial_analysis.R:** This script does an exploratory analysis of distances between site locations as well as carrying out a hierarchical clustering analysis.
- **sun_data_QHI.R:** This script uses the suncalc library to obtain sunrise, sunset, and sun altitude data for Qikiqtaruk - Herschel Island for the period from June 21, 2024, at 21:00:00, to August 12, 2024, at 23:59:59.
- **vegetation_ordination_QHI.R:** This script creates an ordination for vegetation plots based on vegetation composition data for 10 POLCAM sites on Qikiqtaruk-Herschel Island measured in the summer of 2024 by A. Beauchemin and E. Bowman.


**Supplementary material is available at:**
https://www.inaturalist.org/projects/bumblebees-of-qikiqtaruk
https://github.com/abeauche/TundraBUZZ_recognizer
