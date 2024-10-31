# Camera traps were programmed to run on two separate programs simultaneously: 
#   1) A timelapse program taking four photos per day (from 12:00 to 15:00);
#   2) A motion detection program taking three photos separated by 1s intervals, followed by a dynamic video recording up to 10s.
# All files are put out by the RECONYX camera in the same folder with no difference in naming. 
# This is my code to disentangle the timelapse photos from the motion activated photos and videos.


# Set working directory to RECONYX folder in drive
file_dir <- "file path"

# List all files in the directory
files <- list.files(path = file_dir)

# Extract sequence numbers
file_info <- data.frame(
  file_name = files,
  file_extension = tools::file_ext(files),
  # Extract sequence number using base R's gsub function
  sequence_num = as.numeric(gsub("\\D", "", files)),
  stringsAsFactors = FALSE
)

# Sort by sequence number
file_info <- file_info[order(file_info$sequence_num), ]

# 1: Extract all .AVI (video) files
avi_files <- file_info[file_info$file_extension == "AVI", ]

# 2: Backtrack to get associated .JPG files (n-1, n-2, n-3)
motion_activated_files <- file_info[file_info$sequence_num %in% (avi_files$sequence_num - 1) |
                                      file_info$sequence_num %in% (avi_files$sequence_num - 2) |
                                      file_info$sequence_num %in% (avi_files$sequence_num - 3) |
                                      file_info$sequence_num %in% avi_files$sequence_num, ]

# 3: Identify timelapse files (those not in the motion-activated set)
timelapse_files <- setdiff(file_info$file_name, motion_activated_files$file_name)

# Create directories for motion-activated and timelapse files
dir.create(file.path(file_dir, "motion_activated"), showWarnings = FALSE)
dir.create(file.path(file_dir, "timelapse"), showWarnings = FALSE)

# Function to move files
move_files <- function(files, dest_folder) {
  file.rename(from = file.path(file_dir, files),
              to = file.path(file_dir, dest_folder, files))
}

# Move motion-activated files
move_files(motion_activated_files$file_name, "motion_activated")

# Move timelapse files
move_files(timelapse_files, "timelapse")

cat("Files sorted successfully!")

