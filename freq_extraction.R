library(dplyr)
library(tuneR)
library(seewave)
library(purrr)
library(tibble)
library(tidyverse)

det <- read.csv("/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ_2024_bumblebee_detections.csv")

set.seed(123)


############################################################
# 1. Harmonic checking helper function
############################################################

check_harmonics <- function(sp,
                            fund_freq,
                            harm_orders = 2:4,
                            freq_tolerance_hz = 8,
                            rel_amp_threshold = 0.20,
                            min_harmonics = 2) {
  
  freqs <- sp[,1]
  amps  <- sp[,2]
  
  # fundamental amplitude
  fund_idx <- which.min(abs(freqs - fund_freq))
  fund_amp <- amps[fund_idx]
  
  if(is.na(fund_amp) || fund_amp <= 0) 
    return(list(ok = FALSE, n_harm = 0, harmonic_score = 0))
  
  found <- 0
  harm_amps <- numeric(length(harm_orders))
  
  for(i in seq_along(harm_orders)) {
    h <- harm_orders[i]
    target_f <- fund_freq * h
    
    if(target_f > max(freqs)) next
    
    idxs <- which(freqs >= (target_f - freq_tolerance_hz) &
                    freqs <= (target_f + freq_tolerance_hz))
    
    if(length(idxs) == 0) next
    
    harm_amp <- max(amps[idxs], na.rm = TRUE)
    harm_amps[i] <- harm_amp
    
    if(!is.na(harm_amp) && harm_amp >= rel_amp_threshold * fund_amp) {
      found <- found + 1
    }
  }
  
  harmonic_score <- sum(harm_amps, na.rm = TRUE) / (fund_amp + 1e-12)
  
  ok <- found >= min_harmonics
  
  return(list(ok = ok,
              n_harm = found,
              harmonic_score = harmonic_score))
}

############################################################
# 2. Segment processing for one file
############################################################

get_peak_freqs_for_file <- function(file, segments) {
  
  message("Processing WAV: ", file)
  
  # Load WAV
  wav <- readWave(file)
  
  # Downsample to 8 kHz
  wav <- downsample(wav, samp.rate = 8000)
  
  # Ensure segment indices are in bounds
  segments <- segments %>%
    mutate(
      start_time = pmax(start_time, 0),
      end_time   = pmin(end_time, length(wav@left)/wav@samp.rate)
    ) %>%
    filter(end_time > start_time)
  
  map_df(1:nrow(segments), function(i) {
    
    start_sec <- segments$start_time[i]
    end_sec   <- segments$end_time[i]
    
    start_i <- max(floor(start_sec * wav@samp.rate), 1)
    end_i   <- min(floor(end_sec * wav@samp.rate), length(wav@left))
    
    if (start_i >= end_i) return(tibble())
    
    seg <- extractWave(wav, from = start_i, to = end_i, xunit = "samples")
    
    # Optional bandpass filter (can comment out for testing)
    # seg <- ffilter(seg, from = 100, to = 400, output = "Wave")
    
    sp <- seewave::spec(seg, plot = FALSE)
    
    # Restrict to 100–400 Hz
    sp_sub <- sp[sp[,1] >= 100 & sp[,1] <= 400, ]
    
    if (nrow(sp_sub) == 0) return(tibble())
    
    # Fundamental = max amplitude
    peak_idx <- which.max(sp_sub[,2])
    peak_freq <- sp_sub[peak_idx,1]
    peak_amp  <- sp_sub[peak_idx,2]
    
    tibble(
      file = segments$file[1],  # original filename
      start_time = start_sec,
      end_time = end_sec,
      peak_freq_Hz = peak_freq,
      peak_amp = peak_amp
    )
    
  })
}
  
  return(results)
}

############################################################
# 3. Top-level pipeline
############################################################

run_harmonic_pipeline <- function(
    detections_csv,
    wav_dir = "/Volumes/TundraBUZZ/data/raw/aru_audio",
    n_sample = 10
) {
  
  # 1. Load detections CSV
  det <- read_csv(detections_csv)
  
  # 2. Sample subset
  det_sample <- det %>% sample_n(n_sample)
  
  # 3. Group by file
  det_by_file <- split(det_sample, det_sample$file)
  
  # 4. Loop over files
  all_results <- map_df(names(det_by_file), function(file) {
    
    segments <- det_by_file[[file]]
    
    # --- build full path with wildcard ---
    wav_pattern <- file.path(wav_dir, "*", "Data", file)
    wav_file <- Sys.glob(wav_pattern)
    
    if (length(wav_file) == 0) {
      warning("No WAV file found for: ", file)
      return(tibble())
    }
    
    message("Loading WAV: ", wav_file)
    
    wav <- readWave(wav_file)
    
    # Downsample
    wav <- downsample(wav, samp.rate = 8000)
    
    # --- clip segments to WAV length ---
    segments <- segments %>%
      mutate(
        start_time = pmax(start_time, 0),
        end_time   = pmin(end_time, length(wav@left)/wav@samp.rate)
      ) %>%
      filter(end_time > start_time)
    
    # 5. Process segments using your function
    get_peak_freqs_for_file(file = wav_file[1], segments = segments)
  })
  
  return(all_results)
}


results <- run_harmonic_pipeline(
  detections_csv = "/Volumes/TundraBUZZ/outputs/recognizer_outputs/clean/ARUQ_2024_bumblebee_detections.csv",
  n_sample = 10   # or larger if you want
)




file <- "ARUQ0_OLD18_20240724_150000.wav"
wav_pattern <- file.path("/Volumes/TundraBUZZ/data/raw/aru_audio", "*", "Data", file)
Sys.glob(wav_pattern)


length(wav@left)  # total samples
max(det_sample$end_time) * srate




library(tuneR)
library(seewave)

wav_file <- "/Volumes/TundraBUZZ/data/raw/aru_audio/ARUQ0_17Aug2024/Data/ARUQ0_OLD18_20240706_203000.wav"
wav <- readWave(wav_file)
wav <- downsample(wav, samp.rate = 8000)

# pick a segment in seconds
start_sec <- 278.25
end_sec <- 278.55

start_i <- max(floor(start_sec * wav@samp.rate), 1)
end_i <- min(floor(end_sec * wav@samp.rate), length(wav@left))

seg <- extractWave(wav, from = start_i, to = end_i, xunit = "samples")

# Compute spectrum
sp <- seewave::spec(seg, plot = TRUE)  # plot=TRUE to see if any signal appears

# Restrict to 100-400 Hz
sp_sub <- sp[sp[,1] >= 100 & sp[,1] <= 400, ]
nrow(sp_sub)




library(tuneR)
library(seewave)

wav_file <- "/Volumes/TundraBUZZ/data/raw/aru_audio/ARUQ0_17Aug2024/Data/ARUQ0_OLD18_20240706_203000.wav"
wav <- readWave(wav_file)
wav <- downsample(wav, samp.rate = 8000)

start_sec <- 278.25
end_sec   <- 278.55

start_i <- max(floor(start_sec * wav@samp.rate), 1)
end_i   <- min(floor(end_sec * wav@samp.rate), length(wav@left))

seg <- extractWave(wav, from = start_i, to = end_i, xunit = "samples")

# seg = Wave object for the segment
sp <- seewave::spec(
  seg,
  f = wav@samp.rate,      # sampling rate
  wl = length(seg@left),  # full segment = max frequency resolution
  plot = FALSE,            # visualize if you want
  freq = TRUE            # x axis in Hz
)

# restrict to 100–400 Hz
sp_sub <- sp[sp[,1] >= 100 & sp[,1] <= 400, ]

# peak frequency = max amplitude
peak_idx <- which.max(sp_sub[,2])
peak_freq <- sp_sub[peak_idx,1]
peak_amp  <- sp_sub[peak_idx,2]




# segment length
n <- length(seg@left)

# sampling rate
fs <- wav@samp.rate

# compute spectrum (full segment, maximum frequency resolution)
sp <- seewave::spec(seg, f = fs, wl = n, plot = FALSE)  # returns vector of length ~ n/2+1

# correct frequency vector
freqs <- seq(0, fs/2, length.out = length(sp))  # match the amplitude vector length

# combine into tibble
sp_df <- tibble(
  frequency = freqs,
  amplitude = sp
)


# Example WAV segment
wav_file <- "/Volumes/TundraBUZZ/data/raw/aru_audio/ARUQ0_17Aug2024/Data/ARUQ0_OLD18_20240706_203000.wav"
wav <- readWave(wav_file)
wav <- downsample(wav, samp.rate = 8000)

# Segment indices
start_sec <- 278.25
end_sec   <- 278.55
start_i <- floor(start_sec * wav@samp.rate)
end_i   <- floor(end_sec * wav@samp.rate)

seg <- extractWave(wav, from = start_i, to = end_i, xunit = "samples")

# Compute spectrum
sp <- seewave::spec(seg, f = wav@samp.rate, wl = length(seg@left), plot = FALSE)

freqs <- sp[,1]       # x column = frequency
amps  <- sp[,2]       # y column = amplitude

# Combine into tibble
sp_df <- tibble(
  frequency = freqs,
  amplitude = amps
)

# Subset 100-400 Hz
sp_sub <- sp_df %>% filter(frequency >= 0.100, frequency <= 0.400)

# Peak
peak_idx <- which.max(sp_sub$amplitude)
peak_freq <- sp_sub$frequency[peak_idx]
peak_amp  <- sp_sub$amplitude[peak_idx]

peak_freq
peak_amp








# Function to get peak frequency and amplitude for a single segment
get_peak_freq <- function(wav_file, start_sec, end_sec, fs_target = 8000,
                          freq_min = 0.1, freq_max = 0.4) {
  
  # Load WAV and downsample
  wav <- readWave(wav_file)
  wav <- downsample(wav, samp.rate = fs_target)
  
  # Convert times to sample indices
  start_i <- floor(start_sec * wav@samp.rate)
  end_i   <- floor(end_sec   * wav@samp.rate)
  
  # Extract segment
  seg <- extractWave(wav, from = start_i, to = end_i, xunit = "samples")
  
  # Compute spectrum
  sp <- seewave::spec(seg, f = wav@samp.rate, wl = length(seg@left), plot = FALSE)
  
  # Extract frequency and amplitude columns
  freqs <- sp[,1]
  amps  <- sp[,2]
  
  # Combine into tibble
  sp_df <- tibble(frequency = freqs, amplitude = amps)
  
  # Subset to frequency band
  sp_sub <- sp_df %>% filter(frequency >= freq_min, frequency <= freq_max)
  
  # Peak
  peak_idx <- which.max(sp_sub$amplitude)
  tibble(
    peak_freq = sp_sub$frequency[peak_idx]*1000,
    peak_amp  = sp_sub$amplitude[peak_idx]
  )
}


wav_file <- "/Volumes/TundraBUZZ/data/raw/aru_audio/ARUQ0_17Aug2024/Data/ARUQ0_OLD18_20240706_203000.wav"
start_sec <- 278.25
end_sec   <- 278.55

get_peak_freq(wav_file, start_sec, end_sec)








# Function for one WAV file and its segments
get_peak_freqs_for_file <- function(file, segments, fs_target = 8000,
                                    freq_min = 0.1, freq_max = 0.4) {
  
  wav <- readWave(file)
  wav <- downsample(wav, samp.rate = fs_target)
  
  map_df(1:nrow(segments), function(i) {
    start_sec <- segments$start_time[i]
    end_sec   <- segments$end_time[i]
    
    # Convert to sample indices
    start_i <- floor(start_sec * wav@samp.rate)
    end_i   <- floor(end_sec   * wav@samp.rate)
    
    # Extract segment
    seg <- extractWave(wav, from = start_i, to = end_i, xunit = "samples")
    
    # FFT for max frequency resolution
    sp <- seewave::spec(seg, f = wav@samp.rate, wl = length(seg@left), plot = FALSE)
    
    # Columns: x = frequency (kHz), y = amplitude
    freqs <- sp[,1]
    amps  <- sp[,2]
    
    sp_df <- tibble(frequency = freqs, amplitude = amps)
    
    # Subset to band
    sp_sub <- sp_df %>% filter(frequency >= freq_min, frequency <= freq_max)
    
    # Peak
    peak_idx <- which.max(sp_sub$amplitude)
    
    tibble(
      file = segments$file[i],
      start_time = start_sec,
      end_time   = end_sec,
      peak_freq_kHz = sp_sub$frequency[peak_idx],
      peak_amp      = sp_sub$amplitude[peak_idx]
    )
  })
}

# Main pipeline
run_harmonic_pipeline <- function(
    detections_csv,
    wav_dir = "/Volumes/TundraBUZZ/data/raw/aru_audio/ARUQ0_17Aug2024/Data/",
    n_sample = 10
) {
  
  # 1. Load detections CSV
  det <- read_csv(detections_csv)
  
  # 2. Sample subset
  det_sample <- det %>% sample_n(n_sample)
  
  # 3. Group by file
  det_by_file <- split(det_sample, det_sample$file)
  
  # 4. Loop over files
  all_results <- map_df(names(det_by_file), function(file) {
    
    segments <- det_by_file[[file]]
    
    # --- build full path with wildcard ---
    wav_file <- file.path(wav_dir, file)
    if (!file.exists(wav_file)) {
      warning("No WAV file found for: ", file)
      return(tibble())
    }
    
    message("Loading WAV: ", wav_file)
    
    # Clip segments to WAV length
    wav_len_sec <- length(readWave(wav_file[1])@left)/8000  # rough max length in sec
    segments <- segments %>%
      mutate(
        start_time = pmax(start_time, 0),
        end_time   = pmin(end_time, wav_len_sec)
      ) %>%
      filter(end_time > start_time)
    
    # Process segments
    get_peak_freqs_for_file(file = wav_file[1], segments = segments)
  })
  
  return(all_results)
}


# Modified pipeline
run_harmonic_pipeline_df <- function(det, wav_dir, n_sample = 10) {
  
  det_sample <- det %>% sample_n(n_sample)
  det_by_file <- split(det_sample, det_sample$file)
  
  all_results <- purrr::map_df(names(det_by_file), function(file) {
    segments <- det_by_file[[file]]
    
    wav_file <- file.path(wav_dir, file)
    if (!file.exists(wav_file)) return(tibble())
    
    get_peak_freqs_for_file(file = wav_file, segments = segments)
  })
  
  return(all_results)
}



det_beebox <- det %>%
  filter(microclimate2 == "BEEBOX")


# Example usage
results <- run_harmonic_pipeline_df(
  det_beebox,
  wav_dir = "/Volumes/TundraBUZZ/data/raw/aru_audio/ARUQ0_17Aug2024/Data/",
  n_sample = 10
)
