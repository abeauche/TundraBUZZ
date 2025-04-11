#### QIKIQTARUK ARU Data Processing ####
#### Alex Beauchemin                ####
#### 07/07/2024                     ####

# Install packages
# install.packages("seewave")
# install.packages("tuneR")
# install.packages("signal")
# install.packages("oce")

# Load libraries
library(dplyr)
library(ggplot2)
library(seewave)
library(tuneR)
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(oce, warn.conflicts = F, quietly = T) # image plotting functions and nice color maps


# Set working directory
setwd("/Users/alexandrebeauchemin/Desktop/Team_Shrub_2024/Honours_Thesis/ARU_Data_Processing")


# PREPARING SOUND FILES
# from https://hansenjohnson.org/post/spectrograms-in-r/

# Read in sound file
data <- readWave("Data/Raw/ARUQ18_20240628_090000.wav", from = 0, to = 60, units = "seconds")

# extract signal
snd = data@left

# determine duration
dur = length(snd)/data@samp.rate
dur # seconds

# determine sample rate
fs = data@samp.rate
fs # Hz

# resample wave
data4kHz <- resamp(data, 48000, 4000, channel = 1, output="Wave")
snd4k <- data4kHz@left


# FEATURE DETECTION ALGORITHM
# From https://ieeexplore.ieee.org/document/7894089

# divide signal into 20 second texture windows

# produce spectrogram with 100 ms windows and 90 ms overlap

# assess energy within four bands (0-500, 500-1000, 1000-1500, 1500-2000)

# compare energy between first band and top three bands

# if energy in first band is greater than top three bands combined, high pass filter and produce spectrogram

# repeat until first band falls below three top bands

# create focal template that will look for harmonically related bands within 10 ms slices

# filter out T-F elements that do not conform

# look for periods of time where fundamental frequency across multiple T-F slices remains relatively steady or change gradually

# eliminate T-F frequencies below 120 Hz

# within texture window: determine which 4 T-F boxes have the highest energy

# determine if these 4 T-F boxes are found in a harmonic relationship with each other 

# if no harmonic relationship, filter out

# if all 4 boxes have harmonic relationship, pass on to next step



# plot waveform
# demean to remove DC offset
snd = snd - mean(snd)
snd4k = snd4k - mean(snd4k)

# plot waveform
# plot(snd4k, type = 'l', xlab = 'Samples', ylab = 'Amplitude')

# number of points to use for the fft
nfft=1024

# window size (in points)
window=4096

# overlap (in points)
overlap=10


# create spectrogram
spec = specgram(x = snd4k,
                n = nfft,
                Fs = fs,
                window = window,
                overlap = overlap
)

# discard phase information
P = abs(spec$S)

# normalize
P = P/max(P)

# convert to dB
P = 10*log10(P)

# config time axis
t = spec$t

# plot spectrogram
imagep(x = t,
       y = spec$f,
       z = t(P),
       col = oce.colorsViridis,
       ylab = 'Frequency [Hz]',
       xlab = 'Time [s]',
       drawPalette = T,
       decimate = F
)


# Filter frequencies under 2000 Hz
# ARUQ18_2024_0628_090000_2000Hz <- fir(wave = ARUQ18_2024_0628_090000,
            # from = 0, # lower bound frequency in Hz
            # to = 2000, # upper bound frequency in Hz
            # bandpass = TRUE,
            # output = "Wave")

# PRODUCE SPECTROGRAM
# spectro(wave = ARUQ18_2024_0628_090000_2000Hz,
        # flim = c(0,2),,
        # tlim = c(32, 34)) # frequency limits in kHz)

