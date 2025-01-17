#Load packages
library(tidyverse)
library(here)
library(lubridate)
# 
# To obtain water levels for this study, tidal predictions were sourced from the NOAA tidal station Rocky Point, Eld Inlet (TWC1115). Predicted high and low tide data, including the corresponding heights and times, were extracted for the study period. These data were then used to interpolate water levels at finer temporal resolution for the target location.
# The high and low tide times and water levels were processed in R to generate a continuous time series. Specifically, cubic spline interpolation was applied to predict water levels at 15-minute intervals between the recorded high and low tide values. This method assumes a smooth, continuous change in water levels between high and low tides, aligning with established tidal patterns.
# To validate the interpolation, the generated water levels were compared against the recorded high and low tide data. For downstream analyses, these interpolated water levels were matched to additional observational datasets by finding the closest corresponding timestamp. The final dataset was used to assess temporal variations in water levels relative to environmental and biological parameters.
# This approach provides an accurate and high-resolution estimate of water levels for locations where continuous tidal monitoring data are not available.

#Read in the high and low data from eld inlet
raw_data23 <- read.table(here("Data/Tides/2023_ELD_TWC1115_annual.txt"), 
                       skip = 18, 
                       header = TRUE, 
                       sep = "", 
                       stringsAsFactors = FALSE) %>% 
  mutate(Date = ymd(Date))

raw_data24 <- read.table(here("Data/Tides/2024_ELD_TWC1115_annual.txt"), 
                         skip = 18, 
                         header = TRUE, 
                         sep = "", 
                         stringsAsFactors = FALSE) %>% 
  mutate(Date = ymd(Date))
#Read in the thorndyke tide data
# Read the CSV file and convert the Date_time column to POSIXct
thorntides <- read.csv(here("Data/Tides/2023_2024_bangor_tides.csv")) %>%
  mutate(Date_time = as.POSIXct(Date_time, format = "%m/%d/%Y %H:%M", tz = "UTC"))

#Join 2023 and 2024 
tides <- bind_rows(raw_data23, raw_data24) %>%
  rename(
    date = Date,
    day = Day,
    time = Time,
    pred_ft = Pred.Ft.,
    pred_cm = Pred.cm.,
    high_low = High.Low) %>% 
  mutate(datetime = ymd_hms(paste(date, paste0(time, ":00")))) %>% 
  mutate(height = pred_ft) %>% #Change col names to work with the function below
  mutate(high_low = str_replace(high_low, "H", "High")) %>% 
  mutate(high_low = str_replace(high_low, "L", "Low")) %>% 
  mutate(type = high_low)

##Generate interpolated water levels on 15 minute intervals
# Define the extended time range with 15-minute intervals
start_date <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2024-12-30 23:59:59", tz = "UTC")
interval_times <- seq(from = start_date, to = end_date, by = "15 min")

# Filter high/low tide data within the extended range


high_low_data <- tides %>%
  filter(datetime >= start_date & datetime <= end_date) %>%
  select(datetime, height) %>%
  arrange(datetime)

# Perform spline interpolation for 15-minute intervals
spline_result <- spline(
  x = as.numeric(high_low_data$datetime), # Input: times of high/low tides
  y = high_low_data$height,              # Input: corresponding water levels
  xout = as.numeric(interval_times)      # Output: times for 15-minute intervals
)

# Create a data frame for the interpolated results
interpolated_tides <- data.frame(
  datetime = as.POSIXct(spline_result$x, origin = "1970-01-01", tz = "UTC"),
  height = spline_result$y
)

# View the first few rows of the interpolated results
head(interpolated_tides)

# # Plot the results
# plot(high_low_data$datetime, high_low_data$height, type = "p", col = "red",
#      xlab = "Datetime", ylab = "Water Level (ft)", main = "15-Minute Interpolated Water Levels (Jan 2023)")
# lines(interpolated_tides$datetime, interpolated_tides$height, col = "blue")
# legend("topright", legend = c("High/Low Tides", "15-Minute Levels"), col = c("red", "blue"), lty = c(NA, 1), pch = c(1, NA))

##Add a tidal height column to eld inlet 
head(allcheldat)
data <- allcheldat
##Add a column to the cheldat for tidehieght
# Ensure the datetime columns are in POSIXct format
allcheldat$datetime <- as.POSIXct(allcheldat$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
interpolated_tides$datetime <- as.POSIXct(interpolated_tides$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Match the nearest tidal height

# Function to find the nearest datetime in interpolated_tides
find_nearest <- function(target_time, reference_times, reference_heights) {
  idx <- which.min(abs(difftime(target_time, reference_times, units = "secs")))
  return(reference_heights[idx])
}

# Add water_level column
allcheldat <- allcheldat %>%
  rowwise() %>%
  mutate(water_level = find_nearest(datetime, interpolated_tides$datetime, interpolated_tides$height)) %>%
  ungroup()

# View the updated allcheldat
head(allcheldat)

#####################
# Define the date range
plot_start <- as.POSIXct("2023-08-01 00:00:00", tz = "UTC")
plot_end <- as.POSIXct("2023-08-15 23:59:59", tz = "UTC")

# Filter data within the specified range
filtered_data <- data %>%
  filter(datetime >= plot_start & datetime <= plot_end)

filtered_tides <- tides %>%
  filter(datetime >= plot_start & datetime <= plot_end)

ggplot() +
  # Add high/low tide data as points
  geom_point(data = filtered_tides, aes(x = datetime, y = height, color = high_low), size = 3) +
  # Add water_level as a line
  geom_line(data = filtered_data, aes(x = datetime, y = water_level), color = "blue", size = 1) +
  labs(
    title = "Water Levels and High/Low Tide Data",
    x = "Datetime",
    y = "Water Level (ft)"
  ) +
  scale_color_manual(values = c("High" = "red", "Low" = "green"), name = "Tide Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day")

#Now add water levels to the Thorndyke section
# Match the nearest tidal height from thorntides
# Ensure datetime in allthorndat is in POSIXct format
allthorndat <- allthorndat %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# Match the nearest tidal height with a safe fallback
find_nearest <- function(target_time, reference_times, reference_heights) {
  # Calculate time differences
  time_diff <- abs(difftime(target_time, reference_times, units = "secs"))
  
  # Check if there are any valid references to match
  if (length(time_diff) == 0) {
    return(NA)
  }
  
  # Find the index of the minimum time difference (nearest datetime)
  idx <- which.min(time_diff)
  
  # Return the corresponding tidal height, or NA if no valid idx
  if (length(idx) > 0) {
    return(reference_heights[idx])
  } else {
    return(NA)
  }
}

# Add water_level column with error handling for missing values
allthorndat <- allthorndat %>%
  rowwise() %>%
  mutate(water_level = find_nearest(datetime, thorntides$Date_time, thorntides$Tidal_height)) %>%
  ungroup()

# View the updated allthorndat with water_level
head(allthorndat)

#Check that the tides match
# Plot the water levels using ggplot2
# Define the date range for filtering (June 1st to June 20th, 2023)
plot_start <- as.POSIXct("2023-06-01 00:00:00", tz = "UTC")
plot_end <- as.POSIXct("2023-06-20 23:59:59", tz = "UTC")

# Filter allthorndat and thorntides within the specified date range
filtered_allthorndat <- allthorndat %>%
  filter(datetime >= plot_start & datetime <= plot_end)

filtered_thorntides <- thorntides %>%
  filter(Date_time >= plot_start & Date_time <= plot_end)

# Plot the water levels for both allthorndat and thorntides on the same plot
ggplot() +
  # Plot water levels from allthorndat
  geom_line(data = filtered_allthorndat, aes(x = datetime, y = water_level, color = "allthorndat"), size = 1) +
  # Plot tidal heights from thorntides
  geom_line(data = filtered_thorntides, aes(x = Date_time, y = Tidal_height, color = "thorntides"), size = 1) +
  # Add labels and title
  labs(title = "Water Level Comparison (Jun 1 - Jun 20, 2023)",
       x = "Date",
       y = "Water Level (m)",
       color = "Source") +
  # Customize the theme
  theme_minimal() +
  scale_color_manual(values = c("allthorndat" = "blue", "thorntides" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

