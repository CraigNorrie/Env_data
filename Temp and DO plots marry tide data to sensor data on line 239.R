##Script to analyse temperature data from oyster field sites
#Load packages
library(tidyverse)
library(lubridate)
library(here)
library(data.table)
##read in the data 
#Thorndyke
thorndat_junjuly <- read.csv(here("Data", "Thorndyke_5_june_to_2_aug_2023.csv"))
thorndat_julyaug <- read.csv(here("Data", "Thorndyke_5_june_to_2_aug_2023.csv"))
thorndat_augoct <- read.csv(here("Data", "Thorndyke_aug2_october23_2023.csv"))
thorndat_novdec <- read.csv(here("Data", "Thorn_oct_nov_23.csv")) %>% filter(Temp_C > -10)
thorndat_may24 <- read.csv(here("Data", "Thorndyke_DO_may10_June19_24.csv"))
thorndat_june24 <- read.csv(here("Data", "Thorndyke_DO_june21_July24_2024_deploy.csv"))
thorndat_july24 <- read.csv(here("Data", "Thorndyke_DO_23july_20Aug24.csv"), skip = 1)
thorndat_aug24 <- read.csv(here("Data", "Thorndyke_DO_aug_20_sept_17_2024.csv"), skip = 1)
thorndat_sept24 <- read.csv(here("Data", "Thorn_DO_Sep16_Oct22_2024.csv"), skip = 1)


#Chelsea
cheldat_mayaug <- read.csv(here("Data", "Chelsea_Farm_10_may_to_2_Aug_2023.csv"))
cheldat_augoct <- read.csv(here("Data", "Chelsea_Farm__1AUG_october23.csv")) %>% filter(Temp_C > -10)
cheldat_winter_24 <- read.csv(here("Data", "Chel_DO_winter_24.csv"), skip = 1) %>% filter(Temp_C > -10)
cheldat_may_24 <- read.csv(here("Data", "Chel_DO_May9_June21_2024.csv")) %>% filter(Temp_C > -10)
cheldat_june_24 <- read.csv(here("Data", "Chel_DO_June_24_July23_2024.csv")) %>% filter(Temp_C > -10)
cheldat_july_24 <- read.csv(here("Data", "Chel_DO_July23_Aug20_2024_Deploy.csv"), skip = 1)
cheldat_aug24 <- read.csv(here("Data", "Chel_DO_Aug19_sept_17_2024.csv"), skip = 1)
cheldat_sept24 <- read.csv(here("Data", "Chel_DO_Sep16_oct21_2024.csv"), skip = 1)


#hood Head
hooddat_mayjuly <- read.csv(here("Data", "Hood_Head_12_may_to_17July.csv"))
hooddat_winter_24 <- read.csv(here("Data", "Hood_head_DO_April_24_DL.csv")) #%>% filter(Date.Time..GMT.07.00 < 12.5) #From 2/20/2024 - 2/27/2024 the temperarure is very strange.
hooddat_may24 <- read.csv(here("Data", "Hood_DO_May1_June20_2024.csv")) 
hooddat_june24 <- read.csv(here("Data", "Hood_DO_20June_23July_24.csv"))
hooddat_july24 <- read.csv(here("Data", "Hood_DO_July15_Aug15_2024.csv"), skip = 1)
hooddat_aug_nov24 <- read.csv(here("Data", "Hood_DO_Aug_oct24.csv"), skip = 1)


#manchester
mandat_apraug <- read.csv(here("Data", "Manchester_15_april_8_August.csv"), skip = 1)
mandat_augoct <- read.csv(here("Data", "Manchester_8AUG_1NOV.csv")) %>% filter(Temp_C > -10)
mandat_win_24 <- read.csv(here("Data", "Manchester_DO_MAY24_collection.csv"), skip = 1) %>% filter(Temp_C > -10)
mandat_win_24 <- mandat_win_24 %>% rename(Date.Time..GMT.07.00 = Date.Time..GMT.08.00)#Header column is a little different for some reason
mandat_june_24 <- read.csv(here("Data", "Man_june_18_july20_2024_deploy.csv")) %>% filter(Temp_C > -10)
mandat_july24 <- read.csv(here("Data", "Man_july17_aug12_2024.csv"), skip = 1) %>% filter(Temp_C > -10)
mandat_aug24 <- read.csv(here("Data", "Man_August12_sept23_2024.csv"), skip = 1)
mandat_sept24 <- read.csv(here("Data", "Man_DO_Sept29_oct282024.csv"), skip = 1)

##Merge the datasets
allthorndat <- bind_rows(thorndat_junjuly, thorndat_julyaug, thorndat_augoct, thorndat_novdec, thorndat_may24, thorndat_june24, thorndat_july24, thorndat_aug24, thorndat_sept24)
allcheldat <- bind_rows(cheldat_mayaug, cheldat_augoct, cheldat_winter_24, cheldat_may_24, cheldat_june_24, cheldat_july_24, cheldat_aug24, cheldat_sept24)
allhooddat <- bind_rows(hooddat_mayjuly,hooddat_winter_24, hooddat_may24, hooddat_june24,hooddat_july24, hooddat_aug_nov24)
allmandat <- bind_rows(mandat_apraug, mandat_augoct,mandat_win_24, mandat_june_24, mandat_july24, mandat_aug24, mandat_sept24)

#Set the first column as date
allthorndat$datetime <- mdy_hm(allthorndat$Date.Time..GMT.07.00)
allcheldat$datetime  <- mdy_hm(allcheldat$Date.Time..GMT.07.00)
allhooddat$datetime  <- mdy_hm(allhooddat$Date.Time..GMT.07.00)
allmandat$datetime  <- mdy_hm(allmandat$Date.Time..GMT.07.00)
#add a site columin to each of the DFs
allcheldat <- allcheldat %>% mutate(Site = "Chelsea")
allthorndat <- allthorndat %>% mutate(Site = "Thorndyke")
allhooddat <- allhooddat %>% mutate(Site = "Hood_Head")
allmandat <- allmandat %>% mutate(Site = "Manchester")
#add a date column to the two dfs
allcheldat$date <- as.Date(allcheldat$datetime)
allthorndat$date <- as.Date(allthorndat$datetime)
allhooddat$date <- as.Date(allhooddat$datetime)
#Remove the days that we were in the field and cages were out the water, april 11 2024
allhooddat <- allhooddat %>% filter(!date %in% as.Date(c("2024-04-29", "2024-04-30", "2024-06-20", "2024-07-15", "2024-08-14"))) 
allmandat$date <- as.Date(allmandat$datetime)

#Write all data when approporate
# write.csv(allthorndat, "C:/Users/Craig Norrie/OneDrive - UW/Environmental data/Environmental data website/Data/thorn_Do_Temp.csv")
# write.csv(allhooddat, "C:/Users/Craig Norrie/OneDrive - UW/Environmental data/Environmental data website/Data/hood_Do_Temp.csv")
# write.csv(allcheldat, "C:/Users/Craig Norrie/OneDrive - UW/Environmental data/Environmental data website/Data/chelsea_Do_Temp.csv")
# write.csv(allmandat, "C:/Users/Craig Norrie/OneDrive - UW/Environmental data/Environmental data website/Data/man_Do_Temp.csv")

###########Interpolate tidal heights for intertidal sites##############
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
thorntides <- read.csv(here("Data/Tides/2023th_2024_bangor_tides.csv")) %>%
  mutate(Date_time = as.POSIXct(Date_time, format = "%m/%d/%Y %H:%M"))

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
plot_start <- as.POSIXct("2023-06-01 00:00:00")
plot_end <- as.POSIXct("2023-06-20 23:59:59")

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

#######################Calculate averages###########

#calculate averages******ADD A DO FILTER TO BE THE TIDAL HEIGHT********
chelav <- allcheldat %>% group_by(date) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE), meanDO=mean(DO_mgL, na.rm = TRUE))
thornav <- allthorndat %>% group_by(date) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE), meanDO=mean(DO_mgL, na.rm = TRUE))
manav <- allmandat %>% group_by(date) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE), meanDO=mean(DO_mgL, na.rm = TRUE))
hoodav <- allhooddat %>% group_by(date) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE), meanDO=mean(DO_mgL, na.rm = TRUE))
#Only caclulate the days I want
chelav <- chelav %>% filter(date > "2023-06-05")
thornav <- thornav %>% filter(date > "2023-06-05")
hoodav <- hoodav %>% filter(date > "2023-06-05")
manav <- manav %>% filter(date > "2023-06-05")
#calculate cumulative degree days
chelav$degdays <- cumsum(chelav$meantemp)
thornav$degdays <- cumsum(thornav$meantemp)
#Add site to the average dfs
chelav <- chelav %>% mutate(Site = "Chelsea")
thornav <- thornav %>% mutate(Site = "Thorndyke")
manlav <- manav %>% mutate(Site = "Manchester")
hoodav <- hoodav %>% mutate(Site = "Hood_Head")
#merge the two sites temperature data to plot
alldat <- bind_rows(allcheldat, allthorndat, allmandat, allhooddat)
allavdat <- bind_rows(chelav, thornav, hoodav, manav)
#plot this
alldat %>% filter(datetime > "2023-06-05 14:37:00") %>% 
  ggplot(aes(x = datetime, y = Temp_C, colour = Site))+geom_line()+theme_classic()
dailyaverages <- alldat
alldat$date <- as.Date(alldat$datetime) 
averagetemps <- alldat %>% group_by(date, Site) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE))
averageDO <- alldat %>% group_by(date, Site) %>% summarise(meanDO=mean(DO_mgL, na.rm = TRUE))
#plot the average temps
averagetemps %>% 
  filter(date > "2023-06-05") %>% 
           ggplot(aes(x= date , y = meantemp, colour = Site)) + geom_line(size=1)+
  theme_bw()+ylab("Daily Mean Temperature (°C)")+xlab("Date")+
  scale_color_manual(values=c("#B3C863","#756A8C","#037D64","#CEB08E"))+
  theme(text=element_text(),
        axis.title.y=element_text())
#Plot DO
averageDO %>% 
  filter(date > "2023-06-05") %>% #filter(date < "2023-10-17") %>% 
  ggplot(aes(x= date , y = meanDO, colour = Site)) + geom_line(size=1)+
  theme_bw()+ylab("Daily Mean Dissolved Oxygen (mg / L)")+xlab("Date")+
  scale_color_manual(values=c("#B3C863","#756A8C","#037D64","#CEB08E"))+
  theme(axis.title.y=element_text())
  
#####plot cumulative deg days
allavdat %>% ggplot(aes(x=date, y=degdays, colour = Site))+geom_line(size=1)


# Figures for presentations -----------------------------------------------
#Plot DO
averageDO %>% 
  filter(date > "2023-06-05") %>% #filter(date < "2023-10-17") 
  ggplot(aes(x= date , y = meanDO, colour = Site)) + geom_line(size=1)+
  theme_bw()+ylab("Daily Mean Dissolved Oxygen (mg / L)")+xlab("Date")+
  scale_color_manual(values=c("#B3C863","#756A8C","#037D64","#CEB08E"))+
  theme(axis.title.y=element_text())+
  theme(
    text = element_text(size = 14))


averagetemps %>% 
  filter(date > "2023-06-05") %>% 
  ggplot(aes(x= date , y = meantemp, colour = Site)) + geom_line(size=1)+
  theme_bw()+ylab("Daily Mean Temperature (°C)")+xlab("Date")+
  scale_color_manual(values=c("#B3C863","#756A8C","#037D64","#CEB08E"))+
  theme(text=element_text(),
        axis.title.y=element_text())+
  theme(
    text = element_text(size = 14))

