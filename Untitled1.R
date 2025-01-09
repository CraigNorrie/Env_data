##Script to analyse temperature data from oyster field sites
#Load packages
library(tidyverse)
library(lubridate)
library(here)
#read in the data 
thornjun_julydat <- read.csv(here("Data", "Thorndyke_5_june_to_2_aug_2023.csv"))
head(thornjun_julydat)
chelmay_juldat <- read.csv(here("Data", "Chelsea_Farm_10_may_to_2_Aug_2023.csv"))
head(chelmay_juldat)
str(chelmay_juldat)
hoodhead_julydat <- read.csv(here("Data", "Hood_Head_12_may_to_17July.csv"))
manchester_julydat <- read.csv(here("Data", "Manchester_15_april_8_August.csv"))
#Set the first column as date
thornjun_julydat$datetime <- mdy_hm(thornjun_julydat$Date.Time..GMT.07.00)
str(thornjun_julydat)
chelmay_juldat$datetime  <- mdy_hm(chelmay_juldat$Date.Time..GMT.07.00)
str(chelmay_juldat)
hoodhead_julydat$datetime  <- mdy_hm(hoodhead_julydat$Date.Time..GMT.07.00)
manchester_julydat$datetime  <- mdy_hm(manchester_julydat$Date.Time..GMT.07.00)

#add a site columin to each of the DFs
chelmay_juldat <- chelmay_juldat %>% mutate(Site = "Chelsea")
thornjun_julydat <- thornjun_julydat %>% mutate(Site = "Thorndyke")
hoodhead_julydat <- hoodhead_julydat %>% mutate(Site = "Hood_Head")
manchester_julydat <- manchester_julydat %>% mutate(Site = "Manchester")
#add a date column to the two dfs
chelmay_juldat$date <- as.Date(chelmay_juldat$datetime)
thornjun_julydat$date <- as.Date(thornjun_julydat$datetime)
hoodhead_julydat$date <- as.Date(hoodhead_julydat$datetime)
manchester_julydat$date <- as.Date(manchester_julydat$datetime)
#calculate averages
chelav <- chelmay_juldat %>% group_by(date) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE), meanDO=mean(DO_mgL, na.rm = TRUE))
thornav <- thornjun_julydat %>% group_by(date) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE), meanDO=mean(DO_mgL, na.rm = TRUE))
manav <- manchester_julydat %>% group_by(date) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE), meanDO=mean(DO_mgL, na.rm = TRUE))
hoodav <- hoodhead_julydat %>% group_by(date) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE), meanDO=mean(DO_mgL, na.rm = TRUE))
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
alldat <- bind_rows(chelmay_juldat, thornjun_julydat, manchester_julydat, hoodhead_julydat)
allavdat <- bind_rows(chelav, thornav, hoodav, manav)
#plot this
alldat %>% filter(datetime > "2023-06-05 14:37:00") %>% 
  ggplot(aes(x = datetime, y = Temp_C, colour = Site))+geom_line(size=1)+theme_classic()
dailyaverages <- alldat
alldat$date <- as.Date(alldat$datetime) 
averagetemps <- alldat %>% group_by(date, Site) %>% summarise(meantemp=mean(Temp_C, na.rm = TRUE))
averageDO <- alldat %>% group_by(date, Site) %>% summarise(meanDO=mean(DO_mgL, na.rm = TRUE))
#plot the average temps
averagetemps %>% 
  filter(date > "2023-06-05") %>% 
           ggplot(aes(x= date , y = meantemp, colour = Site)) + geom_line(size=1)+
  theme_classic()+ylab("Daily Mean Temperature (°C)")+xlab("Date")
#Plot DO
averageDO %>% 
  filter(date > "2023-06-05") %>% 
  ggplot(aes(x= date , y = meanDO, colour = Site)) + geom_line(size=1)+
  theme_classic()+ylab("Daily Mean Discolved Oxygen (mg l-1)")+xlab("Date")
#####plot cumulative deg days
allavdat %>% ggplot(aes(x=date, y=degdays, colour = Site))+geom_line(size=1)



