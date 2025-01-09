##Carbonate chemistry plots for PCSGA 2024
carbdat <- read.csv(here("Data", "Carbchem_09092024.csv")) %>% dplyr::select(Year, Month, Site, spec_pH)
#Get the means from each pH bottle
carbdat_summary <- carbdat %>% group_by(Site, Month, Year) %>% 
  summarise(meanpH = mean(spec_pH))
#Get each month as an actuial date to plot
carbdat_summary$Month_no <- match(carbdat_summary$Month, month.name)
carbdat_summary$month_year <- as.Date(with(carbdat_summary, paste(Year, Month_no, "1", sep = "-")), format = "%Y-%m-%d")
carbdat_summary$Year <- as.factor(carbdat_summary$Year)

carbdat$Year <- as.factor(carbdat$Year)
carbdat$Site <- as.factor(carbdat$Site)
#Plot them
#carbdat_summary %>% ggplot(aes(x=month_year, y = meanpH, colour = Site))+geom_line()
carbdat_summary %>% filter(Month != "May") %>% filter(Month != "February") %>% 
  ggplot(aes(x=Site, y = meanpH, fill = Year))+geom_boxplot()+theme_classic()+ylab("pH")
  




averageDO %>% 
  filter(date > "2023-06-05") %>% #filter(date < "2023-10-17") %>% 
  ggplot(aes(x= date , y = meanDO, colour = Site)) + geom_line(size=1)+
  theme_bw()+ylab("Daily Mean Dissolved Oxygen (mg / L)")+xlab("Date")+
  scale_color_manual(values=c("#B3C863","#756A8C","#037D64","#CEB08E"))+
  theme(axis.title.y=element_text())
