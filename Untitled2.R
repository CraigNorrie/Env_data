##script to plot mortalities

#read in the data
mortdat <- read.csv("C:/Users/Craig Norrie/OneDrive - UW/Field study/Oyster data/MAY_AUG_2023_morts.csv")
str(mortdat)
mortdat$Site <- as.factor(mortdat$Site)
mortdat$Colour <- as.factor(mortdat$Colour)
mortdat$Month <- as.factor(mortdat$Month)
mortdat %>% ggplot(aes(x=Month, y = Morts, fill = Colour))+geom_bar(stat="identity", position=position_dodge())+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+ylab("Number of Mortalities")+xlab("Month")+
  facet_grid(~Site)+theme_bw()


