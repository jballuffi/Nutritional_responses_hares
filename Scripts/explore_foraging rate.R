
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")



# merge info and make a control and food add dataset --------

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]




# for conceptual diagram --------------------------------------------------

(concept <- ggplot(forag[year == 2018])+
  geom_path(aes(x = date, y = forage, group = id))+
  #geom_point(aes(x = date, y = forage), shape = 1, size = 2)+
  labs(x = "Date", y = "Foraging effort (hr/day)")+
  theme_minimal(base_size = 14))





# Post hoc figure  --------------------------------------------------------

#foraging and temperature
#2018 had fluctuating temperature and a sudden drop in available biomass
#and we can see week to week fluctuation in foraging rates with an overall decline once biomass
#drops in availability

fortemp <- ggplot(foragcon[year == 2017])+
  geom_path(aes(x = date, y = forage, group = id), alpha = 0.3)+
  geom_point(aes(x = date, y = forage, color = temp), size = 2)+
  scale_color_gradient(low = "skyblue", high = "darkblue", guide = NULL)+
  labs(x = "Date", y = "Foraging effort (hr/day)", subtitle = "A)")+
  themepoints

forbio <- ggplot(foragcon[year == 2017])+
  geom_path(aes(x = date, y = forage, group = id), alpha = 0.3)+
  geom_point(aes(x = date, y = forage, color = biomass), size = 2)+
  scale_color_gradient(low = "lightgreen", high = "darkgreen", guide = NULL)+
  labs(x = "Date", y = "Foraging effort (hr/day)", subtitle = "B)")+
  themepoints

temp <- ggplot(dat[year == 2017])+
  geom_path(aes(x = date, y = temp, group = 1), alpha = 0.3)+
  geom_point(aes(x = date, y = temp, color = temp), size = 2)+
  scale_color_gradient(low = "skyblue", high = "darkblue", guide = NULL)+
  labs(x = "Date", y = "Temperature (C)", subtitle = "C)")+
  themepoints

biomass <- ggplot(dat[year == 2017])+
  geom_path(aes(x = date, y = biomass, group = 1), alpha = 0.3)+
  geom_point(aes(x = date, y = biomass, color = biomass), size = 2)+
  scale_color_gradient(low = "lightgreen", high = "darkgreen", guide = NULL)+
  labs(x = "Date", y = "Soluble biomss (kg/ha)", subtitle = "D")+
  themepoints


(fullplot <- ggarrange(fortemp, forbio, temp, biomass, ncol = 2, nrow = 2))


ggsave("Output/Figures/PostHoc_figure.jpeg", fullplot, width = 7, height = 7, unit = "in")
ggsave("Output/Figures/conceptual_figure.jpeg", concept, width = 5, height = 5, unit = "in")

