
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")



# merge info and make a control and food add dataset --------

#merge by week and snow grid
forag1 <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)
setorder(forag1, id, date)

#get controls only
foragcon <- forag1[food == "Control"]



# prep foraging data for conceptual diagram -------------------------------

#cut to just 2018
foragconcept <- forag1[year == 2018]

#get the week prior by ID
foragconcept[, weekprior := shift(week, 1, type = "lag"), id]

#take difference between current week and last week
foragconcept[, weekdiff := week - weekprior]

#pull individuals that had a gap between weeks
gapids <- foragconcept[weekdiff > 1, id]

#remove those individuals
foragconcept <- foragconcept[!id %in% gapids]

#get length of weeks by id
foragconcept[, sample := length(forage), id]

#take only individuals with four or more weeks
foragconcept <- foragconcept[sample > 3]



# for conceptual diagram --------------------------------------------------

(concept <- ggplot(foragconcept)+
  geom_path(aes(x = week, y = forage, group = id))+
  labs(x = "Date", y = "Foraging effort (hr/day)")+
  theme_pubr(base_size = 16))



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
  themethesisright

forbio <- ggplot(foragcon[year == 2017])+
  geom_path(aes(x = date, y = forage, group = id), alpha = 0.3)+
  geom_point(aes(x = date, y = forage, color = biomass), size = 2)+
  scale_color_gradient(low = "lightgreen", high = "darkgreen", guide = NULL)+
  labs(x = "Date", y = "Foraging effort (hr/day)", subtitle = "B)")+
  themethesisright

temp <- ggplot(dat[year == 2017])+
  geom_path(aes(x = date, y = temp, group = 1), alpha = 0.3)+
  geom_point(aes(x = date, y = temp, color = temp), size = 2)+
  scale_color_gradient(low = "skyblue", high = "darkblue", guide = NULL)+
  labs(x = "Date", y = "Temperature (°C)", subtitle = "C)")+
  themethesisright

biomass <- ggplot(dat[year == 2017])+
  geom_path(aes(x = date, y = biomass, group = 1), alpha = 0.3)+
  geom_point(aes(x = date, y = biomass, color = biomass), size = 2)+
  scale_color_gradient(low = "lightgreen", high = "darkgreen", guide = NULL)+
  labs(x = "Date", y = "Soluble biomss (kg/ha)", subtitle = "D")+
  themethesisright


(fullplot <- ggarrange(fortemp, forbio, temp, biomass, ncol = 2, nrow = 2))



ggsave("Output/Figures/PostHoc_figure.jpeg", fullplot, width = 7, height = 7, unit = "in")
ggsave("Output/Figures/conceptual_figure.jpeg", concept, width = 5, height = 5, unit = "in")
