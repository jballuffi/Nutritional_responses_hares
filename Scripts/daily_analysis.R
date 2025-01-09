#daily response part

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
foraging <- readRDS("Output/Data/foraging_rates.rds")
foodadd <- readRDS("Output/Data/food_adds.rds")
twigs <- readRDS("Output/Data/snow_and_food.rds")

#merge food add and foraging
foraging <- merge(foraging, foodadd, by = c("id", "winter"), all.x = TRUE)
foraging[is.na(food), food := "0"]
foraging[, food := as.factor(food)]


#get the lag difference

twigs <- twigs[order(snowgrid, Date)]

twigs[, prevdate := shift(Date, n = 1, type = "lag"), by = .(snowgrid, winter)]
twigs[, daydiff := as.integer(Date - prevdate)]

#twigs <- twigs[daydiff < 4]

twigs[, lagsnow := shift(snow, n = 1, type = "lag"), by = .(snowgrid, winter)]
twigs[, snowfall := (snow - lagsnow)/daydiff, by = .(snowgrid, winter)]




dt <- merge(foraging, twigs, by = c("Date", "snowgrid", "y", "winter"), all.x = TRUE)


sdforage <-
  ggplot(dt)+
  geom_point(aes(x = snow, y = Forage/3600, color = food), alpha = .1)+
  geom_smooth(aes(x = snow, y = Forage/3600, color = food), method = "lm")+
  labs(x = "Daily snow depth", y = "Daily foraging effort (hr)")+
  themepoints


sfforage <- 
  ggplot(dt)+
  geom_point(aes(x = snowfall, y = Forage/3600, color = food), alpha = .1)+
  geom_smooth(aes(x = snowfall, y = Forage/3600, color = food))+
  labs(x = "Daily snowfall (cm)", y = "Daily foraging effort (hr)")+
  themepoints


ggsave("Output/Figures/foraging_snowdepth.jpeg", sdforage, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/foraging_snowfall.jpeg", sfforage, width = 6, height = 4, unit = "in")
