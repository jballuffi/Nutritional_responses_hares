#daily response part

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
foraging <- readRDS("Output/Data/foraging_daily.rds")
twigs <- readRDS("Output/Data/snow_food_daily.rds")
density <- readRDS("Output/Data/hares_daily.rds")

#this will be broken ^correct data up here but rest of script needs to be fixed

# Calculate snow fall -----------------------------------------------------

#order twigs and snow by date and grid
twigs <- twigs[order(snowgrid, Date)]

#make new column for previous snow day within grid and winter
twigs[, prevdate := shift(Date, n = 1, type = "lag"), by = .(snowgrid, winter)]

#difference between current date and the previous date
twigs[, daydiff := as.integer(Date - prevdate)]
twigs[, unique(daydiff)]

#get previous date's snow depth
twigs[, lagsnow := shift(snow, n = 1, type = "lag"), by = .(snowgrid, winter)]

#get difference in snow between current date and previous date
#get rate of snow fall by dividing by the difference in days (max 3 right now)
twigs[, snowfall := (snow - lagsnow)/daydiff, by = .(snowgrid, winter)]



# data merging ------------------------------------------------------------

ind <- ind[, .(id, winter, food)]

#merge food add and individual info
foraging <- merge(foraging, ind, by = c("id", "winter"), all.x = TRUE)

#merge daily snow data with foraging rates
dt <- merge(foraging, twigs, by = c("Date", "snowgrid", "y", "winter"), all.x = TRUE)

#cut a few columns out of daily density
density <- density[, .(date, phase, haredensity)] 

#merge in density data
dt <- merge(dt, density, by.x = "Date", by.y = "date", all.x = TRUE)

dt[, dforag := Forage/3600]


# look into what determines foraging rate ---------------------------------

summary(lm(dforag ~ haredensity, dt))

(sdforage <-
  ggplot(dt)+
  geom_point(aes(x = snow, y = dforag, color = food), alpha = .1)+
  geom_smooth(aes(x = snow, y = dforag, color = food), method = "lm")+
  labs(x = "Daily snow depth", y = "Daily foraging effort (hr)")+
  themepoints)

(taforage <-
  ggplot(dt)+
  geom_point(aes(x = biomassavail, y = dfora, color = food), alpha = .1)+
  geom_smooth(aes(x = biomassavail, y = dfora, color = food), method = "lm")+
  labs(x = "Daily willow biomass available (g/m2)", y = "Daily foraging effort (hr)")+
  themepoints)

(hdforage <-
    ggplot(dt)+
    geom_point(aes(x = haredensity, y = dfora, color = food), alpha = .1)+
    geom_smooth(aes(x = haredensity, y = dfora, color = food), method = "lm")+
    labs(x = "Hare density (ind/ha)", y = "Daily foraging effort (hr)")+
    themepoints)




# how do hares change their foraging behaviour with snow fall even --------



snowdates <- dt[snowfall > 5, unique(Date)]



(sfforage <- 
    ggplot(dt[snowfall > -5])+
    geom_point(aes(x = snowfall, y = dforag, color = food), alpha = .1)+
    geom_smooth(aes(x = snowfall, y = dforag, color = food), method = "lm")+
    labs(x = "Daily snowfall (cm)", y = "Daily foraging effort (hr)")+
    themepoints)




ggsave("Output/Figures/foraging_snowdepth.jpeg", sdforage, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/foraging_snowfall.jpeg", sfforage, width = 6, height = 4, unit = "in")
