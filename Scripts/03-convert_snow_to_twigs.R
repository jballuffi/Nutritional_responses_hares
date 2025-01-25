
#Script takes daily snow depths and converts it to willow availability from an external project


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in snow to willow predictions
pred <- readRDS("../Willow_twigs_snowdepth/Output/Data/05_willow_biomass_prediction.rds")
snow <- readRDS("Output/Data/")


# merge snow data with prediction for willow availability  -----------

#merge food predictions with snow data
food <- merge(snow, pred, by = "snow")

food[, Date := ymd(Date)]

food <- food[!winter == "2014-2015"]

#use november to march to control for different sampling periods between winters
food <- food[month(Date) > 10 | month(Date) < 4]



# Get info for each winter and snow grid ------------------------------------------------

#get mean snow depth and willow availability by winter
wsnow <- food[, .(snow.avg = mean(snow), snow.max = max(snow), biomass.avg = mean(biomassavail)), by = .(winter, snowgrid)]

#get daily snow across grids
dsnow <- food[, .(snow = mean(snow)), .(Date, winter, snowgrid)]

#get number of days where snow was greater than 45 cm by winter
deepdays <- dsnow[snow > 45, .(deepdays = .N), by = .(winter, snowgrid)]

#merge deep days with averages
allwsnow <- merge(wsnow, deepdays, by = c("winter", "snowgrid"), all.x = TRUE)

#where deep days is NA make 0
allwsnow[is.na(deepdays), deepdays := "0"]




# Figures and save --------------------------------------------------------


snowplot <- 
  ggplot(dsnow)+
  geom_line(aes(x = Date, y = snow, color = snowgrid))+
  labs(y = "Snow depth (cm)")+
  facet_wrap(~winter, scales = "free")+
  themepoints

ggplot(food)+
  geom_boxplot(aes(x = winter, y = snow))+
  labs(y = "mean snow depth (cm)")+
  theme_minimal()

ggplot(food)+
  geom_boxplot(aes(x = winter, y = biomassavail))+
  labs(y = "Available willow biomass (g/m2)")+
  theme_minimal()


ggsave("Output/Figures/snow_annual.jpeg", snowplot, width = 10, height = 8, unit = "in")
saveRDS(allwsnow, "Output/Data/annual_snow_conditions.rds")
saveRDS(food, "Output/Data/snow_and_food.rds")
