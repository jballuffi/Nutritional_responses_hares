
#Script takes daily snow depths and converts it to willow availability from an external project


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in snow to willow predictions
pred <- readRDS("../Willow_twigs_snowdepth/Output/Data/05_willow_biomass_prediction.rds")
snow <- readRDS("Output/Data/snow_prepped.rds")


# merge snow data with prediction for willow availability  -----------

#make snow col lowercase
setnames(pred, "Snow", "snow")

#merge food predictions with snow data
food <- merge(snow, pred, by = "snow")

#remove winter of 14/15
food <- food[!winter == "2014-2015"]

#use november to march to control for different sampling periods between winters
food <- food[month(date) > 10 | month(date) < 4]



# Get info for each winter and snow grid ------------------------------------------------

#get mean snow depth and willow availability by winter
wsnow <- food[, .(snow.avg = mean(snow), snow.max = max(snow), biomass.avg = mean(biomassavail)), by = .(winter, snowgrid)]

#get daily snow for grids
dsnow <- food[, .(snow = mean(snow)), .(date, winter, snowgrid)]




# Figures and save --------------------------------------------------------


snowplot <- 
  ggplot(dsnow)+
  geom_line(aes(x = date, y = snow, color = snowgrid))+
  labs(y = "Snow depth (cm)")+
  facet_wrap(~winter, scales = "free")+
  themepoints


ggsave("Output/Figures/snow_over_time.jpeg", snowplot, width = 10, height = 8, unit = "in")

saveRDS(wsnow, "Output/Data/annual_snow_conditions.rds")
saveRDS(food, "Output/Data/snow_and_food.rds")
