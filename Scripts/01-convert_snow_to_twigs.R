
#Script takes daily snow depths and converts it to willow availability from an external project


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
snow <- readRDS("Output/Data/snowgrids.rds")
pred <- readRDS("../Willow_twigs_snowdepth/Output/Data/05_willow_biomass_prediction.rds")



# merge real snow data with prediction for willow availability  -----------

#make snow col lowercase
setnames(pred, "Snow", "snow")

#round snow depth to nearest cm
snow[, snow := round(SD)][, SD := NULL]

#merge food predictions with snow data
food <- merge(snow, pred, by = "snow")

food[, Date := ymd(Date)]

food123 <- food[month(Date) == 1 | month(Date) == 2 | month(Date) == 3]

food123 <- food123[!winter == "2014-2015"]

ggplot(food123)+
  geom_boxplot(aes(x = winter, y = snow))+
  labs(y = "mean snow depth (cm)")+
  theme_minimal()

ggplot(food123)+
  geom_boxplot(aes(x = winter, y = biomassavail))+
  labs(y = "Available willow biomass (g/m2)")+
  theme_minimal()



saveRDS(food, "Output/Data/snow_and_food.rds")
