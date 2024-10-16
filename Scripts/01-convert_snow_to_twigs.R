
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
snow <- readRDS("Output/Data/snowgrids.rds")
pred <- readRDS("../Hare_food_availability/Output/Data/snowdepth_predictions.rds")

#round snow depth to nearest cm
snow[, Snow := round(SD)]

#cut just willow
predwillow <- pred[species == "willow", .(Snow, 
                                          wbiomass = biomassavail, 
                                          wprop = propavail, 
                                          wCPcomp = CPavail_comp)]

#cut just spruce
predspruce <- pred[species == "spruce", .(Snow, 
                                          sbiomass = biomassavail, 
                                          sprop = propavail, 
                                          sCPcomp = CPavail_comp)]

#merge food predictions with snow data
food <- merge(snow, predwillow, by = "Snow")
food <- merge(food, predspruce, by = "Snow")



food123 <- food[month(Date) == 1 | month(Date) == 2 | month(Date) == 3]

food123 <- food123[!winter == "2014-2015"]

ggplot(food123)+
  geom_boxplot(aes(x = winter, y = Snow))+
  labs(y = "mean snow depth (cm)")+
  theme_minimal()

ggplot(food123)+
  geom_boxplot(aes(x = winter, y = sbiomass))+
  labs(y = "Available spruce twigs (g/m2)")+
  theme_minimal()

ggplot(food123)+
  geom_boxplot(aes(x = winter, y = wbiomass))+
  labs(y = "Available willow twigs (g/m2)")+
  theme_minimal()




saveRDS(food, "Output/Data/snow_and_food.rds")
