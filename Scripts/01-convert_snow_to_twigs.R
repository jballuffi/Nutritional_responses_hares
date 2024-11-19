
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
snow <- readRDS("Output/Data/snowgrids.rds")
pred <- readRDS("../Willow_twigs_snowdepth/Output/Data/05_willow_avail_prediction.rds")



#round snow depth to nearest cm
snow[, Snow := round(SD)]

#merge food predictions with snow data
food <- merge(snow, pred, by = "Snow")



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
