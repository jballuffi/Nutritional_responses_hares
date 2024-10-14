


library(data.table)
library(lubridate)
library(ggplot2)

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


saveRDS(food, "Output/Data/snow_and_food.rds")