


library(data.table)
library(lubridate)
library(ggplot2)


snow <- readRDS("Output/Data/snowgrids.rds")
willowpred <- readRDS("../Hare_food_availability/Output/Data/Willow_avail_prediction.rds")


#round snow depth to nearest cm
snow[, Snow := round(SD)]

test <- snow[1:1]
snowdepth <- test$Snow
dat <- willowpred[Snow == snowdepth]
test[, willow := sum(dat$pred)]



get_willow <- function(xvar){
  snowdepth <- as.numeric(xvar)
  dat <- willowpred[Snow %in% snowdepth]
  #return(sum(dat$pred)) THIS IS WRONG
  return(dat)
}

get_willow(xvar = test$Snow)

