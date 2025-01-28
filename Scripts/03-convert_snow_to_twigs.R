
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

#make month col
food[, m := month(date)]

#use november to march to control for different sampling periods between winters
food <- food[m > 10 | m < 4]



# Get info for each winter and snow grid ------------------------------------------------

#get mean snow depth and willow availability by winter and grid
wfood <- food[, .(snow.avg = mean(snow), snow.max = max(snow), biomass.avg = mean(biomassavail)), by = .(winter, snowgrid)]

#pull out just year
wfood[, year := tstrsplit(winter, "-", keep = 1)]
wfood[, year := as.integer(year)]

#get mean snow depth and willow availabitliy by month and grid
mfood <- food[, .(snow.avg = mean(snow), snow.max = max(snow), biomass.avg = mean(biomassavail)), by = .(winter, m, snowgrid)]

#get daily snow for grids
dsnow <- food[, .(snow = mean(snow)), .(date, winter, snowgrid)]



# Figures and save --------------------------------------------------------

saveRDS(wfood, "Output/Data/snow_food_winter.rds")
saveRDS(mfood, "Output/Data/snow_food_monthly.rds")
saveRDS(food, "Output/Data/snow_food_daily.rds")
