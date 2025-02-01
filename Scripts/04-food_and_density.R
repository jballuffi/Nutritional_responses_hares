
#Script takes daily snow depths and converts it to willow availability from an external project


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in snow to willow predictions
pred <- readRDS("../Willow_twigs_snowdepth/Output/Data/05_willow_biomass_prediction.rds")
snow <- readRDS("Output/Data/snow_prepped.rds")
ddensity <- readRDS("Output/Data/hares_daily.rds")
wdensity <- readRDS("Output/Data/hares_annual.rds")



# merge snow data with prediction for willow availability  -----------

#make snow col lowercase
setnames(pred, "Snow", "snow")

#merge food predictions with snow data
food <- merge(snow, pred, by = "snow")



# Get info for each winter and snow grid ------------------------------------------------

#get mean snow depth and willow availability by winter averaged across grids
wfood <- food[, .(snow.avg = mean(snow), snow.max = max(snow), biomass.avg = mean(biomassavail)), by = .(winter, year)]


#get daily snow averaged across grids
dsnow <- food[, .(snow = mean(snow)), .(date, winter, year, m)]



# merge with densities ----------------------------------------------------





# Figures and save --------------------------------------------------------

saveRDS(wfood, "Output/Data/snow_food_winter.rds")
saveRDS(mfood, "Output/Data/snow_food_monthly.rds")
saveRDS(food, "Output/Data/snow_food_daily.rds")
