
#Script takes daily snow depths and converts it to willow availability from an external project

#get means and sd for a table

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in snow to willow predictions
pred <- readRDS("../Willow_twigs_snowdepth/Output/Data/05_willow_biomass_prediction.rds")
snow <- readRDS("Output/Data/snow_prepped.rds")
temp <- readRDS("Output/Data/temperature_prepped.rds")
density <- readRDS("Output/Data/hares_daily.rds")



# merge snow data with prediction for willow availability  -----------

#make snow col lowercase
setnames(pred, "Snow", "snow")

#merge food predictions with snow data
food <- merge(snow, pred[, 1:2], by = "snow")

#biomass of available willow is in g/m2
#convert to kg/hectare by multiplying by 10
food[, biomassavail := biomassavail*10]



# Get snow and twig availability daily ------------------------------------------------

#get mean snow depth and willow availability by winter averaged across grids
dfood <- food[, .(snow = mean(snow), twig = mean(biomassavail)), by = .(date, m, year, winter)]



# merge with density and snow data ----------------------------------------------------

#merge daily snow and food data with density
daily <- merge(dfood, density[, 4:7], by = c("date"), all = TRUE)

#calculate the per capita twig availability
daily[, twigpergrid := twig*36 ] #kg/grid
daily[, harespergrid := haredensity*36] #hares/grid
daily[, percap := twigpergrid/harespergrid] #kg/hare

#merge with temp data
daily <- merge(daily, temp[, 1:3], by = "date", all = TRUE)

#get annual means for each value of interest
annual <- daily[, .(phase = getmode(phase),
                    snow = mean(snow),
                    twig = mean(twig),
                    haredensity = mean(haredensity),
                    mortrate = mean(mortrate),
                    percap = mean(percap),
                    tempmean = mean(tempmean, na.rm = TRUE),
                    snow_station = mean(snow_station, na.rm = TRUE)),
                by = .(year)]



# Figures and save --------------------------------------------------------

saveRDS(annual, "Output/Data/full_data_annual.rds")
saveRDS(daily, "Output/Data/full_data_daily.rds")
