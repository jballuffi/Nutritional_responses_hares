
#Script takes daily snow depths and converts it to willow availability from an external project

#get means and sd for a table

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in snow to willow predictions
pred <- readRDS("../Willow_twigs_snowdepth/Output/Data/05_willow_prediction.rds")
snow <- readRDS("Output/Data/snow_prepped.rds")
temp <- readRDS("Output/Data/temperature_prepped.rds")
density <- readRDS("Output/Data/hares_daily.rds")



# merge snow data with prediction for willow availability  -----------

#make snow col lowercase
setnames(pred, "Snow", "snow")

#merge food predictions with snow data
food <- merge(snow, pred[, .(snow, biomassavail, NDSavail_comp)], by = "snow")

#biomass of available willow is in g/m2
#convert to kg/hectare by multiplying by 10
food[, biomassavail := biomassavail*10]

#convert biomass into soluble biomass
food[, digbiomass := biomassavail*(NDSavail_comp/100)]



# merge with density and snow data ----------------------------------------------------

#merge daily snow and food data with density
daily <- merge(food, density[, 4:7], by = c("date"), all = TRUE)

#calculate the per capita twig availability
daily[, twigpergrid := digbiomass*36 ] #kg/grid
daily[, harespergrid := haredensity*36] #hares/grid
daily[, percap := twigpergrid/harespergrid] #kg/hare

#merge with temp data
daily <- merge(daily, temp, by = "date", all = TRUE)

#make a week column
daily[, week := week(date), year]

#make a year factor col
daily[, yearfactor := as.factor(year)]



# make final data ---------------------------------------------------------

#cut out years without data
daily <- daily[year > 2015 & !year == 2022]

#take variables of interest
dat <- daily[, .(snowgrid, date, week, year, yearfactor, snow, haredensity, biomass = digbiomass, percap, mortrate, temp, VO)]

datnogrid <- dat[, .(snow = mean(snow), 
                     haredensity = mean(haredensity), 
                     biomass = mean(biomass), 
                     percap = mean(percap), 
                     mortrate = mean(mortrate), 
                     temp = mean(temp, na.rm = TRUE),
                     VO = mean(VO)),
                 by = .(date, year, yearfactor)]

datweek <- dat[, .(date = min(date),
                   snow = mean(snow),
                   haredensity = mean(haredensity),
                   mortrate = mean(mortrate),
                   biomass = mean(biomass),
                   percap = mean(percap),
                   temp = mean(temp, na.rm = TRUE),
                   VO = mean(VO)),
               by = .(year, yearfactor, week, snowgrid)]

datweeknogrid <- dat[, .(date = min(date),
                         snow = mean(snow),
                         haredensity = mean(haredensity),
                         mortrate = mean(mortrate),
                         biomass = mean(biomass),
                         percap = mean(percap),
                         temp = mean(temp, na.rm = TRUE),
                         VO = mean(VO)),
               by = .(year, yearfactor, week)]



# Figures and save --------------------------------------------------------

setorder(dat, date)
setorder(datweek, date)
setorder(datnogrid, date)
setorder(datweeknogrid, date)

saveRDS(dat, "Output/Data/full_data_daily.rds")
saveRDS(datweek, "Output/Data/full_data_weekly.rds")

saveRDS(datnogrid, "Output/Data/full_data_daily_nogrid.rds")
saveRDS(datweeknogrid, "Output/Data/full_data_weekly_nogrid.rds")
