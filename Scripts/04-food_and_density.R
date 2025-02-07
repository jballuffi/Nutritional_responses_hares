
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



# Get snow and twig availability daily ------------------------------------------------

#get mean snow depth and willow availability by winter averaged across grids
dfood <- food[, .(snow = mean(snow), biomass = mean(biomassavail), quality = mean(NDSavail_comp)), by = .(date, m, year, winter)]



# merge with density and snow data ----------------------------------------------------

#merge daily snow and food data with density
daily <- merge(dfood, density[, 4:7], by = c("date"), all = TRUE)

#calculate the per capita twig availability
daily[, twigpergrid := biomass*36 ] #kg/grid
daily[, harespergrid := haredensity*36] #hares/grid
daily[, percap := twigpergrid/harespergrid] #kg/hare

#merge with temp data
daily <- merge(daily, temp[, 1:3], by = "date", all = TRUE)

#cut out years without data
dat <- daily[year > 2015 & !year == 2022]

#make a year factor col
dat[, yearfactor := as.factor(year)]

#get annual means for each value of interest
annual <- dat[, .(phase = getmode(phase),
                    
                    snow = mean(snow),
                    snow_sd = sd(snow),
                    
                    biomass = mean(biomass),
                    biomass_sd = sd(biomass),
                    
                    quality = mean(quality),
                    quality_sd = sd(quality),
                    
                    haredensity = mean(haredensity),
                    haredensity_sd = sd(haredensity),
                    
                    mortrate = mean(mortrate),
                    mortrate_sd = sd(mortrate),
                    
                    percap = mean(percap),
                    percap_sd = sd(percap),
                    
                    temp = mean(tempmean, na.rm = TRUE),
                    temp_sd = mean(tempmean)),
                by = .(year, yearfactor)]


#get weekly values from full env dat to merge with forag data
dat[, week := week(date), year]

datweek <- dat[, .(haredensity = mean(haredensity),
                   mortrate = mean(mortrate),
                   snow = mean(snow),
                   biomass = mean(biomass),
                   quality = mean(quality),
                   percap = mean(percap),
                   temp = mean(tempmean, na.rm = TRUE)),
               by = .(year, week)]


# Figures and save --------------------------------------------------------

setorder(dat, date)

saveRDS(annual, "Output/Data/full_data_annual.rds")
saveRDS(dat, "Output/Data/full_data_daily.rds")
saveRDS(datweek, "Output/Data/full_data_weekly.rds")
