
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


density <- readRDS("Output/Data/hare_population.rds")
food <- readRDS("Output/Data/food_adds.rds")
wloss <- readRDS("Output/Data/weight_change.rds")
snow <- readRDS("Output/Data/snow_and_food.rds")
fecal <- readRDS("Output/Data/CP_results_cleaned.rds")



# get density and snow stats by winter -------------------------------------------------------

#remove 2014-2015 winter
wloss <- wloss[!winter == "2014-2015"]

#take the phase and mean density for each winter
wdensity <- density[, .(phase = getmode(phase), densityavg = mean(haredensity)), winter]

#remove 2014/2015 winter
snow <- snow[!winter == "2014-2015"]

#use november to march to control for different sampling periods between winters
snow <- snow[month(Date) > 10 | month(Date) < 4]

#get mean snow depth and willow availability by winter
wsnow <- snow[, .(snowavg = mean(snow), snowmax = max(snow), biomassavg = mean(biomassavail)), by = .(winter, snowgrid)]

#get daily snow across grids
dsnow <- snow[, .(snow = mean(snow)), .(Date, winter, snowgrid)]

ggplot(dsnow)+
  geom_line(aes(x = Date, y = snow, color = snowgrid))+
  facet_wrap(~winter, scales = "free")

#get number of days where snow was greater than 45 cm by winter
deepdays <- dsnow[snow > 45, .(deepdays = .N), by = .(winter, snowgrid)]

#merge deep days with averages
allwsnow <- merge(wsnow, deepdays, by = c("winter", "snowgrid"), all.x = TRUE)

#where deep days is NA make 0
allwsnow[is.na(deepdays), deepdays := 0]



# merge in winter stats with weight loss ----------------------------------

wloss <- merge(wloss, allwsnow, by = c("winter", "snowgrid"), all.x = TRUE)


ggplot(wloss[food == 0])+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_point(aes(x = snowavg, y = weight.s))+
  geom_smooth(aes(x = snowavg, y = weight.s), method = "lm")
