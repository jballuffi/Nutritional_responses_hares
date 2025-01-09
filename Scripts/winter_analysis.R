
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
wdensity <- density[, .(phase = getmode(phase),
                        densityavg = mean(haredensity),
                        mortality = mean(mortality, na.rm = TRUE)), winter]

#remove 2014/2015 winter
snow <- snow[!winter == "2014-2015"]

#use november to march to control for different sampling periods between winters
snow <- snow[month(Date) > 10 | month(Date) < 4]

#get mean snow depth and willow availability by winter
wsnow <- snow[, .(snowavg = mean(snow), snowmax = max(snow), biomassavg = mean(biomassavail)), by = .(winter, snowgrid)]

#get daily snow across grids
dsnow <- snow[, .(snow = mean(snow)), .(Date, winter, snowgrid)]

snowplot <- 
  ggplot(dsnow)+
  geom_line(aes(x = Date, y = snow, color = snowgrid))+
  labs(y = "Snow depth (cm)")+
  facet_wrap(~winter, scales = "free")+
  themepoints

#get number of days where snow was greater than 45 cm by winter
deepdays <- dsnow[snow > 45, .(deepdays = .N), by = .(winter, snowgrid)]

deepdays[, deepdays := cut(deepdays, 3)]

#merge deep days with averages
allwsnow <- merge(wsnow, deepdays, by = c("winter", "snowgrid"), all.x = TRUE)

#where deep days is NA make 0
allwsnow[is.na(deepdays), deepdays := "0"]



# merge in winter stats with weight loss ----------------------------------

wloss <- merge(wloss, allwsnow, by = c("winter", "snowgrid"), all.x = TRUE)


ggplot(wloss[sex == "female"])+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_point(aes(x = snowmax, y = wchange, color = food))+
  geom_smooth(aes(x = snowmax, y = wchange, color = food), method = "lm")

ggplot(wloss[sex == "female"])+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_point(aes(x = snowavg, y = wchange, color = food))+
  geom_smooth(aes(x = snowavg, y = wchange, color = food))

ggplot(wloss[sex == "female"])+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_boxplot(aes(x = deepdays, y = wchange, color = food))

