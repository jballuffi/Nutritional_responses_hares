
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


density <- readRDS("../HR_PopCycle_SnowshoeHares/output/results/dailyharedensities.rds")
food <- readRDS("Output/Data/food_adds.rds")
wloss <- readRDS("Output/Data/weight_change.rds")
snow <- readRDS("Output/Data/snow_and_food.rds")
fecal <- readRDS("Output/Data/CP_results_cleaned.rds")


# merge food add and weight loss --------------------------------------------------

#merge files
wloss <- merge(wloss, food, by = c("id", "winter"), all.x = TRUE)

#fill in empty food column
wloss[is.na(food), food := "0"]

#remove 2014-2015 winter
wloss <- wloss[!winter == "2014-2015"]



# get density and snow stats by winter -------------------------------------------------------

#take the phase and mean density for each winter
wdensity <- density[, .(phase = getmode(phase), densityavg = mean(haredensity)), winter]

#remove 2014/2015 winter
snow <- snow[!winter == "2014-2015"]

#use november to march to control for different sampling periods between winters
snow <- snow[month(Date) > 10 | month(Date) < 4]

#get mean snow depth and willow availability by winter
wsnow <- snow[, .(snowavg = mean(snow), biomassavg = mean(biomassavail)), winter]

#get daily snow across grids
dsnow <- snow[, .(snow = mean(snow)), .(Date, winter)]

ggplot(dsnow)+
  geom_line(aes(x = Date, y = snow))+
  facet_wrap(~winter, scales = "free")

#get number of days where snow was greater than 45 cm by winter
deepdays <- dsnow[snow > 45, .(deepdays = .N), winter]

#merge deep days with averages
wsnow <- merge(wsnow, deepdays, by = "winter", all.x = TRUE)

#where deep days is NA make 0
wsnow[is.na(deepdays), deepdays := 0]



# merge in winter stats with weight loss ----------------------------------





#weight loss
ggplot(wloss)+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_boxplot(aes(x = winter, y = wchange, fill = food), alpha = .7)+
  themepoints

#spring weights
ggplot(wloss)+
  geom_boxplot(aes(x = winter, y = weight.s, fill = food), alpha = .7)+
  themepoints
