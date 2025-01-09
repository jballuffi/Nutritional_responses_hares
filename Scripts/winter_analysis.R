
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



# merging data ----------------------------------

#merge weight change with snow data
dat <- merge(wloss, allwsnow, by = c("winter", "snowgrid"), all.x = TRUE)

#merge weight and snow data with annual demographic data
dat <- merge(dat, wdensity, by = c("winter"), all.x = TRUE)

#fix phase
dat[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]


fdat <- merge(fecal, allwsnow,  by = c("winter", "snowgrid"), all.x = TRUE)
fdat <- merge(fdat, wdensity,  by = c("winter"), all.x = TRUE)

#remove datum that is very high
fdat <- fdat[!CP_dm > 20]

fdat[, id := as.factor(id)]

#merge fecal with weight change

weightcut <- wloss[, .(id, winter, weight.s, wchange)]
fweight <- merge(fdat, weightcut, by = c("id", "winter"), all.x = TRUE)



# Figures -----------------------------------------------------------------


    ### WEIGHT CHANGE 

#weight change in response to max snow with food
WC1 <- ggplot(dat[sex == "female"])+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_point(aes(x = snowmax, y = wchange, color = food))+
  geom_smooth(aes(x = snowmax, y = wchange, color = food), method = "lm")+
  labs(x = "Maximum snow depth (cm)", y = "Weight change (g)")+
  themepoints

#weight change in response to avg snow, no food
WC2 <- ggplot(dat[food == 0])+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_point(aes(x = snowavg, y = wchange))+
  geom_smooth(aes(x = snowavg, y = wchange))+
  labs(x =  "Average snow depth (cm)", y = "Weight change (g)")+
  xlim(14, 50)+
  themepoints

#weight change in response to population density no food
WC3 <- ggplot(dat[food == 0])+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_point(aes(x = densityavg, y = wchange, color = sex))+
  geom_smooth(aes(x = densityavg, y = wchange, color = sex), method = "lm")+
  themepoints

summary(lm(wchange ~ phase*sex, dat))

#weight change in response to phase and sex, no food
WC4 <- ggplot(dat[food == 0])+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_boxplot(aes(x = phase, y = wchange, fill = sex), alpha = 0.5)+
  labs(x = "Cyle phase", y = "Weight change (g)")+
  themepoints


      ### FECAL PROTEIN

#fecal protein in response to avg snow depth (same as max snow depth)
FP1 <- ggplot(fdat)+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_point(aes(x = snowavg, y = CP_dm, color = Food))+
  geom_smooth(aes(x = snowavg, y = CP_dm, color = Food), method = "lm")+
  labs(x = "Average snow depth (cm)", y = "Fecal protein (%)")+
  facet_wrap(~ m)+
  themepoints

FP2 <- ggplot(fdat)+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_point(aes(x = densityavg, y = CP_dm, color = Food))+
  geom_smooth(aes(x = densityavg, y = CP_dm, color = Food), method = "lm")+
  labs(x = "Average hare density", y = "Fecal protein (%)")+
  facet_wrap(~ m)+
  themepoints


ggplot(fweight[m == 3])+
  geom_point(aes(x = CP_dm, y = wchange))+
  geom_smooth(aes(x = CP_dm, y = wchange))




ggsave("Output/Figures/snow_annual.jpeg", snowplot, width = 10, height = 8, unit = "in")

ggsave("Output/Figures/WC1.jpeg", WC1, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/WC2.jpeg", WC2, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/WC3.jpeg", WC3, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/WC4.jpeg", WC4, width = 6, height = 4, unit = "in")

ggsave("Output/Figures/FP1.jpeg", FP1, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/FP2.jpeg", FP2, width = 6, height = 4, unit = "in")

