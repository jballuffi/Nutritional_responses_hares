#script for comparing modelled snow data from Nico with Boutin lab snow data


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
snow <- readRDS("Output/Data/snowgrids.rds")
pred <- readRDS("../Hare_food_availability/Output/Data/snowdepth_predictions.rds")



# Pull in data from Nico's snow model and compare -------------------------

#read in Nico's long term snow model
longsnow <- fread("Input/Downscaled_SnowDepth_DecToMar.csv")
longsnow[, date := mdy(date)]
setnames(longsnow, "snow_depth", "SD")

#categorize fixes into winters
longsnow[month(date) > 10, winter := paste0(year(date), "-", year(date) + 1)]
longsnow[month(date) < 4, winter := paste0(year(date) - 1, "-", year(date))]

#model is in m, convert to cm
longsnow[, SD := SD*100]

#replace the word "hare" with nico
longsnow[, grid := sub("Hare", "Nico", grid)]

longsnow[, source := "modelled"]

#set up real data from 2014-2022
setnames(snow, c("Date", "snowgrid"), c("date", "grid"))
snow <- snow[, .(winter, grid, date, SD)]
snow <- snow[month(date) == 1 | month(date) == 2 | month(date) == 3 | month(date) == 12]
snow[, source := "measured"]

#merge length wise
longtest <- rbind(longsnow, snow)


ggplot(longtest[year(date) > 2014 & year(date) < 2023])+
  geom_line(aes(x = date, y = SD, group = grid, color = grid, linetype = source), size = 1)+
  facet_wrap(~ winter, scale = "free")+
  themepoints


snow <- snow[grid == "Agnes" | grid == "Kloo"]
snow[grid == "Agnes", grid := "Chitty"]
snow[grid == "Kloo", grid := "Sulphur"]

longsnow[, grid := sub(" Nico", "", grid)]
longsnow <- longsnow[grid == "Sulphur" | grid == "Chitty"]


widetest <- merge(snow, longsnow, by = c("date", "grid", "winter"), all.x = TRUE)
setnames(widetest, c("SD.x", "SD.y"), c("SD_measured", "SD_modelled"))

ggplot(widetest)+
  geom_point(aes(x = SD_measured, y = SD_modelled, color = grid))+
  geom_smooth(aes(x = SD_measured, y = SD_modelled, color = grid), method = "lm")+
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2)+
  ylim(0, 220)+
  xlim(0, 100)+
  themepoints

summary(lm(SD_modelled ~ SD_measured, widetest))

