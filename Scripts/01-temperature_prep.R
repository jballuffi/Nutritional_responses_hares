
# Script to collect weather data for daily temperatures taken at Burwash airport weather station
# downloaded from https://www.canada.ca/en/services/environment/weather/data-research.html

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# read in weather data ----------------------------------------------------

#get all the file names in the weather folder
weatherfiles <- dir("Input/Weather/", "*csv", full.names = TRUE)

#fread all files
ls.weather <- lapply(weatherfiles, fread)

#rbind the list of read files
weather <- rbindlist(ls.weather, fill = TRUE, use.names = TRUE)



# clean and prep data -----------------------------------------------------

#take only the columns of interest and rename
weather <- weather[, .SD, .SDcols = c(5, 14, 26, 10, 12)]
names(weather) <- c("date", "temp", "snow_station", "tempmax", "tempmin")

#fix dates
weather[, date := ymd(date)]
weather[, m := month(date)]

#cut to just january to march
winter <- weather[m == 1|m == 2|m == 3]

winter[, temp := round(temp, 1)]

#take just the date and mean temp
winter2 <- winter[, .(date, temp)]



# make relationship between temperature and  -----------------------------------------------

#make a sequence of temperatures
metab <- data.table(
  temp = seq(-40, 15, 0.1)
)

#based on sheriff's paper, calculate metabolism based on ambient temp
metab[temp > -10, VO := 976.68] #resting metabolic rate in the thermal neutral zone
metab[temp <= -10, VO := 686.14 - 30.897*temp] #the increase in metabolic rate below thermal neutral zone

#make a figure showing the relationship between temp and metabolism
ggplot(metab)+
  geom_line(aes(x = temp, y = VO), linewidth = 1)+
  labs(x = "Ambient temperature (C)", y = "Oxygen consumption (ml/hr)")+
  themepoints



# final data and save -----------------------------------------------------

#merge metabolic prediction with real temp data
dat <- merge(winter2, metab, by = "temp", all.x = TRUE)

saveRDS(dat, "Output/Data/temperature_prepped.rds")


