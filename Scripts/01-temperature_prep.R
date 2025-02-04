
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
weather <- weather[, .SD, .SDcols = c(5, 10, 12, 14, 26)]
names(weather) <- c("date", "tempmax", "tempmin", "tempmean", "snow")

#get month and year
weather[, m := month(date)]
weather[, year := year(date)]

#cut to just january to march
winter <- weather[m == 1|m == 2|m == 3]

saveRDS(winter, "Output/Data/temperature_prepped.rds")

