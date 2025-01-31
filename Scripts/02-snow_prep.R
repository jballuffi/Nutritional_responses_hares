
#script to collect snow data that was measured by the team every day on agnes, jo, and kloo
#and fill in later years with camera data


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

# read in data ------------------------------------------------------------

#list snow files from grid-level measurements and fread
snowfiles <- dir("Input/", "Snow_grid*", full.names = TRUE) 

#fread list of files
ls.snowfiles <- lapply(snowfiles, fread)

#rbindlist with an origin column
snowgrids <- rbindlist(ls.snowfiles, fill = TRUE, use.names = TRUE, idcol = "origin")

#now re-assign the origin column the file names
snowgrids[, origin := factor(origin, labels = basename(snowfiles))]

#data from camera traps
snowcams <- fread("Input/Snow_cameras.csv")



# Create blank list of dates and grids ------------------------------------

#each data table is a list of dates for each grid
AG <- data.table(
    Date = seq.Date(as.Date("2015/02/10"), as.Date("2022/04/30"), "day"),
    snowgrid = "Agnes")
KL <- data.table(
  Date = seq.Date(as.Date("2015/02/10"), as.Date("2022/04/30"), "day"),
  snowgrid = "Kloo")
JO <- data.table(
  Date = seq.Date(as.Date("2015/02/10"), as.Date("2022/04/30"), "day"),
  snowgrid = "Jo")

#rbind all dates together
blanks <- rbind(AG, KL, JO)

#state winter months
wintermonths <- c(11, 12, 1, 2, 3)

#subset to only those winter months
blanks <- blanks[month(Date) %in% wintermonths]



# clean up grid data -----------------------------------------------------------

#redo the grid names
snowgrids[grep("agnes", origin), snowgrid := "Agnes"]
snowgrids[grep("jo", origin), snowgrid := "Jo"]
snowgrids[grep("kloo", origin), snowgrid := "Kloo"]

#fix up other data cols
snowgrids[, COMMENTS := NULL]
snowgrids[, Date := lubridate::dmy(DATE)]
setnames(snowgrids, "OPEN SD", "SD")

#cut to just three columns of interest
gsnow <- snowgrids[, .(Date, snowgrid, SD)]
gsnow[, source := "ground"]
gsnow <- gsnow[!is.na(SD)] #remove no data from grids



# clean up camera trap data --------------------------------------------------

snowcams[, Date := tstrsplit(date_detected, " ", keep = 1)]
snowcams[, Date := mdy(Date)]

csnow <- snowcams[, mean(snow_depth_cm, na.rm = TRUE), by = .(Date, snowgrid)]
setnames(csnow, "V1", "SD")
csnow[, source := "camera"]



# merge data sheets -------------------------------------------------------

#bind camera data to human data
snowfull <- rbind(gsnow, csnow)

#merge with the blank list of dates so we can fill in NA dates
snow <- merge(blanks, snowfull, by = c("Date", "snowgrid"), all = TRUE)

#Order by grid and then date
setorder(snow, snowgrid, Date)

#add winter column
#categorize fixes into winters
snow[month(Date) > 10, winter := paste0(year(Date), "-", year(Date) + 1)]
snow[month(Date) < 4, winter := paste0(year(Date) - 1, "-", year(Date))]



# finish prepping full data ----------------------------------------------------------

#grab only winter
snow <- snow[!is.na(winter)]
snow <- snow[!winter == "NA-NA"]

#see what major chunks of grids/winters are missing
snow[is.na(SD), .N, by = .(snowgrid, winter)]

#fill in missing snow depths with the last value (calls backwards in time)
snow[, SD := nafill(SD, "locf"), by = c("snowgrid", "winter")]

#round snow depth to nearest cm, remove extras
snow[, snow := round(SD)][, SD := NULL][, source := NULL]

#change name of Date
setnames(snow, "Date", "date")

#make year and month columns
snow[, m := month(date)]
snow[, year := year(date)]

#cut to just january to march, and start data on jan 1 of 2016
snow <- snow[m == 1 | m == 2 | m == 3]
snow <- snow[year > 2015]



# save --------------------------------------------------------------------

saveRDS(snow, "Output/Data/snow_prepped.rds")
