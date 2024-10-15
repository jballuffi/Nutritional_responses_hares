# script for looking into fecal data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
trap <- fread("Input/Query_for_fecals.csv")
foodadds <- readRDS("Output/data/food_adds.rds")
axy <- fread("../Hare_food_availability/Input/allHareDailyValues2015_2021.csv")
hr <- readRDS("../HR_PopCycle_SnowshoeHares/data/all_gps.rds")



# collect gps, axy, and food add animals ---------------------------------------------

###  GPS DATA
#get unique IDs by winter
gpsind <- hr[, .(id = unique(id)), winter]
#make gps column that says present
gpsind[, GPS := "present"]
gpsind[, id := as.character(id)]

###  AXY DATA
#make axy month
axy[, m := month(Date)]
#cut axy data to just january to april
axy <- axy[m < 5]
#classify axy data into winters
make_winter(axy, datecol = axy$Date, monthcol = axy$m)
#get unique IDs by winter
axyind <- axy[, .(id = unique(id)), winter]
#make axy column to say present
axyind[, axy := "present"]

###  FOOD DATA
setnames(foodadds, "Eartag", "id")
foodadds[, id := as.character(id)]

#merge
inds <- merge(gpsind, axyind, by = c("id", "winter"), all = TRUE)
inds <- merge(inds, foodadds, by = c("id", "winter"), all = TRUE)



# prep trapping data ------------------------------------------------------

#set up date columns
trap[, idate := dmy(dateCap)]
trap[, m := month(idate)]

#just take january through to april
trap <- trap[m < 5]

#create winter column
make_winter(trap, trap$idate, trap$m)

#reduce to 2014 onward
trap <- trap[y > 2014]

#make trap order by date
setorder(trap, idate)

#remove anything from 4th and 3rd trap sessions
#if a hare was trapped 3 or more times in one week
#it's best to not use those data
trap4 <- grep("4", names(trap), value = TRUE)
trap[, c(trap4) := NULL]
trap3 <- grep("3", names(trap), value = TRUE)
trap[, c(trap3) := NULL]

#change names 
setnames(trap, c("Weight", "Hindfoot", "Fecal_VialCK1", "Fecal_VialCK2"), c("Weight_1", "RHF_1", "Vial_1", "Vial_2"))

#remove fecal time columns
trap[, FecalTimeCK1 := NULL]
trap[, FecalTimeCK2 := NULL]

#remove extra columns
trap[, c(1,3,4,5,6) := NULL]



# melt data to show one trapping per row ----------------------------------

#get columns for each data type
W <- grep("Weight", names(trap), value = TRUE)
R <- grep("RHF", names(trap), value = TRUE)
FV <- grep("Vial", names(trap), value = TRUE)

#turn this melt into a function

#melt 3 times.... must be better way
Wtrap <- melt(trap, measure.vars = W, variable.name = "DayW", value.name = "Weight" )
Rtrap <- melt(Wtrap, measure.vars = R, variable.name = "DayR", value.name = "RHF" )
FVtrap <- melt(Rtrap, measure.vars = FV, variable.name = "DayF", value.name = "Vial" )

#order by date then eartag
FVtrap <- FVtrap[order(idate, Eartag)]

FVtrap[, DayW := tstrsplit(DayW, "_", keep = 2)]
FVtrap[, DayR := tstrsplit(DayR, "_", keep = 2)]
FVtrap[, DayF := tstrsplit(DayF, "_", keep = 2)]

#create a new column that shows whether DayW and DayR are equal
FVtrap[, test := ifelse(DayW == DayR, 'equal', 'not equal')]
#keep only the equals
FVtrap <- FVtrap[test == "equal"]
#remake test column for DayW and DayF
FVtrap[, test := ifelse(DayW == DayF, 'equal', 'not equal')]
#keep only the equals
FVtrap <- FVtrap[test == "equal"]

#remove columns that repeat day now
FVtrap[, DayR := NULL][, DayF := NULL][, test := NULL]

setnames(FVtrap, c("DayW", "Eartag"), c("Weekday", "id"))



# Clean values ------------------------------------------------------------

FVtrap[Weight == 0, Weight := NA]
FVtrap[RHF == 0, RHF := NA]

#keep only values with a fecal value
FVtrap <- FVtrap[!is.na(Vial)]

#reclassify id column
FVtrap[, id := as.character(id)]





# merge with food add, GPS, and axy data ------------------------------------------------

#merge the melted trap data with list of food adds
dat <- merge(FVtrap, inds, by = c("id", "winter"), all.x = TRUE)

dat <- dat[order(winter, id, idate)]

#fill in NA values
dat[is.na(GPS), GPS := "absent"]
dat[is.na(axy), axy := "absent"]
dat[is.na(Food), Food := "0"]
dat[, Food := as.factor(Food)]




# body weight trends --------------------------------------------

ggplot(dat[m == 3 & Sex == "2"])+
  geom_boxplot(aes(x = winter, y = Weight, color = Food))+
  theme_minimal()



# cut data different ways -------------------------------------------------

#get sample sizes by bunny-winter
dat[, Nwinter := .N, by = .(id, winter)]

#get sample sizes by bunny
dat[, Nind := .N, id]

#hoe many days covered per bunny-winter
dat[, dayscovered := max(idate) - min(idate), by = .(id, winter)]

#how many samples in jan and march by bunny-winter
dat[m == 1, Njan := .N, by = .(id, winter)]
dat[m == 3, Nmar := .N, by = .(id, winter)]

dat <- dat[order(Vial)]

write.csv(dat, "Output/samplelist.csv")
