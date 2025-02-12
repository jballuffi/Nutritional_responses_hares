#script that collects foraging effort on a weekly basis

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

beh <- fread("Input/allHareDailyValues2015_2021.csv")
inds <- readRDS("Output/Data/individual_info.rds")
traps <- readRDS("Output/Data/trap_nights.rds")
days <- fread("Input/daylength .csv")



# variable prep -----------------------------------------------------------

#create year and month column
beh[, m := month(Date)]
beh[, year := year(Date)]
beh[, date := ymd(Date)]

#name the winter months
wintermonths <- c(1, 2, 3)

#cut to only winter 
beh <- beh[m %in% wintermonths]

#create a winter column
beh[, winter := paste0(year-1, "-", year)]

#swap B's in the collar data for 2's
beh[, id := gsub("B", "2", id)]



# merge in info about trap nights and day length --------------------------

#prep day length data
days[, jday := as.numeric(Julian)]
days[, hours := tstrsplit(Daylight, ':', keep = 1)][, minutes := tstrsplit(Daylight, ":", keep = 2)]
days[, hours := as.numeric(hours)][, minutes := as.numeric(minutes)]
days[, nightlength := round(24 - (hours + (minutes/60)), 2)]
days <- days[, .(jday, nightlength)]

#make a column for january 1st for each year
beh[, start := paste0(01, "-", 01, "-", year), year]
beh[, start := dmy(start)]

#get difference between start day and date to make a julian date
beh[, jday := date - start + 1, year]
beh[, jday := as.numeric(jday)]

#merge in day length with behaviour data
beh2 <- merge(beh, days, by = "jday", all.x = TRUE)

#add column to trap nights called trap nights
traps[, trapnight := "yes"]

#merge in trap nights to behaviour data
beh3 <- merge(beh2, traps, by = c("id", "date"), all.x = TRUE)

#remove trap nights from dataset
beh3 <- beh3[is.na(trapnight)]

#take only main cols and convert foraging to hours
beh4 <- beh3[, .(winter, id, m, year, date, jday, nightlength, forage = Forage/3600)]



# get weekly foraging rate --------------------------------------

#categorize dates into weeks
beh4[, week := week(date), year]

summary(lm(forage ~ nightlength, beh4))

#get mean foraging effort by week and individual
behweek <- beh4[, .(forage = mean(forage), nightlength = mean(nightlength)), by = .(id, year, winter, week)]

#merge in individual data
behweek <- merge(behweek, inds, by = c("id", "winter"), all.x = TRUE)

behweek[, yearfactor := as.factor(year)]



# figures -----------------------------------------------------------------

(allwinters <-
  ggplot(beh4)+
  geom_point(aes(x = date, y = forage))+
  facet_wrap(~year, scales = "free")+
  labs(x = "Date", y = "Daily foraging effort (hr)")+
  theme_minimal())



# Save --------------------------------------------------------------------

behweek <- behweek[year > 2015]

ggsave("Output/Figures/foraging_allwinters.jpg", allwinters, width = 12, height = 10, unit = "in")
saveRDS(behweek, "Output/Data/foraging_weekly.rds")

