#script that collects foraging effort on a weekly basis

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in all data
beh <- fread("Input/allHareDailyValues2015_2021.csv")
inds <- readRDS("Output/Data/individual_info.rds")
traps <- readRDS("Output/Data/trap_nights.rds")
days <- fread("Input/daylength .csv")



# variable prep -----------------------------------------------------------

#create various date columns
beh[, m := month(Date)]
beh[, year := year(Date)]
beh[, date := ymd(Date)]

#list the months of this study and take only those months
wintermonths <- c(1, 2, 3) 
beh <- beh[m %in% wintermonths]

#create a winter column using year
beh[, winter := paste0(year-1, "-", year)]

#swap B's in the collar data for 2's
beh[, id := gsub("B", "2", id)]



# merge in day length to get night length, which decreases over the study period --------------------------

### prep day length data

# make julian day numeric
days[, jday := as.numeric(Julian)]

#separate daylight col into hours and minutes
days[, tstrsplit(Daylight, ':', names = c("hours", "minutes"))]

#calculate total night length, in hours based on hours and minutes of daylight
days[, nightlength := round(24 - (hours + (minutes/60)), 2)]

#take just julian day and night length columns
days <- days[, .(jday, nightlength)]

#make a column for january 1st for each year
beh[, start := dmy(paste0(01, "-", 01, "-", year)), year]

#calculate julian day from jan 1st
beh[, jday := date - start + 1, year][, jday := as.numeric(jday)]

#merge in day length with behaviour data
beh2 <- merge(beh, days, by = "jday", all.x = TRUE)



# remove trap nights ----------------------------------------------------

#these are nights that hares were trapped, we want to remove these nights from axy data

#add column to trap nights called trap nights
traps[, trapnight := "yes"]

#merge in trap nights to behaviour data
beh3 <- merge(beh2, traps, by = c("id", "date"), all.x = TRUE)

#remove trap nights from dataset
beh3 <- beh3[is.na(trapnight)]

#take only main cols and convert foraging to hours
beh4 <- beh3[, .(winter, id, m, year, date, jday, nightlength, forage = Forage/3600)]

beh4[, forageperhour := forage/nightlength]



# figures -----------------------------------------------------------------

#forage effort vs. forage per hour of night
ggplot(beh4)+
  geom_point(aes(x = forage, y = forageperhour))

#test how foraging effort relates to night length
mod <- lm(forage ~ nightlength, beh4)
modpred <- ggpredict(mod, terms = "nightlength")

fig <- 
  ggplot()+
  geom_point(aes(x = nightlength, y = forage), alpha = 0.2, color = "grey40", data = beh4)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill = "red", alpha = 0.5, data = modpred)+
  geom_line(aes(x = x, y = predicted), color = "red4", data = modpred)+
  labs(x = "Night length (hr)", y = "Forage effort (hr)")+
  themepoints



# get weekly foraging rate --------------------------------------

#categorize dates into weeks
beh4[, week := week(date), year]

#get mean foraging effort by week and individual
behweek <- beh4[, .(forage = mean(forage), nightlength = mean(nightlength)), by = .(id, year, winter, week)]

#merge in individual data
behweek <- merge(behweek, inds, by = c("id", "winter"), all.x = TRUE)

#make a year factor col
behweek[, yearfactor := as.factor(year)]



# Save --------------------------------------------------------------------

behweek <- behweek[year > 2015]

ggsave("Output/Figures/nightlength.jpeg", width = 6, height = 5, unit = "in")
saveRDS(behweek, "Output/Data/foraging_weekly.rds")

