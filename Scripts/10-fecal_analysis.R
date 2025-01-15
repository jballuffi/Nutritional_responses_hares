
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


weights <- readRDS("Output/Data/weight_change.rds")
densitya <- readRDS("Output/Data/densities_annual.rds")
snow <- readRDS("Output/Data/annual_snow_conditions.rds")
forag <- readRDS("Output/Data/foraging_rates.rds")
fecal <- readRDS("Output/Data/CP_results_cleaned.rds")

#remove the one outlier
fecal <- fecal[!CP_dm > 25]

#make month a number
fecal[, m := as.numeric(m)]

setnames(forag, "Date", "idate")



# Does foraging rate correlate with fecal protein -------------------------


#get monthly foraging rates
mforag <- forag[, .(mforag = mean(Forage, na.rm = TRUE)/3600), by = .(id, winter, m)]

#merge monthly foraging rates with fecal sample from the same month
fecal <- merge(fecal, mforag, by = c("id", "winter", "m"), all.x = TRUE)

ggplot(fecal)+
  geom_point(aes(x = mforag, y = CP_dm))+
  geom_smooth(aes(x = mforag, y = CP_dm), method = "lm")+
  labs(x = "Monthly foraging rate (hr/day)", y = "Fecal protein (%)")+
  themepoints


#pull dates of each fecal sample, subtract one to get the day before
#create a column called "used for daily"
fecaldates <- fecal[, .(idate = unique(idate) - 1, usefordaily = "yes"), id]

#merge prev days of fecal samples with foraging data
forag <- merge(forag, fecaldates, by = c("id", "idate"), all.x = TRUE) 

#take anything that has a "use for daily" listed -yes- and take id, month, and forage rate
#add a day back to date so it matches the fecal data again
dforag <- forag[usefordaily == "yes", .(id, idate, prev.dforag = Forage/3600)]
dforag[, idate := idate + 1]

fecal <- merge(fecal, dforag, by = c("id", "idate"), all.x = TRUE)


ggplot(fecal)+
  geom_point(aes(x = prev.dforag, y = CP_dm))+
  geom_smooth(aes(x = prev.dforag, y = CP_dm), method = "lm")+
  labs(x = "Foraging rate of previous day (hr/day)", y = "Fecal protein (%)")+
  themepoints


summary(lm(CP_dm ~ mforag, fecal))
summary(lm(CP_dm ~ prev.dforag, fecal))

