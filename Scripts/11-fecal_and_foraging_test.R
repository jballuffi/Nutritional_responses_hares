
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


weights <- readRDS("Output/Data/weight_change.rds")
density_w <- readRDS("Output/Data/hares_lynx_winter.rds")
density_d <- readRDS("Output/Data/hares_daily.rds")

snow_w <- readRDS("Output/Data/snow_food_winter.rds")
snow_d <- readRDS("Output/Data/snow_food_winter.rds")
snow_m <- readRDS("Output/Data/snow_food_monthly.rds")

forag_d <- readRDS("Output/Data/foraging_daily.rds")
forag_w <- readRDS("Output/Data/foraging_winter.rds")
forag_m <- readRDS("Output/Data/foraging_monthly.rds")

fecal <- readRDS("Output/Data/fecal_protein.rds")



# Does foraging rate correlate with fecal protein -------------------------

#merge monthly foraging rates with fecal sample from the same month
fecal_m <- merge(fecal, forag_m, by = c("id", "winter", "m", "snowgrid", "food", "sex"), all.x = TRUE)

ggplot(fecal_m)+
  geom_point(aes(x = forage, y = CP_dm))+
  geom_smooth(aes(x = forage, y = CP_dm), method = "lm")+
  labs(x = "Monthly foraging rate (hr/day)", y = "Fecal protein (%)")+
  themepoints


#pull dates of each fecal sample, subtract one to get the day before
#create a column called "used for daily"
fecaldates <- fecal[, .(date = unique(date) - 1, usefordaily = "yes"), id]

#merge prev days of fecal samples with foraging data
forag_d <- merge(forag_d, fecaldates, by = c("id", "date"), all.x = TRUE) 

#take anything that has a "use for daily" listed -yes- and take id, month, and forage rate
#add a day back to date so it matches the fecal data again
forag_d_fecal <- forag_d[usefordaily == "yes", .(id, date, prev.dforag = forage)]
forag_d_fecal[, date := date + 1]

fecal_d <- merge(fecal, forag_d_fecal, by = c("id", "date"), all.x = TRUE)


ggplot(fecal_d)+
  geom_point(aes(x = prev.dforag, y = CP_dm))+
  geom_smooth(aes(x = prev.dforag, y = CP_dm), method = "lm")+
  labs(x = "Foraging rate of previous day (hr/day)", y = "Fecal protein (%)")+
  themepoints


summary(lm(CP_dm ~ forage, fecal_m))
summary(lm(CP_dm ~ prev.dforag, fecal_d))

