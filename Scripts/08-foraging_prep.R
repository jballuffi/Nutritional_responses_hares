#script that investigates foraging data from previous winters
#also includes snow depths from those years

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

beh <- fread("Input/allHareDailyValues2015_2021.csv")
#trapping <- fread("Input/Trapping_data_all_records.csv")
inds <- readRDS("Output/Data/individual_info.rds")



# foraging data -----------------------------------------------------------

#create year and month column
beh[, m := month(Date)]
beh[, year := year(Date)]
beh[, date := ymd(Date)]

#name the winter months
wintermonths <- c(1, 2, 3, 11, 12)

#cut to only winter 
beh <- beh[m %in% wintermonths]

#create a winter column
beh[m < 4, winter := paste0(year-1, "-", year)]
beh[m > 8, winter := paste0(year, "-", year+1)]

#swap B's in the collar data for 2's
beh[, id := gsub("B", "2", id)]

#take only main cols and convert foraging to hours
beh <- beh[, .(winter, id, m, year, date, forage = Forage/3600)]



# Get foraging by month and winter ----------------------------------------

behwinter <- beh[, .(forage = mean(forage)), by = .(winter, id)]

behmonth <- beh[, .(forage = mean(forage)), by = .(winter, m, id)]


#merge grids into behaviour data set
dat <- merge(beh, inds, by = c("id", "winter"), all.x = TRUE)
datwinter <- merge(behwinter, inds, by = c("id", "winter"), all.x = TRUE)
datmonth <- merge(behmonth, inds, by = c("id", "winter"), all.x = TRUE)



# figures -----------------------------------------------------------------

allwinters <-
  ggplot(dat[m < 5 & m > 1])+
  geom_point(aes(x = date, y = forage))+
  facet_wrap(~winter, scales = "free")+
  labs(x = "Date", y = "Daily foraging effort (hr)")+
  theme_minimal()



# Save --------------------------------------------------------------------

ggsave("Output/Figures/foraging_allwinters.jpg", allwinters, width = 12, height = 10, unit = "in")


saveRDS(dat, "Output/Data/foraging_daily.rds")
saveRDS(datmonth, "Output/Data/foraging_monthly.rds")
saveRDS(datwinter, "Output/Data/foraging_winter.rds")

