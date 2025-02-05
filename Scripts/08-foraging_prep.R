#script that collects foraging effort on a weekly basis

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

beh <- fread("Input/allHareDailyValues2015_2021.csv")
inds <- readRDS("Output/Data/individual_info.rds")



# foraging data -----------------------------------------------------------

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
# ^^^ different line than other scripts because we already cut to just jan-march

#swap B's in the collar data for 2's
beh[, id := gsub("B", "2", id)]

#take only main cols and convert foraging to hours
beh <- beh[, .(winter, id, m, year, date, forage = Forage/3600)]



# get weekly foraging rate --------------------------------------

#categorize dates into weeks
beh[, week := week(date), year]

#get mean foraging effort by week and individual
behweek <- beh[, .(forage = mean(forage)), by = .(id, year, winter)]

#merge in individual data
behweek <- merge(behweek, inds, by = c("id", "winter"), all.x = TRUE)

behweek[, yearfactor := as.factor(year)]



# figures -----------------------------------------------------------------

(allwinters <-
  ggplot(beh)+
  geom_point(aes(x = date, y = forage))+
  facet_wrap(~year, scales = "free")+
  labs(x = "Date", y = "Daily foraging effort (hr)")+
  theme_minimal())



# Save --------------------------------------------------------------------

behweek <- behweek[year > 2015]

ggsave("Output/Figures/foraging_allwinters.jpg", allwinters, width = 12, height = 10, unit = "in")
saveRDS(behweek, "Output/Data/foraging_weekly.rds") #make weekly

