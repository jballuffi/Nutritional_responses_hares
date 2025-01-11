
#explain weight change residuals

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

wloss <- readRDS("Output/Data/weight_change.rds")
densitym <- readRDS("Output/Data/hare_population_monthly.rds")
densitya <- readRDS("Output/Data/densities_annual.rds")
snow <- readRDS("Output/Data/annual_snow_conditions.rds")


#take only weight changes of females
w <- wloss[!is.na(weight.c) & sex == "female"]


#merge with annual hare densities
w <- merge(w, densitya, by = "winter", all.x = TRUE)

#merge with annual snow
w <- merge(w, snow, by = c("winter", "snowgrid"), all.x = TRUE )
