
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

weights <- readRDS("Output/Data/weight_change.rds")
densitya <- readRDS("Output/Data/densities_annual.rds")
snow <- readRDS("Output/Data/annual_snow_conditions.rds")
forag <- readRDS("Output/Data/foraging_rates.rds")


#remove cases without a sex
weights <- weights[!is.na(sex)]

#merge with annual hare densities
weights <- merge(weights, densitya, by = "winter", all.x = TRUE)

#merge with annual snow
weights <- merge(weights, snow, by = c("winter", "snowgrid"), all.x = TRUE )

#cut dataset to be only instances with weight change
weights <- weights[!is.na(weight.c)]



# prep foraging data ------------------------------------------------------

forag[, dailyf := Forage/3600]

forag <- forag[m == 1 | m == 2 | m == 3]

avgforag <- forag[, .(dailyf = mean(dailyf)), by = .(winter, id, snowgrid)]

fdat <- merge(avgforag, snow, by = c("winter", "snowgrid"), all.x = TRUE)

fdat <- merge(fdat, densitya, by = "winter", all.x = TRUE)



# merge foraging rates with weight changes --------------------------------

weights <- merge(weights, avgforag, by = c("winter", "id"), all.x = TRUE)

#take only females for food add comparisons
wfem <- weights[sex == "female"]

#take only the years where there was food add
foodyears <- wfem[food == 1, unique(winter)]
wfem <- wfem[winter %in% foodyears]

#make a controls only dataset
wcon <- weights[food == 0]


ggplot(wcon)+
  geom_point(aes(x = dailyf, y = weight.c.resid, color = sex))

ggplot(wfem)+
  geom_point(aes(x = dailyf, y = weight.c.resid, color = food))+
  geom_smooth(aes(x = dailyf, y = weight.c.resid, color = food), method = "lm")


ggplot(fdat)+
  geom_boxplot(aes(x = winter, y = dailyf, fill = sex))

ggplot(fdat)+
  geom_point(aes(x = snow.max, y = dailyf))

#go back to foraging script and prep to have sex in foraging data already

summary(lm(dailyf ~ biomass.avg, fdat))
