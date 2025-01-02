#daily response part

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

foraging <- readRDS("Output/Data/foraging_rates.rds")
foodadd <- readRDS("Output/Data/food_adds.rds")
twigs <- readRDS("Output/Data/snow_and_food.rds")

#merge food add and foraging
foraging <- merge(foraging, foodadd, by = c("id", "winter"), all.x = TRUE)
foraging[is.na(Food), Food := 0]
foraging[, Food := as.factor(Food)]

#summarize data
foraging[, unique(winter)]
twigs[, unique(winter)]
twigs[, .N, winter]
#We should have foraging data from 2022?

twigscut <- twigs[, .(snowgrid, Date, snow, biomassavail)]


dt <- merge(foraging, twigscut, by = c("Date", "snowgrid"), all.x = TRUE)

dt <- dt[order(snowgrid, Date)]

#get the lag difference
dt[, lagsnow := shift(snow, n = 2, type = "lag"), by = .(snowgrid, winter)]
dt[, snowfall := snow - lagsnow, by = .(snowgrid, winter)]
