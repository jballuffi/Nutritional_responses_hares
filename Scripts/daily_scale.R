#daily response part

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
foraging <- readRDS("Output/Data/foraging_rates.rds")
foodadd <- readRDS("Output/Data/food_adds.rds")
twigs <- readRDS("Output/Data/snow_and_food.rds")

#merge food add and foraging
foraging <- merge(foraging, foodadd, by = c("id", "winter"), all.x = TRUE)
foraging[is.na(Food), Food := 0]
foraging[, Food := as.factor(Food)]


#get the lag difference

twigs <- twigs[order(snowgrid, Date)]

twigs[, prevdate := shift(Date, n = 1, type = "lag"), by = .(snowgrid, winter)]
twigs[, daydiff := as.integer(Date - prevdate)]

twigs <- twigs[daydiff < 4]

twigs[, lagsnow := shift(snow, n = 1, type = "lag"), by = .(snowgrid, winter)]
twigs[, snowfall := (snow - lagsnow)/daydiff, by = .(snowgrid, winter)]




dt <- merge(foraging, twigs, by = c("Date", "snowgrid"), all.x = TRUE)


