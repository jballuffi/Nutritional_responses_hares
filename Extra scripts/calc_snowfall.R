

# Calculate snow fall -----------------------------------------------------

#order twigs and snow by date and grid
snow_d <- snow_d[order(snowgrid, date)]

#make new column for previous snow day within grid and winter
snow_d[, prevdate := shift(date, n = 1, type = "lag"), by = .(snowgrid, winter)]

#difference between current date and the previous date
snow_d[, daydiff := as.integer(date - prevdate)]
snow_d[, unique(daydiff)]

#get previous date's snow depth
snow_d[, lagsnow := shift(snow, n = 1, type = "lag"), by = .(snowgrid, winter)]

#get difference in snow between current date and previous date
#get rate of snow fall by dividing by the difference in days (max 3 right now)
snow_d[, snowfall := (snow - lagsnow)/daydiff, by = .(snowgrid, winter)]
