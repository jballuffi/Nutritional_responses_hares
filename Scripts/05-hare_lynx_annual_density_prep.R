# script to collect annual hare and lynx densities over winter
# get cycle phase from monthly data set

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


hares <- fread("Input/Hare_density_longterm.csv")
lynx <- fread("Input/Lynx_density_longterm.csv")
monthly <- readRDS("Output/Data/hares_monthly.rds")




# fix up hare densities ---------------------------------------------------

#split up year and season
hares[, season := tstrsplit(Season_year, " ", keep = 1)]
hares[, year := tstrsplit(Season_year, " ", keep = 2)]

#make year an integer
hares[, year := as.integer(year)]

#classify into winters
hares[season == "Fall", winter := paste0(year, "-", year + 1)]
hares[season == "Spring", winter := paste0(year - 1, "-", year)]

#grab just spring densities
haresspring <- hares[season == "Spring", .(Density, winter)]
setnames(haresspring, "Density", "hares.s")

#grabe just fall densities
haresautumn <- hares[season == "Fall", .(Density, winter)]
setnames(haresautumn, "Density", "hares.a")

#merge fall and spring
hareswide <- merge(haresautumn, haresspring, "winter")

#get mean density for whole winter season
hareswide[, hares.avg := (hares.a + hares.s)/2 ]



# fix up lynx densities --------------------------------------------------

#make column for the first year of winter
lynx[, firstyear := tstrsplit(Season, "/", keep = 1)]
lynx[, firstyear := as.integer(firstyear)]

#make column for the second year of winter
lynx[, secondyear := tstrsplit(Season, "/", keep = 2)]
lynx[, secondyear := as.integer(secondyear)]

#make winter col
lynx[, winter := paste0(20, firstyear, "-", 20, secondyear)]

#cut down columns
lynx <- lynx[, .(winter, density)]
setnames(lynx, "density", "lynx")



# merge and save annual densities -----------------------------------------

#get phases from montly data set
phases <- monthly[, .(phase = getmode(phase)), winter]

#merge hares and lynx
d <- merge(hareswide, lynx, by = "winter")

#merge densities and phases
d <- merge(d, phases, by = "winter", all.x = TRUE)

#two earlier winters that are increase but not captured in the monthly 
d[is.na(phase), phase := "increase"]

#pull out just year
d[, year := tstrsplit(winter, "-", keep = 1)]
d[, year := as.integer(year)]



saveRDS(d, "Output/Data/hares_lynx_winter.rds")

