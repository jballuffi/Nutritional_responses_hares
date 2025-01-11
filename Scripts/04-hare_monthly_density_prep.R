
#script is adapted from HR project but without the daily densities etc

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#import hare densities
hdensity <- fread("Input/Hare_density_monthly.csv")



# clean hare density ------------------------------------------------------

#create a mortality rate column
hdensity[!is.na(survival), mortality := 1-survival]

#change col names
setnames(hdensity, "Year", "winter")
setnames(hdensity, "hdensity", "haredensity")

#pull months, years, and days into separate col
hdensity[, date := mdy(Time)]

#categorize into winters
hdensity[month(date) > 6, winter := paste0(year(date), "-", year(date) + 1)]
hdensity[month(date) < 6, winter := paste0(year(date) - 1, "-", year(date))]

#remove time col
hdensity[, Time := NULL]

#make month col
hdensity[, month := month(date)]


# Categorize years into cycle phases -------------------------------------

#categorizing years into cycle phases based on Keith 1990
#took the information from Mike's oecologia paper

#spring to spring, take all april densities
springs <- hdensity[month(date) == 4]

#compare last year's density to this year's density
springs[, nextdens := shift(haredensity, n = 1, type = "lead")]

#calculate the finite rate of change
springs[, change := nextdens/haredensity]

#categorize based on rate of change for increase vs decrease
springs[change > 1.89, phase := "increase"]
springs[change < 0.44, phase := "decrease"]
# and population dens for low vs high
springs[is.na(phase) & haredensity < .1, phase := "low"]
springs[is.na(phase) & haredensity > .4, phase := "peak"]

#pull out just the phases and winters
phases <- springs[, .(winter, phase)]

#merge phase with densities
hdensity <- merge(hdensity, phases, by = "winter", all.x = TRUE)

#make phase a leveled factor
hdensity[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]


#save
saveRDS(hdensity, "Output/Data/hare_population_monthly.rds")
