#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#import hare densities
hdensity <- fread("data/Hare_density_monthly.csv")



# clean hare density ------------------------------------------------------

#create a mortality rate column
hdensity[!is.na(survival), mortality := 1-survival]

#classify months as either early or late winter for year stuff
late <- c(1, 2, 3, 4)
early <- c(10, 11, 12)

#change col names
setnames(hdensity, "Year", "winter")
setnames(hdensity, "hdensity", "haredensity")

#pull months, years, and days into separate col
hdensity[, date := mdy(Time)]
hdensity[, mnth := month(date)]
hdensity[mnth %in% early, y := tstrsplit(winter, "-", keep = 1)]
hdensity[mnth %in% late, y := tstrsplit(winter, "-", keep = 2)]
#hdensity[, day := day(date)]

#remove time col
hdensity[, Time := NULL]

#remove winter of 2021-2022
hdensity <- hdensity[!winter == "2021-2022"]

#create a day col
hdensity[, winterday := date - min(date), winter]
hdensity[, winterday := as.integer(winterday)]



# get mortality rates by month for predation risk -------------------------

predrisk <- hdensity[, mean(mortality), by = .(mnth, winter)]
setnames(predrisk, "V1", "mortrate")



# Categorize years into cycle phases -------------------------------------

#categorizing years into cycle phases based on Keith 1990
#took the information from Mike's oecologia paper

#spring to spring, take all april densities
springs <- hdensity[mnth == 4]

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



# run linear models of density decrease by winter -------------------------

#run predictions function by winter (lm of density over time, predicts for each day)
densitypred <- hdensity[, .(haredensity = predictdens(yvar = haredensity, xvar = winterday), 
                            lower = predictdens(yvar = hdensity_low95, xvar = winterday),
                            upper = predictdens(yvar = hdensity_up95, xvar = winterday)),
                        by = winter]

#create the same sequence for winterday as in predictdens function
densitypred[, winterday := seq(1, 197, by = 1), winter]

#recreate date based on winter day if the min date is oct 1
densitypred[, minyear := tstrsplit(winter, "-", keep = 1)]
densitypred[, mindate := dmy(paste0("30-09", "-", minyear))]
densitypred[, date := mindate + winterday]

#merge in cycle phases 
densitypred <- merge(densitypred, phases, by = "winter", all.x = TRUE)

#delete 31 days from the winterday col because HR data starts at December 1st, not October 1st
densitypred[, winterday := winterday - 61]
densitypred <- densitypred[winterday > 0]



# Figures -----------------------------------------------------------------

(densityregressions <- 
  ggplot(densitypred)+
  #geom_ribbon(aes(x = date, ymin = lower, ymax = upper), alpha = 0.3, color = "grey40", data = densitypred)+
  geom_line(aes(x = date, y = haredensity, group = 1), data = densitypred)+
  geom_point(aes(x = date, y = haredensity), data = hdensity[winterday > 61])+
  geom_errorbar(aes(x = date, ymax = hdensity_up95, ymin = hdensity_low95), width = 3, data = hdensity[winterday > 61])+
  facet_wrap(~winter, scales = "free_x")+
  labs(x = "Date", y = "Hare density (hares/ha)")+
  theme_minimal())




saveRDS(predrisk, "output/results/mortalityrates.rds")
saveRDS(densitypred, "output/results/dailyharedensities.rds")
ggsave("output/figures/densityestimates.jpeg", densityregressions, width = 10, height = 6, unit = "in")
