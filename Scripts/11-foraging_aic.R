
#script to assess how environmental factors 

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


density_w <- readRDS("Output/Data/hares_lynx_winter.rds")
snow_w <- readRDS("Output/Data/snow_food_winter.rds")
forag_w <- readRDS("Output/Data/foraging_winter.rds")

density_d <- readRDS("Output/Data/hares_daily.rds")
snow_d <- readRDS("Output/Data/snow_food_daily.rds")
forag_d <- readRDS("Output/Data/foraging_daily.rds")



# merge info and make a control and food add dataset --------

foragw <- merge(forag_w, density_w, by = "winter", all.x = TRUE)
foragw <- merge(foragw, snow_w, by = c("winter", "year", "snowgrid"))

#get controls only
foragwc <- foragw[food == 0]

#take only the years where there was food add
foodyears <- foragw[food == 1, unique(winter)]

#take only females for food add comparisons
foragwf <- foragw[winter %in% foodyears]

#no difference between foraging effort of males and control females
summary(lm(forage ~ sex, data = foragwc))



# AIC to explain winter foraging for controls only ------------------------

#models for controls only
n <- lm(forage ~ 1, foragwc)
s <- lm(forage ~ snow.avg, foragwc)
t <- lm(forage ~ biomass.avg, foragwc)
h <- lm(forage ~ hares.avg, foragwc)
l <- lm(forage ~ lynx, foragwc)
p <- lm(forage ~ phase, foragwc)

sh <- lm(forage ~ snow.avg + hares.avg, foragwc)
sl <- lm(forage ~ snow.avg + lynx, foragwc)
sp <- lm(forage ~ snow.avg + phase, foragwc)
th <- lm(forage ~ biomass.avg + hares.avg, foragwc)
tl <- lm(forage ~ biomass.avg + lynx, foragwc)
tp <- lm(forage ~ biomass.avg + phase, foragwc)

#list models
mods <- list(n, s, t, h, l, p,
             sh, sl, sp,
             th, tl, tp)

#name models
Names <- c('null', 'snow', 'twig', 'hares', 'lynx', 'phase',
           'snow + hares', 'snow + lynx', 'snow + phase',
           'twigs + hares', 'twigs + lynx', 'twigs + phase')

#make AIC table
AICcon <- as.data.table(aictab(REML = F, cand.set = mods, modnames = Names, sort = TRUE))

#remove unwanted columns
AICcon[, ModelLik := NULL]
AICcon[, Cum.Wt := NULL]

#round whole table to 3 dec places
AICcon <- AICcon %>% mutate_if(is.numeric, round, digits = 3)

#run function and get R2s for all models
R2scon <- lapply(mods, collectR2)
R2scon <- rbindlist(R2scon, fill = TRUE)
setnames(R2scon, "V1", "R2")
R2scon$Modnames <- Names

#merge R2s with AIC table
AICcon <- merge(AICcon, R2scon, by = "Modnames")
setorder(AICcon, "Delta_AICc")

#most parsimonious model is the twig availability with hares and lynx
summary(sp)
sp_int <- coefficients(sp)['(Intercept)']
sp_s <- coefficients(sp)['snow.avg']

ggplot(foragwc)+
  geom_point(aes(x = snow.avg, y = forage))+
  labs(x = "Average snow depth (cm)", y = "Average foraging effort (hr/day)")+
  themepoints

ggplot(foragwc)+
  geom_boxplot(aes(x = phase, y = forage))+
  labs(x = "Cycle Phase", y = "Average foraging effort (hr/day)")+
  themepoints



# AIC to explain winter foraging for food add dataset - ------------------------------------------------

foodn <- lm(forage ~ 1, foragwf)
foodf <- lm(forage ~ food, foragwf)
foods <- lm(forage ~ food*snow.avg, foragwf)
foodt <- lm(forage ~ food*biomass.avg, foragwf)
foodh <- lm(forage ~ food*hares.avg, foragwf)
foodl <- lm(forage ~ food*lynx, foragwf)
foodp <- lm(forage ~ food*phase, foragwf)

#list models
modsfood <- list(foodn, foodf, foods, foodt, foodh, foodl, foodp)

#name models
Namesfood <- c('Null', 'Food', 'Food*Snow', 'Food*Twigs', 'Food*Hares', 'Food*Lynx', 'Food*Phase')

#make AIC table
AICfood <- as.data.table(aictab(REML = F, cand.set = modsfood, modnames = Namesfood, sort = TRUE))

#remove unwanted columns
AICfood[, ModelLik := NULL]
AICfood[, Cum.Wt := NULL]

#round whole table to 3 dec places
AICfood <- AICfood %>% mutate_if(is.numeric, round, digits = 3)

#run function and get R2s for all models
R2sfood <- lapply(modsfood, collectR2)
R2sfood <- rbindlist(R2sfood, fill = TRUE)
setnames(R2sfood, "V1", "R2")
R2sfood$Modnames <- Namesfood

#merge R2s with AIC table
AICfood <- merge(AICfood, R2sfood, by = "Modnames")
setorder(AICfood, "Delta_AICc")


ggplot(foragwf)+
  geom_boxplot(aes(x = phase, y = forage, fill = food), alpha = .5)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Cycle Phase", y = "Average foraging effort (hr/day)")+
  themepoints



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



# AIC to predict daily foraging effort for food adds and controls ---------------------------------------------

foragd <- merge(forag_d, snow_d, by = c("winter", "snowgrid", "m", "date"), all.x = TRUE)
foragd <- merge(foragd, density_d, by = c("winter", "date"))

dayn <- lm(forage ~ 1, foragd)
dayf <- lm(forage ~ food, foragd)
days <- lm(forage ~ food*snow, foragd)
dayt <- lm(forage ~ food*biomassavail, foragd)
dayh <- lm(forage ~ food*haredensity, foragd)

modsday <- list(dayn, dayf, days, dayt, dayh)
namesday <- c('Null', "Food", "Food*Snow", "Food*Twigs", "Food*Hares")

#make AIC table
AICday <- as.data.table(aictab(REML = F, cand.set = modsday, modnames = namesday, sort = TRUE))

#remove unwanted columns
AICday[, ModelLik := NULL]
AICday[, Cum.Wt := NULL]

#round whole table to 3 dec places
AICday <- AICday %>% mutate_if(is.numeric, round, digits = 3)

#run function and get R2s for all models
R2sday <- lapply(modsday, collectR2)
R2sday <- rbindlist(R2sday, fill = TRUE)
setnames(R2sday, "V1", "R2")
R2sday$Modnames <- namesday

#merge R2s with AIC table
AICday <- merge(AICday, R2sday, by = "Modnames")
setorder(AICday, "Delta_AICc")

summary(dayt)


# save --------------------------------------------------------------------




write.csv(AICcon, "Output/Tables/AIC_foraging_winter_controls.csv")
write.csv(AICfood, "Output/Tables/AIC_foraging_winter_foods.csv")
write.csv(AICday, "Output/Tables/AIC_foraging_daily_foods.csv")


