
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

weights <- readRDS("Output/Data/weight_change.rds")
densitya <- readRDS("Output/Data/densities_annual.rds")
snow <- readRDS("Output/Data/annual_snow_conditions.rds")
forag <- readRDS("Output/Data/foraging_rates.rds")
ind <- readRDS("Output/Data/individual_info.rds")



# prep winter summary foraging data ------------------------------------------------------
#this is all about how much, on average, a hare forages in Jan - Mar

#convert daily foraging effort to hours 
forag[, dailyf := Forage/3600]

#cut to only include foraging rates from jan - mar
forag <- forag[m == 1 | m == 2 | m == 3]

#take the mean foraging rate over winter for each individual
wforag <- forag[, .(dailyf = mean(dailyf)), by = .(winter, id, snowgrid)]

#merge winter foraging rates with snow, density, and sexes
wforag <- merge(wforag, ind, by = c("id", "snowgrid", "winter"))
wforag <- merge(wforag, snow, by = c("winter", "snowgrid"), all.x = TRUE)
wforag <- merge(wforag, densitya, by = "winter", all.x = TRUE)

#merge winter foraging rates with weight changes
weights <- weights[, .(id, winter, weight.a, weight.s, rhf.a, rhf.s, weight.c, weight.c.resid )]
wforag <- merge(wforag, weights, by = c("id", "winter"), all.x = TRUE)

#make a controls only dataset
wforagcon <- wforag[food == 0]

#is there any difference between males and female avg foraging rate?
summary(lm(dailyf ~ sex, wforagcon))

#take only the years where there was food add
foodyears <- wforag[food == 1, unique(winter)]

#take only females for food add comparisons
wforagfood <- wforag[winter %in% foodyears]



ggplot(wforagcon)+
  geom_boxplot(aes(x = winter, y = dailyf, fill = sex))+
  themepoints

ggplot(wforagfood)+
  geom_boxplot(aes(x = winter, y = dailyf, fill = food))+
  themepoints



# AIC to explain foraging rates for controls ------------------------------


#models with single terms only
n <- lm(dailyf ~ 1, wforagcon)
s <- lm(dailyf ~ snow.avg, wforagcon)
t <- lm(dailyf ~ biomass.avg, wforagcon)
h <- lm(dailyf ~ hares.avg, wforagcon)
l <- lm(dailyf ~ lynx, wforagcon)
#p <- lm(dailyf ~ phase, wforagcon)

#models with two terms each
sh <- lm(dailyf ~ snow.avg + hares.avg, wforagcon)
sl <- lm(dailyf ~ snow.av + lynx, wforagcon)
#sp <- lm(dailyf ~ snow.avg*phase, wforagcon)
th <- lm(dailyf ~ biomass.avg + hares.avg, wforagcon)
tl <- lm(dailyf ~ biomass.avg + lynx, wforagcon)
#tp <- lm(dailyf ~ biomass.avg*phase, wforagcon)

#list models
mods <- list(n, s, t, h, l,
             sh, sl,
             th, tl)

#name models
Names <- c('null', 'snow', 'twig', 'hares', 'lynx',
           'snow + hares', 'snow + lynx',
           'twigs + hares', 'twigs + lynx')

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
summary(th)
th_int <- coefficients(th)['(Intercept)']
th_t <- coefficients(th)['biomass.avg']
th_h <- coefficients(th)['hares.avg']

summary(tl)
tl_int <- coefficients(tl)['(Intercept)']
tl_t <- coefficients(tl)['biomass.avg']
tl_l <- coefficients(tl)['lynx']


#do ggeffects?
#use an interaction?
ggplot(wforagcon)+
  geom_point(aes(x = biomass.avg, y = dailyf))+
  geom_abline(intercept = th_int, slope = th_t)

ggplot(wforagcon)+
  geom_point(aes(x = hares.avg, y = dailyf))+
  geom_abline(intercept = th_int, slope = th_h)





# AIC for food add dataset ------------------------------------------------

femn <- lm(dailyf ~ 1, wforagfood)
femf <- lm(dailyf ~ food, wforagfood)
fems <- lm(dailyf ~ food*snow.avg, wforagfood)
femt <- lm(dailyf ~ food*biomass.avg, wforagfood)
femh <- lm(dailyf ~ food*hares.avg, wforagfood)
feml <- lm(dailyf ~ food*lynx, wforagfood)
femp <- lm(dailyf ~ food*phase, wforagfood)

#list models
modsfem <- list(femn, femf, fems, femt, femh, feml, femp)

#name models
Namesfem <- c('femn', 'femf', 'fems', 'femt', 'femh', 'feml', 'femp')

#make AIC table
AICfem <- as.data.table(aictab(REML = F, cand.set = modsfem, modnames = Namesfem, sort = TRUE))

#remove unwanted columns
AICfem[, ModelLik := NULL]
AICfem[, Cum.Wt := NULL]

#round whole table to 3 dec places
AICfem <- AICfem %>% mutate_if(is.numeric, round, digits = 3)

#run function and get R2s for all models
R2sfem <- lapply(modsfem, collectR2)
R2sfem <- rbindlist(R2sfem, fill = TRUE)
R2sfem$Modnames <- Namesfem

#merge R2s with AIC table
AICfem <- merge(AICfem, R2sfem, by = "Modnames")
setorder(AICfem, "Delta_AICc")



ggplot(wforagfood)+
  geom_point(aes(x = snow.avg, y = dailyf, color = food))+
  geom_smooth(aes(x = snow.avg, y = dailyf, color = food), method = "lm")
  

