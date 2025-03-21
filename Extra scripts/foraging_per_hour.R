#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# prep final datasets --------

#read in data
forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")

#make travel per hour of night
forag[, travelhr := move/nightlength] #minute/hour

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]

#take only the years where there was food add
foodyears <- forag[food == 1, unique(winter)]

#take only females for food add comparisons
foragfood <- forag[winter %in% foodyears]



# difference between foraging per day and foraging per hour of nig --------

ggplot(forag)+
  geom_point(aes(x = nightlength, y = forage), alpha = 0.5)+
  geom_smooth(aes(x = nightlength, y = forage), method = "lm", color = "black")+
  labs(y = "Foraging effort (hr/day)", x = "Night length (hr)")+
  theme_bw()

ggplot(forag)+
  geom_point(aes(x = nightlength, y = foragehr), alpha = 0.5)+
  geom_smooth(aes(x = nightlength, y = foragehr), color = "black")+
  labs(y = "Foraging effort (min/hr)", x = "Night length (hr)")+
  theme_bw()

ggplot(forag)+
  geom_point(aes(x = forage, y = foragehr, color = week), alpha = 0.5)+
  labs(y = "Foraging effort (min/hr)", x = "Foraging effort (hr/day)")+
  theme_bw()

ggplot(forag)+
  geom_point(aes(x = week, y = forage), alpha = 0.5)+
  labs(y = "Foraging effort (hr/day)", x = "Week from Jan 1")+
  theme_bw()

ggplot(forag)+
  geom_point(aes(x = week, y = foragehr), alpha = 0.5)+
  labs(y = "Foraging effort (min/hr)", x = "Week from Jan 1")+
  theme_bw()



# AIC to explain foraging effort of controls only ------------------------

#MODELS
#models for controls only
n <- lmer(foragehr ~ 1 + (1|id), foragcon) #null model

#single terms 
S1 <- lmer(foragehr ~ biomass + (1|id), foragcon) #biomass food
S2 <- lmer(foragehr ~ haredensity + (1|id), foragcon) #percapita food
S3 <- lmer(foragehr ~ mortrate + (1|id), foragcon) #predation risk/mortality rate
S4 <- lmer(foragehr ~ temp + (1|id), foragcon) #temperature

#double terms 
D1 <- lmer(foragehr ~ biomass + mortrate + (1|id), foragcon) #biomass and mortality
D2 <- lmer(foragehr ~ biomass + temp + (1|id), foragcon) #biomass and temp
D3 <- lmer(foragehr ~ haredensity + mortrate + (1|id), foragcon) #percap and mortality
D4 <- lmer(foragehr ~ haredensity + temp + (1|id), foragcon) #percap and temp
D5 <- lmer(foragehr ~ mortrate + temp + (1|id), foragcon) #mortality and temp
D6 <- lmer(foragehr ~ haredensity + biomass + (1|id), foragcon)

#triple terms
T1 <- lmer(foragehr ~ biomass + temp + haredensity + (1|id), foragcon)
T2 <- lmer(foragehr ~ biomass + mortrate + temp + (1|id), foragcon)
T3 <- lmer(foragehr ~ biomass + haredensity + mortrate + (1|id), foragcon)
T4 <- lmer(foragehr ~ haredensity + temp + mortrate + (1|id), foragcon)

Q1 <- lmer(foragehr ~ haredensity + biomass + mortrate + temp + (1|id), foragcon)


#list models
mods <- list(n, 
             S1, S2, S3, S4, 
             D1, D2, D3, D4, D5, D6, 
             T1, T2, T3, T4, 
             Q1)

codes <- c("Null", 
           "S1", "S2", "S3", "S4", 
           "D1", "D2", "D3", "D4", "D5", "D6", 
           "T1", "T2", "T3", "T4", 
           "Q1")

#use function from R/ folder to make an AIC table comparing all models.
#function also extracts R2s
AICcon <- make_aic_lmer(modlist = mods, modnames = codes)


#print formulas from list of models
getform <- function(x){
  as.character(formula(x)[3])
}
formulas <- lapply(mods, getform)
formulas <- unlist(formulas)

#make data table with model names and formulas
moddesign <- data.table(
  Modnames = codes,
  Variables = formulas
)

#merge model designs with AIC table
AICcon <- merge(moddesign, AICcon, by = "Modnames")
setorder(AICcon, "Delta_AICc")




# results of top model -------------------------------------------------------------

#biomass and temperature
summary(T1)

#make predictive tables
sb_pred <- as.data.table(ggpredict(T1, terms = c("biomass"))) #soluble biomass (sb)
t_pred <- as.data.table(ggpredict(T1, terms = c("temp"))) #temperature (t)
d_pred <- as.data.table(ggpredict(T1, terms = c("haredensity"))) #hare density 


#figure for foraging effort in response to soluble biomass
(bfig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = foragehr), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = sb_pred)+
    geom_line(aes(x = x, y = predicted), data = sb_pred)+
    labs(x = "Twig biomass (kg/ha)", y = "Foraging effort (min/hr)")+
    themepoints)

#figures for foraging effort in response to temperature
(tfig <- 
    ggplot()+
    geom_point(aes(x = temp, y = foragehr), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = t_pred)+
    geom_line(aes(x = x, y = predicted), data = t_pred)+
    labs(x = "Temperature (C)", y = "Foraging effort (min/hr)")+
    themepoints)

#figures for foraging effort in response to temperature
(dfig <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = foragehr), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = d_pred)+
    geom_line(aes(x = x, y = predicted), data = d_pred)+
    labs(x = "Hare density (hare/ha)", y = "Foraging effort (min/hr)")+
    themepoints)





# test top model on food add dataset ------------------------------------

T1food <- lmer(foragehr ~ biomass*food + temp*food + haredensity*food + (1|id), foragfood)
summary(T1food)

#show effect of biomass*food
foodb_pred <- as.data.table(ggpredict(T1food, terms = c("biomass", "food")))
setnames(foodb_pred, "group", "food")

#show effect of temperature*food
foodt_pred <- as.data.table(ggpredict(T1food, terms = c("temp", "food")))
setnames(foodt_pred, "group", "food")

#hare density 
foodd_pred <- as.data.table(ggpredict(T1food, terms = c("haredensity", "food")))
setnames(foodd_pred, "group", "food")



# Figure for controls and food add test -------------------------------------------

#foraging rate of controls and food adds in response to food biomass
(foodb_fig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = foragehr, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodb_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodb_pred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Twig biomass (kg/ha)", y = "Foraging effort (min/hr)")+
    themepoints)

#foraging rate of food adds and controls in response to temp
(foodt_fig <- 
    ggplot()+
    geom_point(aes(x = temp, y = foragehr, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodt_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodt_pred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Daily temperature (C)", y = "Foraging effort (min/hr)")+
    themepoints)

(foodd_fig <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = foragehr, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodd_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodd_pred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Hare density (hare/ha)", y = "Foraging effort (min/hr)")+
    themepoints)

#make full paneled figure
fullfig <- ggarrange(foodb_fig, foodt_fig, foodd_fig, nrow = 3, ncol = 2)



