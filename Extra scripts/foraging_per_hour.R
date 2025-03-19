#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# prep final datasets --------

#read in data
forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")

#make forage per hour of night
forag[, foragehr := forage*60/nightlength] #minutes/hour

#make travel per hour of night
forag[, travelhr := move/nightlength] #minute/hour

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]

#take only the years where there was food add
foodyears <- forag[food == 1, unique(winter)]

#take only females for food add comparisons
foragfood <- forag[winter %in% foodyears & sex == "female"]



# correlation tests -------------------------------------------------------

cor(forag$biomass, forag$nightlength)

cor(forag$biomass, forag$temp)
cor(forag$biomass, forag$percap)
cor(forag$percap, forag$temp)
cor(forag$percap, forag$mortrate)



# AIC to explain foraging effort of controls only ------------------------

#models for controls only
n <- lmer(foragehr ~ (1|id), foragcon) #null model

#single terms 
S1 <- lmer(foragehr ~ biomass + (1|id), foragcon) #biomass food
S2 <- lmer(foragehr ~ percap + (1|id), foragcon) #percapita food
S3 <- lmer(foragehr ~ mortrate + (1|id), foragcon) #predation risk/mortality rate
S4 <- lmer(foragehr ~ temp + (1|id), foragcon) #temperature

#double terms 
D1 <- lmer(foragehr ~ biomass + mortrate + (1|id), foragcon) #biomass and mortality
D2 <- lmer(foragehr ~ biomass + temp + (1|id), foragcon) #biomass and temp
D3 <- lmer(foragehr ~ percap + mortrate + (1|id), foragcon) #percap and mortality
D4 <- lmer(foragehr ~ percap + temp + (1|id), foragcon) #percap and temp
D5 <- lmer(foragehr ~ mortrate + temp + (1|id), foragcon) #mortality and temp

#triple terms
T1 <- lmer(foragehr ~ biomass + mortrate + temp + (1|id), foragcon)
T2 <- lmer(foragehr ~ percap + mortrate + temp + (1|id), foragcon)

#list models
mods <- list(n, S1, S2, S3, S4, D1, D2, D3, D4, D5, T1, T2)
codes <- c("Null", "S1", "S2", "S3", "S4", "D1", "D2", "D3", "D4", "D5", "T1", "T2")
vars <- c("None", "SB", "PCSB", "Mortality", "Temperature",
          "SB + Mortality", "SB + Temperature", "PCSB + Mortality", "PCSB + Temperature", "Mortality + Temperature",
          "SB + Mortality + Temperature", "PCSB + Mortality + Temperature")

#use function from R/ folder to make an AIC table comparing all models.
#function also extracts R2s
AICcon <- make_aic_lmer(modlist = mods, modnames = codes)

#make a dataframe with model codes and their designs
moddesign <- data.table(
  Modnames = codes,
  Variables = vars
)

AICcon <- merge(moddesign, AICcon, by = "Modnames")
setorder(AICcon, "Delta_AICc")



# results of top model -------------------------------------------------------------

#biomass and temperature
summary(D2)

#make predictive tables
sb_pred <- as.data.table(ggpredict(D2, terms = c("biomass"))) #soluble biomass (sb)
t_pred <- as.data.table(ggpredict(D2, terms = c("temp"))) #temperature (t)

#get t-values
sb_t = round(coef(summary(D2))[,"t value"][2], 2)
t_t = round(coef(summary(D2))[,"t value"][3], 2)

#get f-values
D2anova <- anova(D2)
sb_f <- round(D2anova$`F value`[1], 2)
t_f <- round(D2anova$`F value`[2], 2)

#get coefficients
sb_coef <- round(fixef(D2)[2], 3)
t_coef <- round(fixef(D2)[3], 3)

#get standard errorts
t_se <- round(se.fixef(D2)[3], 3)
sb_se <- round(se.fixef(D2)[2], 3)



# test top model on food add dataset ------------------------------------

foodmod <- lmer(foragehr ~ biomass*food + temp*food + (1|id), foragfood) 
summary(foodmod)

#get t-values
foodsb_t = round(coef(summary(foodmod))[,"t value"][2], 2)
foodt_t = round(coef(summary(foodmod))[,"t value"][4], 2)
food_t = round(coef(summary(foodmod))[,"t value"][3], 2)

#get coefficients
foodt_coef <- round(fixef(foodmod)[4], 3)
food_coef <- round(fixef(foodmod)[3], 2)

#get standard errorts
foodt_se <- round(se.fixef(foodmod)[4], 3)
food_se <- round(se.fixef(foodmod)[3], 2)

#show effect of biomass*food
foodb_pred <- as.data.table(ggpredict(foodmod, terms = c("biomass", "food")))
setnames(foodb_pred, "group", "food")

#show effect of temperature*food
foodt_pred <- as.data.table(ggpredict(foodmod, terms = c("temp", "food")))
setnames(foodt_pred, "group", "food")



# Figure for controls and food add test -------------------------------------------

#figure for foraging effort in response to soluble biomass
(bfig <- 
   ggplot()+
   geom_point(aes(x = biomass, y = foragehr), alpha = .3, data = foragcon)+
   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = sb_pred)+
   geom_line(aes(x = x, y = predicted), data = sb_pred)+
   labs(x = NULL, y = "Foraging effort (min/hr)", subtitle = "A)")+
   themepoints)

#figures for foraging effort in response to temperature
(tfig <- 
    ggplot()+
    geom_point(aes(x = temp, y = foragehr), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = t_pred)+
    geom_line(aes(x = x, y = predicted), data = t_pred)+
    labs(x = NULL, y = NULL, subtitle = "B)")+
    themepoints)

#foraging rate of controls and food adds in response to food biomass
(foodb_fig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = foragehr, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodb_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodb_pred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Soluble biomass (kg/ha)", y = "Foraging effort (min/hr)", subtitle = "C)")+
    themepoints)

#foraging rate of food adds and controls in response to temp
(foodt_fig <- 
    ggplot()+
    geom_point(aes(x = temp, y = foragehr, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodt_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodt_pred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Daily temperature (C)", y = NULL, subtitle = "D)")+
    themepoints)

#make full paneled figure
fullfig <- ggarrange(bfig, tfig, foodb_fig, foodt_fig, nrow = 2, ncol = 2)


plot(forag$forage ~ forag$week)


# percap result -----------------------------------------------------------

summary(D4)
pc_pred <- as.data.table(ggpredict(D4, terms = "percap"))

ggplot(foragcon)+
  geom_point(aes(x = percap, y = foragehr), alpha = 0.2)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.5, data = pc_pred)+
  geom_line(aes(x = x, y = predicted), data = pc_pred)+
  labs(x = "Hare density (hares/ha)", y = "Foraging effort (min/hr)")+
  themepoints



# look at hare density instead --------------------------------------------

test1 <- lmer(foragehr ~ haredensity + (1|id), foragcon)
test1dt <- as.data.table(ggpredict(test1, terms = "haredensity"))

ggplot(foragcon)+
  geom_point(aes(x = haredensity, y = foragehr), alpha = 0.2)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.5, data = test1dt)+
  geom_line(aes(x = x, y = predicted), data = test1dt)+
  labs(x = "Hare density (hares/ha)", y = "Foraging effort (min/hr)")+
  themepoints

test2 <- lmer(foragehr ~ haredensity*food + (1|id), foragfood)
test2dt <- as.data.table(ggpredict(test2, terms = c("haredensity", "food")))
setnames(test2dt, "group", "food")

ggplot(foragfood)+
  geom_point(aes(x = haredensity, y = foragehr, color = food), alpha = .2, data = foragfood)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = test2dt)+
  geom_line(aes(x = x, y = predicted, color = food), data = test2dt)+
  scale_color_manual(values = foodcols, guide = NULL)+
  scale_fill_manual(values = foodcols, guide = NULL)+
  labs(x = "Hare density (hares/ha)", y = "Foraging effort (min/hr)")+
  themepoints


