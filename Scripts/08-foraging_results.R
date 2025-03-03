
#script to assess how environmental factors impact foraging rates on a winter and daily basis

#To do: make a full AIC function to shorten script

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")



# merge info and make a control and food add dataset --------

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]

#take only the years where there was food add
foodyears <- forag[food == 1, unique(winter)]

#take only females for food add comparisons
foragfood <- forag[winter %in% foodyears & sex == "female"]



# AIC to explain weekly foraging for controls only ------------------------

#models for controls only
n <- lmer(forage ~ sex + nightlength + (1|id), foragcon) #null model

#single terms 
S1 <- lmer(forage ~ biomass + sex + nightlength + (1|id), foragcon) #biomass food
S2 <- lmer(forage ~ percap + sex + nightlength + (1|id), foragcon) #percapita food
S3 <- lmer(forage ~ mortrate + sex + nightlength + (1|id), foragcon) #predation risk/mortality rate
S4 <- lmer(forage ~ temp + sex + nightlength + (1|id), foragcon) #temperature

#double terms 
D1 <- lmer(forage ~ biomass + mortrate + sex + nightlength + (1|id), foragcon) #biomass and mortality
D2 <- lmer(forage ~ biomass + temp + sex + nightlength + (1|id), foragcon) #biomass and temp
D3 <- lmer(forage ~ percap + mortrate + sex + nightlength + (1|id), foragcon) #percap and mortality
D4 <- lmer(forage ~ percap + temp + sex + nightlength + (1|id), foragcon) #percap and temp
D5 <- lmer(forage ~ mortrate + temp + sex + nightlength + (1|id), foragcon) #mortality and temp

#triple terms
T1 <- lmer(forage ~ biomass + mortrate + temp + sex + nightlength + (1|id), foragcon)
T2 <- lmer(forage ~ percap + mortrate + temp + sex + nightlength + (1|id), foragcon)

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



# top model  -------------------------------------------------------------

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

#2nd model with mortality rate (not relevant)
summary(T1)
mort_t = round(coef(summary(T1))[,"t value"][3], 2)



# Figure for control top models -------------------------------------------

#figure for foraging effort in response to soluble biomass
(bfig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = forage), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = sb_pred)+
    geom_line(aes(x = x, y = predicted), data = sb_pred)+
    labs(x = "Soluble biomass (kg/ha)", y = "Foraging effort (hr/day)", title = "A)")+
    themepoints)

#figures for foraging effort in response to temperature
(tfig <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = t_pred)+
    geom_line(aes(x = x, y = predicted), data = t_pred)+
    labs(x = "Daily temperature (C)", y = "Foraging effort (hr/day)", title = "B)")+
    themepoints)

config <- ggarrange(bfig, tfig, nrow = 2, ncol = 1)



# take top model and test on food adds ------------------------------------

foodmod <- lmer(forage ~ biomass*food + temp*food + nightlength + (1|id), foragfood) 
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

(foodb_fig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = forage, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodb_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodb_pred)+
    scale_color_manual(values = foodcols, name = "Food")+
    scale_fill_manual(values = foodcols, name = "Food")+
    labs(x = "Soluble biomass (kg/ha)", y = "Foraging effort (hr)", title = "A)")+
    themepoints)

#show effect of temperature*food
foodt_pred <- as.data.table(ggpredict(foodmod, terms = c("temp", "food")))
setnames(foodt_pred, "group", "food")

(foodt_fig <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodt_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodt_pred)+
    scale_color_manual(values = foodcols, name = "Food")+
    scale_fill_manual(values = foodcols, name = "Food")+
    labs(x = "Daily temperatre (C)", y = "Foraging effort (hr)", title = "B)")+
    themepoints)

foodfig <- ggarrange(foodb_fig, foodt_fig, nrow = 2, ncol = 1)



# save --------------------------------------------------------------------

#save results from control only AIC
write.csv(AICcon, "Output/Tables/AIC_foraging_winter_controls.csv")

ggsave("Output/Figures/Control_results.jpeg", config, width = 5, height = 8, unit = "in")
ggsave("Output/Figures/foodadd_results.jpeg", foodfig, width = 5, height = 8, unit = "in")

