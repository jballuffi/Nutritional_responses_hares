
#script to assess how environmental factors impact foraging rates on a winter and daily basis

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# merge info and make a control and food add dataset --------

#read data
forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

#get controls only
foragcon <- forag[food == "Control"]

#take only the years where there was food add
foodyears <- forag[food == "Suppl.", unique(winter)]

#take only females for food add comparisons
foragfood <- forag[winter %in% foodyears]



# descriptive results --------------------------------------------------

#effect of food on food add dataset
suppmod <- lm(forage ~ food, foragfood)
suppanova <- anova(suppmod)
suppsum <- summary(suppmod)
supp_p <- round(suppanova$`Pr(>F)`[1], 3)
supp_t <- round(suppsum$coefficients[, 3][2], 2) #t-value
supp_df <- suppanova$Df[2]
rsq(suppmod)


#effect of sex on controls
sexmod <- anova(lm(forage ~ sex, foragcon))
psex <- round(sexmod$`Pr(>F)`[1], 3)

#effect of night length on foraging rate
nightmod <- lm(forage ~ nightlength, forag) #make model
nightsum <- summary(nightmod) #sum of model
nightanova <- anova(nightmod) #anova of model
nightcoef <- round(nightsum$coefficients[, 1][2]*60, 1) #coefficient
nightse <- round(nightsum$coefficients[, 2][2]*60, 1) #standard error
pnight <- round(nightanova$`Pr(>F)`[1], 3) #p-value
tnight <- round(nightsum$coefficients[, 3][2], 2) #t-value
dfnight <- nightanova$Df[2]
rsq(nightmod)


# correlation tests -------------------------------------------------------

cor(forag$nightlength, forag$haredensity)
cor(forag$nightlength, forag$biomass)
cor(forag$biomass, forag$temp)
cor(forag$biomass, forag$percap)
cor(forag$biomass, forag$haredensity)



# AIC to explain weekly foraging for controls only ------------------------

#get combos for triple terms
combn(c("biomass", "temp", "haredensity", "mortrate"), 3)

   #MODELS
#models for controls only
n <- lmer(forage ~ nightlength + (1|id), foragcon) #null model

#single terms 
S1 <- lmer(forage ~ biomass + nightlength + (1|id), foragcon) #biomass food
S2 <- lmer(forage ~ haredensity + nightlength + (1|id), foragcon) #percapita food
S3 <- lmer(forage ~ mortrate + nightlength + (1|id), foragcon) #predation risk/mortality rate
S4 <- lmer(forage ~ temp + nightlength + (1|id), foragcon) #temperature

#double terms 
D1 <- lmer(forage ~ biomass + mortrate + nightlength + (1|id), foragcon) #biomass and mortality
D2 <- lmer(forage ~ biomass + temp + nightlength + (1|id), foragcon) #biomass and temp
D3 <- lmer(forage ~ haredensity + mortrate + nightlength + (1|id), foragcon) #percap and mortality
D4 <- lmer(forage ~ haredensity + temp + nightlength + (1|id), foragcon) #percap and temp
D5 <- lmer(forage ~ mortrate + temp + nightlength + (1|id), foragcon) #mortality and temp
D6 <- lmer(forage ~ haredensity + biomass + nightlength + (1|id), foragcon)

#triple terms
T1 <- lmer(forage ~ biomass + temp + haredensity + nightlength + (1|id), foragcon)
T2 <- lmer(forage ~ biomass + mortrate + temp + nightlength + (1|id), foragcon)
T3 <- lmer(forage ~ biomass + haredensity + mortrate + nightlength + (1|id), foragcon)
T4 <- lmer(forage ~ haredensity + temp + mortrate + nightlength + (1|id), foragcon)

Q1 <- lmer(forage ~ haredensity + biomass + mortrate + temp + nightlength + (1|id), foragcon)


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



# top model results -------------------------------------------------------------

#biomass and temperature
summary(D2)

confint(D2)

#get R2s
D2MR2 <- round(r.squaredGLMM(D2), 2)[1]
D2CR2 <- round(r.squaredGLMM(D2), 2)[2]

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

#get standard errors
t_se <- round(se.fixef(D2)[3], 3)
sb_se <- round(se.fixef(D2)[2], 3)



# take top model and test on food adds ------------------------------------

foodmod <- lmer(forage ~ biomass*food + temp*food + nightlength + (1|id), foragfood) 
summary(foodmod)
confint(foodmod)

#R2s
foodMR2 <- round(r.squaredGLMM(foodmod), 2)[1]
foodCR2 <- round(r.squaredGLMM(foodmod), 2)[2]


#get t-values
foodsb_t = round(coef(summary(foodmod))[,"t value"][2], 2)
foodt_t = round(coef(summary(foodmod))[,"t value"][4], 2)
food_t = round(coef(summary(foodmod))[,"t value"][3], 2)
intsb_t = round(coef(summary(foodmod))[,"t value"][6], 2 )
intt_t = round(coef(summary(foodmod))[,"t value"][7], 2 )

#get coefficients
foodt_coef <- round(fixef(foodmod)[4], 3)
food_coef <- round(fixef(foodmod)[3], 2)
foodsbf_coef <- round(fixef(foodmod)[6], 2)
foodtf_coef <- round(fixef(foodmod)[7], 2)

#get standard errorts
foodt_se <- round(se.fixef(foodmod)[4], 3)
food_se <- round(se.fixef(foodmod)[3], 2)
foodsbf_se <- round(se.fixef(foodmod)[6], 2)
foodtf_se <- round(se.fixef(foodmod)[7], 2)


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
   geom_point(aes(x = biomass, y = forage), alpha = .3, data = foragcon)+
   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = sb_pred)+
   geom_line(aes(x = x, y = predicted), data = sb_pred)+
   labs(x = NULL, y = "Foraging effort (hr/day)", subtitle = "A)")+
   themethesisright)

#figures for foraging effort in response to temperature
(tfig <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = t_pred)+
    geom_line(aes(x = x, y = predicted), data = t_pred)+
    labs(x = NULL, y = NULL, subtitle = "B)")+
    themethesisright)

#foraging rate of controls and food adds in response to food biomass
(foodb_fig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = forage, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodb_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodb_pred)+
    scale_color_manual(values = foodcols, name = "Food treatment")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    labs(x = "Twig biomass (kg/ha)", y = "Foraging effort (hr/day)", subtitle = "C)")+
    themethesisright+
    theme(legend.position = c(.2, .85)))

#foraging rate of food adds and controls in response to temp
(foodt_fig <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodt_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodt_pred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Temperature (°C)", y = NULL, subtitle = "D)")+
    themethesisright)

#make full paneled figure
fullfig <- ggarrange(bfig, tfig, foodb_fig, foodt_fig, nrow = 2, ncol = 2)




# save --------------------------------------------------------------------

#save results from control only AIC
write.csv(AICcon, "Output/Tables/AIC_foraging_winter_controls.csv")

ggsave("Output/Figures/foraging_results.jpeg", fullfig, width = 9, height = 9, unit = "in")


