
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
n <- lmer(forage ~ 1 + (1|id), foragcon) #null model

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
mods <- list(S1, S2, S3, S4, D1, D2, D3, D4, D5, T1, T2)
names <- c("S1", "S2", "S3", "S4", "D1", "D2", "D3", "D4", "D5", "T1", "T2")

#make AIC table
AICcon <- as.data.table(aictab(cand.set = mods, sort = TRUE, modnames = names))

#remove unwanted columns
AICcon[, ModelLik := NULL]
AICcon[, Cum.Wt := NULL]

#round whole table to 3 dec places
AICcon <- AICcon %>% mutate_if(is.numeric, round, digits = 3)

#run function and get R2s for all models
R2scon <- lapply(mods, collectR2_mixed)
R2scon <- rbindlist(R2scon, fill = TRUE)
R2scon$Modnames <- names

#merge R2s with AIC table
AICcon <- merge(AICcon, R2scon, by = "Modnames")
setorder(AICcon, "Delta_AICc")



# top model  -------------------------------------------------------------

#biomass and temperature
summary(D2)
b_pred <- as.data.table(ggpredict(D2, terms = c("biomass")))

(bfig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = forage), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = b_pred)+
    geom_line(aes(x = x, y = predicted), data = b_pred)+
    labs(x = "Soluble biomass (kg/ha)", y = "Foraging effort (hr/day)")+
    themepoints)

t_pred <- as.data.table(ggpredict(D2, terms = c("temp")))

(tfig <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = t_pred)+
    geom_line(aes(x = x, y = predicted), data = t_pred)+
    labs(x = "Daily temperature (C)", y = "Foraging effort (hr/day)")+
    themepoints)


config <- ggarrange(bfig, tfig, nrow = 2, ncol = 1)



# take top model and test on food adds ------------------------------------

foodmod <- lmer(forage ~ biomass*food + temp*food + nightlength + (1|id), foragfood) 
summary(foodmod)

#show effect of biomass*food
foodb_pred <- as.data.table(ggpredict(foodmod, terms = c("biomass", "food")))
setnames(foodb_pred, "group", "food")

(foodb_fig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = forage, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodb_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodb_pred)+
    scale_color_manual(values = foodcols, name = "Food Treatment:")+
    scale_fill_manual(values = foodcols, name = "Food Treatment:")+
    labs(x = "Soluble biomass (kg/ha)", y = "Foraging effort (hr)")+
    themepoints_top)

#show effect of temperature*food
foodt_pred <- as.data.table(ggpredict(foodmod, terms = c("temp", "food")))
setnames(foodt_pred, "group", "food")

(foodt_fig <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodt_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodt_pred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Daily temperatre (C)", y = "Foraging effort (hr)")+
    themepoints_top)

foodfig <- ggarrange(foodb_fig, foodt_fig, nrow = 2, ncol = 1)



# save --------------------------------------------------------------------

#save results from control only AIC
write.csv(AICcon, "Output/Tables/AIC_foraging_winter_controls.csv")

ggsave("Output/Figures/Control_results.jpeg", config, width = 5, height = 8, unit = "in")
ggsave("Output/Figures/foodadd_results.jpeg", foodfig, width = 5, height = 8, unit = "in")

