
#script to assess how environmental factors impact foraging rates on a winter and daily basis

#To do: make a full AIC function to shorten script

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly.rds")



# merge info and make a control and food add dataset --------

forag <- merge(forag, dat, by = c("year", "yearfactor", "week", "snowgrid"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]

#take only the years where there was food add
foodyears <- forag[food == 1, unique(winter)]

#take only females for food add comparisons
foragfood <- forag[winter %in% foodyears & sex == female]

#difference between foraging effort of males and control females?
summary(lm(forage ~ sex, data = foragcon))



# look at potential correlations within data ------------------------------

#correlations: biomass and quality
#              hare density and per cap

cor(dat$percap, dat$biomass)

plot(foragcon$percap ~ foragcon$biomass)



# AIC to explain weekly foraging for controls only ------------------------

#models for controls only
n <- lmer(forage ~ 1 + (1|id), foragfood[food == 0]) #null model
st <- lmer(forage ~ temp + sex + mortrate + (1|id), foragfood[food == 0]) #base model: temp and sex and mortality
b <- lmer(forage ~ biomass + temp + sex + mortrate + (1|id), foragfood[food == 0]) #biomass food
pc <- lmer(forage ~ percap + temp + sex + mortrate + (1|id), foragfood[food == 0]) #percapita food
q <- lmer(forage ~ quality + temp + sex + mortrate + (1|id), foragfood[food == 0]) #quality food
h <- lmer(forage ~ haredensity + temp + sex + mortrate + (1|id), foragfood[food == 0]) #hare density

#list models
mods <- list(n, st, b, pc, q, h)

#name models
Names <- c('Null', 'Base', 'Biomass', 'Per Capita', 'Quality', 'Density')

#make AIC table
AICcon <- as.data.table(aictab(REML = F, cand.set = mods, modnames = Names, sort = TRUE))

#remove unwanted columns
AICcon[, ModelLik := NULL]
AICcon[, Cum.Wt := NULL]

#round whole table to 3 dec places
AICcon <- AICcon %>% mutate_if(is.numeric, round, digits = 3)

#run function and get R2s for all models
R2scon <- lapply(mods, collectR2_mixed)
R2scon <- rbindlist(R2scon, fill = TRUE)
R2scon$Modnames <- Names

#merge R2s with AIC table
AICcon <- merge(AICcon, R2scon, by = "Modnames")
setorder(AICcon, "Delta_AICc")



# top models  -------------------------------------------------------------

#quality is the top model
summary(q)
q_pred <- as.data.table(ggpredict(q, terms = "quality"))

#
(qualfig <- 
    ggplot()+
    geom_point(aes(x = quality, y = forage), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = q_pred)+
    geom_line(aes(x = x, y = predicted), data = q_pred)+
    labs(x = "Twig solubility (%)", y = "Weekly foraging effort (hr/day)")+
    themepoints)

#second model is biomass
summary(b)
b_pred <- as.data.table(ggpredict(b, terms = "biomass"))

(biofig <- 
    ggplot()+
    geom_point(aes(x = biomass, y = forage), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = b_pred)+
    geom_line(aes(x = x, y = predicted), data = b_pred)+
    labs(x = "Twig biomass (hare/ha)", y = "Weekly foraging effort (hr/day)")+
    themepoints)




# AIC to explain weekly foraging in food add data set ------------------------------------------------

#models for controls only
foodnull <- lmer(forage ~ 1 + (1|id), foragfood) #null model
foodbase <- lmer(forage ~ food + temp + sex + mortrate + (1|id), foragfood) #base model: temp and sex and mortality
foodb <- lmer(forage ~ biomass*food + temp + sex + mortrate + (1|id), foragfood) #biomass food
foodpc <- lmer(forage ~ percap*food + temp + sex + mortrate + (1|id), foragfood) #percapita food
foodq <- lmer(forage ~ quality*food + temp + sex + mortrate + (1|id), foragfood) #quality food
foodh <- lmer(forage ~ haredensity*food + temp + sex + mortrate + (1|id), foragfood) #hare density

#list models
modsfood <- list(foodnull, foodbase, foodb, foodpc, foodq, foodh)

#name models
Namesfood <- c('Null', 'Base', 'Biomass', 'Per Capita', 'Quality', 'Density')

#make AIC table
AICfood <- as.data.table(aictab(REML = F, cand.set = modsfood, modnames = Namesfood, sort = TRUE))

#remove unwanted columns
AICfood[, ModelLik := NULL]
AICfood[, Cum.Wt := NULL]

#round whole table to 3 dec places
AICfood <- AICfood %>% mutate_if(is.numeric, round, digits = 3)

#run function and get R2s for all models
R2sfood <- lapply(modsfood, collectR2_mixed)
R2sfood <- rbindlist(R2sfood, fill = TRUE)
R2sfood$Modnames <- Namesfood

#merge R2s with AIC table
AICfood <- merge(AICfood, R2sfood, by = "Modnames")
setorder(AICfood, "Delta_AICc")



# Top models for food AIC -------------------------------------------------

#not far behind = per cap
foodh_pred <- as.data.table(ggpredict(foodh, terms = c("haredensity", "food")))
setnames(foodh_pred, "group", "food")

(foodh_fig <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = forage, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodh_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodh_pred)+
    scale_color_manual(values = foodcols)+
    scale_fill_manual(values = foodcols)+
    labs(x = "Hare density (hare/ha)", y = "Weekly foraging effort (hr)")+
    themepoints)


#not far behind = per cap
foodpc_pred <- as.data.table(ggpredict(foodpc, terms = c("percap", "food")))
setnames(foodpc_pred, "group", "food")

(foodpc_fig <- 
  ggplot()+
  geom_point(aes(x = percap, y = forage, color = food), alpha = .2, data = foragfood)+
  geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodpc_pred)+
  geom_line(aes(x = x, y = predicted, color = food), data = foodpc_pred)+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Weekly twig availability (kg/hare)", y = "Weekly foraging effort (hr)")+
  themepoints)



# save --------------------------------------------------------------------

#save results from control only AIC
write.csv(AICcon, "Output/Tables/AIC_foraging_winter_controls.csv")
ggsave("Output/Figures/foraging_quality.jpeg", qualfig, width = 6, height = 5, unit = "in")
ggsave("Output/Figures/foraging_biomass.jpeg", biofig, width = 6, height = 5, unit = "in")


write.csv(AICfood, "Output/Tables/AIC_foraging_winter_foods.csv")
ggsave("Output/Figures/foraging_food_density.jpeg", foodh_fig, width = 6, height = 5, unit = "in")
ggsave("Output/Figures/foraging_food_percapita.jpeg", foodpc_fig, width = 6, height = 5, unit = "in")


