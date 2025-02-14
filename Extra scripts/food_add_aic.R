
#old script for AIC on food add data

# AIC to explain weekly foraging in food add data set ------------------------------------------------

#models for controls only
foodnull <- lmer(forage ~ 1 + (1|id), foragfood) #null model
foodb <- lmer(forage ~ biomass*food + nightlength + (1|id), foragfood) #biomass food
foodp <- lmer(forage ~ percap*food + nightlength + (1|id), foragfood) #percapita food
foodm <- lmer(forage ~ mortrate*food + nightlength + (1|id), foragfood) #quality food
foodt <- lmer(forage ~ temp*food + nightlength + (1|id), foragfood) #hare density

#list models
modsfood <- list(foodnull, foodb, foodp, foodm, foodt)

#name models
Namesfood <- c('Null', 'Biomass', 'Per Capita', 'Predation', 'Temperature')

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

#not far behind = per cap
foodt_pred <- as.data.table(ggpredict(foodt, terms = c("temp", "food")))
setnames(foodt_pred, "group", "food")

(foodt_fig <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage, color = food), alpha = .2, data = foragfood)+
    geom_ribbon(aes(x = x, ymax = conf.high, ymin = conf.low, fill = food), alpha = .5, data = foodt_pred)+
    geom_line(aes(x = x, y = predicted, color = food), data = foodt_pred)+
    scale_color_manual(values = foodcols)+
    scale_fill_manual(values = foodcols)+
    labs(x = "Daily temperature", y = "Foraging effort (hr)")+
    themepoints)
