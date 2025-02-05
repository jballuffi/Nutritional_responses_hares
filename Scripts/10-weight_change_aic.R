
#explain weight change residuals

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

weights <- readRDS("Output/Data/weight_change.rds")
datannual <- readRDS("Output/Data/full_data_annual.rds")

#remove cases without a sex
weights <- weights[!is.na(sex)]

#merge with annual env data
weights <- merge(weights, datannual, by = c("year", "yearfactor"), all.x = TRUE)

#take only the years where there was food add
foodyears <- weights[food == 1, unique(winter)]
wfood <- weights[winter %in% foodyears]

#make a controls only dataset
wcon <- weights[food == 0]

#end with two datasets: wfem (food add and control weights, food add years only) 
#                       wcon (control weights, all years)



# AIC for control individuals only -----------------------------------------

#no difference between males and females
summary(lm(weight.c.resid ~ sex, wcon))

#make models

#models with single terms only
n <- lm(weight.c.resid ~ 1 + sex, wcon)
t <- lm(weight.c.resid ~ tempmean + sex, wcon)

s <- lm(weight.c.resid ~ snow + sex, wcon)
f <- lm(weight.c.resid ~ twig + sex, wcon)

h <- lm(weight.c.resid ~ haredensity + sex, wcon)
m <- lm(weight.c.resid ~ mortrate + sex, wcon)

p <- lm(weight.c.resid ~ percap + sex, wcon)




#models with two terms each
sh <- lm(weight.c.resid ~ snow.avg + hares.avg, wcon)
sl <- lm(weight.c.resid ~ snow.avg + lynx, wcon)
sp <- lm(weight.c.resid ~ snow.avg + phase, wcon)
th <- lm(weight.c.resid ~ biomass.avg + hares.avg, wcon)
tl <- lm(weight.c.resid ~ biomass.avg + lynx, wcon)
tp <- lm(weight.c.resid ~ biomass.avg + phase, wcon)

#list models
mods <- list(n, s, t, h, l, p,
             sh, sl, sp,
             th, tl, tp)

#name models
Names <- c('n', 's', 't', 'h', 'l', 'p',
           'sh', 'sl', 'sp',
           'th', 'tl', 'tp')

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



# Plot main effect from top model of AIC ----------------------------------

(weightbyphase <- 
  ggplot(wcon)+
  geom_abline(intercept = 0, slope = 0, linetype = 2)+
  geom_boxplot(aes(y = weight.c.resid, x = phase, fill = sex), alpha = .5)+
  labs(y = "Weight change residual (g)", x = "Cyle phase")+
  themepoints)



# AIC for food add female years -------------------------------------------

#food add, snow depth, food availability, hare density, lynx density, phase
#snowgrid, winter

femn <- lm(weight.c.resid ~ 1, wfem)
femf <- lm(weight.c.resid ~ food, wfem)
fems <- lm(weight.c.resid ~ food*snow.avg, wfem)
femt <- lm(weight.c.resid ~ food*biomass.avg, wfem)
femh <- lm(weight.c.resid ~ food*hares.avg, wfem)
feml <- lm(weight.c.resid ~ food*lynx, wfem)
femp <- lm(weight.c.resid ~ food*phase, wfem)

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




# Plot main effect from top model -----------------------------------------


#to get line predictions for both variables
effs_fems <- as.data.table(ggpredict(fems, terms = c("snow.avg", "food")))
setnames(effs_fems, "group", "food")


(weightbysnow <- 
  ggplot(wfem)+
  geom_abline(intercept = 0, slope = 0, linetype = 2, linewidth = 0.75)+
  geom_point(aes(x = snow.avg, y = weight.c.resid, color = food), alpha = .5, data = wfem)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = food, fill = food), alpha = .2, data = effs_fems)+
  geom_line(aes(x = x, y = predicted, group = food, color = food), linewidth = 1, data = effs_fems)+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(y = "Weight change residual (g)", x = "Average snow depth (cm)")+
  themepoints)

(weightbyyear <- 
  ggplot(wfem)+
  geom_abline(intercept = 0, slope = 0, linetype = 2, linewidth = 0.75)+
  geom_boxplot(aes(x = winter, y = weight.c.resid, fill = food), alpha = .5)+
  scale_fill_manual(values = foodcols)+
  labs(y = "Weight change residual (g)", x = "Winter")+
  themepoints)





# save AIC tables ---------------------------------------------------------

write.csv(AICcon, "Output/Tables/AIC_weightchange_controls.csv")
write.csv(AICfem, "Output/Tables/AIC_weightchange_foods.csv")

ggsave("Output/Figures/weight_by_phase_controls.jpeg", weightbyphase, width = 5, height = 4, unit = "in")
ggsave("Output/Figures/weight_by_year_foods.jpeg", weightbyyear, width = 5, height = 4, unit = "in")
ggsave("Output/Figures/weight_by_snow_foods.jpeg", weightbysnow, width = 5, height = 4, unit = "in")



