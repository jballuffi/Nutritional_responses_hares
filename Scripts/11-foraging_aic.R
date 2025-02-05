
#script to assess how environmental factors impact foraging rates on a winter and daily basis

#TO DO: mixed models for daily. 
#To do: make a full AIC function to shorten script

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_daily.rds")



# merge info and make a control and food add dataset --------

#get weekly values from full env dat to merge with forag data

dat[, week := week(date), year]

datweek <- dat[, .(haredensity = mean(haredensity),
                   mortrate = mean(mortrate),
                   snow = mean(snow),
                   twig = mean(twig),
                   percap = mean(percap),
                   temp = mean(tempmean, na.rm = TRUE)),
               by = .(year, week)]

forag <- merge(forag, datweek, by = c("year", "week"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]

#take only the years where there was food add
foodyears <- forag[food == 1, unique(winter)]

#take only females for food add comparisons
foragfood <- forag[winter %in% foodyears]

#difference between foraging effort of males and control females?
summary(lm(forage ~ sex, data = foragcon))

ggplot(datweek)+
  geom_path(aes(x = week, y = snow))+
  facet_wrap(~year)



# AIC to explain weekly foraging for controls only ------------------------

#models for controls only
n <- lm(forage ~ 1 + sex, foragcon)
s <- lm(forage ~ snow + sex, foragcon)
tw <- lm(forage ~ twig + sex, foragcon)
pc <- lm(forage ~ percap + sex, foragcon)
h <- lm(forage ~ haredensity + sex, foragcon)
m <- lm(forage ~ mortrate + sex, foragcon)
te <- lm(forage ~ temp +sex, foragcon)

#list models
mods <- list(n, s, tw, pc, h, m, te)

#name models
Names <- c('null', 'snow', 'twig', 'per-capita', 'hares', 'mortality', 'temperature')

#make AIC table
AICcon <- as.data.table(aictab(REML = F, cand.set = mods, modnames = Names, sort = TRUE))
summary(lm(forage ~ sex, data = foragcon))

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
summary(pc)
pc_int <- coefficients(pc)['(Intercept)']
pc_s <- coefficients(pc)['percap']

ggplot(foragcon)+
  geom_point(aes(x = percap, y = forage), alpha = .3)+
  geom_abline(intercept = pc_int, slope = pc_s)+
  labs(x = "Weekly twig availability (kg/hare)", y = "Weekly foraging effort (hr/day)")+
  themepoints



# AIC to explain weekly foraging in food add data set ------------------------------------------------

foodn <- lm(forage ~ 1, foragfood)
foodf <- lm(forage ~ food, foragfood)
foods <- lm(forage ~ food*snow, foragfood)
foodtw <- lm(forage ~ food*twig, foragfood)
foodpc <- lm(forage ~ food*percap, foragfood)
foodh <- lm(forage ~ food*haredensity, foragfood)
foodm <- lm(forage ~ food*mortrate, foragfood)
foodte <- lm(forage ~ food*temp, foragfood)

#list models
modsfood <- list(foodn, foodf, foods, foodtw, foodpc, 
                 foodh, foodm, foodte)

#name models
Namesfood <- c('Null', 'Food', 'Food*Snow', 'Food*Twigs', 'Food*PerCap',
               'Food*Hares', 'Food*Mortality', 'Food*Temp')

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


#top model = temp
ggplot(foragfood)+
  geom_point(aes(x = temp, y = forage, color = food), alpha = .2)+
  geom_smooth(aes(x = temp, y = forage, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Weekly mean temperature (C))", y = "Weekly foraging effort (hr)")+
  themepoints

#not far behind = per cap
ggplot(foragfood)+
  geom_point(aes(x = percap, y = forage, color = food), alpha = .2)+
  geom_smooth(aes(x = percap, y = forage, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Weekly twig availability (kg/hare)", y = "Weekly foraging effort (hr)")+
  themepoints





# save --------------------------------------------------------------------

write.csv(AICcon, "Output/Tables/AIC_foraging_winter_controls.csv")
write.csv(AICfood, "Output/Tables/AIC_foraging_winter_foods.csv")

