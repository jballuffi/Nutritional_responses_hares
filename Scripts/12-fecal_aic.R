
# script to explain fecal protein content on a monthly basis


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

density_m <- readRDS("Output/Data/hares_monthly.rds")
snow_m <- readRDS("Output/Data/snow_food_monthly.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")



#merge
fecal <- merge(fecal, density_m, by = c("winter", "m"))
fecal <- merge(fecal, snow_m, by = c("winter", "m", "snowgrid"))

plot(fecal$CP_dm ~ fecal$ash)


N <- lm(CP_dm ~ 1, fecal)
food <- lm(CP_dm ~ food, fecal)
snow <- lm(CP_dm ~ snow.avg*food, fecal)
twig <- lm(CP_dm ~ biomass.avg*food, fecal)
hare <- lm(CP_dm ~ haredensity*food, fecal)
pred <- lm(CP_dm ~ mortality*food, fecal)

mods <- list(N, food, snow, twig, hare, pred)
names <- c("Null", "Food", "Snow*Food", "Twig*Food", "Hares*Food", "Mortality*Food")


#make AIC table
AICfood <- as.data.table(aictab(REML = F, cand.set = mods, modnames = names, sort = TRUE))

#remove unwanted columns
AICfood[, ModelLik := NULL]
AICfood[, Cum.Wt := NULL]

#round whole table to 3 dec places
AICfood <- AICfood %>% mutate_if(is.numeric, round, digits = 3)

#run function and get R2s for all models
R2sfood <- lapply(mods, collectR2)
R2sfood <- rbindlist(R2sfood, fill = TRUE)
setnames(R2sfood, "V1", "R2")
R2sfood$Modnames <- names

#merge R2s with AIC table
AICfood <- merge(AICfood, R2sfood, by = "Modnames")
setorder(AICfood, "Delta_AICc")


summary(snow)

ggplot(fecal)+
  geom_point(aes(x = snow.avg, y = CP_dm, color = food), alpha = .2)+
  geom_smooth(aes(x = snow.avg, y = CP_dm, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Average monthly snow depth (cm)", y = "Fecal protein (%)")+
  themepoints

summary(twig)

ggplot(fecal)+
  geom_point(aes(x = biomass.avg, y = CP_dm, color = food), alpha = .2)+
  geom_smooth(aes(x = biomass.avg, y = CP_dm, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Average willow available (g/m2)", y = "Fecal protein (%)")+
  themepoints



write.csv(AICfood, "Output/Tables/AIC_fecal_protein.csv")
