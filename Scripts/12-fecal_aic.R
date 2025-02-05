
# script to explain fecal protein content on a monthly basis


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

datweek <- readRDS("Output/Data/full_data_weekly.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")


fecal <- merge(fecal, datweek, by = c("year", "week"), all.x = TRUE)


N    <- lm(CP_dm ~ 1, fecal)
food <- lm(CP_dm ~ food, fecal)
snow <- lm(CP_dm ~ snow*food, fecal)
twig <- lm(CP_dm ~ twig*food, fecal)
pcap <- lm(CP_dm ~ percap*food, fecal)
hare <- lm(CP_dm ~ haredensity*food, fecal)
pred <- lm(CP_dm ~ mortrate*food, fecal)

mods <- list(N, food, snow, twig, 
             pcap, hare, pred)

names <- c("Null", "Food", "Snow*Food", "Twig*Food", 
           "PerCap*Food", "Hares*Food", "Mortality*Food")


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
  geom_abline(intercept = 10, slope = 0, linetype = 2)+
  geom_point(aes(x = snow, y = CP_dm, color = food), alpha = .2)+
  geom_smooth(aes(x = snow, y = CP_dm, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Average monthly snow depth (cm)", y = "Fecal protein (%)")+
  themepoints

summary(twig)

ggplot(fecal)+
  geom_abline(intercept = 10, slope = 0, linetype = 2)+
  geom_point(aes(x = twig, y = CP_dm, color = food), alpha = .2)+
  geom_smooth(aes(x = twig, y = CP_dm, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Average willow available (g/m2)", y = "Fecal protein (%)")+
  themepoints



write.csv(AICfood, "Output/Tables/AIC_fecal_protein.csv")
