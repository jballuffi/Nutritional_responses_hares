
# script to explain fecal protein content

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
dat <- readRDS("Output/Data/full_data_daily.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")

#merge by date and snowgrid
fecal <- merge(fecal, dat, by = c("date", "year", "yearfactor", "snowgrid"), all.x = TRUE)



# run AIC -----------------------------------------------------------------

#make models
null    <- lm(CP_dm ~ 1, fecal)
base <- lm(CP_dm ~ food + tempmean + sex + mortrate, fecal)
biomass <- lm(CP_dm ~ biomass*food + tempmean + sex + mortrate, fecal)
quality <- lm(CP_dm ~ quality*food + tempmean + sex + mortrate, fecal)
pcap <- lm(CP_dm ~ percap*food + tempmean + sex + mortrate, fecal)
hare <- lm(CP_dm ~ haredensity*food + tempmean + sex + mortrate, fecal)

#list models and name
mods <- list(null, base, biomass, quality, pcap, hare)
names <- c("Null", "Base", "Biomass", "Quality", "PerCapita", "Density")

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



# Results of top models ---------------------------------------------------

#top model is quality
summary(quality)
qualpred <- ggpredict(quality, terms = c("quality", "food"))
setnames(qualpred, "group", "food")

qualfig <- 
  ggplot()+
  geom_abline(intercept = 10, slope = 0, linetype = 2)+
  geom_point(aes(x = quality, y = CP_dm, color = food), alpha = .2, data = fecal)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = qualpred)+
  geom_line(aes(x = x, y = predicted, color = food), data = qualpred)+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Twig solubility (cm)", y = "Fecal protein (%)")+
  themepoints



#second top model is biomass
summary(biomass)
biopred <- ggpredict(biomass, terms = c("biomass", "food"))
setnames(biopred, "group", "food")

biofig <- 
  ggplot()+
  geom_abline(intercept = 10, slope = 0, linetype = 2)+
  geom_point(aes(x = biomass, y = CP_dm, color = food), alpha = .2, data = fecal)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = biopred)+
  geom_line(aes(x = x, y = predicted, color = food), data = biopred)+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Average willow available (g/m2)", y = "Fecal protein (%)")+
  themepoints



write.csv(AICfood, "Output/Tables/AIC_fecal_protein.csv")
ggsave("Output/Figures/fecal_quality.jpeg", qualfig, width = 6, height = 5, unit = "in")
ggsave("Output/Figures/fecal_biomass.jpeg", biofig, width = 6, height = 5, unit = "in")

