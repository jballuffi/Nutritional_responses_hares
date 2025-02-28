
# script to explain fecal protein content

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
dat <- readRDS("Output/Data/full_data_daily_nogrid.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")

#merge by date and snowgrid
fecal <- merge(fecal, dat, by = c("date", "year", "yearfactor"), all.x = TRUE)



# run AIC -----------------------------------------------------------------

#make models
null <- lm(CP_dm ~ 1, fecal)

S1 <- lm(CP_dm ~ biomass*food, fecal)
S2 <- lm(CP_dm ~ percap*food, fecal)
S3 <- lm(CP_dm ~ mortrate*food, fecal)
S4 <- lm(CP_dm ~ temp*food, fecal)

D1 <- lm(CP_dm ~ biomass*food + mortrate*food, fecal)
D2 <- lm(CP_dm ~ biomass*food + temp*food, fecal)
D3 <- lm(CP_dm ~ percap*food + mortrate*food, fecal)
D4 <- lm(CP_dm ~ percap*food + temp*food, fecal)
D5 <- lm(CP_dm ~ mortrate*food + temp*food, fecal)


#list models and name
mods <- list(null, S1, S2, S3, S4, D1, D2, D3, D4, D5)
names <- c("Null", "S1", "S2", "S3", "S4", "D1", "D2", "D3", "D4", "D5")

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

summary(D2)
biopred <- ggpredict(D2, terms = c("biomass", "food"))
setnames(biopred, "group", "food")

(biofig <- 
  ggplot()+
  geom_abline(intercept = 10, slope = 0, linetype = 2)+
  geom_point(aes(x = biomass, y = CP_dm, color = food), alpha = .2, data = fecal)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = biopred)+
  geom_line(aes(x = x, y = predicted, color = food), data = biopred)+
  scale_color_manual(values = foodcols, name = "Food")+
  scale_fill_manual(values = foodcols, name = "Food")+
  labs(x = "Soluble biomass (kg/ha)", y = "Fecal protein (%)", title = "A)")+
  themepoints)

temppred <- ggpredict(D2, terms = c("temp", "food"))
setnames(temppred, "group", "food")

(tempfig <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = temp, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = temppred)+
    geom_line(aes(x = x, y = predicted, color = food), data = temppred)+
    scale_color_manual(values = foodcols, name = "Food")+
    scale_fill_manual(values = foodcols, name = "Food")+
    labs(x = "Daily temperature (C)", y = "Fecal protein (%)", title = "B)")+
    themepoints)

fecalfig <- ggarrange(biofig, tempfig, nrow = 2, ncol = 1)



# save --------------------------------------------------------------------

write.csv(AICfood, "Output/Tables/AIC_fecal_protein.csv")
ggsave("Output/Figures/fecal_results.jpeg", fecalfig, width = 5, height = 8, unit = "in")

