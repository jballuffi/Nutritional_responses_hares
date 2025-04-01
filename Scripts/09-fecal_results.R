
# script to explain fecal protein content

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
dat <- readRDS("Output/Data/full_data_daily_nogrid.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")


#merge by date and snowgrid
fecal <- merge(fecal, dat, by = c("date", "year", "yearfactor"), all.x = TRUE)



# descriptive results ----------------------------------------------

### fecal vs food add
foodmod <- lm(CP_dm ~ food, fecal)
foodanova <- anova(foodmod)
foodsum <- summary(foodmod)
pfood <- round(foodanova$`Pr(>F)`[1], 3)
tfood <- round(foodsum$coefficients[, 3][2], 2) #t-value
dffood <- foodanova$Df[2]

### fecal vs month
fecmod <- lm(CP_dm ~ m, fecal) #make model
fecanova <- anova(fecmod) #get anova
fecsum <- summary(fecmod) #summary
pfec <- round(fecanova$`Pr(>F)`[1], 3) #pull p value
tfec <- round(fecsum$coefficients[, 3][2], 2) #t-value
dffec <- fecanova$Df[2]


### fecal vs foraging rate
#get avg fecal by week. only a few cases that had multiple fecal in 1 week
fecal2 <- fecal[, .(CP = mean(CP_dm)), by = .(id, year, week)]
#merge shortened fecal with foraging rates
fecfor <- merge(forag, fecal2, by = c("id", "year", "week"))
#slight negative correlation between weekly foraging rate and fecal protein
fecformod <- lm(CP ~ forage, fecfor)
fecforanova <- anova(fecformod)
fecforsum <- summary(fecformod)
pfecfor <- round(fecforanova$`Pr(>F)`[1], 3)
tfecfor <- round(fecforsum$coefficients[, 3][2], 2)
dffecfor <- fecforanova$Df[2]
fecforslope <- round(fecformod$coefficients[2], 2)



# run AIC -----------------------------------------------------------------

#make models
null <- lm(CP_dm ~ 1, fecal)
S1 <- lm(CP_dm ~ biomass*food, fecal)
S2 <- lm(CP_dm ~ haredensity*food, fecal)
S3 <- lm(CP_dm ~ mortrate*food, fecal)
S4 <- lm(CP_dm ~ temp*food, fecal)

D1 <- lm(CP_dm ~ biomass*food + mortrate*food, fecal)
D2 <- lm(CP_dm ~ biomass*food + temp*food, fecal)
D3 <- lm(CP_dm ~ haredensity*food + mortrate*food, fecal)
D4 <- lm(CP_dm ~ haredensity*food + temp*food, fecal)
D5 <- lm(CP_dm ~ mortrate*food + temp*food, fecal)
D6 <- lm(CP_dm ~ haredensity*food + biomass*food, fecal)


#list models, names, and designs
mods <- list(null, S1, S2, S3, S4, D1, D2, D3, D4, D5, D6)
codes <- c("Null", "S1", "S2", "S3", "S4", "D1", "D2", "D3", "D4", "D5", "D6")

vars <- c("None", "Biomass*Food", "Density*Food", "Mortality*Food", "Temperature*Food",
          "Biomass*Food + Mortality*Food", 
          "Biomass*Food + Temperature*Food", 
          "Density*Food + Mortality*Food", 
          "Density*Food + Temperature*Food", 
          "Mortality*Food + Temperature*Food",
          "Density*Food + Biomass*Food")

AICfood <- make_aic_lm(modlist = mods, modnames = codes)

#make a dataframe with model codes and their designs
moddesign <- data.table(
  Modnames = codes,
  Variables = vars
)

AICfood <- merge(moddesign, AICfood, by = "Modnames")
setorder(AICfood, "Delta_AICc")



# Results of top models ---------------------------------------------------

summary(D2)

#make predictive table
biopred <- as.data.table(ggpredict(D2, terms = c("biomass", "food")))
setnames(biopred, "group", "food")

temppred <- as.data.table(ggpredict(D2, terms = c("temp", "food")))
setnames(temppred, "group", "food")

#get t-values
sb_t = round(coef(summary(D2))[,"t value"][2], 2)

#get p-values
D2anova <- anova(D2)
sb_p <- round(D2anova$`Pr(>F)`[1], 2)
food_p <- round(D2anova$`Pr(>F)`[2], 2)
foodsb_p <- round(D2anova$`Pr(>F)`[4], 2)
foodtemp_p <- round(D2anova$`Pr(>F)`[5], 2)

#get coefficients
sb_coef <- round(coef(summary(D2))[,"Estimate"][2], 2)
food_coef <- round(coef(summary(D2))[,"Estimate"][3], 2)

#get standard errors
sb_se <- round(coef(summary(D2))[,"Std. Error"][2], 2)
food_se <- round(coef(summary(D2))[,"Std. Error"][3], 2)


### second top model for hare density result

summary(D4)
denspred <- as.data.table(ggpredict(D4, terms = c("haredensity", "food")))
setnames(denspred, "group", "food")

D4anova <- anova(D4)
dens_p <- round(D4anova$`Pr(>F)`[1], 2)
dens_coef <- round(coef(summary(D4))[,"Estimate"][2], 2)
dens_se <- round(coef(summary(D4))[,"Std. Error"][2], 2)
fooddens_p <- round(D4anova$`Pr(>F)`[4], 2)


# figures for top model ---------------------------------------------------


(biofig <- 
  ggplot()+
  geom_abline(intercept = 10, slope = 0, linetype = 2)+
  geom_point(aes(x = biomass, y = CP_dm, color = food), alpha = .2, data = fecal)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = biopred)+
  geom_line(aes(x = x, y = predicted, color = food), data = biopred)+
  scale_color_manual(values = foodcols, name = "Food")+
  scale_fill_manual(values = foodcols, name = "Food")+
  labs(x = "Twig biomass (kg/ha)", y = "Fecal protein (%)", subtitle = "A)")+
  themepoints)


(tempfig <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = temp, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = temppred)+
    geom_line(aes(x = x, y = predicted, color = food), data = temppred)+
    scale_color_manual(values = foodcols, name = "Food")+
    scale_fill_manual(values = foodcols, name = "Food")+
    labs(x = "Temperature (C)", y = "Fecal protein (%)", subtitle = "B)")+
    themepoints)

(densfig <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = haredensity, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = denspred)+
    geom_line(aes(x = x, y = predicted, color = food), data = denspred)+
    scale_color_manual(values = foodcols, name = "Food")+
    scale_fill_manual(values = foodcols, name = "Food")+
    labs(x = "Hare density (hares/ha)", y = "Fecal protein (%)", subtitle = "C)")+
    themepoints)



fecalfig <- ggarrange(biofig, tempfig, densfig, nrow = 3, ncol = 1)



# save --------------------------------------------------------------------

write.csv(AICfood, "Output/Tables/AIC_fecal_protein.csv")
ggsave("Output/Figures/fecal_results.jpeg", fecalfig, width = 5, height = 10, unit = "in")

