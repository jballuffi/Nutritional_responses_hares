
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


#list models, names, and designs
mods <- list(null, S1, S2, S3, S4, D1, D2, D3, D4, D5)
codes <- c("Null", "S1", "S2", "S3", "S4", "D1", "D2", "D3", "D4", "D5")

vars <- c("None", "SB*Food", "PCSB*Food", "Mortality*Food", "Temperature*Food",
          "SB*Food + Mortality*Food", 
          "SB*Food + Temperature*Food", 
          "PCSB*Food + Mortality*Food", 
          "PCSB*Food + Temperature*Food", 
          "Mortality*Food + Temperature*Food")

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





# figures for top model ---------------------------------------------------



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

