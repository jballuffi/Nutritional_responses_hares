#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
datD <- readRDS("Output/Data/full_data_daily_nogrid.rds") #daily data
fecal <- readRDS("Output/Data/fecal_protein.rds")
datW <- readRDS("Output/Data/full_data_weekly_nogrid.rds") #weekly data
forag <- readRDS("Output/Data/foraging_weekly.rds")

#merge by date and snow grid
fecal <- merge(fecal, datD, by = c("date", "year", "yearfactor"), all.x = TRUE)
forag <- merge(forag, datW, by = c("week", "year", "yearfactor"), all.x = TRUE)



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
fecforR2 <- round(rsq(fecformod), 2)



# Model Analysis ------------------------------------------------------------

#MODEL
#Build model
Q1 <- lmer(CP_dm ~ haredensity*food + biomass*food + mortrate*food + temp*food + (1|snowgrid), fecal)

#summarize model
summary(Q1)
anova(Q1) #gives f value
Anova(Q1) #gives p value
lmerTest::ranova(Q1) #residuals

#get table of results
Q1_sum <- lmer_out(Q1)

#get R2
Q1R2m <- round(r.squaredGLMM(Q1), 2)[1]
Q1R2c <- round(r.squaredGLMM(Q1), 2)[2]

#get t-values
t_density = round(coef(summary(Q1))[,"t value"][2], 2)
t_food    = round(coef(summary(Q1))[,"t value"][3], 2)
t_biomass = round(coef(summary(Q1))[,"t value"][4], 2)
t_mort    = round(coef(summary(Q1))[,"t value"][5], 2)
t_temp    = round(coef(summary(Q1))[,"t value"][6], 2)
t_tempint = round(coef(summary(Q1))[,"t value"][10], 2 )

#get coefficients
b_food <- round(fixef(Q1)[3], 3)
b_biomass <- round(fixef(Q1)[4], 3)
b_temp <- round(fixef(Q1)[6], 3)
b_tempint <- round(fixef(Q1)[10], 3)

#get standard errors
se_density <- round(se.fixef(Q1)[2], 3)
se_food <- round(se.fixef(Q1)[3], 2)
se_biomass <- round(se.fixef(Q1)[4], 2)
se_temp <- round(se.fixef(Q1)[6], 2)
se_tempint <- round(se.fixef(Q1)[10], 2)

#confident limits
round(confint(Q1), 2)



# Figure ------------------------------------------------------------------

#get fecal prediction for biomass*food
biopred <- as.data.table(ggpredict(Q1, terms = c("biomass", "food")))
setnames(biopred, "group", "food")

#get fecal prediction for temp*food
temppred <- as.data.table(ggpredict(Q1, terms = c("temp", "food")))
setnames(temppred, "group", "food")

#figure
(bio_fecal <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = biomass, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = biopred)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = biopred)+
    scale_color_manual(values = foodcols, name = "Food treatment")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    labs(x = "Twig biomass (kg/ha)", y = "Fecal protein (%)", subtitle = "A)")+
    themethesisright+
    theme(legend.position = c(.18, .88),
          legend.background = element_blank()))

(temp_fecal <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = temp, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = temppred)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = temppred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Temperature (°C)", y = "Fecal protein (%)", subtitle = "B)")+
    themethesisright)



# Create final figure and save --------------------------------------------

fecalfig <- ggarrange(bio_fecal, temp_fecal, ncol = 1, nrow = 2, align = c("hv"))
fecalfig

ggsave("Output/Figures/Fecal_Figure.jpeg", fecalfig, width = 4, height = 8, unit = "in")
write.csv(Q1_sum, "Output/Tables/fecal_model_results.csv")
#saveRDS(Q1R2, "Output/Data/fecal_R2.rds")

