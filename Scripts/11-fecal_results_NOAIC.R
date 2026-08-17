#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
datD <- readRDS("Output/Data/full_data_daily_nogrid.rds") #daily data
datW <- readRDS("Output/Data/full_data_weekly_nogrid.rds") #weekly data
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")

#merge by date and snow grid
fecal <- merge(fecal, datD, by = c("date", "year", "yearfactor"), all.x = TRUE)

#merge by week and snow grid
forag <- merge(forag, datW, by = c("week", "year", "yearfactor"), all.x = TRUE)



# Fecal Protein Analysis ------------------------------------------------------------

#ADD A RANDOM EFFECT FOR THIS MODEL?
Q1 <- lmer(CP_dm ~ biomass*food + temp*food + haredensity*food + mortrate*food + (1|snowgrid), fecal)
summary(Q1)
anova(Q1)
round(r.squaredGLMM(Q1), 2)[1]

#make predictive table
biopred <- as.data.table(ggpredict(Q1, terms = c("biomass", "food")))
setnames(biopred, "group", "food")

temppred <- as.data.table(ggpredict(Q1, terms = c("temp", "food")))
setnames(temppred, "group", "food")

# mortpred <- as.data.table(ggpredict(Q1, terms = c("mortrate", "food")))
# setnames(mortpred, "group", "food")
# 
# denspred <- as.data.table(ggpredict(Q1, terms = c("haredensity", "food")))
# setnames(denspred, "group", "food")

(biofig <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = biomass, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = biopred)+
    geom_line(aes(x = x, y = predicted, color = food), data = biopred)+
    scale_color_manual(values = foodcols, name = "Food treatment")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    labs(x = "Twig biomass (kg/ha)", y = "Fecal protein (%)", subtitle = "A)")+
    themethesisright+
    theme(legend.position = c(.15, .85),
          legend.background = element_blank()))

(tempfig <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = temp, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = temppred)+
    geom_line(aes(x = x, y = predicted, color = food), data = temppred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Temperature (°C)", y = "Fecal protein (%)", subtitle = "B)")+
    themethesisright)




# Foraging Rate Analysis -------------------------------------------------------

Q2 <- lmer(forage ~ haredensity*food + biomass*food + mortrate*food + temp*food + nightlength + (1|id) + (1|snowgrid), forag)
summary(Q2)
anova(Q2)
round(r.squaredGLMM(Q2), 2)[1]

#density
#temperature
#density*food
#night length









