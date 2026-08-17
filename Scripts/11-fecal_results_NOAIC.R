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
biopred_fecal <- as.data.table(ggpredict(Q1, terms = c("biomass", "food")))
setnames(biopred_fecal, "group", "food")

temppred_fecal <- as.data.table(ggpredict(Q1, terms = c("temp", "food")))
setnames(temppred_fecal, "group", "food")

# mortpred <- as.data.table(ggpredict(Q1, terms = c("mortrate", "food")))
# setnames(mortpred, "group", "food")
# 
# denspred <- as.data.table(ggpredict(Q1, terms = c("haredensity", "food")))
# setnames(denspred, "group", "food")

(bio_fecal <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = biomass, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = biopred_fecal)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = biopred_fecal)+
    scale_color_manual(values = foodcols, name = "Food treatment")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    labs(x = "Twig biomass (kg/ha)", y = "Fecal protein (%)", subtitle = "A)")+
    themethesisright+
    theme(legend.position = c(.15, .85),
          legend.background = element_blank()))

(temp_fecal <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = temp, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = temppred_fecal)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = temppred_fecal)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Temperature (°C)", y = "Fecal protein (%)", subtitle = "B)")+
    themethesisright)




# Foraging Rate Analysis -------------------------------------------------------

Q2 <- lmer(forage ~ haredensity*food + biomass*food + mortrate*food + temp*food + nightlength + (1|id) + (1|snowgrid), forag)
summary(Q2)
anova(Q2)
round(r.squaredGLMM(Q2), 2)[1]

#show effect of biomass*food
densitypred_forage <- as.data.table(ggpredict(Q2, terms = c("haredensity", "food")))
setnames(densitypred_forage, "group", "food")

#show effect of temperature*food
temppred_forage <- as.data.table(ggpredict(Q2, terms = c("temp", "food")))
setnames(temppred_forage, "group", "food")



(density_forage <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = forage, color = food), alpha = .2, data = forag)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = densitypred_forage)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = densitypred_forage)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Hare density (hares/ha)", y = "Foraging rate (hr/day)", subtitle = "C)")+
    themethesisright)

(temp_forage <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage, color = food), alpha = .2, data = forag)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = temppred_forage)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = temppred_forage)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Temperature (°C)", y = "Foraging rate (hr/day)", subtitle = "D)")+
    themethesisright)




# Create final figure and save --------------------------------------------

# 4 panel figure
fullfig <- ggarrange(bio_fecal, temp_fecal, density_forage, temp_forage, align = c("hv"))
fullfig

# save
ggsave("Output/Figures/Full_Figure.jpeg", width = 8, height = 8, unit = "in")






