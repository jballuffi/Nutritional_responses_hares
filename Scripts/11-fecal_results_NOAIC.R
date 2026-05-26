#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
dat <- readRDS("Output/Data/full_data_daily_nogrid.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")


#merge by date and snowgrid
fecal <- merge(fecal, dat, by = c("date", "year", "yearfactor"), all.x = TRUE)



# create model ------------------------------------------------------------

Q1 <- lm(CP_dm ~ biomass*food + temp*food + haredensity*food + mortrate*food, fecal)
summary(Q1)
anova(Q1)

#make predictive table
biopred <- as.data.table(ggpredict(Q1, terms = c("biomass", "food")))
setnames(biopred, "group", "food")

temppred <- as.data.table(ggpredict(Q1, terms = c("temp", "food")))
setnames(temppred, "group", "food")

mortpred <- as.data.table(ggpredict(Q1, terms = c("mortrate", "food")))
setnames(mortpred, "group", "food")

denspred <- as.data.table(ggpredict(Q1, terms = c("haredensity", "food")))
setnames(denspred, "group", "food")



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

(densfig <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = haredensity, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = denspred)+
    geom_line(aes(x = x, y = predicted, color = food), data = denspred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Hare density (hares/ha)", y = "Fecal protein (%)", subtitle = "C)")+
    themethesisright)

(mortfig <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = mortrate, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.5, data = mortpred)+
    geom_line(aes(x = x, y = predicted, color = food), data = mortpred)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Mortality rate", y = "Fecal protein (%)", subtitle = "D)")+
    themethesisright)



