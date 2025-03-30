

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# merge info and make a control and food add dataset --------

#read data
forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]


# movement rates in relation to the same model ----------------------------

#apply top model to movement rates
move <- lmer(move ~ biomass + temp + nightlength + (1|id), foragcon) #biomass and temp
summary(move)

#make predictive tables
sbmove_pred <- as.data.table(ggpredict(move, terms = c("biomass"))) #soluble biomass (sb)
tmove_pred <- as.data.table(ggpredict(move, terms = c("temp"))) #temperature (t)

#get t-values
movesb_t = round(coef(summary(move))[,"t value"][2], 2)
movet_t = round(coef(summary(move))[,"t value"][3], 2)

#get f-values
moveanova <- anova(move)
movesb_f <- round(moveanova$`F value`[1], 2)
movet_f <- round(moveanova$`F value`[2], 2)

#get coefficients
movesb_coef <- round(fixef(move)[2], 4)
movet_coef <- round(fixef(move)[3], 4)

#get standard errorts
movet_se <- round(se.fixef(move)[3], 3)
movesb_se <- round(se.fixef(move)[2], 3)


#figure for foraging effort in response to soluble biomass
(bmov <- 
    ggplot()+
    geom_point(aes(x = biomass, y = move), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = sbmove_pred)+
    geom_line(aes(x = x, y = predicted), data = sbmove_pred)+
    labs(x = "Twig biomass (kg/ha)", y = "Traveling (min/day)", subtitle = "A)")+
    themepoints)

#figures for foraging effort in response to temperature
(tmov <- 
    ggplot()+
    geom_point(aes(x = temp, y = move), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = tmove_pred)+
    geom_line(aes(x = x, y = predicted), data = tmove_pred)+
    labs(x = "Temperature (C)", y = "Traveling (min/day)", subtitle = "B)")+
    themepoints)

movfig <- ggarrange(bmov, tmov, ncol = 1, nrow = 2)


ggsave("Output/Figures/movement_results.jpeg", movfig, width = 5, height = 8, unit = "in")
