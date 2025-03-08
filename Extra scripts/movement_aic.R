
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")



# merge info and make a control and food add dataset --------

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]

#take only the years where there was food add
foodyears <- forag[food == 1, unique(winter)]

#take only females for food add comparisons
foragfood <- forag[winter %in% foodyears & sex == "female"]



# AIC to explain weekly foraging for controls only ------------------------

#models for controls only
n <- lmer(moveresid ~ sex + (1|id), foragcon) #null model

#single terms 
S1 <- lmer(moveresid ~ biomass + sex + (1|id), foragcon) #biomass food
S2 <- lmer(moveresid ~ percap + sex + (1|id), foragcon) #percapita food
S3 <- lmer(moveresid ~ mortrate + sex + (1|id), foragcon) #predation risk/mortality rate
S4 <- lmer(moveresid ~ temp + sex + (1|id), foragcon) #temperature

#double terms 
D1 <- lmer(moveresid ~ biomass + mortrate + sex + (1|id), foragcon) #biomass and mortality
D2 <- lmer(moveresid ~ biomass + temp + sex + (1|id), foragcon) #biomass and temp
D3 <- lmer(moveresid ~ percap + mortrate + sex + (1|id), foragcon) #percap and mortality
D4 <- lmer(moveresid ~ percap + temp + sex + (1|id), foragcon) #percap and temp
D5 <- lmer(moveresid ~ mortrate + temp + sex + (1|id), foragcon) #mortality and temp

#triple terms
T1 <- lmer(moveresid ~ biomass + mortrate + temp + sex + (1|id), foragcon)
T2 <- lmer(moveresid ~ percap + mortrate + temp + sex + (1|id), foragcon)

#list models
mods <- list(n, S1, S2, S3, S4, D1, D2, D3, D4, D5, T1, T2)
codes <- c("Null", "S1", "S2", "S3", "S4", "D1", "D2", "D3", "D4", "D5", "T1", "T2")
vars <- c("None", "SB", "PCSB", "Mortality", "Temperature",
          "SB + Mortality", "SB + Temperature", "PCSB + Mortality", "PCSB + Temperature", "Mortality + Temperature",
          "SB + Mortality + Temperature", "PCSB + Mortality + Temperature")

#use function from R/ folder to make an AIC table comparing all models.
#function also extracts R2s
AICcon <- make_aic_lmer(modlist = mods, modnames = codes)

#make a dataframe with model codes and their designs
moddesign <- data.table(
  Modnames = codes,
  Variables = vars
)

AICcon <- merge(moddesign, AICcon, by = "Modnames")
setorder(AICcon, "Delta_AICc")

summary(D5)



# make predictive tables and extract coefficients -------------------------

#make predictive tables
t_pred <- as.data.table(ggpredict(D5, terms = c("temp"))) #temperature (t)

#figures for foraging effort in response to temperature
(tfig <- 
    ggplot()+
    geom_point(aes(x = temp, y = moveresid), alpha = .3, data = foragcon)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, data = t_pred)+
    geom_line(aes(x = x, y = predicted), data = t_pred)+
    labs(x = "Daily temperature (C)", y = "Movement residual (min)", title = "B)")+
    themepoints)


