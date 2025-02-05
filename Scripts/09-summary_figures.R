
#script for one into figure showing annual trends of data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

dat <- readRDS("Output/Data/full_data_daily.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")
weights <- readRDS("Output/Data/weight_change.rds")

#cut to years with all data
dat <- dat[year > 2015 & !year == 2022]
setorder(dat, date)
dat[, yearfactor := as.factor(year)]
fecal[, yearfactor := as.factor(year)]
forag[, yearfactor := as.factor(year)]
weights[, yearfactor := as.factor(year)]



# plots that show annual trends -------------------------------------------

(density <- ggplot(dat)+
    geom_boxplot(aes(x = yearfactor, y = haredensity), alpha = 0.5)+
    labs(x = "", y = "Hare density (hares/ha)")+
    themepoints)

(mortality <- ggplot(dat)+
    geom_boxplot(aes(x = yearfactor, y = mortrate), alpha = 0.5)+
    labs(x = "", y = "Mortality rate")+
    themepoints)

(snow <- ggplot(dat)+
    geom_boxplot(aes(x = yearfactor, y = snow), alpha = 0.5)+
    labs(x = "", y = "Snow depth (cm)")+
    themepoints)

(twigs <- ggplot(dat)+
    geom_boxplot(aes(x = yearfactor, y = twig), alpha = 0.5)+
    labs(x = "", y = "Twig availability (kg/ha)")+
    themepoints)

(percap <- ggplot(dat)+
    geom_boxplot(aes(x = yearfactor, y = percap), alpha = 0.5)+
    labs(x = "", y = "Twig availability (kg/ha)")+
    themepoints)

(temp <- ggplot(dat)+
    geom_boxplot(aes(x = yearfactor, y = tempmean), alpha = 0.5)+
    labs(x = "", y = "Temperature (C)")+
    themepoints)

sumenvfig <- ggarrange(density, snow, mortality, twigs, temp, percap, ncol = 2, nrow = 3)
sumenvfig



# Daily snow and twig data ------------------------------------------------

#daily density by year
ggplot(dat)+
  geom_path(aes(x = date, y = haredensity, group = year))+
  labs(x = "", y = "Hare density (hares/ha)")+
  themepoints

#daily snow depth
(sdaily <- 
  ggplot(dat)+
  geom_line(aes(x = date, y = snow))+
  labs(y = "Snow depth (cm)", x = "Date")+
  facet_wrap(~year, scales = "free")+
  themepointstop)

(tdaily <- 
    ggplot(dat)+
    geom_line(aes(x = date, y = twig))+
    labs(y = "Twig availability (kg/ha)", x = "Date")+
    facet_wrap(~winter, scales = "free")+
    themepointstop)



# Figure showing summary of dependent variables ---------------------------

(feces <- 
   ggplot(fecal)+
   geom_boxplot(aes(x = yearfactor, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
   geom_abline(intercept = 10, slope = 0, linetype = 2)+
   labs(y = "Fecal crude protein (%)", x = "year")+
   scale_fill_manual(values = foodcols)+
   themepoints)

(foraging <- 
  ggplot(forag)+
  geom_boxplot(aes(x = yearfactor, y = forage, fill = food), alpha = .5)+
  scale_fill_manual(values = foodcols, guide = NULL)+
  labs(y = "Foraging effort (hr/day)", x = "Winter")+
  themepoints)

#weight loss by year
(wcresid <- ggplot(weights)+
    geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
    geom_boxplot(aes(x = yearfactor, y = weight.c.resid, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Weight change residual (g)", x = "Winter")+
    themepoints)


sumdepfig <- ggarrange(wcresid, foraging, feces, nrow = 3, ncol = 1)



# save -----------------------------------------

ggsave("Output/Figures/env_summary_figure1.jpeg", sumenvfig, width = 10, height = 10, unit = "in")

ggsave("Output/Figures/snow_daily_figure.jpeg", sdaily, width = 8, height = 7, unit = "in")
ggsave("Output/Figures/twig_daily_figure.jpeg", tdaily, width = 9, height = 7, unit = "in")

ggsave("Output/Figures/dep_var_figure.jpeg", sumdepfig, width = 8, height = 14, unit = "in")


