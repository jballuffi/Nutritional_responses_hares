
#script for one into figure showing annual trends of data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

dat <- readRDS("Output/Data/full_data_daily.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")
weights <- readRDS("Output/Data/weight_change.rds")
winters <- readRDS("Output/Data/full_data_annual.rds")




# plots that show annual trends -------------------------------------------

(temp <- ggplot(dat)+
   geom_abline(intercept = median(dat$tempmean, na.rm = TRUE), slope = 0, linetype = 3)+
   geom_boxplot(aes(x = yearfactor, y = tempmean), alpha = 0.5)+
   labs(x = "", y = "Temperature (C)", title = "A)")+
   themepoints)

(density <- ggplot(dat)+
    geom_abline(intercept = median(dat$haredensity, na.rm = TRUE), slope = 0, linetype = 3)+
    geom_boxplot(aes(x = yearfactor, y = haredensity), alpha = 0.5)+
    labs(x = "", y = "Hare density (hares/ha)", title = "B)")+
    themepoints)

(mortality <- ggplot(dat)+
    geom_abline(intercept = median(dat$mortrate, na.rm = TRUE), slope = 0, linetype = 3)+
    geom_boxplot(aes(x = yearfactor, y = mortrate), alpha = 0.5)+
    labs(x = "", y = "Mortality rate", title = "C)")+
    themepoints)

(snow <- ggplot(dat)+
    geom_abline(intercept = median(dat$snow, na.rm = TRUE), slope = 0, linetype = 3)+
    geom_boxplot(aes(x = yearfactor, y = snow), alpha = 0.5)+
    labs(x = "", y = "Snow depth (cm)", title = "D)")+
    themepoints)

(twigs <- ggplot(dat)+
    geom_abline(intercept = median(dat$twig, na.rm = TRUE), slope = 0, linetype = 3)+
    geom_boxplot(aes(x = yearfactor, y = twig), alpha = 0.5)+
    labs(x = "", y = "Twig availability (kg/hectare)", title = "E)")+
    themepoints)

(percap <- ggplot(dat)+
    geom_abline(intercept = median(dat$percap, na.rm = TRUE), slope = 0, linetype = 3)+
    geom_boxplot(aes(x = yearfactor, y = percap), alpha = 0.5)+
    labs(x = "", y = "Twig availability (kg/hare)", title = "F)")+
    themepoints)

sumenvfig <- ggarrange(temp, density, mortality, snow, twigs, percap, ncol = 2, nrow = 3)
sumenvfig



# Figure showing summary of dependent variables ---------------------------

#weight loss by year
(wcresid <- ggplot(weights)+
   geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
   geom_boxplot(aes(x = yearfactor, y = weight.c.resid, fill = food), alpha = .5)+
   scale_fill_manual(values = foodcols, guide = NULL)+
   labs(y = "Weight change residual (g)", x = "Winter", title = "A)")+
   themepoints)

(feces <- 
    ggplot(fecal)+
    geom_boxplot(aes(x = yearfactor, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    labs(y = "Fecal crude protein (%)", x = "year", title = "B)")+
    scale_fill_manual(values = foodcols)+
    themepoints)

(foraging <- 
    ggplot(forag)+
    geom_boxplot(aes(x = yearfactor, y = forage, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Foraging effort (hr/day)", x = "Winter", title = "C)")+
    themepoints)

sumdepfig <- ggarrange(wcresid, feces, foraging, nrow = 3, ncol = 1)




# Daily snow and twig data ------------------------------------------------

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
    facet_wrap(~year, scales = "free")+
    themepointstop)

(pcdaily <- 
    ggplot(dat)+
    geom_line(aes(x = date, y = percap))+
    labs(y = "Twig availability (kg/hare)", x = "Date")+
    facet_wrap(~year, scales = "free")+
    themepointstop)



# save -----------------------------------------

ggsave("Output/Figures/env_summary_figure1.jpeg", sumenvfig, width = 10, height = 10, unit = "in")

ggsave("Output/Figures/snow_daily_figure.jpeg", sdaily, width = 8, height = 7, unit = "in")
ggsave("Output/Figures/twig_daily_figure.jpeg", tdaily, width = 8, height = 7, unit = "in")
ggsave("Output/Figures/percap_daily_figure.jpeg", pcdaily, width = 8, height = 7, unit = "in")

ggsave("Output/Figures/dep_var_figure.jpeg", sumdepfig, width = 8, height = 14, unit = "in")


