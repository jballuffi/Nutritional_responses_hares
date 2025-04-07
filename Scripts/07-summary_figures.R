
#script for figure showing annual trends of data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")



# Dependent variables ---------------------------

#foraging effort by year
(foraging <- 
    ggplot(forag)+
    geom_boxplot(aes(x = yearfactor, y = forage, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Weekly foraging effort (hr/day)", x = "", title = "A)")+
    themethesisright)

#fecal protein by year
(feces <- 
    ggplot(fecal)+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_boxplot(aes(x = yearfactor, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
    labs(y = "Fecal crude protein (%)", x = "", title = "B)")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    themethesisright)

sumdepfig <- ggarrange(foraging, feces, nrow = 2, ncol = 1)



# Independent variables ------------------------------------------------------

#hare density
(dweek <- 
   ggplot(dat)+
   geom_abline(intercept = median(dat$haredensity, na.rm = TRUE), slope = 0, linetype = 2)+
   geom_line(aes(x = date, y = haredensity), linewidth = .8)+
   labs(y = "Hare density (hares/ha)", x = "", title = "A)")+
   facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
   themepoints_small)

#mortality rate
(mweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$mortrate, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = mortrate), linewidth = .8)+
    labs(y = "Mortality rate (%)", x = "", title = "B)")+
    facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
    themepoints_small)

#temperature
(tweek <- 
   ggplot(dat)+
   geom_abline(intercept = median(dat$temp, na.rm = TRUE), slope = 0, linetype = 2)+
   geom_line(aes(x = date, y = temp), linewidth = .8)+
   labs(y = "Temperature (C)", x = "", title = "C)")+
   facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
   themepoints_small)

#soluble biomass per hectare
(bweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$biomass, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = biomass), linewidth = .8)+
    labs(y = "Twig biomass (kg/ha)", x = "", title = "D)")+
    facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
    themepoints_small)


sumindfig <- ggarrange(dweek, mweek, tweek, bweek, ncol = 1, nrow = 4)



# save -----------------------------------------

ggsave("Output/Figures/vars_weekly.jpeg", sumindfig, width = 10, height = 10, unit = "in")
ggsave("Output/Figures/dep_vars_.jpeg", sumdepfig, width = 5, height = 8, unit = "in")


