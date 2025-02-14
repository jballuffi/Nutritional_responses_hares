


###Show grids now. Cut out un-used variables.

#script for one into figure showing annual trends of data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")



# Dependent variables ---------------------------

(feces <- 
    ggplot(fecal)+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_boxplot(aes(x = yearfactor, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
    labs(y = "Fecal crude protein (%)", x = "", title = "A)")+
    scale_fill_manual(values = foodcols, name = "Food")+
    themepoints)

(foraging <- 
    ggplot(forag)+
    geom_boxplot(aes(x = yearfactor, y = forage, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Weekly foraging effort (hr/day)", x = "", title = "B)")+
    themepoints)

sumdepfig <- ggarrange(feces, foraging, nrow = 2, ncol = 1)



# Non-food variables ------------------------------------------------------

#mortality rate
(mweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$mortrate, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = mortrate), linewidth = .8)+
    labs(y = "Mortality rate", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)

#temperature
(tweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$temp, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = temp), linewidth = .8)+
    labs(y = "Temperature (C)", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)



# Food variables ----------------------------------------------------------

#soluble biomass per hectare
(bweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$biomass, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = biomass), linewidth = .8)+
    labs(y = "Soluble biomass (kg/ha)", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)

#soluble biomass per hare
(pcweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$percap, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = percap), linewidth = .8)+
    labs(y = "Per capita soluble biomass (kg/hare)", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)

sumindfig <- ggarrange(tweek, mweek, bweek, pcweek, ncol = 2, nrow = 2)



# save -----------------------------------------

ggsave("Output/Figures/vars_weekly.jpeg", sumindfig, width = 10, height = 10, unit = "in")
ggsave("Output/Figures/dep_vars_.jpeg", sumdepfig, width = 5, height = 8, unit = "in")


