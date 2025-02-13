


###Show grids now. Cut out un-used variables.

#script for one into figure showing annual trends of data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

dat <- readRDS("Output/Data/full_data_weekly.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")



# Dependent variables ---------------------------

(feces <- 
    ggplot(fecal)+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_boxplot(aes(x = yearfactor, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
    labs(y = "Fecal crude protein (%)", x = "", title = "A)")+
    scale_fill_manual(values = foodcols)+
    themepoints)

(foraging <- 
    ggplot(forag)+
    geom_boxplot(aes(x = yearfactor, y = forage, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Weekly foraging effort (hr/day)", x = "", title = "B)")+
    themepoints)

sumdepfig <- ggarrange(feces, foraging, nrow = 2, ncol = 1)



# Non-food variables ------------------------------------------------------

(dweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$haredensity, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = haredensity))+
    labs(y = "Hare density (hare/ha)", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)

(mweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$mortrate, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = mortrate))+
    labs(y = "Mortality rate", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)

(tweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$temp, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = temp))+
    labs(y = "Temperature (C)", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)



# Food variables ------------------------------------------------

#biomass
(bweek <- 
    ggplot(dat)+
   geom_abline(intercept = median(dat$biomass, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = biomass))+
    labs(y = "Biomass availability (kg/ha)", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)

(pcweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$percap, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = percap))+
    labs(y = "Per capita availability (kg/hare)", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)

(qweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$quality, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = quality))+
    labs(y = "Solubility (NDS; %)", x = "")+
    facet_wrap(~year, scales = "free_x")+
    themepoints_small)


weeklyfig <- ggarrange(dweek, mweek, tweek, bweek, pcweek, qweek, ncol = 2, nrow = 3)



# save -----------------------------------------

ggsave("Output/Figures/food_weekly.jpeg", weeklyfig, width = 10, height = 12, unit = "in")
ggsave("Output/Figures/dep_var_figure.jpeg", sumdepfig, width = 5, height = 8, unit = "in")


