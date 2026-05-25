
#script for figure showing annual trends of data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")


#get dates associated with weeks
dates <- dat[, .(date, week, year)]

#merge foraging rates with dates associated with weeks
forag2 <- merge(forag, dates, by = c("week", "year"), all.y = TRUE)

#get median foraging rate by week and food treatment, using dates of weeks, 
foragesum <- forag2[, .(forage = median(forage)), by = .(date, food, year)]

#make a month factor col for fecal data
fecal[m == 1, monthfactor := "Jan"][m == 3, monthfactor := "Mar"]



# Dependent variables ---------------------------


# #fecal protein by year
# (feces <-
#     ggplot(fecal)+
#     geom_abline(intercept = 10, slope = 0, linetype = 2)+
#     geom_boxplot(aes(x = yearfactor, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
#     labs(y = "Fecal crude protein (%)", x = "Year")+
#     scale_fill_manual(values = foodcols, name = "Food treatment")+
#     themethesisright)



# Independent variables ------------------------------------------------------

#hare density
(dweek <- 
   ggplot(dat)+
   geom_abline(intercept = median(dat$haredensity, na.rm = TRUE), slope = 0, linetype = 2)+
   geom_line(aes(x = date, y = haredensity), linewidth = .8)+
   labs(y = "Hare density (hares/ha)", x = "", title = "A)")+
   facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
   themethesisright)

#mortality rate
(mweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$mortrate, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = mortrate), linewidth = .8)+
    labs(y = "Mortality rate", x = "", title = "B)")+
    facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
    themethesisright)

#temperature
(tweek <- 
   ggplot(dat)+
   geom_abline(intercept = median(dat$temp, na.rm = TRUE), slope = 0, linetype = 2)+
   geom_line(aes(x = date, y = temp), linewidth = .8)+
   labs(y = "Temperature (°C)", x = "", title = "C)")+
   facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
   themethesisright)

#soluble biomass per hectare
(bweek <- 
    ggplot(dat)+
    geom_abline(intercept = median(dat$biomass, na.rm = TRUE), slope = 0, linetype = 2)+
    geom_line(aes(x = date, y = biomass), linewidth = .8)+
    labs(y = "Twig biomass (kg/ha)", x = "", title = "D)")+
    facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
    themethesisright)

#foraging effort on a weekly basis
(fweek <- ggplot(foragesum)+
    geom_line(aes(x = date, y = forage, color = food), linewidth = .8)+
    scale_color_manual(values = foodcols, guide = NULL)+
    labs(y = "Weekly foraging (hr/day)", x = "", title = "E)")+
    facet_wrap(~year, scales = "free_x", nrow = 1, ncol = 6)+
    themethesisright)

(cpweek <-
    ggplot(fecal)+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_boxplot(aes(x = monthfactor, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Fecal CP (%)", x = "", title = "F)")+
    facet_wrap(~year, scales ="free_x",  nrow = 1, ncol = 4)+
    themethesisright)



sumindfig <- ggarrange(dweek, mweek, tweek, bweek, fweek, cpweek,
                       align = c("h"), ncol = 1, nrow = 6)



# save -----------------------------------------

ggsave("Output/Figures/vars_weekly.jpeg", sumindfig, width = 8, height = 14, unit = "in")
#ggsave("Output/Figures/fecal_data.jpeg", feces, width = 5, height = 4, unit = "in")


