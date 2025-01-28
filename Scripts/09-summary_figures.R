
#script for one into figure showing annual trends of data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


density <- readRDS("Output/Data/hares_daily.rds")
winterdensity <- readRDS("Output/Data/hares_lynx_winter.rds")

snow <- readRDS("Output/Data/snow_food_daily.rds")
wintersnow <- readRDS("Output/Data/snow_food_winter.rds")

forag <- readRDS("Output/Data/foraging_daily.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
wchange <- readRDS("Output/Data/weight_change.rds")


#remove last winter of snow data 
snow <- snow[!winter == "2021-2022"]

setorder(winterdensity, year)

setorder(wintersnow, snowgrid, year)




# environmental data by winter ------------------------------------

(ddaily <- ggplot(density)+
    geom_path(aes(x = date, y = haredensity, group = winter, color = phase))+
    scale_color_manual(values = phasecols, breaks=c('increase', 'peak', 'decrease', 'low'))+
    labs(x = "", y = "Hare density (hares/ha)")+
    themepoints)

(dwinter <- ggplot(density)+
    geom_boxplot(aes(x = winter, y = haredensity, fill = phase), alpha = 0.5)+
    scale_fill_manual(values = phasecols, breaks=c('increase', 'peak', 'decrease', 'low'))+
    labs(x = "", y = "Hare density (hares/ha)")+
    themepoints)

(lwinter <- ggplot(winterdensity)+
    geom_path(aes(x = year, y = lynx, group = 1, color = phase), size = .75)+
    scale_color_manual(values = phasecols, breaks=c('increase', 'peak', 'decrease', 'low'))+
    labs(x = "", y = "Lynx density (lynx/?)")+
    themepoints)

(swinter <- ggplot(wintersnow)+
   geom_path(aes(x = year, y = snow.avg, group = snowgrid, linetype = snowgrid), size = .75)+
   labs(x = "", y = "Average snow depth (cm)")+
   themepoints)

(twinter <- ggplot(wintersnow)+
  geom_path(aes(x = year, y = biomass.avg, group = snowgrid, linetype = snowgrid), size = .75)+
  labs(x = "", y = "Average available willow (g/m2)")+
  themepoints)

sumenvfig <- ggarrange(dwinter, lwinter, swinter, twinter, ncol = 2, nrow = 2)
sumenvfig


sumenvfig2 <- ggarrange(ddaily, lwinter, swinter, twinter, ncol = 2, nrow = 2)
sumenvfig2


# Daily snow and twig data ------------------------------------------------

(sdaily <- 
  ggplot(snow)+
  geom_line(aes(x = date, y = snow, linetype = snowgrid))+
  labs(y = "Snow depth (cm)", x = "Date")+
  facet_wrap(~winter, scales = "free")+
  themepointstop)

(tdaily <- 
    ggplot(snow)+
    geom_line(aes(x = date, y = biomassavail, linetype = snowgrid))+
    labs(y = "Available willow biomass (g/m2)", x = "Date")+
    facet_wrap(~winter, scales = "free")+
    themepointstop)

dailysnowfig <- ggarrange(sdaily, tdaily, ncol = 2, nrow = 1)



# Figure showing summary of dependent variables ---------------------------

fecal[m == "1", month := "January"][m == "3", month := "March"]

(feces <- 
   ggplot(fecal)+
   geom_boxplot(aes(x = winter, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
   geom_abline(intercept = 10, slope = 0, linetype = 2)+
   labs(x = "Winter", y = "Fecal crude protein (%)")+
   scale_fill_manual(values = foodcols)+
   themepoints)

(foraging <- ggplot(forag)+
  geom_boxplot(aes(x = winter, y = forage, fill = food), alpha = .5)+
  scale_fill_manual(values = foodcols, guide = NULL)+
  labs(y = "Foraging effort (hr/day)", x = "Winter")+
  themepoints)

#weight loss by year
(wcresid <- ggplot(wchange)+
    geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
    geom_boxplot(aes(x = winter, y = weight.c.resid, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Weight change residual (g)", x = "Winter")+
    themepoints)


sumdepfig <- ggarrange(wcresid, foraging, feces, nrow = 3, ncol = 1)



ggsave("Output/Figures/env_summary_figure1.jpeg", sumenvfig, width = 14, height = 10, unit = "in")
ggsave("Output/Figures/env_summary_figure2.jpeg", sumenvfig2, width = 14, height = 10, unit = "in")

ggsave("Output/Figures/snow_daily_figure.jpeg", dailysnowfig, width = 14, height = 7, unit = "in")

ggsave("Output/Figures/dep_var_figure.jpeg", sumdepfig, width = 6, height = 14, unit = "in")


