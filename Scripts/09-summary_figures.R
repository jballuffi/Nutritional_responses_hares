
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




# figure for the independent variables ------------------------------------

(d <- ggplot(density)+
    geom_path(aes(x = date, y = haredensity, group = winter, color = phase))+
    scale_color_manual(values = phasecols, breaks=c('increase', 'peak', 'decrease', 'low'))+
    labs(x = "", y = "Hare density (hares/ha)", subtitle = "A")+
    themepoints)

(s <- 
  ggplot(snow)+
  geom_line(aes(x = date, y = snow, color = snowgrid))+
  labs(y = "Snow depth (cm)", x = "Date")+
  facet_wrap(~winter, scales = "free")+
  scale_color_ordinal(guide = NULL)+
  themepoints)

(t <- 
    ggplot(snow)+
    geom_line(aes(x = date, y = biomassavail, color = snowgrid))+
    labs(y = "Available willow biomass (g/m2)", x = "Date")+
    facet_wrap(~winter, scales = "free")+
    scale_color_ordinal(guide = NULL)+
    themepoints)

ind.var.fig <- ggarrange(s, t, ncol = 1, nrow = 2)



# Figure showing summary of dependent variables ---------------------------

fecal[m == "1", month := "January"][m == "3", month := "March"]

(fecalplot <- 
   ggplot(fecal)+
   geom_boxplot(aes(x = winter, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
   #geom_abline(intercept = 7.5, slope = 0, linetype = 2)+
   geom_abline(intercept = 10, slope = 0, linetype = 2)+
   labs(x = "Winter", y = "Fecal crude protein (%)")+
   scale_fill_manual(values = foodcols)+
   #ylim(6, 18)+
   themepoints+
   facet_wrap(~month, nrow = 2, ncol = 1))

(forageplot <- ggplot(forag)+
  geom_boxplot(aes(x = winter, y = forage, fill = food), alpha = .5)+
  scale_fill_manual(values = foodcols)+
  labs(y = "Foraging effort (hr/day)", x = "Winter")+
  themepoints)

#weight loss by year
(wcplot <- ggplot(wchange)+
    geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
    geom_boxplot(aes(x = winter, y = weight.c, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols)+
    labs(title = "Weight change by winter", y = "Weight change (g)", x = "Winter")+
    themepoints)




ggsave("Output/Figures/ind_var_summary_figure.jpeg", ind.var.fig, width = 7, height = 12, unit = "in")
ggsave("Output/Figures/weightchange_summary.jpeg", wcplot, width = 5, height = 4, unit = "in")
ggsave("Output/Figures/fecalprotein_summary.jpeg", fecalplot, width = 6, height = 8, unit = "in")
ggsave("Output/Figures/forage_summary.jpeg", forageplot, width = 7, height = 4, unit = "in")


