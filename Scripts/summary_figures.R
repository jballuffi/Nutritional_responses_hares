
#script for one into figure showing annual trends of data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


density <- readRDS("Output/Data/hares_daily.rds")
snow <- readRDS("Output/Data/snow_food_daily.rds")
forag <- readRDS("Output/Data/foraging_daily.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
wchange <- readRDS("Output/Data/weight_change.rds")



ggplot(fecal)+
  geom_boxplot(aes(x = winter, y = CP_dm, fill = food), alpha = .5)+
  scale_fill_manual(values = foodcols)+
  labs(y = "Fecal protein (%)", x = "Winter")+
  themepoints

ggplot(forag)+
  geom_boxplot(aes(x = winter, y = forage, fill = food), alpha = .5)+
  scale_fill_manual(values = foodcols)+
  labs(y = "Foraging effort (hr/day)", x = "Winter")+
  themepoints

ggplot(snow)+
  geom_boxplot(aes(x = winter, y = snow, fill = snowgrid), alpha = .5)+
  labs(y = "Snow depth (cm)", x = "Winter")+
  themepoints

ggplot(snow)+
  geom_boxplot(aes(x = winter, y = biomassavail, fill = snowgrid), alpha = .5)+
  labs(y = "Snow depth (cm)", x = "Winter")+
  themepoints



#weight loss by year
(wc <- ggplot(wchange)+
    geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
    geom_boxplot(aes(x = winter, y = weight.c, fill = food), alpha = .7)+
    scale_fill_manual(values = foodcols)+
    labs(title = "Weight change by winter", y = "Weight change (g)", x = "Winter")+
    themepoints)

summary(lm(weight.c ~ food, wchange))


ggsave("Output/Figures/weightchange_summary.jpeg", wc, width = 5, height = 4, unit = "in")


