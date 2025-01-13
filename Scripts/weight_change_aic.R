
#explain weight change residuals

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

wloss <- readRDS("Output/Data/weight_change.rds")
densitym <- readRDS("Output/Data/hare_population_monthly.rds")
densitya <- readRDS("Output/Data/densities_annual.rds")
snow <- readRDS("Output/Data/annual_snow_conditions.rds")


#take only weight changes of females
w <- wloss[!is.na(weight.c) & sex == "female"]

#merge with annual hare densities
w <- merge(w, densitya, by = "winter", all.x = TRUE)

#merge with annual snow
w <- merge(w, snow, by = c("winter", "snowgrid"), all.x = TRUE )



# make models to compare with AIC -----------------------------------------

#food add, snow depth, food availability, hare density, lynx density, phase
#snowgrid, winter


n <- lm(weight.c.resid ~ 1, w)
f <- lm(weight.c.resid ~ food, w)
s <- lm(weight.c.resid ~ snowavg, w)
t <- lm(weight.c.resid ~ biomassavg, w)
h <- lm(weight.c.resid ~ hares.avg, w)
l <- lm(weight.c.resid ~ lynx, w)
p <- lm(weight.c.resid ~ phase, w)



mods <- list(n, f, s, t, h, l, p)

Names <- c('n', 'f', 's', 't', 'h', 'l', 'p')

AIC <- as.data.table(aictab(REML = F, cand.set = mods, modnames = Names, sort = TRUE))

AIC[, ModelLik := NULL]

AIC[, Cum.Wt := NULL]

#round whole table to 3 dec places
AIC <- AIC %>% mutate_if(is.numeric, round, digits = 3)



ggplot(w)+
  geom_point(aes(x = snowavg, y = weight.c.resid, color = food))+
  geom_smooth(aes(x = snowavg, y = weight.c.resid, color = food), method = "lm")

ggplot(w)+
  geom_boxplot(aes(x = winter, y = weight.c.resid, color = food))

ggplot(w)+
  geom_boxplot(aes(x = phase, y = weight.c.resid, color = food))

