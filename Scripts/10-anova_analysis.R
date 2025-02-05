#script for making two way anova

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

dat <- readRDS("Output/Data/full_data_daily.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")
forag <- readRDS("Output/Data/foraging_weekly.rds")
weights <- readRDS("Output/Data/weight_change.rds")



# categorize winters ------------------------------------------------------

#make this a function!

#get medians for hare density, percap, snow, temp, twig, categorize days based on that
#hare density
dat[, md := median(haredensity)][haredensity >= md, dcat := "high"][haredensity < md, dcat := "low"]
#per capita twig availability
dat[, mpc := median(percap)][percap >= mpc, pccat := "high"][percap < mpc, pccat := "low"]
#snow depth
dat[, ms := median(snow)][snow >= ms, scat := "high"][snow < ms, scat := "low"]
#temperature
dat[, mtemp := median(tempmean, na.rm = TRUE)][tempmean >= mtemp, tempcat := "high"][tempmean < mtemp, tempcat := "low"]
#twig availability
dat[, mtwig := median(twig)][twig >= mtwig, twigcat := "high"][twig < mtwig, twigcat := "low"]


#get the modes of each category by year, to categorize year into a high or low snow and density 
yearcat <- dat[, .(dcat = getmode(dcat),
                   pccat = getmode(pccat),
                   scat = getmode(scat),
                   tempcat = getmode(tempcat),
                   twigcat = getmode(twigcat)), year]

#make categories factors
yearcat[, dcat := as.factor(dcat)][, scat := as.factor(scat)]

#paste categories together to get the full two-way category
yearcat[, cat := paste0(dcat, "-", scat)]

#merge in year categories with dependent data
weights <- merge(weights, yearcat, by = "year", all.x = TRUE)
fecal <- merge(fecal, yearcat, by = "year", all.x = TRUE)
forag <- merge(forag, yearcat, by = "year", all.x = TRUE)

# ^^^ what to do about mort rates?



# two-way anova on controls --------------------------------------------

#weight change residual as response
wmod <- lm(weight.c.resid ~ dcat + scat, weights[food == 0])
summary(wmod)
wanova <- anova(wmod)
waov <- aov(wmod)
wposthoc <- TukeyHSD(x = waov, conf.level = 0.95)
wposthoc

(wcontrol <- ggplot(weights[food == 0])+
    geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
    geom_boxplot(aes(x = cat, y = weight.c.resid, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Weight change residual (g)", x = "Density-Snow Year")+
    themepoints)


#fecal protein as response
fecmod <- lm(CP_dm ~ dcat + scat, fecal[food == 0])
summary(fecmod)
fecanova <- anova(fecmod)
fecaov <- aov(fecmod)
fecposthoc <- TukeyHSD(x = fecaov, conf.level = 0.95)
fecposthoc

(fecescontrol <- 
    ggplot(fecal[food == 0])+
    geom_boxplot(aes(x = cat, y = CP_dm, fill = food), alpha = .5, outlier.shape = NA)+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Fecal crude protein (%)", x = "Density-Snow Year")+
    themepoints)


#foraging rate
formod <- lm(forage ~ dcat + scat, forag[food == 0])
summary(formod)
foranova <- anova(formod)
foraov <- aov(formod)
forposthoc <- TukeyHSD(x = foraov, conf.level = 0.95)
forposthoc

(forcontrol <- 
    ggplot(forag[food == 0])+
    geom_boxplot(aes(x = cat, y = forage, fill = food), alpha = .5)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "Weekly foraging effort (hr/day)", x = "Density-Snow Year")+
    themepoints)






# save stats and figures table ---------------------------------------------------------

names(yearcat) <- c("Year", "Density", "Per Capita Twigs", "Snow", "Temperature", "Twigs", "Density-Snow")
write.csv(yearcat, "Output/Tables/year_categories.csv")


write.csv(wanova, "Output/Tables/weight_anova.csv")
write.csv(fecanova, "Output/Tables/fecal_anova.csv")
write.csv(foranova, "Output/Tables/forage_anova.csv")

ggsave("Output/Figures/weight_anova.jpeg", wcontrol,  width = 6, height = 5, unit = "in")
ggsave("Output/Figures/fecal_anova.jpeg", fecescontrol,  width = 6, height = 5, unit = "in")
ggsave("Output/Figures/foraging_anova.jpeg", forcontrol,  width = 6, height = 5, unit = "in")
