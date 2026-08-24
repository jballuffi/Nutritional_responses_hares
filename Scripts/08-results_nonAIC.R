#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
datW <- readRDS("Output/Data/full_data_weekly_nogrid.rds") #weekly data
forag <- readRDS("Output/Data/foraging_weekly.rds")

#merge by week and snow grid
forag <- merge(forag, datW, by = c("week", "year", "yearfactor"), all.x = TRUE)



# Foraging Rate Analysis -------------------------------------------------------

#DESCRIPTIVE RESULTS
#effect of food on food add data set
suppmod <- lm(forage ~ food, forag)
suppanova <- anova(suppmod)
suppsum <- summary(suppmod)
supp_p <- round(suppanova$`Pr(>F)`[1], 3)
supp_t <- round(suppsum$coefficients[, 3][2], 2) #t-value
supp_df <- suppanova$Df[2]
rsq(suppmod)

#effect of sex on controls
sexmod <- anova(lm(forage ~ sex, forag[food == "Control"]))
psex <- round(sexmod$`Pr(>F)`[1], 3)

#effect of night length on foraging rate
nightmod <- lm(forage ~ nightlength, forag) #make model
nightsum <- summary(nightmod) #sum of model
nightanova <- anova(nightmod) #anova of model
nightcoef <- round(nightsum$coefficients[, 1][2]*60, 1) #coefficient
nightse <- round(nightsum$coefficients[, 2][2]*60, 1) #standard error
pnight <- round(nightanova$`Pr(>F)`[1], 3) #p-value
tnight <- round(nightsum$coefficients[, 3][2], 2) #t-value
dfnight <- nightanova$Df[2]
rsq(nightmod)


#MODEL
#build model
Q2 <- lmer(forage ~ haredensity*food + biomass*food + mortrate*food + temp*food + nightlength + (1|id) + (1|snowgrid), forag)

#model summaries
summary(Q2)
anova(Q2) #fvalues
Anova(Q2)[3] #pvalues
lmerTest::ranova(Q2) #residuals

#get table of results
Q2_sum <- lmer_out(Q2)

#get R2
Q2R2 <- round(r.squaredGLMM(Q2), 2)[1]


#get t-values
t_density = round(coef(summary(Q2))[,"t value"][2], 2)
t_food    = round(coef(summary(Q2))[,"t value"][3], 2)
t_biomass = round(coef(summary(Q2))[,"t value"][4], 2)
t_mort = round(coef(summary(Q2))[,"t value"][5], 2)
t_temp = round(coef(summary(Q2))[,"t value"][6], 2)
t_densityint = round(coef(summary(Q2))[,"t value"][8], 2 )
t_tempint = round(coef(summary(Q2))[,"t value"][11], 2 )

#get coefficients
b_density <- round(fixef(Q2)[2], 3)
b_food <- round(fixef(Q2)[3], 2)
b_temp <- round(fixef(Q2)[6], 3)
b_densityint <- round(fixef(Q2)[8], 3)

#get standard errorts
se_density <- round(se.fixef(Q2)[2], 3)
se_food <- round(se.fixef(Q2)[3], 2)
se_temp <- round(se.fixef(Q2)[6], 2)
se_densityint <- round(se.fixef(Q2)[8], 2)





# Figures -----------------------------------------------------------------

#FORAGING
#get forage prediction for density*food
densitypred_forage <- as.data.table(ggpredict(Q2, terms = c("haredensity", "food")))
setnames(densitypred_forage, "group", "food")

#get forage prediction for temperature*food
temppred_forage <- as.data.table(ggpredict(Q2, terms = c("temp", "food")))
setnames(temppred_forage, "group", "food")

(density_forage <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = forage, color = food), alpha = .2, data = forag)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = densitypred_forage)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = densitypred_forage)+
    scale_color_manual(values = foodcols, name = "Food treatment")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    labs(x = "Hare density (hares/ha)", y = "Foraging rate (hr/day)", subtitle = "A)")+
    themethesisright +
    theme(legend.position = c(.18, .88),
          legend.background = element_blank()))

(temp_forage <- 
    ggplot()+
    geom_point(aes(x = temp, y = forage, color = food), alpha = .3, data = forag)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.3, data = temppred_forage)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = temppred_forage)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Temperature (°C)", y = "Foraging rate (hr/day)", subtitle = "B)")+
    themethesisright)



# Create final figure and save --------------------------------------------

foragefig <- ggarrange(density_forage, temp_forage, ncol = 1, nrow = 2, align = c("hv"))
foragefig

ggsave("Output/Figures/Foraging_Figure.jpeg", foragefig, width = 4, height = 8, unit = "in")
write.csv(Q2_sum, "Output/Tables/foraging_model_results.csv")
saveRDS(Q2R2, "Output/Data/foraging_R2.rds")

