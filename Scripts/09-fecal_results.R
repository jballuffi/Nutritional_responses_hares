#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
datD <- readRDS("Output/Data/full_data_daily_nogrid.rds") #daily data
fecal <- readRDS("Output/Data/fecal_protein.rds")

#merge by date and snow grid
fecal <- merge(fecal, datD, by = c("date", "year", "yearfactor"), all.x = TRUE)



# Fecal Protein Analysis ------------------------------------------------------------

#MODEL
#Build model
Q1 <- lmer(CP_dm ~ biomass*food + temp*food + haredensity*food + mortrate*food + (1|snowgrid), fecal)

#summarize model
summary(Q1)
anova(Q1) #gives f value
Anova(Q1) #gives p value
lmerTest::ranova(Q1) #residuals

#get table of results
Q1_sum <- lmer_out(Q1)

#get R2
Q1R2 <- round(r.squaredGLMM(Q1), 2)[1]



# Figure ------------------------------------------------------------------

#get fecal prediction for biomass*food
biopred_fecal <- as.data.table(ggpredict(Q1, terms = c("biomass", "food")))
setnames(biopred_fecal, "group", "food")

#get fecal prediction for temp*food
temppred_fecal <- as.data.table(ggpredict(Q1, terms = c("temp", "food")))
setnames(temppred_fecal, "group", "food")

#figure
(bio_fecal <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = biomass, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = biopred_fecal)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = biopred_fecal)+
    scale_color_manual(values = foodcols, name = "Food treatment")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    labs(x = "Twig biomass (kg/ha)", y = "Fecal protein (%)", subtitle = "A)")+
    themethesisright+
    theme(legend.position = c(.18, .88),
          legend.background = element_blank()))

(temp_fecal <- 
    ggplot()+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    geom_point(aes(x = temp, y = CP_dm, color = food), alpha = .2, data = fecal)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = food), alpha = 0.4, data = temppred_fecal)+
    geom_line(aes(x = x, y = predicted, color = food), linewidth = 0.7, data = temppred_fecal)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(x = "Temperature (°C)", y = "Fecal protein (%)", subtitle = "B)")+
    themethesisright)



# Create final figure and save --------------------------------------------

fecalfig <- ggarrange(bio_fecal, temp_fecal, ncol = 1, nrow = 2, align = c("hv"))
fecalfig

ggsave("Output/Figures/Fecal_Figure.jpeg", fecalfig, width = 4, height = 8, unit = "in")
write.csv(Q1_sum, "Output/Tables/fecal_model_results.csv")
saveRDS(Q1R2, "Output/Data/fecal_R2.rds")

