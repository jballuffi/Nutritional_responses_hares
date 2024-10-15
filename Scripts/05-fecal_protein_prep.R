#preliminary work. Assessing fecal protein content from preliminary sample

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

cp <- fread("Input/fecal_protein_round1and2.csv")
cp <- cp[order(vial)]

dm <- fread("Input/fecal_drymatter.csv")
dm <- dm[order(Vial)]

samps <- fread("Output/samplelist.csv")

#some vials turned out to be duplicated. Grab all the vial numbers for now
dupvials <- samps[duplicated(Vial) == TRUE, unique(Vial)]

cp <- cp[!vial %in% dupvials]
dm <- dm[!Vial %in% dupvials]


# merge nutritional composition results --------------------------

#merge crude protein and Dry matter data
out1 <- merge(cp, dm, by.x = "vial", by.y = "Vial", all = TRUE)

#calculate CP on a dry matter basis
out1[, CP_dm := CP/DM]

#take just CP on a DM basis and vial number
out2 <- out1[, .(vial, CP_dm, Ash)]



# merge nutritional data with trapping data -------------------------------

#merge out2 (final CP data) and the trapping data of these samples
dat <- merge(samps, out2, by.x = "Vial", by.y = "vial", all.y = TRUE)
dat <- dat[!is.na(CP_dm)]
dat <- dat[!is.na(m)]

#make factors
dat[, m := as.factor(m)]
dat[, Food := as.factor(Food)]
dat[, y := as.factor(y)]

#model prediction for CP. The interaction of month, year and food add
mod <- lm(CP_dm ~ m*y*Food, data = dat)
summary(mod)

foodcols <- c("1" = "red3", "0" = "grey40")


(plot <- 
  ggplot(dat)+
  geom_boxplot(aes(x = y, y = CP_dm, color = Food), outlier.shape = NA)+
  geom_abline(intercept = 7.5, slope = 0, linetype = 2)+
  geom_abline(intercept = 10, slope = 0, linetype = 2)+
  labs(x = "Winter", y = "Fecal crude protein (%)")+
  scale_color_manual(values = foodcols)+
  ylim(6, 18)+
  theme_minimal()+
  facet_wrap(~m)
  )




 # save things -------------------------------------------------------------

saveRDS(dat, "Output/data/CP_results_cleaned.rds")

ggsave("Output/figures/fecal_protein_preliminary_withmonth.jpeg", plot, width = 8, height = 4, units = "in")


