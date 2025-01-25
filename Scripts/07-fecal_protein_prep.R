#preliminary work. Assessing fecal protein content from preliminary sample

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

cp <- fread("Input/fecal_CP_2016_2019.csv")
cp <- cp[order(vial)] #reorder by vial number

dm <- fread("Input/fecal_DM_2016_2019.csv")
dm <- dm[order(Vial)] #reocrder by vial number

samps <- fread("Input/samplelist.csv")

#some vials turned out to be duplicated. Grab all the vial numbers for now
dupvials <- samps[duplicated(Vial) == TRUE, unique(Vial)]

#remove duplicate vials
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
#dat[, m := as.factor(m)]
dat[, food := as.factor(food)]
dat[, y := as.factor(y)]
dat[, id := as.character(id)]
dat[, date := ymd(idate)]

#fix sex variable name
setnames(dat, c("Sex", "Vial", "Weight", "Ash"), c("sex", "vial", "weight", "ash"))



# Fix sex issue -----------------------------------------------------------

#for sex: 0 = no data, 1 = female, 2 = male 
#turn 0s to NAs
dat[sex == 0, sex := NA]

#make character
dat[, sex := as.character(sex)]

#change sex numbers to words
dat[sex == 1, sex := "male"][sex == 2, sex := "female"]

#change to factor
dat[, sex := as.factor(sex)]



# make snow grid col ------------------------------------------------------

dat[grid == "Agnes" | grid == "Chitty", snowgrid := "Agnes"]
dat[grid == "Kloo" | grid == "Sulphur" | grid == "Chadbear" | grid == "Rolo" | grid == "Leroy", snowgrid := "Kloo"]
dat[grid == "Jo", snowgrid := "Jo"]

dat[is.na(snowgrid)]



# Look at trends ----------------------------------------------------------

(plot <- 
    ggplot(dat)+
    geom_boxplot(aes(x = y, y = CP_dm, color = food), outlier.shape = NA)+
    geom_abline(intercept = 7.5, slope = 0, linetype = 2)+
    geom_abline(intercept = 10, slope = 0, linetype = 2)+
    labs(x = "Winter", y = "Fecal crude protein (%)")+
    scale_color_manual(values = foodcols)+
    ylim(6, 18)+
    theme_minimal()+
    facet_wrap(~m)
)



# cut to only important variables -----------------------------------------

dat2 <- dat[, .(vial, snowgrid, winter, m, date, id, sex, weight, RHF, GPS, axy, food, Nwinter, CP_dm, ash)]

#remove the one strange outlier
dat2 <- dat2[!CP_dm > 25]



 # save things -------------------------------------------------------------

saveRDS(dat2, "Output/Data/fecal_protein.rds")

ggsave("Output/Figures/fecal_protein_preliminary_withmonth.jpeg", plot, width = 8, height = 4, units = "in")


