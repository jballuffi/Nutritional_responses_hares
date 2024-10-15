#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in trapping records data
trap <- fread("Input/trapping_all_records.csv")


# column prep ----------------------------------------------------------

#set up data columns
trap[, idate := dmy(dateCap)]
trap[, y := year(idate)]
trap[, m := month(idate)]
setorder(trap, idate)

foods[, Eartag := as.factor(Eartag)]

#list months that count as winter
wintermonths <- c("1", "2", "3", "4", "9", "10","11", "12")

#rename hindfoot column to RHF so the two feet measures match
setnames(trap, "Hindfoot", "RHF")

#rename eartag to ID for ease
setnames(trap, "Eartag", "ID")

#make ID a factor
trap[, ID := as.factor(ID)]



# avg values --------------------------------------------------------------

#for sex: 0 = no data, 1 = female, 2 = male 
#turn 0s to NAs
trap[Sex == 0, Sex := NA]
#get mode by ID, this function doesn't account for NAs
trap[, Sex := getmode(Sex), by = ID]
#change to factor
trap[, Sex := as.factor(Sex)]

#turn zeros in weights/RHF to NAs
trap[RHF == 0, RHF := NA][RHF_2 == 0, RHF_2 := NA]
trap[Weight == 0, Weight := NA][Weight_2 == 0, Weight_2 := NA][Weight_3 == 0, Weight_3 := NA][Weight_4 == 0, Weight_4 := NA]

#some cases where RHF_2 and weight_2 are incorrectly low values 
trap[RHF - RHF_2 > 100, RHF_2 := NA]
trap[Weight - Weight_2 > 800, Weight_2 := NA]

#mean RHF for a whole week (most of the time RHF was only taken once)
trap$RHFweek <- rowMeans(trap[, .(RHF, RHF_2)], na.rm = TRUE)

#mean weight for the whole week
trap$Weightweek <- rowMeans(trap[, .(Weight, Weight_2, Weight_3, Weight_4)], na.rm = TRUE)




# subsetting --------------------------------------------------------------

#remove juvs and work with just adults
adults <- trap[!grep("Juv", Maturity)]  #change this to just grab adults instead?

#cut to just relevant columns
adults <- adults[, .(ID, grid, idate, y, m, Sex, RHFweek, Weightweek)]

#replace NaN with NA
adults[RHFweek == "NaN", RHFweek := NA]

#subset to only include winter months
winters <- adults[m %in% wintermonths]



# create winter classifications -------------------------------------------

#classify winters
winters[m < 5, winter := y - 1]
winters[m > 8, winter := y]
winters[, winter := paste0(winter, "-", winter+1)]
winters[, winter := as.character(winter)]



# save just winter data and all cleaned trapping data ---------------------

saveRDS(winters, "Output/Data/winter_weights.rds")
saveRDS(adults, "Output/Data/fullyear_weights.rds")
