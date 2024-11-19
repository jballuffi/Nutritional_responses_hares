
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in trapping records data
trap <- fread("Input/trapping_all_records.csv")


# column prep ----------------------------------------------------------

#set up data columns
trap[, date := dmy(dateCap)]
trap[, y := year(date)]
trap[, m := month(date)]
setorder(trap, date)

#list months that count as winter
wintermonths <- c("1", "2", "3", "4", "9", "10","11", "12")

#rename cols
setnames(trap, c("Hindfoot", "Eartag", "Sex", "Weight", "Maturity"), c("rhf", "id", "sex", "weight", "age"))

#make ID a factor
trap[, id := as.factor(id)]

#grab only specific columns
trap <- trap[, .(y, m, date, grid, id, sex, weight, rhf, age)]


#for sex: 0 = no data, 1 = female, 2 = male 
#turn 0s to NAs
trap[sex == 0, sex := NA]
#get mode by ID, this function doesn't account for NAs
trap[, sex := getmode(sex), by = id]
#change to factor
trap[, sex := as.factor(sex)]

#turn zeros in weights/RHF to NAs
trap[rhf == 0 | rhf == 1, rhf := NA]
trap[weight == 0, weight := NA]

#insanely big or small rhf become NA
trap[rhf > 200 | rhf < 20, rhf := NA]



# sub-setting --------------------------------------------------------------

#take only adults and 2013 onward
trap <- trap[age == "Adult" & date > "2014-06-01"]

#subset to only include winter months
trap <- trap[m %in% wintermonths]

#categorize into winters
trap[month(date) > 6, winter := paste0(year(date), "-", year(date) + 1)]
trap[month(date) < 6, winter := paste0(year(date) - 1, "-", year(date))]







# save just winter data and all cleaned trapping data ---------------------

saveRDS(winters, "Output/Data/winter_weights.rds")
saveRDS(adults, "Output/Data/fullyear_weights.rds")
