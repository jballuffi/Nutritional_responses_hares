
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
trap <- fread("Input/trapping_all_records.csv")
density <- readRDS("../HR_PopCycle_SnowshoeHares/output/results/dailyharedensities.rds")
food <- readRDS("Output/Data/food_adds.rds")


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



# calculating weight change -----------------------------------------------

#subset to just october for fall weights
fall <- trap[m == 9 | m == 10]

#create col for earliest date caught by bunny that fall
fall[, mindate := min(date), by = .(id, winter)]

#use only first traps of a fall
fall <- fall[date == mindate]

fall <- fall[, .(winter, date, id, sex, weight, rhf)]
setnames(fall, c("rhf", "weight", "date"), c("rhf.a", "weight.a", "date.a"))

#subset to just march for spring dates
spring <- trap[m == 3 | m == 4]

#create col for earliest date caught by bunny that fall
spring[, maxdate := max(date), by = .(id, winter)]

#take only latest trap dates
spring <- spring[date == maxdate]

spring <- spring[, .(winter, date, id, sex, weight, rhf)]
setnames(spring, c("rhf", "weight", "date"), c("rhf.s", "weight.s", "date.s"))



wloss <- merge(fall, spring, by = c("winter", "id", "sex"))

wloss[, daylength := date.s - date.a]
wloss[, daylength := as.numeric(daylength)]

wloss[, wchange := (weight.s - weight.a)/daylength] #decide if you want to do it per day

#recreate figure 5 in Hodges 2006
ggplot(wloss)+
  geom_point(aes(x = weight.a, y = wchange, color = sex))

#remove any hares that were less than 1000 g in fall
wloss <- wloss[weight.a > 1000]

ggplot(wloss)+
  geom_boxplot(aes(x = sex, y = wchange))



# merge in food add info --------------------------------------------------

setnames(food, c("Eartag", "Food"), c("id", "food"))

food[, id := as.factor(id)]

wloss <- merge(wloss, food, by = c("id", "winter"), all.x = TRUE)

wloss[is.na(food), food := 0]

wloss[, food := as.factor(food)]

ggplot(wloss)+
  geom_boxplot(aes(x = winter, y = wchange, color = food))


# save just winter data and all cleaned trapping data ---------------------

saveRDS(winters, "Output/Data/winter_weights.rds")
saveRDS(adults, "Output/Data/fullyear_weights.rds")
