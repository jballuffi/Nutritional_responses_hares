
#use trapping data to calculate individual weight change over winter

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
trap <- fread("Input/trapping_all_records.csv")
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

#take only adults and 2013 onward
trap <- trap[date > "2014-06-01"]

#subset to only include winter months
trap <- trap[m %in% wintermonths]

#categorize into winters
trap[month(date) > 6, winter := paste0(year(date), "-", year(date) + 1)]
trap[month(date) < 6, winter := paste0(year(date) - 1, "-", year(date))]

#merge with food add individuals
trap <- merge(trap, food, by = c("id", "winter"), all.x = TRUE)

#fill in empty food column
trap[is.na(food), food := "0"]


# use closest snow grid ----------------------------------------------------

trap[grid == "Agnes" | grid == "Chitty", snowgrid := "Agnes"]
trap[grid == "Kloo" | grid == "Sulphur" | grid == "Chadbear" | grid == "Rolo", snowgrid := "Kloo"]
trap[grid == "Jo", snowgrid := "Jo"]

trap <- trap[!is.na(snowgrid)]



# create fall data -----------------------------------------------

#subset to just october for fall weights
fall <- trap[m == 9 | m == 10]

# #create col for earliest date caught by bunny that fall
# fall[, mindate := min(date), by = .(id, winter)]
# 
# #use only first traps of a fall
# fall <- fall[date == mindate]

fallsum <- fall[, .(weight.a = mean(weight, na.rm = TRUE), sex = getmode(sex), rhf.a = mean(rhf, na.rm = TRUE)), by = .(id, winter, snowgrid, grid, food)]

# fall <- fall[, .(winter, snowgrid, date, id, sex, weight, rhf)]
# setnames(fall, c("rhf", "weight", "date"), c("rhf.a", "weight.a", "date.a"))



# create spring data ------------------------------------------------------

#subset to just march for spring dates
spring <- trap[m == 3 | m == 4]

# #create col for earliest date caught by bunny that fall
# spring[, maxdate := max(date), by = .(id, winter)]
# 
# #take only latest trap dates
# spring <- spring[date == maxdate]
# 
# spring <- spring[, .(winter, snowgrid, date, id, sex, weight, rhf)]
# setnames(spring, c("rhf", "weight", "date"), c("rhf.s", "weight.s", "date.s"))

springsum <- spring[, .(weight.s = mean(weight, na.rm = TRUE), sex = getmode(sex), rhf.s = mean(rhf, na.rm = TRUE)), by = .(id, winter, snowgrid, grid, food)]


# calculate weight change -------------------------------------------------

wloss <- merge(fallsum, springsum, by = c("winter", "snowgrid", "grid", "id", "sex", "food"), all = TRUE)

# wloss[, daylength := date.s - date.a]
# wloss[, daylength := as.numeric(daylength)]

wloss[, wchange := (weight.s - weight.a)] #decide if you want to do it per day



# look at trends ----------------------------------------------------------

#recreate figure 5 in Hodges 2006
ggplot(wloss)+
  geom_point(aes(x = weight.a, y = wchange))+
  geom_abline(intercept = 0, slope = 0, linetype = 2)+
  geom_smooth(aes(x = weight.a, y = wchange), method = "lm", color = "black")+
  labs(x = "Weight in autumn (g)", y = "Weight change over winter (g)")+
  themepoints

#remove any hares that were less than 1000 g in fall
wloss[!is.na(weight.a) & weight.a < 1000, include := "no"]
wloss[is.na(include), include := "yes"]

wlossyes <- wloss[include == "yes"]

#weight loss by year
ggplot(wlossyes)+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_boxplot(aes(x = winter, y = wchange, fill = food), alpha = .7)+
  themepoints

#spring weights by year and sex
ggplot(wlossyes[!is.na(sex)])+
  geom_boxplot(aes(x = winter, y = weight.s, fill = sex), alpha = .7)+
  themepoints

#spring weights by year
ggplot(wlossyes[sex = 2])+
  geom_boxplot(aes(x = winter, y = weight.s, fill = food), alpha = .7)+
  themepoints



# save prepped data -------------------------------------------------------

#save weight change data
saveRDS(wlossyes, "Output/Data/weight_change.rds")


