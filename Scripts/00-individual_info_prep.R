
#use trapping data to calculate individual weight change over winter

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
trap <- fread("Input/trapping_all_records.csv")
food <- readRDS("Input/food_adds.rds")



# variable prep ----------------------------------------------------------

#set up data columns
trap[, date := dmy(dateCap)]
trap[, year := year(date)]
trap[, m := month(date)]
setorder(trap, date)

#rename cols
setnames(trap, c("Hindfoot", "Eartag", "Sex", "Weight", "Maturity"), c("rhf", "id", "sex", "weight", "age"))

#grab only specific columns
trap <- trap[, .(year, m, date, grid, id, sex, weight, rhf, age)]

#for sex: 0 = no data, 1 = female, 2 = male 
#turn 0s to NAs
trap[sex == 0, sex := NA]

#get mode by ID, this function doesn't account for NAs
trap[, sex := getmode(sex), by = id]

#set sex as character
trap[, sex := as.character(sex)]

#change sex numbers to words
trap[sex == 1, sex := "male"][sex == 2, sex := "female"]

#change sex to to factor
trap[, sex := as.factor(sex)]

#id
trap[, id := as.character(id)]

#turn zeros in weights/RHF to NAs
trap[rhf == 0 | rhf == 1, rhf := NA]
trap[weight == 0, weight := NA]

#remove NA weights
trap <- trap[!is.na(weight)]

#insanely big or small rhf become NA
trap[rhf > 200 | rhf < 20, rhf := NA]

#take only adults and 2013 onward
#trap <- trap[date > "2014-06-01"]

#categorize into winters
trap[month(date) > 6, winter := paste0(year(date), "-", year(date) + 1)]
trap[month(date) < 6, winter := paste0(year(date) - 1, "-", year(date))]

#merge with food add individuals
trap <- merge(trap, food, by = c("id", "winter"), all.x = TRUE)

#fill in empty food column
trap[is.na(food), food := "0"]



# use closest snow grid ----------------------------------------------------

trap[grid == "Agnes" | grid == "Chitty", snowgrid := "Agnes"]
trap[grid == "Kloo" | grid == "Sulphur" | grid == "Chadbear" | grid == "Rolo" | grid == "Leroy", snowgrid := "Kloo"]
trap[grid == "Jo", snowgrid := "Jo"]

trap <- trap[!is.na(snowgrid)]



# pull out individual sex and grid for other future scripts ---------------

ind <- trap[, .(sex = getmode(sex), snowgrid = getmode(snowgrid), food = getmode(food)), by = .(id, winter)]



# create fall and spring data -----------------------------------------------

#subset fall weights
fall <- trap[m == 9 | m == 10 | m == 11]

#take mean weight, sex, mean rhf for each individual in spring
fallsum <- fall[, .(weight.a = round(mean(weight, na.rm = TRUE)), sex = getmode(sex), rhf.a = round(mean(rhf, na.rm = TRUE))), by = .(id, winter, snowgrid, grid, food)]

#subset to just march for spring dates
spring <- trap[m == 3 | m == 4]

#take mean weight, sex, mean rhf for each individual in spring
springsum <- spring[, .(weight.s = round(mean(weight, na.rm = TRUE)), sex = getmode(sex), rhf.s = round(mean(rhf, na.rm = TRUE))), by = .(id, winter, snowgrid, grid, food)]



# calculate weight change -------------------------------------------------

#merge fall and spring data
wloss <- merge(fallsum, springsum, by = c("winter", "grid", "snowgrid", "id", "sex", "food"), all = TRUE)

#calculate weight change from fall to spring, in grams
wloss[, weight.c := (weight.s - weight.a)]

#summary of sample size between food adds and controls
wloss[!is.na(weight.c), .N, by = .(food)]



# get weight change residuals based on autumn weight ----------------------------------------------------------

#recreate figure 5 in Hodges 2006
#this is the relationship between autumn weight and weight change over winter

#make linear model for weight change ~ autumn weight
weightline <- lm(weight.c ~ weight.a, wloss)

#to get line and confidence limits from linear model
effs_weightline <- as.data.table(ggpredict(weightline, terms = c("weight.a")))

#make plot with line and confidence limits
(hodges <- 
  ggplot()+
  geom_point(aes(x = weight.a, y = weight.c), data = wloss)+
  geom_abline(intercept = 0, slope = 0, linetype = 2)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, data = effs_weightline)+
  geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_weightline)+
  labs(x = "Weight in autumn (g)", y = "Overwinter weight change (g)")+
  #xlim(1000, 2100)+
  themepoints)

#get intercept and slope from linear model
i <- as.numeric(coef(weightline)["(Intercept)"])
slope <- as.numeric(coef(weightline)["weight.a"])

#get predicted weight change for every recorded autumn weight
wloss[, weight.c.pred := i + (weight.a*slope)]

#take difference between predicted weight change and actual weight change
wloss[, weight.c.resid := weight.c - weight.c.pred]

#remove any hares that were less than 1000 g in fall
wloss[!is.na(weight.a) & weight.a < 1000, include := "no"]
wloss[is.na(include), include := "yes"]
wlossyes <- wloss[include == "yes"]

#pull out year and make numeric. Make a year factor
wlossyes[, year := tstrsplit(winter, "-", keep = 2)]
wlossyes[, year := as.numeric(year)]
wlossyes[, yearfactor := as.factor(year)]


#wlossyes[, yearshort := substr(yearfactor, 3, 4)]

forstan <- 
  ggplot(wlossyes)+
  geom_abline(intercept = 0, slope = 0, linetype = 2, color = "grey30")+
  geom_boxplot(aes(x = yearfactor, y = weight.c.resid))+
  ylim(-350, 350)+
  labs(x = "Spring Year", y = "Weight change residual (g)")+
  themepoints


#remove 2015
wlossyes <- wlossyes[year > 2015]



# final data --------------------------------------------------------------

#take just rows with weight change
wdata <- wlossyes[!is.na(weight.c)]

#take all the rows with spring weights
sdata <- wlossyes[!is.na(weight.s)]



# save prepped data and figures -------------------------------------------------------

# #save weight change data
# saveRDS(wdata, "Output/Data/weight_change.rds")
# 
# #save spring weight data
# saveRDS(sdata, "Output/Data/spring_weights.rds")

#save individual sex and grid
saveRDS(ind, "Output/Data/individual_info.rds")

# #save figure
# ggsave("Output/Figures/hodges_figure.jpeg", hodges, width = 6, height = 4, unit = "in")
# ggsave("Output/Figures/forstan.jpeg", width = 12, height = 6, unit = "in")
