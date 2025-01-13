
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

trap[, sex := as.character(sex)]

#change sex numbers to words
trap[sex == 1, sex := "male"][sex == 2, sex := "female"]

#change to factor
trap[, sex := as.factor(sex)]

#turn zeros in weights/RHF to NAs
trap[rhf == 0 | rhf == 1, rhf := NA]
trap[weight == 0, weight := NA]

#insanely big or small rhf become NA
trap[rhf > 200 | rhf < 20, rhf := NA]

#take only adults and 2013 onward
trap <- trap[date > "2014-06-01"]

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



# create fall and spring data -----------------------------------------------

#subset fall weights
fall <- trap[m == 10 | m == 11]

#take mean weight, sex, mean rhf for each individual in spring
fallsum <- fall[, .(weight.a = round(mean(weight, na.rm = TRUE)), sex = getmode(sex), rhf.a = round(mean(rhf, na.rm = TRUE))), by = .(id, winter, snowgrid, grid, food)]

#subset to just march for spring dates
spring <- trap[m == 3 | m == 4]

#take mean weight, sex, mean rhf for each individual in spring
springsum <- spring[, .(weight.s = round(mean(weight, na.rm = TRUE)), sex = getmode(sex), rhf.s = round(mean(rhf, na.rm = TRUE))), by = .(id, winter, snowgrid, grid, food)]


# calculate weight change -------------------------------------------------

#merge fall and spring data
wloss <- merge(fallsum, springsum, by = c("winter", "snowgrid", "grid", "id", "sex", "food"), all = TRUE)

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
  geom_line(aes(x = x, y = predicted), size = 1, data = effs_weightline)+
  labs(x = "Weight in autumn (g)", y = "Overwinter weight change (g)")+
  xlim(1000, 2100)+
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



# look at trends ----------------------------------------------------------

#weight loss by year
(wchange <- ggplot(wlossyes)+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_boxplot(aes(x = winter, y = weight.c, fill = food), alpha = .7)+
  labs(title = "Weight change by winter", y = "Weight change (g)", x = "Winter")+
  themepoints)

#spring weights by year and sex
(springweight <- 
  ggplot(wlossyes[!is.na(sex) & food == 0])+
  geom_boxplot(aes(x = winter, y = weight.s, fill = sex), alpha = .7)+
  labs(title = "Control spring weights by winter", x = "Winter", y = "Spring weight (g)")+
  themepoints)

#spring weights by year
(springweightfem <- 
  ggplot(wlossyes[sex == "female"])+
  geom_boxplot(aes(x = winter, y = weight.s, fill = food), alpha = .7)+
  labs(title = "Female spring weight by winter", x = "Winter", y = "Spring weight (g)")+
  themepoints)


#sample sizes for both male and female
Nbothsex <- wlossyes[, .N, by = .(winter, food)]

#sample size for just females
Nfemale <- wlossyes[sex == "female", .N, by = .(winter, food)]


# save prepped data and figures -------------------------------------------------------

#save weight change data
saveRDS(wlossyes, "Output/Data/weight_change.rds")

#save sample size summary tables
write.csv(Nbothsex, "Output/Data/weights_samplesize_bothsexes.csv")
write.csv(Nfemale, "Output/Data/weights_samplesize_females.csv")

#save figures
ggsave("Output/Figures/hodges_figure.jpeg", hodges, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/wchange_winter.jpeg", wchange, width = 7, height = 4, unit = "in")
ggsave("Output/Figures/sweight_winter_sex.jpeg", springweight, width = 7, height = 4, unit = "in")
ggsave("Output/Figures/sweight_winter_food.jpeg", springweightfem, width = 7, height = 4, unit = "in")
