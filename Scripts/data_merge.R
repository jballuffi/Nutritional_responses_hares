
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped data
fecal <- readRDS("Output/data/CP_results_cleaned.rds")
foraging <- readRDS("Output/Data/foraging_rates.rds")
foodadd <- readRDS("Output/Data/food_adds.rds")
twigs <- readRDS("Output/Data/snow_and_food.rds")
weights <- readRDS("Output/Data/winter_weights.rds")
density <- readRDS("../HR_PopCycle_SnowshoeHares/output/results/dailyharedensities.rds")




# merge weight and food add -----------------------------------------------

foodadd[, ID := as.factor(Eartag)]

weights <- merge(weights, foodadd, by = c("ID", "winter"), all.x = TRUE)

weights[is.na(Food), Food := 0]
weights[, Food := as.factor(Food)]
weights <- weights[y < 2020]
weights[, y := as.factor(y)]

foodcols <- c("1" = "red3", "0" = "grey40")

ggplot(weights[Sex == 2 & m == 1])+
  geom_boxplot(aes(x = y, y = Weightweek, color = Food))+
  labs(y = "Weight (g)", title = "Female weight in january")+
  scale_color_manual(values = foodcols)+
  ylim(1100, 2000)+
  theme_minimal()












fecal[, Eartag := as.factor(Eartag)]

#read in winter weight data

alltrap <- alltrap[m > 10|m < 4]

alltrap[, Food := as.factor(Food)]

inds <- as.character(fecals[, unique(Eartag)])

samptrap <- alltrap[ID %in% inds]
samptrap <- samptrap[, .(grid, ID, Sex, idate, y, m, RHFweek, Weightweek, winter, Food)]

samptrap[Weightweek == "NaN", Weightweek := "NA"]
samptrap[is.na(Weightweek), .N]


#get weight change for winter and individual, merge to fecal data
#does crude protein predict weight change?

#calculate day count since first capture for an individual that winter
samptrap[, day := as.integer(idate - min(idate)), by = .(ID, winter)]

#get number of weights by each individual and winter
samptrap[, n := .N, by = .(ID, winter)]

#get weight change by winter and individual
#samptrap[, wchange := get_slope(x = day, y = Weightweek), by = .(ID, winter)]

#get avg weight by winter and individual
samptrap[, waverage := mean(Weightweek, na.rm = TRUE), by = .(ID, winter)]

wplot <- ggplot(samptrap)+
  geom_boxplot(aes(x = winter, y = waverage, color = Food))+
  labs(x = "Winter", y = "Mean weight (g)", title = "Winter weights (Nov-Mar, n = 426)")+
  theme_minimal()


avgweights <- samptrap[, .(waverage = mean(Weightweek)), by = .(winter, ID)]
#prob should split up weight within weeks. take 1 and 2 in the weight prep script


ggsave("Output/figures/winter_weights_female.jpeg", wplot, width = 6, height = 4, unit = "in")
