
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


density <- readRDS("../HR_PopCycle_SnowshoeHares/output/results/dailyharedensities.rds")
food <- readRDS("Output/Data/food_adds.rds")





# merge in food add info --------------------------------------------------

setnames(food, c("Eartag", "Food"), c("id", "food"))

food[, id := as.factor(id)]

wloss <- merge(wloss, food, by = c("id", "winter"), all.x = TRUE)

wloss[is.na(food), food := 0]

wloss[, food := as.factor(food)]

ggplot(wloss)+
  geom_boxplot(aes(x = winter, y = wchange, color = food))

