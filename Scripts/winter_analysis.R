
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


density <- readRDS("../HR_PopCycle_SnowshoeHares/output/results/dailyharedensities.rds")
food <- readRDS("Output/Data/food_adds.rds")
wloss <- readRDS("Output/Data/weight_change.rds")



# merge in food add info --------------------------------------------------

setnames(food, c("Eartag", "Food"), c("id", "food"))

food[, id := as.factor(id)]

wloss <- merge(wloss, food, by = c("id", "winter"), all.x = TRUE)

wloss[is.na(food), food := 0]

wloss[, food := as.factor(food)]


wlossmeans <- wloss[, .(wchange_mean = mean(wchange, na.rm = TRUE), 
                        wchange_sd = sd(wchange, na.rm = TRUE)), by = .(food, winter)]



ggplot(wlossmeans)+
  geom_errorbar(aes(x = winter, color = food, ymax = wchange_mean + wchange_sd, ymin = wchange_mean - wchange_sd), width = .2)+
  geom_bar(aes(x = winter, y = wchange_mean, fill = food), position = "dodge", stat = "identity", width = .5)
   
ggplot(wloss)+
  geom_abline(aes(intercept = 0, slope = 0), linetype = 2)+
  geom_boxplot(aes(x = winter, y = wchange, fill = food), alpha = .7)+
  themepoints
  
