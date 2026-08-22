#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# merge info and make a control and food add dataset --------

#read data
forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

forag <- forag[month(date) == 1]

#get controls only
foragcon <- forag[food == 0]

foragsum <- forag[, .(forage = mean(forage), haredensity = mean(haredensity)), by = .(id, yearfactor)]


samps <- foragcon[, .(N = .N, max = max(forage)), yearfactor]

ggplot()+
  geom_boxplot(aes(x = yearfactor, y = forage), data = foragcon)+
  geom_text(aes(x = yearfactor, y = max + .35, label = N), data = samps)+
  labs(x = "year", y = "Foraging effort (hr/day)")+
  themepoints

summary(lm(forage ~ haredensity, foragsum))

ggplot(foragsum)+
  geom_point(aes(x = haredensity, y = forage))+
  geom_abline(aes(intercept = 8.9, slope = 1.319), linewidth = 1)+
  geom_text(aes(x = 0.28, y = 14, label = "8.9 + 1.3X (p = 0.0009)"))+
  labs(x = "Hare density (hare/ha)", y = "Monthly foraging rate (hrs/day)")+
  themepoints




