
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

forag <- readRDS("Output/Data/foraging_weekly.rds")
dat <- readRDS("Output/Data/full_data_weekly_nogrid.rds")



# merge info and make a control and food add dataset --------

#merge by week and snow grid
forag <- merge(forag, dat, by = c("week", "year", "yearfactor"), all.x = TRUE)

#get controls only
foragcon <- forag[food == 0]

#take only the years where there was food add
foodyears <- forag[food == 1, unique(winter)]

#take only females for food add comparisons
foragfood <- forag[winter %in% foodyears & sex == "female"]


ggplot(foragcon[year == 2017])+
  geom_path(aes(x = date, y = forage, group = id))+
  geom_point(aes(x = date, y = forage))+
  theme_bw()

ggplot(dat[year == 2017])+
  geom_path(aes(x = date, y = temp, group = 1))+
  geom_point(aes(x = date, y = temp))+
  theme_bw()

