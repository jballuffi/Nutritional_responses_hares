
#Script that looks at the relationship between fecal protein and foraging rate
# and looks at whether foraging rate or fecal protein impact overwinter weight change
# author: Juliana Balluffi-Fry

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
weights <- readRDS("Output/Data/weight_change.rds")
forag_d <- readRDS("Output/Data/foraging_daily.rds")
forag_w <- readRDS("Output/Data/foraging_winter.rds")
forag_m <- readRDS("Output/Data/foraging_monthly.rds")
fecal <- readRDS("Output/Data/fecal_protein.rds")



# Does foraging rate correlate with fecal protein? -------------------------

#merge monthly foraging rates with fecal sample from the same month
fecal_m <- merge(fecal, forag_m, by = c("id", "winter", "m", "snowgrid", "food", "sex"), all.x = TRUE)

#plot fecal protein in response to monthly foraging rate
ggplot(fecal_m)+
  geom_point(aes(x = forage, y = CP_dm, color = food))+
  geom_smooth(aes(x = forage, y = CP_dm, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Monthly foraging rate (hr/day)", y = "Fecal protein (%)")+
  themepoints

#summarise the trend in this figure
summary(lm(CP_dm ~ forage*food, fecal_m))

#pull dates of each fecal sample, subtract one to get the day before
#create a column called "used for daily"
fecaldates <- fecal[, .(date = unique(date) - 1, usefordaily = "yes"), id]

#merge prev days of fecal samples with foraging data
forag_d <- merge(forag_d, fecaldates, by = c("id", "date"), all.x = TRUE) 

#take anything that has a "use for daily" listed -yes- and take id, month, and forage rate
#add a day back to date so it matches the fecal data again
forag_d_fecal <- forag_d[usefordaily == "yes", .(id, date, prev.dforag = forage)]
forag_d_fecal[, date := date + 1]

#merge in the foraging rates from the day before with their respective fecal samples
fecal_d <- merge(fecal, forag_d_fecal, by = c("id", "date"), all.x = TRUE)

#plot fecal protein in response to foraging effort of previous day
ggplot(fecal_d)+
  geom_point(aes(x = prev.dforag, y = CP_dm, color = food))+
  geom_smooth(aes(x = prev.dforag, y = CP_dm, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Foraging effort of previous day (hr)", y = "Fecal protein (%)")+
  themepoints

#summarize trend in figure
summary(lm(CP_dm ~ prev.dforag*food, fecal_d))



# Does foraging rates and fecal protein predict weight change? ---------------------

#merge in winter foraging rates with weight change
weights <- merge(weights, forag_w, by = c("id", "winter", "sex", "food", "snowgrid"), all.x = TRUE)

#get mean fecal protein by ind and winter
meanfecal <- fecal[, .(CP_dm_avg = mean(CP_dm)), by = .(id, winter)]

#pull just fecals from march and average those by individual and winter
marchfecal <- fecal[m == 3, .(CP_dm_march = mean(CP_dm)), by = .(id, winter)]

#merge in winter and march average fecals with weight data
weights <- merge(weights, meanfecal, by = c("id", "winter"), all.x = TRUE)
weights <- merge(weights, marchfecal, by = c("id", "winter"), all.x = TRUE)

#plot weight change resid in response to average fecal protein across winter
ggplot(weights)+
  geom_abline(intercept = 0, slope = 0, linetype = 2, color = "grey30", linewidth = 0.75)+
  geom_point(aes(x = CP_dm_avg, y = weight.c.resid, color = food))+
  geom_smooth(aes(x = CP_dm_avg, y = weight.c.resid, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Fecal protein (%)", y = "Weight change residual (g)")+
  themepoints

summary(lm(weight.c.resid ~ CP_dm_avg*food, weights))

#plot weight change resid in response to average foraging rate
ggplot(weights)+
  geom_abline(intercept = 0, slope = 0, linetype = 2, color = "grey30", linewidth = 0.75)+
  geom_point(aes(x = forage, y = weight.c.resid, color = food))+
  geom_smooth(aes(x = forage, y = weight.c.resid, color = food, fill = food), method = "lm")+
  scale_color_manual(values = foodcols)+
  scale_fill_manual(values = foodcols)+
  labs(x = "Average foraging effort (hr/day)", y = "Weight change residual (g)")+
  themepoints

summary(lm(weight.c.resid ~ forage*food, weights))



