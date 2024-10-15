#script that investigates foraging data from previous winters
#also includes snow depths from those years

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

beh <- fread("Input/allHareDailyValues2015_2021.csv")
trapping <- fread("Input/Trapping_data_all_records.csv")



# foraging data -----------------------------------------------------------

#create year and month column
beh[, m := month(Date)]
beh[, y := year(Date)]

#name the winter months
wintermonths <- c(1, 2, 3, 11, 12)

#cut to only winter 
beh <- beh[m %in% wintermonths]

#create a winter column
beh[m < 4, winter := paste0(y-1, "-", y)]
beh[m > 8, winter := paste0(y, "-", y+1)]

#swap B's in the collar data for 2's
beh[, id := gsub("B", "2", id)]

#pull out the grid of every individual using the trapping data
trapping[, Eartag := as.character(Eartag)]
grids <- trapping[, getmode(grid), Eartag]
names(grids) <- c("id", "grid")

#merge grids into behaviour data set
beh <- merge(beh, grids, by = "id", all.x = TRUE)

#now we need to match grid in behaviour data set to the snow data set.
#more grids with collar data than grids with snow data

#when grid with bunny is one of the snow grids, just copy to new col snowgrid
beh[grid == "Agnes" | grid == "Kloo" | grid == "Jo", snowgrid := grid]

#all other grids and their closest snow grid, but where is leroy?
beh[grid == "Sulphur" | grid == "Rolo" | grid == "Chadbear" | grid == "Leroy", snowgrid := "Kloo"]
beh[grid == "Chitty", snowgrid := "Agnes"]




 # figures -----------------------------------------------------------------


allwinters <-
  ggplot(beh[m < 5 & m > 1])+
  geom_point(aes(x = Date, y = Forage/3600))+
  facet_wrap(~winter, scales = "free")+
  theme_minimal()



# Save --------------------------------------------------------------------

ggsave("Output/Figures/foraging_allwinters.jpg", allwinters, width = 12, height = 10, unit = "in")
saveRDS(beh, "Output/Data/foraging_rates.rds")

