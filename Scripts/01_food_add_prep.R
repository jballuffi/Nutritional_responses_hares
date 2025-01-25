#script to collect food add individuals

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#create path to all food add files
files <- dir("Input/FoodAdds/", full.names = TRUE)

#fread and assign year based on file name
data = lapply(files, function(x) {
  res <- fread(x)
  res[, winter := tstrsplit(x, "/", keep = 3)] #grab year from file path
  res[, winter := gsub(".csv", "", winter)] #remove .csv from winter
  res
})

#rbindlist all food addds
foodadds <- rbindlist(data, fill = TRUE, use.names = TRUE)

#fix eartag and food column
setnames(foodadds, c("Eartag", "Food"), c("id", "food"))
foodadds[, food := as.factor(food)]
foodadds[, id := as.character(id)]

#save food adds to output
saveRDS(foodadds, "Output/Data/food_adds.rds")
