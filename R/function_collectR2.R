
# Function to extract R2s from linear models
# to use, lapply to a list of models

collectR2 <- function(model) {
  #collect R2s
  rsqOut <- data.table(rsq(model))
  rsqOut<- round(rsqOut, 2)
  #return each datatable binded together by row
  return(data.table(rsqOut))
}
