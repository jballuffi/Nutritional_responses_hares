
#function to collect R2 from linear mixed models
# to use, lapply to a list of models

collectR2_mixed <- function(model) {
  #collect R2s
  rsqOut <- data.table(r.squaredGLMM(model))
  rsqOut<- round(rsqOut, 2)
  #return each datatable binded together by row
  return(data.table(rsqOut))
}
