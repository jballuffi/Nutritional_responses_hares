
#function that makes AIC table with R2s from linear models (lm made models)
#modlist = supply a list of models
#modnames = supply a list of names that matches the models


make_aic_lm <- function(modlist, modnames){
  #make AIC table
  aic <- as.data.table(aictab(cand.set = modlist, sort = TRUE, modnames = modnames))
  
  #remove unwanted columns
  aic[, ModelLik := NULL]
  aic[, Cum.Wt := NULL]
  
  #round whole table to 3 dec places
  aic <- aic %>% mutate_if(is.numeric, round, digits = 3)
  
  #run function and get R2s for all models
  R2s <- lapply(modlist, collectR2)
  R2s <- rbindlist(R2s, fill = TRUE)
  R2s$Modnames <- modnames
  
  #merge R2s with AIC table
  aic <- merge(aic, R2s, by = "Modnames")
  setorder(aic, "Delta_AICc")
  
  setnames(aic, "V1", "R2")
  
  return(aic)
}
