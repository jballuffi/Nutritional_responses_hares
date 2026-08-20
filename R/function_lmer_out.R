
lmer_out <- function(model) {
  
  coef <- round(fixef(model), 2) #collect coefficients
  se <- round(se.fixef(model), 2) #collect standard errors
  
  #start table of effect names and coef +/- standard error
  modcoef <- data.table(
    Effect = names(fixef(model)),
    Slope = paste0(coef, " ± ", se)
    )
  
  modcoef <- modcoef[!grep("Inter", Effect)] #remove intercept
  modcoef[, `F-value` := round(anova(model)[4], 2)] #add F value
  modcoef[, `p-value` := round(Anova(model)[3], 3)] #add p values
  
  return(modcoef) #return full table
}

