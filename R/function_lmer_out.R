
lmer_out <- function(model) {
  
  #collect coef values
  coef <- round(fixef(model), 2)
  se <- round(se.fixef(model), 2)

  #start table  
  modcoef <- data.table(
    Effect = names(fixef(Q1)),
    Coef = paste0(coef, " ± ", se)
    )
  
  #remove intercept
  modcoef <- modcoef[!grep("Inter", Effect)]
  
  #add in F value
  modcoef[, `F-value` := anova(Q1)[4]]
  
  #add in P value
  modcoef[, `p-value` := ]
  
  return(modcoef)
}

Q1_sum <- lmer_out(Q1)


car::Anova(Q1)[3]
