#function runs model for density changes over time
#creates a prediction for density of every day
#only arguments are an x and y for the model. In this case x is some sort of time, y is density

predictdens <- function(yvar, xvar) {
  # Make the model
  model <- lm(yvar ~ xvar)
  #pull out slopes and intercepts
  slope <- coef(model)["xvar"]
  int <- coef(model)["(Intercept)"]
  #create data frame of dates
  output <- data.table(winterday = seq(1, 197, by = 1))
  #predict densities for each date
  output[, haredensity := (slope*winterday) + int]
  return(output$haredensity)
}
