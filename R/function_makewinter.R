
#function to classify dates into winters (e.g., winter2015-2016)


#dt is the data.table you want the winter column to be created in
#datecol is the column (dt$colname) that holds the date, must be an IDate 

make_winter <- function(dt, datecol, monthcol){
  #make year column
  dt[, y := year(datecol)]
  #if spring, take year before
  dt[monthcol < 5, winter := y - 1]
  #if fall, take current year
  dt[monthcol > 8, winter := y]
  #paste in the following year
  dt[, winter := paste0(winter, "-", winter + 1)]
  return(dt)
}

