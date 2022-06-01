source("./database/function/utility.R")

readlistcomp_test = function() {
  df = readlistcomp()
  column = all(colnames(df) %in% c("symbol", "Name", "Market", "Sector", "Industry"))
  have = dim(df)[1] >= 1
  return(all(column, have))
}
