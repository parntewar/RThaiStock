source("./database/database.R")

update_indicator_test = function() {
  a = update_indicator("PTT", EMA, n=10)
  return(class(a)==class("a") & a=="EMA(n=10)")
}
