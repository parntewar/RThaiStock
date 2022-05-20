library(quantmod)

source("./database/database.R")

run = function(symbols) {
  inc = 1 / length(symbols)
  strategy_name = "emacross"
  r = update_price(symbols)
  if(r=="Ok") print("Update Done")
  else print("Update Failed")
  r = update_favor(symbols)
  if(r=="Ok") print("Get Favor Done")
  else print("Get Favor Failed")
  nema10 = update_indicator(symbols, EMA, n=10)
  nema21 = update_indicator(symbols, EMA, n=21)
  tryCatch({
    progress = Progress$new()
    progress$set(0, "updating Trade")
  }, error=function(e) "error")
  for(symbol in symbols) {
    ema10 = getxtsindicator(symbol, nema10)
    ema21 = getxtsindicator(symbol, nema21)
    sig = Lag(ifelse(ema10[, "value"] > ema21[, "value"], 1, 0))
    df = update_and_get_sigdf(sig, "Enter", strategy_name, symbol)
    print("post uags Ok")
    df = df[c("date", "symbol", "value", "strategy")]
    df["value"] = gentraderle(df[["value"]])
    q = qinsertdf(df, "Trade")
    setmem(mem, disk, "Trade", q)
    tryCatch(progress$inc(inc), error=function(e) "error")
  }
  tryCatch(progress$close(), error=function(e) "error")
  print("OK done")
}

