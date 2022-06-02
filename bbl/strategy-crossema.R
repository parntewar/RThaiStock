library(quantmod)

source("./database/database.R")

run = function(symbols) {
  inc = 1 / length(symbols)
  strategy_name = "emacross"
  r = update_price(symbols)
  qs = "select symbol, count(date) from Price group by symbol"
  symbols = querydb(disk, qs)
  symbols = symbols[symbols$`count(date)` >= 30,]$symbol
  if(r!="Ok") print("Update Failed")
  r = update_favor(symbols)
  if(r!="Ok") print("Get Favor Failed")
  nema10 = update_indicator(symbols, EMA, n=10)
  nema21 = update_indicator(symbols, EMA, n=21)
  tryCatch({
    progress = Progress$new()
    progress$set(0, "updating Trade")
  }, error=function(e) "error")
  foreach(symbol = symbols) %do% {
    sigmaxdate = getsigmaxdate(symbol, strategy_name)[1,1]
    have = !is.na(sigmaxdate)
    if(!have) sigmaxdate = NULL
    ema10 = getxtsindicator(symbol, nema10, sigmaxdate)
    ema21 = getxtsindicator(symbol, nema21, sigmaxdate)
    sig = ifelse(ema10[, "value"] > ema21[, "value"], 1, 0)
    df = update_and_get_sigdf(sig, "Enter", strategy_name, symbol, sigmaxdate)
    update_trade(symbol, strategy_name)
    tryCatch(progress$inc(inc), error=function(e) "error")
  }
  tryCatch(progress$close(), error=function(e) "error")
  print("OK done")
}

update_trade = function(symbol, strategy) {
  qmaxdate = qselectwhere("max(date)", symbol=symbol, strategy=strategy)
  maxdate = get(disk, "Trade", qmaxdate)[1,1]
  have = !is.na(maxdate)
  buffer_trade = NULL
  if(have) {
    qdelete = qdeletewhere(date=maxdate, symbol=symbol, strategy=strategy)
    set(disk, "Trade", qdelete)
    maxdate2 = get(disk, "Trade", qmaxdate)[1,1]
    qbuffer = qselectwhere("*", symbol=symbol, strategy=strategy, date=maxdate2)
    buffer_trade = get(disk, "Trade", qbuffer)
  }
  qsig = qselectwhere("value, date", name="Enter", symbol=symbol, strategy=strategy)
  if(have) qsig = qaddsincedate(qsig, maxdate)
  sig = get(disk, "Sig", qsig)
  sig_value = sig[["value"]]
  sig_date = sig[["date"]]
  qclose = qselectwhere("close", symbol=symbol)
  if(have) qclose = qaddsincedate(qclose, maxdate)
  close = get(disk, "Price", qclose)[[1]]
  # close=close, rle=rle, type=sig_value, start=1st value
  rle_value = gen_rle(sig_value, buffer_trade)
  start_value = gen_start(close, sig_value, buffer_trade)
  df = data.frame(date=sig_date, symbol=symbol, strategy=strategy, close=close, 
                  rle=rle_value, type=sig_value, start=start_value)
  df = pivot_longer(df, c("close", "rle", "type", "start"))
  df = as.data.frame(df)
  q = qinsertdf(df, "Trade")
  set(disk, "Trade", q)
  return(df)
}

trade_report = function(date) {
  qtrade = sprintf("select * from Trade where date = %s",
                   shQuote(date))
  trade = querydb(disk, qtrade)
  trade = pivot_wider(trade)
  trade = as.data.frame(trade)
  qfavor = sprintf("select * from Favor")
  favor = querydb(disk, qfavor)
  favor = favor[favor$name=="sharpe",][c("symbol", "value")]
  colnames(favor) = c("symbol", "favor")
  report = merge(trade, favor, by="symbol")
  return(report)
}

report_buy = function(date, value) {
  if(value <= 0) value = Inf
  report = trade_report(date)
  history = querydb(disk, "select symbol from HistoryOrder")[[1]]
  report = report[!(report$symbol %in% history), ]
  report = report[report$type==1, ]
  filter = ifelse(report$rle==1 | report$close < report$start, TRUE, FALSE)
  report = report[filter, ]
  report = report[order(report$favor, decreasing = TRUE), ]
  report$value = report$close * 100
  report$comm = report$value * 0.0018
  report$sum_value = report$value + report$comm
  report = report[report$value < value, ]
  cash = 100
  return(report)
}

report_sell = function(date) {
  report = trade_report(date)
  history = querydb(disk, "select symbol from HistoryOrder")[[1]]
  report = report[report$symbol %in% history, ]
  report = report[order(report$rle, decreasing = TRUE), ]
  return(report)
}