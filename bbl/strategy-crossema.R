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

vector_run = function(symbols) {
  senddb(disk, "begin transaction")
  split1000_index = function(length) {
    start = seq(from=1, to=length, by=1000)
    stop = c(start[-1] - 1, length)
    list_seq = list(start=start, stop=stop)
    return(list_seq)
  strategy_name = "emacross"
  #result = update_price(symbols)
  # if(result!="Ok") print("Update Failed")
  q = "select symbol, count(date) from Price group by symbol"
  symbols = querydb(disk, q)
  q = "select max(date) as date from Price"
  max_date = querydb(disk, q)[1,1]
  symbols = symbols[symbols$`count(date)` >= 30 & symbols$date == max_date,]$symbol
  q = "select symbol, max(date) from Favor where symbol in (%s) group by symbol"
  q = sprintf(q, paste0(shQuote(symbols), collapse = ", "))
  today_quarter = floor_date(Sys.Date(), "quarter") - days(1)
  favor_quarter = querydb(disk, q)
  favor_quarter$date = floor_date(favor_quarter$`max(date)`, "quarter") - days(1)
  favor_symbols = favor_quarter[favor_quarter$date != today_quarter, "symbol"]
  if(length(favor_symbols)!=0) {
    close_xts = lapply(favor_symbols, getpricexts, 
                       as.character(today_quarter - years(1)))
    close_xts = lapply(close_xts, Cl)
    favor_list = mapply(genfavorTable, close_xts, favor_symbols, today_quarter, 
                        SIMPLIFY = FALSE, USE.NAMES = FALSE)
    favor_combind = do.call(rbind, favor_list)
    }
    split1000 = split1000_index(dim(favor_combind)[1])
    for(i in length(split1000$start)) {
      df = favor_combind[split1000$start[i]:split1000$stop[i], ]
      q = qinsertdf(df, "Favor")
      senddb(disk, q)
    }
  }
  vector_update_indicator = function(symbols, indicator, ...) {
    indicator_parameter = list(...)
    indicator_name = deparse(substitute(indicator))
    indicator_args = paste0(names(indicator_parameter), "=", 
                            unname(unlist(indicator_parameter)))
    indicator_args = paste(indicator_args, collapse = ", ")
    indicator_fullname = paste0(indicator_name, "(", indicator_args, ")")
    q = "delete from Indicator where symbol in (%s) and name = %s"
    q = sprintf(q, paste0(shQuote(symbols), collapse = ", "), 
                shQuote(indicator_fullname))
    senddb(disk, q)
    q = "select * from Price where symbol in (%s)"
    q = sprintf(q, paste0(shQuote(symbols), collapse = ", "))
    Price = querydb(disk, q)
    Price_split = split(Price, Price$symbol)
    Price_name = names(Price_split)
    Price_xts_close = lapply(Price_split, function(x) {
      xts = xts(x$close, as.Date(x$date))
      colnames(xts) = "close"
      return(xts)
    })
    Price_indicator = lapply(Price_xts_close, indicator, indicator_parameter$n)
    Price_dataframe = mapply(function(x, n, s) {
      dataframe = data.frame(date=index(x), value=coredata(x))
      colnames(dataframe) = c("date", "value")
      dataframe$name = n
      dataframe$symbol = s
      return(dataframe)
    }, Price_indicator, indicator_fullname, Price_name, SIMPLIFY = FALSE, 
    USE.NAMES = FALSE)
    Price_combind = do.call(rbind, Price_dataframe)
    split1000 = split1000_index(dim(Price_combind)[1])
    for(i in length(split1000$start)) {
      df = Price_combind[split1000$start[i]:split1000$stop[i], ]
      q = qinsertdf(df, "Indicator")
      senddb(disk, q)
    }
    return(indicator_fullname)
  }
  ema10 = vector_update_indicator(symbols, EMA, n=10)
  ema21 = vector_update_indicator(symbols, EMA, n=21)
  q = "select max(date) as date from Sig"
  sig_max_date = querydb(disk, q)[1,1]
  q = "delete from Sig where symbol in (%s) and date = (%s) and strategy = (%s)"
  q = sprintf(q, paste0(shQuote(symbols), collapse = ", "), shQuote(sig_max_date), 
              shQuote(strategy_name))
  senddb(disk, q)
  q = "select * from Indicator where symbol in (%s) and name in (%s) and date >= %s"
  q = sprintf(q, 
              paste0(shQuote(symbols), collapse = ", "), 
              paste0(shQuote(c(ema10, ema21)), collapse = ", "),
              shQuote(sig_max_date))
  ema = querydb(disk, q)
  ema_split = split(ema, ema$symbol)
  ema_split = lapply(ema_split, function(x) split(x, x$name))
  sig_list = lapply(ema_split, 
                    function(x) ifelse(x[[ema10]]$value > x[[ema21]]$value, 1, 0))
  sig_dataframe = mapply(function(date, symbol, value) {
    dataframe = data.frame(date=date, symbol=symbol, value=value)
    dataframe$name = "Enter"
    dataframe$strategy = strategy_name
    return(dataframe)
  },
  lapply(ema_split, function(x) x[[1]]$date), names(sig_list), sig_list,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)
  sig_combind = do.call(rbind, sig_dataframe)
  split1000 = split1000_index(dim(sig_combind)[1])
  for(i in length(split1000$start)) {
    df = sig_combind[split1000$start[i]:split1000$stop[i], ]
    q = qinsertdf(df, "Sig")
    senddb(disk, q)
  }
  q = "select max(date) as date from Trade"
  trade_max_date = querydb(disk, q)[1,1]
  q = paste0("select distinct date from Trade ",
             "order by date desc limit 2")
  trade_buffer_date = querydb(disk, q)[2,1]
  q = sprintf("delete from Trade where symbol in (%s) and date = %s", 
              paste0(shQuote(symbols), collapse = ", "),
              shQuote(trade_max_date))
  senddb(disk, q)
  q = sprintf("select * from Trade where symbol in (%s) and date = %s", 
              paste0(shQuote(symbols), collapse = ", "),
              shQuote(trade_buffer_date))
  trade_buffer = querydb(disk, q)
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

report_buy = function(date, value=NULL, cash=NULL, favor=NULL, history=NULL) {
  if(value <= 0) value = Inf
  if(is.null(value)) value = Inf
  report = trade_report(date)
  if(is.null(history)) history = querydb(disk, "select symbol from Portfolio")[[1]]
  else history = history$symbol
  report = report[!(report$symbol %in% history), ]
  report = report[report$type==1, ]
  filter = ifelse(report$rle==1 | report$close < report$start, TRUE, FALSE)
  report = report[filter, ]
  if(is.numeric(favor)) report = report[report$favor >= favor, ]
  report = report[order(report$favor, decreasing = TRUE), ]
  report$value = report$close * 100
  report$comm = report$value * 0.0018
  report$sum_value = report$value + report$comm
  report = report[report$value <= value, ]
  if(!is.null(cash)) report = report[cumsum(report$sum_value) < cash, ]
  return(report)
}

report_sell = function(date, history=NULL) {
  report = trade_report(date)
  if(is.null(history)) history = querydb(disk, "select symbol from Portfolio")[[1]]
  else history = history$symbol
  report = report[report$symbol %in% history, ]
  report = report[report$type==0, ]
  report = report[order(report$rle, decreasing = TRUE), ]
  return(report)
}
