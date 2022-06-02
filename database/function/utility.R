library(DBI)
library(tidyr)
library(rvest)
library(quantmod)

quotedf = function(df) {
  df[is.na(df)] = "NULL"
  colclass = sapply(df, function(x) class(x))
  colselector = colclass %in% c(class(as.Date("1997-01-01")), 
                                class("a"), 
                                class(FALSE))
  df[colselector] = apply(df[colselector], 2, shQuote)
  return(df)
}

getpricexts = function(sym, from=NULL) {
  q = qfindHistorySince(sym, from=from)
  price_history = get(disk, "Price", q)
  history_xts = xts(
    price_history[,c("open", "high", "low", "close", "volume")],
    as.Date(price_history[["date"]]))
  return(history_xts)
}

getindicatorforsig = function(symbol, name, date) {
  q = qselectwhere("*", symbol=symbol, name=name)
  if(!is.null(date)) q = qaddsincedate(q, date)
  indicator = get(disk, "Indicator", q)
  return(indicator)
}

getxtsindicator = function(symbol, name, date=NULL) {
  x = getindicatorforsig(symbol, name, date)
  r = xts(x["value"], as.Date(x[["date"]]))
  return(r)
}

update_and_get_sigdf = function(xtssig, name, strategy, symbol, deldate) {
  if (!is.null(deldate)) {
    qdelete = qdeletewhere(symbol=symbol, strategy=strategy, date=deldate)
    set(disk, "Sig", qdelete)
  }
  df = data.frame("date"=index(xtssig), "value"=as.numeric(xtssig[, 1]))
  df["name"] = name
  df["strategy"] = strategy
  df["symbol"] = symbol
  q = qinsertdf(df, "Sig")
  set(disk, "Sig", q)
  return(df)
}

getsigmaxdate = function(symbol, strategy) {
  qmaxdate = qselectwhere("max(date)", symbol=symbol, strategy=strategy)
  maxdate = get(disk, "Sig", qmaxdate)
  return(maxdate)
}

gentraderle = function(x) {
  x_value = x[!is.na(x)]
  x_na = x[is.na(x)]
  r = rle(x_value)
  r = do.call(c, sapply(r[["lengths"]], function(x) seq(1, x)))
  return(r)
}

loadsymbol_stats = function(symbol) {
  s1 = "https://finance.yahoo.com/quote/"
  s2 = "/key-statistics?p="
  sym = paste0(sub(" ", "-", symbol), ".BK")
  site = paste0(s1, sym, s2, sym)
  html = read_html(site)
  node = html_nodes(html, "table")
  table = html_table(node)
  table = do.call(rbind, table)
  names(table) = c("name", "value")
  table["symbol"] = symbol
  return(table)
}

genfavorTable = function(price, sym, to) {
  roc = ROC(price, type="discrete")
  ssharpe = SharpeRatio.annualized(roc)
  sreturn = Return.annualized(roc)
  ssd = sd.annualized(roc)
  smaxdd = maxDrawdown(roc)
  ap_db = data.frame(as.character(max(index(price))),
                     quarters(as.Date(to)),
                     sym, 
                     ssharpe, 
                     sreturn, 
                     ssd, 
                     smaxdd)
  colnames(ap_db) = c("date", 
                      "quarter", 
                      "symbol" ,
                      "sharpe", "return", "deviation", "maxDrawdown")
  rownames(ap_db) = NULL
  ap_db = pivot_longer(ap_db, colnames(ap_db)[4:length(colnames(ap_db))])
  ap_db = as.data.frame(ap_db)
  return(ap_db)
}

gen_rle = function(type, buffer_trade=NULL) {
  r = rep(NA, length(type))
  if(!is.null(buffer_trade)) {
    r = c(buffer_trade$value[buffer_trade$name=="rle"], r)
    type = c(buffer_trade$value[buffer_trade$name=="type"], type)
  }
  else r[1] = ifelse(is.na(type[1]), NA, 1)
  for(i in seq_along(r)) {
    if(i==1) next
    if(is.na(type[i-1]) & is.na(type[i])) {
      r[i] = NA
      next
    }
    if(is.na(type[i-1]) & !is.na(type[i])) {
      r[i] = 1
      next
    }
    r[i] = ifelse(type[i-1]==type[i], r[i-1]+1, 1)
  }
  if(!is.null(buffer_trade)) r = r[-1]
  return(r)
}

gen_start = function(close, sig, buffer_trade=NULL) {
  start = rep(NA, length(sig))
  if(!is.null(buffer_trade)) {
    start0 = buffer_trade$value[buffer_trade$name=="start"]
    sig0 = buffer_trade$value[buffer_trade$name=="type"]
    close0 = buffer_trade$value[buffer_trade$name=="close"]
    start = c(start0, start)
    sig = c(sig0, sig)
    close = c(close0, close)
  }
  else start[1] = ifelse(is.na(sig[1]), NA, close[1])
  for(i in seq_along(start)) {
    if(i==1) next
    if(is.na(sig[i-1]) & is.na(sig[i])) {
      start[i] = NA
      next
    }
    if(is.na(sig[i-1]) & !is.na(sig[i])) {
      start[i] = close[i]
      next
    }
    start[i] = ifelse(sig[i-1]==sig[i], start[i-1], close[i])
  }
  if(!is.null(buffer_trade)) start = start[-1]
  return(start)
}

downloadPrice = function(symbol, from, to) {
  symbolpaste = paste0(sub(" ", "-",symbol), ".BK")
  sym_price = getSymbols(
    symbolpaste, from=from, to=to, auto.assign=FALSE)
  names(sym_price) = c(
    "open", "high", "low", "close", "volume", "adjusted")
  A = data.frame(sym_price)
  A$date = as.character(index(sym_price))
  A$symbol = symbol
  row.names(A) = NULL
  A = A[!duplicated(A$date), ]
  return(A)
}

indicator_name = function(indi, ...) {
  d = list(...)
  indi_name = deparse(substitute(indi))
  indi_args = paste0(names(d), "=", unname(unlist(d)))
  indi_args = paste(indi_args, collapse = ", ")
  indi_fullname = paste0(indi_name, "(", indi_args, ")")
  return(indi_fullname)
}


readlistcomp = function(file="https://classic.set.or.th/dat/eod/listedcompany/static/listedCompanies_th_TH.xls") {
  url = file
  html = read_html(url, encoding="TIS-620")
  table = as.data.frame(html_table(html)[[1]])
  colnames(table) = table[2, ]
  table = table[-c(1,2), ]
  table = table[c(1, 2, 3, 5, 4)]
  colnames(table) = c("symbol", "Name", "Market", "Sector", "Industry")
  return(table)
}

downloadset100 = function() {
  url = "https://classic.set.or.th/mkt/sectorquotation.do?sector=SET100&language=th&country=TH"
  html = read_html(url)
  node = html_elements(html, "table")[[3]]
  table = html_table(node)
  set100_pool = table[[1]]
  return(set100_pool)
}

download_siam = function() {
  url = "http://siamchart.com/"
  s = session(url)
  form = html_form(s)
  form = html_form_set(form, u="PANTEWA", p="q74qfrVHE@7#Xman")
  session_submit(s, form = form)
  session_jump_to(x, "http://siamchart.com/download.php?type=last")
  html_text(s)
}
