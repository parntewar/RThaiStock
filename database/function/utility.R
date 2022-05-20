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
  price_history = getmem(mem, disk, "Price", q)
  history_xts = xts(
    price_history[,c("open", "high", "low", "close", "volume")],
    as.Date(price_history[["date"]]))
  return(history_xts)
}

getindicatorforsig = function(symbol, name) {
  qmaxdate = qselectwhere("date", symbol=symbol, name=name)
  maxdate = getmem(mem, disk, "Sig", qmaxdate)
  have = dim(maxdate)[1]!=0
  if(have) maxdate = max(as.Date(maxdate[["date"]]))
  q = qselectwhere("*", symbol=symbol, name=name)
  if(have) q = qaddsincedate(q, maxdate)
  indicator = getmem(mem, disk, "Indicator", q)
  return(indicator)
}

getxtsindicator = function(symbol, name) {
  x = getindicatorforsig(symbol, name)
  r = xts(x["value"], as.Date(x[["date"]]))
  return(r)
}

update_and_get_sigdf = function(xtssig, name, strategy, symbol) {
  df = data.frame("date"=index(xtssig), "value"=as.numeric(xtssig[, 1]))
  df["name"] = name
  df["strategy"] = strategy
  df["symbol"] = symbol
  q = qinsertdf(df, "Sig")
  setmem(mem, disk, "Sig", q)
  return(df)
}

gentraderle = function(x) {
  r = rle(x)
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
  return(ap_db)
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

file = "./database/db_file/listedCompanies_th_TH.xls"
readlistcomp = function(file="./database/db_file/listedCompanies_th_TH.xls") {
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
  url = "https://marketdata.set.or.th/mkt/sectorquotation.do?sector=SET100&language=th&country=TH"
  html = read_html(url)
  node = html_elements(html, "table")[[3]]
  table = html_table(node)
  set100_pool = table[[1]]
  return(set100_pool)
}