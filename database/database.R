source("./database/function/function.R")
source("./database/interface/interface.R")

library(quantmod)
library(rvest)
library(lubridate)
library(PerformanceAnalytics)
library(shiny)

update_statistic = function(symbols) {
  error_report = ""
  for(symbol in symbols) {
    response = tryCatch(symbol_stats(symbol), error=function(e) "error")
    Sys.sleep(10)
    if(class(response)[1]==class("error")) {
      error_report = paste(error_report, symbol)
      next}
    q = sprintf("select 1 from Statistics where symbol = %s;",
                shQuote(symbol))
    Ex = querydb(q)
    if(dim(Ex)[1]==0) appenddb("Statistics", response)
    else {
      v = sprintf("when name = %s then %s", 
                  shQuote(response[["name"]]), 
                  shQuote(response[["value"]]))
      v = paste(v, collapse = " ")
      q = sprintf(
        "update Statistics set value = (case %s end) where symbol = %s;",
        v, shQuote(symbol))
      t = tryCatch(senddb(q), error=function(e) "error")
      if(class(t)[1]==class("error")) {
        if(t=="error") {
          error_report = paste(error_report, symbol)
          next
        }
      }
    }
    if(error_report=="") return("Ok")
    error_report = strsplit(error_report, " ")
    return(error_report[-1])
  }
}

update_price = function(symbols) {
  inc = 1 / length(symbols)
  tryCatch({
    progress = Progress$new()
    progress$set(0, "updating Price")
  }, error=function(e) "error")
  error_report = ""
  for(symbol in symbols) {
    q = qselectwhere("*", symbol=symbol)
    price = getmem(mem, disk, "Price", q)
    have = dim(price)[1]!=0
    if(have) from = max(as.Date(price$date))
    else from = as.Date("2010-01-01")
    to = Sys.Date() + days()
    A = downloadPrice(symbol, from, to)
    A = A[complete.cases(A), ]
    Sys.sleep(10)
    t = tryCatch({
      if(have) {
        q = qdeletewhere(date=as.character(from), symbol=symbol)
        setmem(mem, disk, "Price", q)
      }
      q = qinsertdf(A, "Price")
      setmem(mem, disk, "Price", q)
    }, error=function(e) return("error"))
    tryCatch(progress$inc(inc), error=function(e) "error")
    if(class(t)==class("error")) {
      if(t=="error") {
        error_report = paste(error_report, symbol)
        next
      }
    }
  }
  tryCatch(progress$close(), error=function(e) "error")
  if(error_report=="") return("Ok")
  error_report = strsplit(error_report, " ")[[1]]
  return(error_report[-1])
}

update_favor = function(symbol) {
  for(sym in symbol) {
    q = qselectwhere("*", symbol=sym)
    favor = getmem(mem, disk, "Favor", q)
    have = dim(favor)[1]!=0
    if(have) {
      q = qselectwhere("date", symbol=sym)
      price_date = getmem(mem, disk, "Price", q)
      price_max_date = as.character(max(as.Date(price_date[["date"]])))
      if(favor[["date"]] == price_max_date) next
      q = qdeletewhere(symbol=sym)
      setmem(mem, disk, "Favor", q)
    }
    sym_xts = getpricexts(sym)
    to = floor_date(Sys.Date(), "quarter")
    from = as.character(to - years(1))
    to = as.character(as.Date(to) - days(1))
    sym_xts = sym_xts[paste0(from, "/", to)]
    ap_db = genfavorTable(Cl(sym_xts), sym, to)
    q = qinsertdf(ap_db)
    setmem(mem, disk, "Favor", q)
  }
  return("Ok")
}

update_indicator = function(symbols, indi, ...) {
  d = list(...)
  indi_name = deparse(substitute(indi))
  indi_args = paste0(names(d), "=", unname(unlist(d)))
  indi_args = paste(indi_args, collapse = ", ")
  indi_fullname = paste0(indi_name, "(", indi_args, ")")
  for (sym in symbols) {
    q = qselectwhere("*", symbol=sym, name=indi_fullname)
    indicator = getmem(mem, disk, "Indicator", q)
    have = dim(indicator)[1] != 0
    from_date = NULL
    if (have) {
      q = sprintf(
        "select min(a.date) from (select date from Indicator where symbol = %s and name = %s order by date desc limit %i) as a", 
        shQuote(sym), shQuote(indi_fullname), d$n)
      from_date = querydb(disk, q)
      q = sprintf(
        "select max(date) from Indicator where symbol = %s and name = %s",
        shQuote(sym), shQuote(indi_fullname)
      )
      last_date = querydb(disk, q)[[1,1]]
      q = qdeletewhere(symbol=sym, name=indi_fullname, date=last_date)
      setmem(mem, disk, "Indicator", q)
    }
    sym_xts = getpricexts(sym, from_date)
    sym_xts = Cl(sym_xts)
    sym_xts = sym_xts[!is.na(sym_xts)]
    indi_xts = indi(sym_xts, ...)
    indi_df = data.frame(indi_xts)
    indi_df$date = as.character(index(indi_xts))
    indi_df$symbol = sym
    indi_df$name = indi_fullname
    colnames(indi_df) = c("value", "date", "symbol", "name")
    row.names(indi_df) = NULL
    if (have) indi_df = indi_df[which(!is.na(indi_df$value)),]
    q = qinsertdf(indi_df, "Indicator")
    setmem(mem, disk, "Indicator", q)
  }
  return(indi_fullname)
}

update_indicators = function(symbols) {
  indicators = dbGetQuery(
    conndb,
    "select distinct indicator from IndicatorList"
  )
  for (indi in indicators) {
    indi_split = strsplit(indi, "\\(")[[1]]
    indi_name = indi_split[1]
    indi_arges = gsub(")", "", indi_split[2])
    ex = str2expression(sprintf("update_indicator(symbols, %s, %s)", indi_name, indi_arges))
    a = eval(ex)
  }
}

add_indicator = function(indi, ...) {
  name = indicator_name(indi, ...)
  if(dim(querydb(sprintf("select * from IndicatorList where indicator = %s;", name)))[1]==0) appenddb("IndicatorList", data.frame("indicator"=name))
}

update_sig = function(symbol, strategy) {
  for (sym in symbol) {
    
  }
}
