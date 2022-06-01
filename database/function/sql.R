qselectwhere = function(select, ...) {
  l = list(...)
  s = sprintf("select %s from %%s", select)
  if(length(l)!=0) {
    cname = names(l)
    cvalue = shQuote(as.character(l))
    wc = paste(cname, cvalue, sep = " = ", collapse = " and ")
    s = paste0(s, " where ", wc, ";")
  }
  return(s)
}

qdeletewhere = function(...) {
  l = list(...)
  s = sprintf("delete from %%s")
  if(length(l)!=0) {
    cname = names(l)
    cvalue = shQuote(as.character(l))
    wc = paste(cname, cvalue, sep = " = ", collapse = " and ")
    s = paste0(s, " where ", wc, ";")
  }
  return(s)
}

qfindHistorySince = function(symbol, from=NULL) {
  s = sprintf(qselectwhere("*", symbol=symbol), "Price")
  s = gsub(";", "", s)
  if (!is.null(from)) s = paste0(s, sprintf(" and date >= %s", shQuote(from)))
  q = paste0(s, ";")
  return(q)
}

qinsertdf = function(df, table=NULL) {
  if(is.null(table)) i = "insert into %%s (%s) values"
  else i = sprintf("insert into %s (%%s) values", table)
  s = sprintf(i, paste(colnames(df), collapse=", "))
  a = quotedf(df)
  a = paste(apply(a, 1, function(x) sprintf("(%s)", paste(x, collapse = ", "))),
            collapse = ", ")
  s = sprintf("%s %s;", s, a)
  s = gsub("\"NULL\"", "NULL", s)
  return(s)
}

qaddsincedate = function(q, since) {
  since = as.character(since)
  q = gsub(";", "", q)
  if(grepl("where", q)) q = sprintf("%s and date >= %s;", q, shQuote(since))
  else q = sprintf("%s where date >= %s;", q, shQuote(since))
  return(q)
}