source("./database/function/sql.R")
source("./database/function/utility.R")

library(DBI)

senddb = function(conndb, q)
  dbSendStatement(conndb, q)

querydb = function(conndb, q)
  return(dbGetQuery(conndb, q))

set = function(db, table, q) {
  if(grepl("%s", q)) q = sprintf(q, table)
  senddb(db, q)
}

get = function(db, table, q) {
  if(grepl("%s", q)) q = sprintf(q, table)
  d = querydb(db, q)
  return(d)
}
