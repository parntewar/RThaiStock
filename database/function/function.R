source("./database/function/sql.R")
source("./database/function/utility.R")

library(DBI)

senddb = function(conndb, q)
  dbSendStatement(conndb, q)

querydb = function(conndb, q)
  return(dbGetQuery(conndb, q))

setdisk = function(disk, table, q) {
  if(grepl("%s", q)) q = sprintf(q, table)
  senddb(disk, q)
}

setmem = function(mem, disk, table, q) {
  if(grepl("%s", q)) q = sprintf(q, table)
  senddb(mem, q)
  setdisk(disk, table, q)
}

getdisk = function(disk, table, q) {
  if(grepl("%s", q)) q = sprintf(q, table)
  d = querydb(disk, q)
  return(d)
}

getmem = function(mem, disk, table, q) {
  if(grepl("%s", q)) q = sprintf(q, table)
  d = querydb(mem, q)
  have = dim(d)[1]!=0
  if(have) return(d)
  d = getdisk(disk, mem, q)
  have = dim(d)[1]!=0
  if(!have) return(d)
  dq = qinsertdf(d, table)
  senddb(mem, dq)
  return(d)
}
