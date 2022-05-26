library(DBI)

source("./database/interface/table.R")

Sys.setlocale("LC_CTYPE", "Thai")

disk = dbConnect(RSQLite::SQLite(), "./database/db_file/stock-database.db")

create_table(disk)
