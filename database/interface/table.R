create_table = function(conndb) {
  if(!dbExistsTable(conndb, "Price")) dbCreateTable(
    conndb, "Price", c("symbol"="TEXT",
                       "date"="TEXT",
                       "open"="REAL",
                       "high"="REAL",
                       "low"="REAL",
                       "close"="REAL",
                       "volume"="REAL",
                       "adjusted"="REAL"))
  
  if(!dbExistsTable(conndb, "Statistic")) dbCreateTable(
    conndb, "Statistic", c("symbol"="TEXT",
                           "name"="TEXT",
                           "value"="TEXT")
  )
  
  if(!dbExistsTable(conndb, "Symbol")) dbCreateTable(
    conndb, "Symbol", c("symbol"="TEXT",
                        "Name"="TEXT",
                        "Market"="TEXT",
                        "Sector"="TEXT",
                        "Industry"="TEXT")
  )
  
  if(!dbExistsTable(conndb, "SymbolTrade")) dbCreateTable(
    conndb, "SymbolTrade", c("symbol"="TEXT")
  )
  
  if(!dbExistsTable(conndb, "OpenOrder")) dbCreateTable(
    conndb, "OpenOrder", c("date"="TEXT", 
                           "symbol"="TEXT",
                           "strategy"="TEXT",
                           "price"="REAL",
                           "unit"="REAL",
                           "thb"="REAL")
  )
  
  if(!dbExistsTable(conndb, "Favor")) dbCreateTable(
    conndb, "Favor", c("date"="TEXT", 
                       "quarter"="INTEGER",
                       "symbol"="TEXT", 
                       "name"="TEXT", 
                       "value"="REAL")
  )
  
  if(!dbExistsTable(conndb, "Indicator")) dbCreateTable(
    conndb, "Indicator", c("date"="TEXT",
                           "symbol"="TEXT",
                           "name"="TEXT",
                           "value"="REAL")
  )
  
  if(!dbExistsTable(conndb, "Sig")) dbCreateTable(
    conndb, "Sig", c("date"="TEXT",
                     "symbol"="TEXT",
                     "name"="TEXT",
                     "value"="TEXT",
                     "strategy"="TEXT")
  )
  
  if(!dbExistsTable(conndb, "Trade")) dbCreateTable(
    conndb, "Trade", c("date"="TEXT",
                       "symbol"="TEXT",
                       "value"="INTEGER",
                       "strategy"="TEXT")
  )
  
  if(!dbExistsTable(conndb, "StrategyMeta")) dbCreateTable(
    conndb, "StrategyMeta", c("pool"="INTEGER",
                              "symbol"="TEXT",
                              "strategy"="TEXT",
                              "name"="TEXT",
                              "value"="REAL")
  )
  
  if(!dbExistsTable(conndb, "HistoryOrder")) dbCreateTable(
    conndb, "HistoryOrder", c("symbol"="TEXT",
                              "date"="TEXT",
                              "open"="REAL",
                              "close"="REAL",
                              "strategy"="TEXT",
                              "pool"="INTEGER")
  )
}
