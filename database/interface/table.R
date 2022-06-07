create_table = function(conndb) {
  # Price open high low close
  if(!dbExistsTable(conndb, "Price")) dbCreateTable(
    conndb, "Price", c("symbol"="TEXT",
                       "date"="TEXT",
                       "open"="REAL",
                       "high"="REAL",
                       "low"="REAL",
                       "close"="REAL",
                       "volume"="REAL",
                       "adjusted"="REAL"))
  
  if(!dbExistsTable(conndb, "PriceSiam")) dbCreateTable(
    conndb, "PriceSiam", c("symbol"="TEXT",
                           "date"="TEXT",
                           "open"="REAL",
                           "high"="REAL",
                           "low"="REAL",
                           "close"="REAL",
                           "volume"="REAl")
  )
  # P/E ratio, D/E ratio, etc. ratio..
  if(!dbExistsTable(conndb, "Statistic")) dbCreateTable(
    conndb, "Statistic", c("symbol"="TEXT",
                           "name"="TEXT",
                           "value"="TEXT")
  )
  # All thai symbol
  if(!dbExistsTable(conndb, "Symbol")) dbCreateTable(
    conndb, "Symbol", c("symbol"="TEXT",
                        "Name"="TEXT",
                        "Market"="TEXT",
                        "Sector"="TEXT",
                        "Industry"="TEXT")
  )
  # symbol to trade from All symbol, etc SET100
  if(!dbExistsTable(conndb, "SymbolTrade")) dbCreateTable(
    conndb, "SymbolTrade", c("symbol"="TEXT")
  )
  # Buy and still Open order
  if(!dbExistsTable(conndb, "OpenOrder")) dbCreateTable(
    conndb, "OpenOrder", c("date"="TEXT", 
                           "symbol"="TEXT",
                           "strategy"="TEXT",
                           "price"="REAL",
                           "unit"="REAL",
                           "thb"="REAL")
  )
  # > sharpe ratio, standard deviation
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
                     "value"="REAL",
                     "strategy"="TEXT")
  )
  
  if(!dbExistsTable(conndb, "Trade")) dbCreateTable(
    conndb, "Trade", c("date"="TEXT",
                       "symbol"="TEXT",
                       "name"="TEXT",
                       "value"="REAL",
                       "strategy"="TEXT")
  )
  # Not used
  if(!dbExistsTable(conndb, "StrategyMeta")) dbCreateTable(
    conndb, "StrategyMeta", c("pool"="INTEGER",
                              "symbol"="TEXT",
                              "strategy"="TEXT",
                              "name"="TEXT",
                              "value"="REAL")
  )
  # All orders (Buy and Sell)
  if(!dbExistsTable(conndb, "HistoryOrder")) dbCreateTable(
    conndb, "HistoryOrder", c("symbol"="TEXT",
                              "date"="TEXT",
                              "type"="TEXT",
                              "units"="REAL",
                              "price"="REAL",
                              "value"="REAL")
  )
  # Not done yet
  if(!dbExistsTable(conndb, "UnitOrder")) dbCreateTable(
    conndb, "UnitOrder", c("date"="TEXT")
  )
  # Open Order simplify
  if(!dbExistsTable(conndb, "Portfolio")) dbCreateTable(
    conndb, "Portfolio", c("symbol"="TEXT",
                           "units"="REAL",
                           "value"="REAL")
  )
  # Summary Status
  if(!dbExistsTable(conndb, "Status")) dbCreateTable(
    conndb, "Status", c("date"="TEXT",
                        "money"="TEXT",
                        "open"="INTEGER",
                        "priceupdate"="TEXT",
                        "favorupdate"="TEXT")
  )
  
  if(!dbExistsTable(conndb, "Cash")) dbCreateTable(
    conndb, "Cash", c("date"="TEXT",
                      "name"="TEXT",
                      "value"="REAL")
  )
}
