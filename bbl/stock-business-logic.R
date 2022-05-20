source("stock-data-center.R")

symbols = dbReadTable(conndb, "symbolpool")[["symbol"]]
r = update_history(symbols)
update_indi_strategy(NULL, symbols)

strategy_add_logic("ema10-ema21.cross", buy="EMA(x, 10) > EMA(x, 21)")
