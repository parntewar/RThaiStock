library(shiny)
library(DT)
library(xlsx)

source("./database/database.R")
source("./bbl/strategy-crossema.R")

tradesymbols = function() {
  symbol = downloadset100()
  #symbol = querydb(disk, "select symbol from SymbolTrade")[[1]]
  return(symbol)
}

bank_cash = function(cash, deposit) {
  if(cash==0) {
    print("0")
    return()}
  if(deposit=="deposit") cash = abs(cash)
  else {
    cash = abs(cash) * -1
    balance_now = querydb(disk, "select sum(value) from cash")[1,1] + cash
    if(is.na(balance_now) | balance_now < 0) {
      print("Can't be negative")
      return()
      }
    }
  qcash = sprintf("insert into Cash (date, name, value) values (%s, %s, %s)", 
                  shQuote(Sys.Date()), shQuote(deposit), cash)
  senddb(disk, qcash)
}

cash_balance = function() {
  q = "select sum(value) from cash"
  value = querydb(disk, q)[1,1]
  if(is.na(value)) value = NULL
  return(value)
}

order_update = function(date=NULL) {
  table = read.xlsx("./Upload.xls", 1)
  if(dim(table)[1]==0) return()
  if(is.null(date)) date=Sys.Date()
  table = table[, c(1, 2, 4, 8)]
  colnames(table) = c("type", "symbol", "units", "value")
  table$type = ifelse(table$type=="B", "buy", "sell")
  table$date = date
  table$price = abs(table$value / table$units)
  table$value = abs(table$value) * ifelse(table$type=="buy", -1, 1)
  #HistoryOrder
  qdf_history = qinsertdf(table, "HistoryOrder")
  set(disk, "HistoryOrder", qdf_history)
  #Portfolio
  portfolio = data.frame(symbol=table$symbol, units=100, value=table$value)
  port_buy = portfolio[table$type=="buy", ]
  port_sell = portfolio[table$type=="sell", ]
  if(dim(port_buy)[1]!=0) {
    qdf_port_buy = qinsertdf(port_buy, "Portfolio")
    set(disk, "Portfolio", qdf_port_buy)
  }
  if(dim(port_sell)[1]!=0) {
    sym_sell = paste0(shQuote(port_sell$symbol), collapse = ", ")
    qdelete_port_sell = sprintf("delete from Portfolio where symbol in (%)", sym_sell)
    senddb(disk, qdelete_port_sell)
  }
  #Cash
  Cash = data.frame(date=date, name=table$symbol, value=table$value)
  qdf_cash = qinsertdf(Cash, "Cash")
  set(disk, "Cash", qdf_cash)
}

ui = fluidPage(
  actionButton("update", "Update Data"),
  br(),
  navbarPage("Let's Go!!", 
             tabPanel("Trade Report",
                      fluidRow(column(2,
                                      selectInput("dateinput", "Date", date_report()),
                                      numericInput("valueinput", "Value", 5000),
                                      numericInput("favorinput", "favor", 0.5),
                                      actionButton("multiorder_xls", "Download Orders"),
                                      br(),
                                      dateInput("dateinput_order", "For orders update"),
                                      actionButton("orderupdate", "Orders Update")),
                               column(10,
                                      h3("To Sell"),
                                      tableOutput("reportsell"),
                                      br(),
                                      h3("To Buy"),
                                      tableOutput("reportbuy"))
                               )
                      ),
             tabPanel("History Data",
                      fluidRow(column(2,
                                      selectInput("symbolinput", "Symbol", tradesymbols()),
                                      ),
                               column(10,
                                      plotOutput("chartseries"),
                                      column(5, tableOutput("pricetable")),
                                      column(1, ""),
                                      column(4, tableOutput("tradetable"))
                                      )
                               )
                      ),
             tabPanel("Cash Balance",
                      fluidRow(column(2, 
                                      radioButtons("depositinput", "", 
                                                   c("Deposit"="deposit",
                                                     "Withdraw"="withdraw")), 
                                      numericInput("cashinput", "Balance", 0, min=0),
                                      actionButton("cash_confirm", "Confirm")
                                      ),
                               column(10, tableOutput("cashbalance"))
                               )
                      ),
             tabPanel("Portfolio",
                      fluidRow(column(2),
                               column(10, tableOutput("portfolio"))))
             )
  )

server = function(input, output) {
  #output$date_report = date_report()
  observeEvent(input$update, run(tradesymbols()))
  output$chartseries = renderPlot({
    chartSeries(tail(getpricexts(input$symbolinput), 100), name=input$symbolinput)
  })
  output$pricetable = renderTable({
    table = tail(getpricexts(input$symbolinput), 10)
    date = as.Date(index(table))
    table = data.frame(table)
    table$date = as.character(date)
    table = table[c("date", "open", "high", "low", "close", "volume")]
  })
  # From Trade database
  output$tradetable = renderTable({
    q = qselectwhere("*", symbol=input$symbolinput)
    table = tail(get(disk, "Trade", q), 10)
  })
  output$reportbuy = renderTable({
    report_buy(input$dateinput, input$valueinput, cash_balance(), input$favorinput)
  })
  output$cashbalance = renderTable(tail(dbReadTable(disk, "Cash"), 10))
  observeEvent(input$cash_confirm, {
    bank_cash(input$cashinput, input$depositinput)
    output$cashbalance = renderTable(tail(dbReadTable(disk, "Cash"), 10))
    output$reportbuy = renderTable({
      report_buy(input$dateinput, input$valueinput, cash_balance(), input$favorinput)
    })
    })
  output$reportsell = renderTable(report_sell(input$dateinput))
  observeEvent(input$multiorder_xls, {
    buy = report_buy(input$dateinput, input$valueinput, cash_balance(), input$favorinput)
    sell = report_sell(input$dateinput)
    df = data.frame(Side=character(), `Stock Code`=character(),	NVDR=integer(),
                    Quantity=integer(),	Price=character(),
                    Validity=character(),	`Iceberg Vol`=integer())
    cnames = c("Side", "Stock Code", "NVDR", "Quantity", "Price",
                     "Validity", "Iceberg Vol")
    colnames(df) = cnames
    if(dim(buy)[1]!=0) {
      df_buy = data.frame(buy$symbol)
      df_buy$Side = "B"
      df_buy = df_buy[, c(2, 1)]
      df_buy$NVDR = 0
      df_buy$Quantity = 100
      df_buy$Price = "ATO"
      df_buy$Validity = NA
      df_buy$`Iceberg Vol` = NA
      colnames(df_buy) = cnames
      df = rbind(df, df_buy)
    }
    if(dim(sell)[1]!=0) {
      df_sell = data.frame(sell$symbol)
      df_sell$Side = "S"
      df_sell = df_sell[, c(2, 1)]
      df_sell$NVDR = 0
      df_sell$Quantity = 100
      df_sell$Price = "ATO"
      df_sell$Validity = NA
      df_sell$`Iceberg Vol` = NA
      colnames(df_sell) = cnames
      df = rbind(df, df_sell)
    }
    write.xlsx(df, "./Upload.xls", row.names = FALSE, showNA = FALSE)
  })
  observeEvent(input$orderupdate, order_update(input$dateinput_order))
  output$portfolio = renderTable({
    report = trade_report(date_report()[1])
    history = querydb(disk, "select symbol from Portfolio")[[1]]
    report = report[report$symbol %in% history, ]
    report
  })
}
        