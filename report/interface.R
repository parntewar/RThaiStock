library(shiny)
library(DT)

source("./database/database.R")
source("./bbl/strategy-crossema.R")

tradesymbols = function() {
  symbol = downloadset100()
  #symbol = querydb(disk, "select symbol from SymbolTrade")[[1]]
  return(symbol)
}

date_report = function() {
  qdate = sprintf("select distinct date from Trade")
  date = querydb(disk, qdate)
  date = tail(date, 10)
  date = sort(date[[1]], decreasing = TRUE)
  return(date)
}

update = function(symbols) {
  run(symbols)
}

ui = fluidPage(
  actionButton("update", "Update Data"),
  selectInput("symbolinput", "Symbol", tradesymbols()),
  plotOutput("chartseries"),
  tableOutput("pricetable"),
  tableOutput("tradetable"),
  selectInput("dateinput", "Date", date_report()),
  numericInput("valueinput", "Value", 0),
  tableOutput("reportbuy")
)

server = function(input, output) {
  observeEvent(input$update, update(tradesymbols()))
  output$chartseries = renderPlot({
    chartSeries(tail(getpricexts(input$symbolinput), 100))
  })
  output$pricetable = renderTable({
    table = tail(getpricexts(input$symbolinput), 10)
    date = as.Date(index(table))
    table = data.frame(table)
    table$date = as.character(date)
    table = table[c("date", "open", "high", "low", "close", "volume")]
  })
  output$tradetable = renderTable({
    q = qselectwhere("*", symbol=input$symbolinput)
    table = tail(get(disk, "Trade", q), 10)
  })
  output$reportbuy = renderTable({
    report_buy(input$dateinput, input$valueinput)
  })
}

shinyApp(ui, server)
