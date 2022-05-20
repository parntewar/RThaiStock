library(shiny)
library(DT)

source("./database/database.R")
source("./bbl/strategy-crossema.R")

trade = NULL
favor = NULL
date = Sys.Date() - days(30)
symbols = c("PTT", "ACE")

update = function(symbols) {
  run(symbols)
  q = qselectwhere("*")
  q = qaddsincedate(q, date)
  trade = getmem(mem, disk, "Trade", q)
  q = qselectwhere("*")
  favor = getmem(mem, disk, "Favor", q)
}

ui = fluidPage(
  actionButton("update", "Update Data"),
  selectInput("symbolinput", "Symbol Input", symbols),
  plotOutput("chartseries"),
  tableOutput("pricetable")
)

server = function(input, output) {
  observeEvent(input$update, update(symbols))
  output$chartseries = renderPlot({
    chartSeries(tail(getpricexts(input$symbolinput), 100))
  })
  output$pricetable = renderTable({
    table = tail(getpricexts(input$symbolinput), 20)
    date = as.Date(index(table))
    table = data.frame(table)
    table$date = as.character(date)
    table = table[c("date", "open", "high", "low", "close", "volume")]
  })
}

shinyApp(ui, server)
