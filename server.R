library(shiny) 
library(googleVis)
library(data.table)

source('./helpers.R')

shinyServer(function(input, output, session){
  
  conn <- dbConnector(dbname = './commodities.sqlite')
  
  trade <- reactive({
    commodity_id = commodities[commodity == input$commodity_selection]$id
    dbGetData(conn, commodity_id)
  })  
  
  observeEvent(input$commodity_selection, {
    years <- unique(trade()$year)
    updateSliderInput(session, inputId = 'year_selection', value = max(years), min = min(years), max = max(years))
  })
  
  countries <- reactive({
    data_trade <- trade()
    data_trade[year == input$year_selection & commodity == input$commodity_selection & flow == input$flow_selection]
  }) 
  
  output$world_map <- renderGvis({
    data_countries <- countries()
    gvisGeoChart(data = data_countries, locationvar = "country_or_area", colorvar = 'trade_usd',
                 options=list(region="world", displayMode="regions", 
                              resolution="countries",
                              width="auto", height="auto"))
  })
})