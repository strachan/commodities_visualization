library(shiny) 
library(googleVis)
library(data.table)

shinyServer(function(input, output, session){
  
  conn <- dbConnector(session, dbname = './commodities.sqlite')
  
  trade <- reactive({
    commodity_id = commodities[commodity == input$commodity_selection]$id
    dbGetData(conn, commodity_id)
  })  
  
  observeEvent(input$category_selection, {
    commodities_to_select <- commodities[category_id == categories[category == input$category_selection]$id]$commodity
    updateSelectInput(session, inputId = 'commodity_selection', choices = commodities_to_select)
  })
  
  observeEvent(input$commodity_selection, {
    years <- unique(trade()$year)
    updateSliderInput(session, inputId = 'year_selection', value = max(years), min = min(years), max = max(years))
  })
  
  countries <- reactive({
    commodity_id = commodities[commodity == input$commodity_selection]$id
    data_trade <- dbGetData(conn, commodity_id)
    data_trade[year == input$year_selection & flow == input$flow_selection]
  }) 
  
  output$world_map <- renderGvis({
    data_countries <- countries()
    gvisGeoChart(data = data_countries, locationvar = "country_or_area", colorvar = 'trade_usd',
                 options=list(region="world", displayMode="regions", 
                              resolution="countries",
                              width="auto", height="auto"))
  })
})