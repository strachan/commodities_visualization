library(shiny) 
library(googleVis)
library(data.table)

# load data 
commodities = fread(file ='../commodity_trade_statistics_data.csv')

shinyServer(function(input, output){
  
  commodities_options = unique(commodities$commodity)
  years = unique(commodities$year)
  
  country_commodity <- reactive({
    commodities[year == input$year_selected & commodity == input$commodity_selected & flow == input$flow_selected]
  }) 
  
  output$world_map <- renderGvis({
    gvisGeoChart(country_commodity(), "country_or_area", 'trade_usd',
                 options=list(region="world", displayMode="regions", 
                              resolution="countries",
                              width="auto", height="auto"))
  })
  
  output$commodities_options <- renderUI({
    selectInput('commodity_selected', 'Select Commodity', as.list(commodities_options))
  })
  
  output$year_options <- renderUI({
    sliderInput("year_selected", "Year", min = min(years), max = max(years), 
                value = max(years), step = 1)
  })
  
  output$flow_options <- renderUI({
    radioButtons("flow_selected", "Flow:", c("Import", "Export"))
  })
})