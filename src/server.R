library(shiny) 
library(ggplot2)
library(googleVis)
library(data.table)
library(tidyr)
library(dplyr)

shinyServer(function(input, output, session){
  
  conn <- dbConnector(session, dbname = './commodities.sqlite')
  
  trade <- reactive({
    commodity_id = commodities[commodity == input$commodity_selection]$id
    dbGetDataByCommodity(conn, commodity_id)
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
    trade_data <- dbGetDataByCommodity(conn, commodity_id)
    if (input$flow_selection == 'Balance') {
      trade_usd_balance = trade_data[year == input$year_selection] %>% 
        mutate(trade_usd = as.numeric(trade_usd)) %>% spread(flow, trade_usd, fill = 0) %>%
        mutate(trade_usd = Export - Import)
      return(trade_usd_balance)
    }
    trade_data[year == input$year_selection & flow == input$flow_selection]
  }) 
  
  trade_by_country <- reactive({
    trade_data <- dbGetDataByCountry(conn, input$country_selection)
    
    if (input$flow_selection_bar == 'Balance') {
      
      trade_usd_balance = trade_data[year == input$year_selection_bar & category != 'all_commodities'] %>% 
                          mutate(trade_usd = as.numeric(trade_usd)) %>% 
                          spread(flow, trade_usd, fill = 0) %>%
                          mutate(trade_usd = Export - Import)
      return(as.data.table(trade_usd_balance))
    }
    
    trade_data[category != 'all_commodities' & 
                 year == input$year_selection_bar & 
                 flow == input$flow_selection_bar]
  })
  
  # trade_by_category <- reactive({
  #   category_id <- categories[category == input$category_selection_bar]$id
  #   trade_data <- dbGetDataByCountryAndCategory(conn, input$country_selection, category_id)
  #   trade_data[year == input$year_selection & flow == input$flow_selection]
  # })
  
  output$world_map <- renderGvis({
    data_countries <- countries()
    color <- paste0('{values:[',min(data_countries$trade_usd),',0,',max(data_countries$trade_usd),"],colors:['red', 'white', 'green']}")
    gvisGeoChart(data = data_countries, locationvar = "country_or_area", colorvar = 'trade_usd',
                 options=list(region="world", displayMode="regions", 
                              resolution="countries",
                              width="auto", height="auto", colorAxis = ifelse(input$flow_selection == 'Balance', color, 
                                                                              ifelse(input$flow_selection == 'Export', 
                                                                                     "colors:['green']", "colors:['red']"))))
  })
  
  output$category_sum <- renderPlot({
    category_data = trade_by_country()[,sum(trade_usd)/1000000,by=.(country_or_area, category)][order(-V1)][1:input$number_categories_selection]
    ggplot(data = category_data, aes(x = reorder(category, -as.numeric(V1)), y = as.numeric(V1))) + geom_bar(stat = 'identity')
  })
  
  output$commodities_bar <- renderPlot({
    commodity_data = trade_by_country()[category == input$category_selection_bar][order(-trade_usd)][1:input$number_commodities_selection]
    ggplot(data = commodity_data, aes(x = reorder(commodity, -as.numeric(trade_usd)), y = as.numeric(trade_usd))) + geom_bar(stat = 'identity')
  })
})