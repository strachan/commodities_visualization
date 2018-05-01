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
  
  #### code for the bar graph ####
  
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
                 flow == input$flow_selection_bar &
                 year == input$year_selection_bar]
  })
  
  observeEvent(input$country_selection, {
    selectable_categories <- sort(unique(trade_by_country()[, category]))
    updateSelectInput(session, inputId = 'category_selection_bar', choices = selectable_categories)
  })
  
  observeEvent(input$category_selection_bar, {
    number_of_commodities <- length(unique(trade_by_country()[category == input$category_selection_bar, commodity]))
    number_of_commodities_options <- ifelse(number_of_commodities < 10,
                                            data.frame(1:number_of_commodities),
                                            data.frame(1:10))
    updateSelectInput(session, inputId = 'number_commodities_selection', 
                      choices = number_of_commodities_options[[1]], 
                      selected = max(number_of_commodities_options[[1]]))
  })
  
  output$category_sum <- renderPlot({
    category_data = trade_by_country()[,sum(trade_usd)/1000000,by=.(country_or_area, category)][order(-V1)][1:input$number_categories_selection]
    ggplot(data = category_data, aes(x = reorder(category, -as.numeric(V1)), y = as.numeric(V1))) + 
      geom_bar(stat = 'identity') + xlab('Categories') + ylab('Trade in US$ (x 1MM)') + 
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
  })
  
  output$commodities_bar <- renderPlot({
    commodity_data = trade_by_country()[category == input$category_selection_bar][order(-trade_usd)][1:input$number_commodities_selection]
    ggplot(data = commodity_data, aes(x = reorder(commodity, -as.numeric(trade_usd)), y = as.numeric(trade_usd))) + 
      geom_bar(stat = 'identity') + xlab('Commodities') + ylab('Trade in US$') +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
  })
  
  #### code for the correlation tab ####
  
  trade_by_all <- reactive({
    commodity_id_1 <- commodities[commodity == input$commodity_1_selection_corr]$id
    commodity_id_2 <- commodities[commodity == input$commodity_2_selection_corr]$id
    country_names <- paste0("'", input$country_selection_corr, "'", collapse = ", ")
    dbGetDataByCountryAndCommodities(conn, country_names, commodity_id_1, commodity_id_2)
  })
  
  trade_by_countries <- reactive({
    country_names <- paste0("'", input$country_selection_corr, "'", collapse = ", ")
    dbGetDataByCountryList(conn, country_names)
  })
  
  # this observe event will update the categories and commodities
  # whenever a country is selected to ensure that it has data to plot
  observeEvent(input$country_selection_corr, {
    filtered_categories <- trade_by_countries()[flow == input$flow_selection_corr & year == input$year_selection_corr,
                                                .N, by=.(category, country_or_area)][,.N, by=category][N >= length(input$country_selection_corr), 
                                                                                     category]
    selectable_categories <- unique(filtered_categories)
    updateSelectInput(session, inputId = 'category_1_selection_corr', choices = selectable_categories)
    updateSelectInput(session, inputId = 'category_2_selection_corr', choices = selectable_categories)
  })
  
  observeEvent(input$category_1_selection_corr, {
    category_id_selected <- categories[category == input$category_1_selection_corr]$id
    commodities_to_select <- commodities[category_id == category_id_selected]$commodity
    filtered_data <- trade_by_countries() 
    unique_commodities <- unique(filtered_data[category == input$category_1_selection_corr & 
                                                 year == input$year_selection_corr & 
                                                 flow == input$flow_selection_corr, 
                                               .N, by=.(commodity, country_or_area)][,.N,by=commodity][N >= length(input$country_selection_corr), 
                                                                    commodity])
    selectable_commodities <- ifelse(nrow(filtered_data) > 0, 
                                     unique_commodities,
                                     commodities_to_select)
    updateSelectInput(session, inputId = 'commodity_1_selection_corr', choices = selectable_commodities)
  })
  
  observeEvent(input$category_2_selection_corr, {
    category_id_selected <- categories[category == input$category_2_selection_corr]$id
    commodities_to_select <- commodities[category_id == category_id_selected]$commodity
    filtered_data <- trade_by_countries() 
    unique_commodities <- unique(filtered_data[category == input$category_2_selection_corr, 
                                               .N, by=.(commodity, country_or_area)][,.N,by=commodity][N >= length(input$country_selection_corr), 
                                                                                commodity])
    selectable_commodities <- ifelse(nrow(filtered_data) > 0, 
                                     unique_commodities,
                                     commodities_to_select)
    updateSelectInput(session, inputId = 'commodity_2_selection_corr', choices = selectable_commodities)
  })
  
  output$corr_graph <- renderGvis({
    if (length(input$country_selection_corr) < 2) {
      return(gvisTable(data.frame(Warning = c('Select at least two countries'))))
    }
    data <- trade_by_all()[year == input$year_selection_corr & flow == input$flow_selection_corr] %>% 
      select(-commodity_id) %>%
      mutate(trade_usd = as.numeric(trade_usd)) %>% 
      spread(commodity, trade_usd, fill = 0)
    gvisBubbleChart(data = data, idvar = 'country_or_area', xvar = input$commodity_1_selection_corr,
                    yvar = input$commodity_2_selection_corr, colorvar = 'country_or_area',
                    options = list(hAxis ="{viewWindowMode:'pretty'}",
                                   vAxis = "{viewWindowMode:'pretty'}",
                                   height = "400px"))
  })
})