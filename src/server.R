library(shiny) 
library(ggplot2)
library(googleVis)
library(data.table)
library(tidyr)
library(dplyr)
library(dygraphs)

shinyServer(function(input, output, session){
  
  conn <- dbConnector(session, dbname = './commodities.sqlite')
  
  #### common functions ####
  
  # function to calculate the difference between Export and Import trade values
  calculateBalanceTrade <- function(data) {
    data %>% 
      mutate(trade_usd = as.numeric(trade_usd)) %>% spread(flow, trade_usd, fill = 0) %>%
      mutate(trade_usd = Export - Import)
  }
  
  getCommodityId <- function(commodity_name) {
    commodities[commodity == commodity_name]$id
  }
  
  #### code for the map graph ####
  
  getCategoryId <- function(category_name) {
    categories[category == category_name]$id
  }
  
  getCommoditiesByCategory <- function(category_name) {
    commodities[category_id == getCategoryId(category_name)]$commodity
  }
  
  trade_by_commodity <- reactive({
    commodity_id = getCommodityId(input$commodity_selection_map)
    dbGetDataByCommodity(conn, commodity_id)
  })  
  
  # updates commodity list according to category selected
  observeEvent(input$category_selection_map, {
    commodities_to_select <- getCommoditiesByCategory(input$category_selection_map)
    updateSelectInput(session, inputId = 'commodity_selection_map', choices = commodities_to_select)
  })
  
  # updates year range according to commodity selected
  observeEvent(input$commodity_selection_map, {
    years <- unique(trade_by_commodity()$year)
    updateSliderInput(session, inputId = 'year_selection_map', value = max(years), min = min(years), max = max(years))
  })
  
  # reactive function to get the data according to the inputs selected
  # for the world map graph
  countries_trade <- reactive({
    commodity_id = getCommodityId(input$commodity_selection_map)
    trade_data <- dbGetDataByCommodity(conn, commodity_id)[year == input$year_selection_map]
    
    # when Balance flow is selected we must calculate the difference
    # between export and import
    if (input$flow_selection_map == 'Balance') {
      return(calculateBalanceTrade(trade_data))
    }
    
    trade_data[flow == input$flow_selection_map]
  }) 

  getMapChartOptions <- function(min_value, max_value) {
    
    # color variable for balance flow
    color <- paste0('{values:[',min_value, ',0,',
                    max_value, "],colors:['red', 'white', 'green']}")
    
    # choose the color depending on the flow selected
    color_axis <- ifelse(input$flow_selection_map == 'Balance', 
                         color, 
                         "colors:['green']")
    
    list(region="world", displayMode="regions", resolution="countries",
         width="auto", height="auto", colorAxis = color_axis)
  }
  
  output$world_map <- renderGvis({
    countries_data <- countries_trade()
    min_value <- min(countries_data$trade_usd)
    max_value <- max(countries_data$trade_usd)
    options <- getMapChartOptions(min_value, max_value)
    gvisGeoChart(data = countries_data, locationvar = "country_or_area", 
                 colorvar = 'trade_usd', options = options)
  })
  
  #### code for the bar graph ####
  
  # reactive function to update data when inputs are changed
  trade_by_country <- reactive({
    trade_data <- dbGetDataByCountry(conn, input$country_selection)[year == input$year_selection_bar & 
                                                                      category != 'all_commodities']
    
    if (input$flow_selection_bar == 'Balance') {
      trade_balance_data = calculateBalanceTrade(trade_data)
      return(as.data.table(trade_balance_data))
    }
    
    trade_data[flow == input$flow_selection_bar]
  })
  
  # observe changes in country selection to update to categories that has data
  observeEvent(input$country_selection, {
    selectable_categories <- sort(unique(trade_by_country()[, category]))
    updateSelectInput(session, inputId = 'category_selection_bar', choices = selectable_categories)
  })
  
  # observe changes in category selection to update the commodities that has data
  observeEvent(input$category_selection_bar, {
    number_of_commodities <- length(unique(trade_by_country()[category == input$category_selection_bar, commodity]))
    # limit number of commodities to select
    number_of_commodities_options <- ifelse(number_of_commodities < 10,
                                            number_of_commodities,
                                            10)
    updateSelectInput(session, inputId = 'number_commodities_selection', 
                      choices = 1:number_of_commodities_options, 
                      selected = max(number_of_commodities_options[[1]]))
  })
  
  getTotalAmount <- function(data) {
    sum(data[, trade_usd])
  }
  
  calculatePercentage <- function(data, total_value) {
    sum(as.numeric(data)) * 100 / total_value
  }
  
  # infobox to show the total trade amount in US dollars for the country and flow selected
  output$total_trade_country <- renderInfoBox({
    total_value <- getTotalAmount(trade_by_country())
    # format the total amount to currency
    total_value_formatted <- paste0("$", formatC(as.numeric(total_value), format="f", digits=2, big.mark=","))
    infoBox("Total Trade (in USD)", total_value_formatted, icon = icon("calculator"))
  })
  
  # infobox to show the percentage that the categories selected influence on the total amount
  output$categories_influence_percentage <- renderInfoBox({
    total_value <- getTotalAmount(trade_by_country())
    category_data <- trade_by_country()[,sum(trade_usd) ,by=category][order(-V1)][1:input$number_categories_selection]
    percentage <- calculatePercentage(category_data[,V1], total_value)
    infoBox("Influence Percentage", paste0(format(percentage, format="f", digits = 2), "%"), color = 'olive')
  })
  
  # infobox to show the percentage that the commodities selected influence on the total amount
  output$commodities_influence_percentage <- renderInfoBox({
    total_value <- getTotalAmount(trade_by_country())
    commodity_data <- trade_by_country()[category == input$category_selection_bar][order(-trade_usd)][1:input$number_commodities_selection]
    percentage <- calculatePercentage(commodity_data[,trade_usd], total_value)
    infoBox("Influence Percentage", paste0(format(percentage, format="f", digits = 2), "%"), color = 'green')
  })
  
  getTopCategories <- function(data, n) {
    data[,sum(trade_usd)/1000000, by=category][order(-V1)][1:n]
  }
  
  getTopCommodities <- function(data, n) {
    data[order(-trade_usd)][1:n]
  }
  
  # table to show what each category id means on the bar graph and the exact trade value
  output$categories_legend <- renderTable({
    category_data = getTopCategories(trade_by_country(), input$number_categories_selection)
    categories_plotted <- right_join(categories, category_data)
    data.table(id = categories_plotted$id, category = categories_plotted$category, trade_usd_X1MM = categories_plotted$V1)
  })
  
  # table to show what each commodity id means on the bar graph and the exact trade value
  output$commodities_legend <- renderTable({
    one_category_data <- trade_by_country()[category == input$category_selection_bar]
    commodity_data <- getTopCommodities(one_category_data, input$number_commodities_selection)
    commodities_plotted <- right_join(commodities, commodity_data)
    data.table(id = commodities_plotted$id, commodity = commodities_plotted$commodity, trade_usd = as.numeric(commodities_plotted$trade_usd))
  })
  
  # bar graph by categories
  output$category_sum <- renderPlot({
    category_data = getTopCategories(trade_by_country(), input$number_categories_selection)
    categories_plotted <- right_join(categories, category_data)
    ggplot(data = category_data, aes(x = reorder(category, -as.numeric(V1)), y = as.numeric(V1))) + 
      geom_bar(stat = 'identity', fill = '#768858') + ylab('Trade in US$ (x 1MM)') + 
      theme(axis.text=element_text(size=30), axis.title=element_text(size=15,face="bold"),
            axis.text.x = element_text(size = 30)) + theme_bw() +
      scale_x_discrete('Categories', labels = categories_plotted$id)
  })
  
  # bar graph by commodities
  output$commodities_bar <- renderPlot({
    one_category_data <- trade_by_country()[category == input$category_selection_bar]
    commodity_data <- getTopCommodities(one_category_data, input$number_commodities_selection)
    commodities_plotted <- right_join(commodities, commodity_data)
    ggplot(data = commodity_data, aes(x = reorder(commodity, -as.numeric(trade_usd)), y = as.numeric(trade_usd))) + 
      geom_bar(stat = 'identity', fill = '#609040') + xlab('Commodities') + ylab('Trade in US$') +
      theme(axis.text=element_text(size=30), axis.title=element_text(size=15,face="bold"),
            axis.text.x = element_text(size = 30)) + theme_bw() +
      scale_x_discrete('Commodities', labels = commodities_plotted$id)
  })
  
  #### code for the correlation tab ####
  
  # reactive function to get new data when a new country is selected
  trade_by_countries <- reactive({
    country_names <- paste0("'", input$country_selection_corr, "'", collapse = ", ")
    dbGetDataByCountryList(conn, country_names)
  })
  
  # function to filter by commodities and flow selected
  trade_filtered <- function() {
    trade_by_countries()[(commodity == input$commodity_1_selection_corr | 
                           commodity == input$commodity_2_selection_corr) &
                           flow == input$flow_selection_corr]
  }
  
  # this observe event will update the categories and commodities
  # whenever a country is selected to ensure that it has data to plot
  observeEvent(input$country_selection_corr, {
    filtered_categories <- trade_by_countries()[flow == input$flow_selection_corr & year == input$year_selection_corr,
                                                .N, by=.(category, country_or_area)][,.N, by=category][N >= length(input$country_selection_corr), 
                                                                                     category]
    selectable_categories <- sort(unique(filtered_categories))
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
                                     data.frame(unique_commodities),
                                     data.frame(commodities_to_select))
    updateSelectInput(session, inputId = 'commodity_1_selection_corr', choices = selectable_commodities[[1]])
  })
  
  observeEvent(input$category_2_selection_corr, {
    category_id_selected <- categories[category == input$category_2_selection_corr]$id
    commodities_to_select <- commodities[category_id == category_id_selected]$commodity
    filtered_data <- trade_by_countries()
    unique_commodities <- unique(filtered_data[category == input$category_2_selection_corr, 
                                               .N, by=.(commodity, country_or_area)][,.N,by=commodity][N >= length(input$country_selection_corr), 
                                                                                commodity])
    selectable_commodities <- ifelse(nrow(filtered_data) > 0, 
                                     data.frame(unique_commodities),
                                     data.frame(commodities_to_select))
    updateSelectInput(session, inputId = 'commodity_2_selection_corr', choices = selectable_commodities[[1]])
  })
  
  getSpreadedDataByCommodity <- function(data) {
    data %>% 
      select(-category, -commodity_id) %>%
      mutate(trade_usd = as.numeric(trade_usd)) %>% 
      spread(commodity, trade_usd, fill = 0)
  }
  
  getCorrGraphOptions <- function(max_commodity_1, max_commodity_2) {
    list(hAxis =paste0("{viewWindowMode:'pretty', format:'currency', maxValue:'", 
                       1.1 * max_commodity_1,"', title:'", 
                       input$commodity_1_selection_corr, "'}"),
         vAxis =paste0("{viewWindowMode:'pretty', format:'currency', maxValue:'", 
                       1.1 * max_commodity_2,"', title:'", 
                       input$commodity_2_selection_corr, "'}"),
         height = "400px")
  }
  
  output$corr_graph <- renderGvis({
    # only plot the bubble graph when at least two countries are selected
    if (length(input$country_selection_corr) < 2) {
      return(NULL)
    }
    data <- getSpreadedDataByCommodity(trade_filtered()[year == input$year_selection_corr])
    
    # get the highest values for commodities selected during the range of years for axis setting
    # this is done to maintain the same parameter for comparison during the animation
    data2 <- trade_filtered()
    max_commodity_1 = max(data2[commodity == input$commodity_1_selection_corr, trade_usd])
    max_commodity_2 = max(data2[commodity == input$commodity_2_selection_corr, trade_usd])
    
    options <- getCorrGraphOptions(max_commodity_1, max_commodity_2)
    
    gvisBubbleChart(data = data, idvar = 'country_or_area', xvar = input$commodity_1_selection_corr,
                    yvar = input$commodity_2_selection_corr, colorvar = 'country_or_area',
                    options = options)
  })
  
  output$dygraph <- renderDygraph({
    # only plot the dygraph when there is only one country selected
    if (length(input$country_selection_corr) != 1) {
      return(NULL)
    }
    
    # for the dygraph date should be the first column
    data <- getSpreadedDataByCommodity(trade_filtered()) %>% select(year, everything())
    data$year = as.Date(paste(data$year, 1, 1, sep = "-"))
    data = as.xts.data.table(as.data.table(data))
    dygraph(data = data) %>% dyRangeSelector()
  })
})