library(shiny) 
library(googleVis)
library(dplyr)

# load the .RData data 
load(file = './commodities.RData')

shinyServer(function(input, output){
  
  country_commodity = commodities[year == '2016' & commodity == 'Swine, live pure-bred breeding' & flow == 'Export',
                                  .(total_trade_usd = sum(quantity)),
                                  by = .(country_or_area)]
  
  output$world_map <- renderGvis({
    gvisGeoChart(country_commodity, "country_or_area", 'total_trade_usd',
                 options=list(region="world", displayMode="regions", 
                              resolution="countries",
                              width="auto", height="auto"))
  })
})