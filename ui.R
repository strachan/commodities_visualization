library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = 'Commodities'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Map', tabName = 'map', icon = icon('map'))
    )
  ),
  dashboardBody(
    tabItems(
      # Map tab content
      tabItem(tabName = 'map',
        fluidRow(
          column(6, selectizeInput(inputId = 'commodity_selection', label = 'Commodity', 
                                   choices = c('Sheep, live'), selected = 'Sheep, live')),
          column(6, sliderInput(inputId = "year_selection", label = "Year", min = NULL, max = NULL, 
                                value = NULL, step = 1))
        ),
        fluidRow(
          column(6, radioButtons(inputId = "flow_selection", label = "Flow:", 
                                 choices = c("Export", "Import")))
        ),
        fluidRow(
          box(
            htmlOutput('world_map'), width = 'auto'
          )
        )
      )
    )
  )
))