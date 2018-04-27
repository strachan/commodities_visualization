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
          column(9, selectizeInput(inputId = 'category_selection', label = 'Category',
                                   choices = categories$category, width = '100%'))
        ),
        fluidRow(
          column(9, selectizeInput(inputId = 'commodity_selection', label = 'Commodity', 
                                   choices = commodities$commodity, width = '100%'))
        ),
        fluidRow(
          column(6, radioButtons(inputId = "flow_selection", label = "Flow:", 
                                 choices = c("Export", "Import"))),
          
          column(6, sliderInput(inputId = "year_selection", label = "Year", min = 2016, max = 2016, 
                                value = 2016, step = 1))
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