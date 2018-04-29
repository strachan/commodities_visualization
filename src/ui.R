library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = 'Commodities'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Map', tabName = 'map', icon = icon('map')),
      menuItem('Bar Graph', tabName = 'bar_graph', icon = icon('bar-chart-o'))
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
                                 choices = flow_types)),
          
          column(6, sliderInput(inputId = "year_selection", label = "Year", min = 2016, max = 2016, 
                                value = 2016, step = 1))
        ),
        fluidRow(
          column(12, htmlOutput('world_map')
          )
        )
      ),
      tabItem(tabName = 'bar_graph',
        fluidRow(
          column(6, sliderInput(inputId = "year_selection_bar", label = "Year", min = min(years$year), max = max(years$year), 
                                value = max(years$year), step = 1)),
          column(6, radioButtons(inputId = "flow_selection_bar", label = "Flow:", 
                                 choices = flow_types))
        ),
        fluidRow(
          column(9, selectizeInput(inputId = 'country_selection', label = 'Country',
                                   choices = countries$country_or_area, width = '100%')),
          column(3, selectizeInput(inputId = 'number_categories_selection', label = 'N Top Categories',
                                   choices = 1:10, selected = 5))
        ),
        fluidRow(
          column(12, plotOutput('category_sum'))
        )
      )
    )
  )
))