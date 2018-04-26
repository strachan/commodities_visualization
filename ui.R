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
          column(6, uiOutput("commodities_options")),
          column(6, uiOutput("year_options"))
        ),
        fluidRow(
          column(6, uiOutput("flow_options"))
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