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
        fluidRow(),
        fluidRow(
          box(
            htmlOutput('world_map'), width = 'auto'
          )
        )
      )
    )
  )
))