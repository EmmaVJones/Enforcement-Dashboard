

source('global.R')



ui <- dashboardPage(
  dashboardHeader(title = "Enforcement Monitoring Dashboard", titleWidth = 500),   
  dashboardSidebar(
    sidebarMenu(id='tabs',
                menuItem("Statewide View", tabName = "statewide", icon = icon("dashboard")),
                menuItem("DEQ Regions",tabName = "regions",icon= icon("bar-chart-o"),
                         menuSubItem("Blue Ridge", tabName = 'BRRO'),
                         menuSubItem("Central", tabName = 'CO'),
                         menuSubItem("Northern", tabName = 'NRO'),
                         menuSubItem("Piedmont", tabName = 'PRO'),
                         menuSubItem("Southwest", tabName = 'SWRO'),
                         menuSubItem("Tidewater", tabName = 'TRO'),
                         menuSubItem("Valley", tabName = 'VRO')) )),
    dashboardBody(
      tabItems(
        # Dashboard tab content
        tabItem(tabName = "statewide",
                h1("More information to follow"),
                fluidRow(cashTotalsUI('cashMoney'))
        ),
        tabItem(tabName = 'BRRO', eachRegionUI("BRRO_"))
      )))

server <- function(input, output) {
  
  # display the loading feature until data
  load_data()
  
  # Statewide Modules
  callModule(cashTotals, 'cashMoney', dat)
  
  # Regional Modules
  callModule(eachRegion, "BRRO_", filter(dat, `Facility Region` == "BRRO"))
}



shinyApp(ui, server)    

                         