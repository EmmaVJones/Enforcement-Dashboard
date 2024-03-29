

source('global.R')



ui <- dashboardPage(
  dashboardHeader(title = "Enforcement Dashboard", titleWidth = 280,
                  dropdownMenuOutput("pullDate")),   
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
      tags$style(HTML(".small_icon_test { font-size: 40px; }")),
      tags$head(tags$style(HTML('.value-box {min-height: 70px;} .value-box-icon {height: 70px; line-height: 70px;} .value-box-content {padding-top: 0px; padding-bottom: 0px;}
                                .wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))), # wrapper is money to avoid double y axes appearing from body
      
      tabItems(
        # Dashboard tab content
        tabItem(tabName = "statewide",
                fluidRow(
                  pendingStatewideRegionUI("statewidePendingRegion"),
                  pendingStatewideMediaUI("statewidePendingMedia"),
                  monitoredStatewideRegionUI("statewideMonitoredRegion"),
                  monitoredStatewideMediaUI("statewideMonitoredMedia"),
                  enforcementActionsStatewideRegionUI("statewideEnforcementActionsRegion"),
                  enforcementActionsStatewideMediaUI("statewideEnforcementActionsMedia"),
                  cashTotalsRegionUI('cashMoneyRegion'),
                  cashTotalsMediaUI('cashMoneyMedia'),
                  terminatedStatewideRegionUI("statewideTerminatedRegion"),
                  terminatedStatewideMediaUI("statewideTerminatedMedia"))#,
                # fluidRow(
                #          
                #          #enforcemtn actions
                #          cashTotalsUI('cashMoneyMedia'),
                #          terminatedStatewideUI("statewideTerminatedMedia") )
                #          
                #          # cashTotalsUI('cashMoney'),
                #          # FTEdeficitStatewideUI('FTEstatewide')
                # 
        ),
        tabItem(tabName = 'BRRO', eachRegionUI("BRRO_")),
        tabItem(tabName = 'CO', eachRegionUI("CO_")),
        tabItem(tabName = 'NRO', eachRegionUI("NRO_")),
        tabItem(tabName = 'PRO', eachRegionUI("PRO_")),
        tabItem(tabName = 'SWRO', eachRegionUI("SWRO_")),
        tabItem(tabName = 'TRO', eachRegionUI("TRO_")),
        tabItem(tabName = 'VRO', eachRegionUI("VRO_"))
      )))

server <- function(input, output) {
  
  # display the loading feature until data
  load_data()
  
  # Dashboard header pull date warning UI
  output$pullDate <- renderMenu({
    req(dat)
    notificationItem(text = paste0('Dashboard Data Pulled On: ', format(unique(dat$pullDate), "%m/%d/%y")),
                     icon = icon("exclamation-triangle"))  })
  
  # Statewide Modules- By Region
  callModule(pendingStatewideRegion, "statewidePendingRegion", dat)
  callModule(monitoredStatewideRegion, "statewideMonitoredRegion", dat)
  callModule(enforcementActionsStatewideRegion, "statewideEnforcementActionsRegion", dat)
  callModule(cashTotalsRegion, 'cashMoneyRegion', dat)
  callModule(terminatedStatewideRegion, "statewideTerminatedRegion", dat)
  
  
  # Statewide Modules- By Media
  callModule(pendingStatewideMedia, "statewidePendingMedia", dat)
  callModule(monitoredStatewideMedia, "statewideMonitoredMedia", dat)
  callModule(enforcementActionsStatewideMedia, "statewideEnforcementActionsMedia", dat)
  callModule(cashTotalsMedia, 'cashMoneyMedia', dat)
  callModule(terminatedStatewideMedia, "statewideTerminatedMedia", dat)
  
  # old version included these
  # callModule(cashTotals, 'cashMoney', dat)
  # callModule(FTEdeficitStatewide, 'FTEstatewide', dat, OrgData, Referral)
  
  
  
  
  # Regional Modules
  callModule(eachRegion, "BRRO_", filter(dat, `Facility Region` == "BRRO"))
  callModule(orgchart, "BRRO_", filter(OrgData, `Region` == "BRRO"), filter(Referral, `Region`=="BRRO"),filter(dat, `Facility Region` == "BRRO") )
  callModule(eachRegion, "CO_", filter(dat, `Facility Region` == "CO"))
  callModule(orgchart, "CO_", filter(OrgData, `Region` == "CO"), filter(Referral, `Region`=="CO"),filter(dat, `Facility Region` == "CO"))
  callModule(eachRegion, "NRO_", filter(dat, `Facility Region` == "NRO"))
  callModule(orgchart, "NRO_", filter(OrgData, `Region` == "NRO"), filter(Referral, `Region`=="NRO"),filter(dat, `Facility Region` == "NRO"))
  callModule(eachRegion, "PRO_", filter(dat, `Facility Region` == "PRO"))
  callModule(orgchart, "PRO_", filter(OrgData, `Region` == "PRO"),filter(Referral, `Region`=="PRO"),filter(dat, `Facility Region` == "PRO"))
  callModule(eachRegion, "SWRO_", filter(dat, `Facility Region` == "SWRO"))
  callModule(orgchart, "SWRO_", filter(OrgData, `Region` == "SWRO"), filter(Referral, `Region`=="SWRO"),filter(dat, `Facility Region` == "SWRO"))
  callModule(eachRegion, "TRO_", filter(dat, `Facility Region` == "TRO"))
  callModule(orgchart, "TRO_", filter(OrgData, `Region` == "TRO"), filter(Referral, `Region`=="TRO"),filter(dat, `Facility Region` == "TRO"))
  callModule(eachRegion, "VRO_", filter(dat, `Facility Region` == "VRO"))
  callModule(orgchart, "VRO_", filter(OrgData, `Region` == "VRO"), filter(Referral, `Region`=="VRO"),filter(dat, `Facility Region` == "VRO"))
  
  
}



shinyApp(ui, server)    

                         