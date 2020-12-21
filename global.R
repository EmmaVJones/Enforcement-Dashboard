library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(pool)
library(config)

source('modules/cashCivilChargesModule.R')
source('modules/statewideModules.R')

# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}

# get configuration settings
conn <- config::get("connectionSettings")


# Set up pool connection to production environment
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
  # Production Environment
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  UID = conn$UID_prod, 
  PWD = conn$PWD_prod,
  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
  # Test environment
  #Server= "WSQ04151,50000",
  #dbname = "ODS_test",
  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
  trusted_connection = "yes"
)
#### Production Environment For testing
#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQL Server Native Client 11.0", 
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  trusted_connection = "yes"
#)

onStop(function() {
  poolClose(pool)
})

# Bring in live data from ODS
# Facilities Data (for joining)
enfFacilities <- pool %>% tbl("Enf_Facilities_View") %>%
  as_tibble()

# get start of year filter information
startOfYear <- as.Date(paste0(year(Sys.Date())-1, '-01-01')) # use 2019 as starting point

# Case data
dat <- pool %>% tbl("Enf_enforcement_Cases_View") %>%
  filter(between(Enc_Executed_Date, !! startOfYear, !! Sys.Date()) |
           between(Enc_Nov_Date, !! startOfYear, !! Sys.Date()) |
           between(Enc_Terminated_Date, !! startOfYear, !! Sys.Date()) ) %>%
  as_tibble() %>%
  dplyr::select(Enc_Enf_Facility_Id, 
                `EA Number` = Enc_Ea_Number,
                `Facility Region` = EFC_REGION,
                `Case Manager` = Enf_Case_Mgr,
                `Registration Number` = Enc_Reg_Fac_Num,
                `Program Name` = Program_Name,
                `Permit Number` = Enc_Pmt_Pc_Num,
                `NOV Number` = Enc_Nov_Number,
                `NOV Date` = Enc_Nov_Date,
                `Executed Date` = Enc_Executed_Date,
                `Term Date` = Enc_Terminated_Date, 
                `Cash Civil Charge` = ECC_CASH_CIVIL_CHARGE, 
                `Action Code` = EAC_DESCRIPTION, 
                `Status Code` = Status_Code_Desc, 
                Comments = Enc_Status_Comments) %>%
  left_join(dplyr::select(enfFacilities, Enc_Enf_Facility_Id = Efc_Id, `Facility Name` = Efc_Facility_Name),
            by = "Enc_Enf_Facility_Id") %>%
  dplyr::select(`Facility Name`, everything(), -c(Enc_Enf_Facility_Id)) %>%
  # drop all rows with no data
  drop_na(`Facility Name`, `EA Number`) %>%
  # first fix date fields
  mutate(`NOV Date` = as.POSIXct(`NOV Date`, format = "%m/%d/%Y", tz = 'EST'),
         `Executed Date` = as.POSIXct(`Executed Date`, format = "%m/%d/%Y", tz = 'EST'),
         `Term Date` = as.POSIXct(`Term Date`, format = "%m/%d/%Y", tz = 'EST')) %>%
  # fix factor levels for plotting by program
  mutate(Program = as.factor(case_when(`Program Name` %in% c( "Solid Waste", "Hazardous Waste") ~ "Waste",
                                       `Program Name` %in% c("VWPP") ~ "VWP",
                                       is.na(`Program Name`) | 
                                         `Program Name` %in% c("PReP", "VPA", 
                                                               "Multimedia", "Oil Spill", 
                                                               "Other", "Groundwater","Const. Storm.") ~ "Other", 
                                       `Program Name` %in% c("UST", "AST") ~ "AST/UST", 
                                       TRUE ~ as.character(`Program Name`)))) %>%
  # add date of pull and calculate days Pending
  mutate(pullDate = as.Date(Sys.Date()),
         `Days Pending` = case_when(is.na(`Term Date`) ~ trunc(time_length(interval(`NOV Date`, pullDate), unit = 'day')),
                                    TRUE ~ trunc(time_length(interval(`NOV Date`, `Term Date`), unit = 'day'))))


# Bring in test dataset, had to manually convert from .xls (Logi pull default) to .csv and delete header matter
# Pull was from April 14, 2020 to Jan 1, 2019
#dat <- read_csv('LogiPulls/01012019_04142020.csv') %>%
#  # drop all rows with no data
#  drop_na(`Facility Name`, `EA Number`) %>%
#  # first fix date fields
#  mutate(`NOV Date` = as.POSIXct(`NOV Date`, format = "%m/%d/%Y", tz = 'EST'),
#         `Executed Date` = as.POSIXct(`Executed Date`, format = "%m/%d/%Y", tz = 'EST'),
#         `Term Date` = as.POSIXct(`Term Date`, format = "%m/%d/%Y", tz = 'EST')) %>%
#  # fix factor levels for plotting by program
#  mutate(Program = as.factor(case_when(`Program Name` %in% c( "Solid Waste", "Hazardous Waste") ~ "Waste",
#                                       `Program Name` %in% c("VWPP") ~ "VWP",
#                                       is.na(`Program Name`) | 
#                                         `Program Name` %in% c("PReP", "VPA", 
#                                                               "Multimedia", "Oil Spill", 
#                                                               "Other", "Groundwater") ~ "Other", 
#                                       `Program Name` %in% c("UST", "AST") ~ "AST/UST", 
#                                       TRUE ~ as.character(`Program Name`)))) %>%
#  # add date of pull and calculate days Pending
#  mutate(pullDate = as.Date('2020-04-14'), #Sys.Date(),
#         `Days Pending` = trunc(time_length(interval(`NOV Date`, pullDate), unit = 'day')))


OrgData<- read_csv('LogiPulls/OrgCharts.csv') # mostly static, needs to be reviewed annually by enf program for accuracy

Referral<- read_csv('LogiPulls/ReferralRates.csv') # mostly static, needs to be reviewed annually by enf program for accuracy
# this could be calculated in time with guidance from enforcement program


# tabItem module
eachRegionUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      uiOutput(ns("Manager")),
      uiOutput(ns("Specialists"))
      
    ),
    fluidRow(
      box(title =  h4(strong("Workload Summary")),footer = paste0("*Case Capacity and FTE Deficit are determined by capcity assigned in the Organization Chart (above); ",  "FTE Deficit calculation assumes no vacant positions."), height = 350,
          column(width=6, textOutput(ns("pendingcases")), br(),
                 textOutput(ns("pendingcasesover365")), br(),
                 textOutput(ns("pendingcasesless365")), br(),
                 textOutput(ns("MonitoredCases")),  br(), 
                 textOutput(ns("TerminatedCases"))),
          column(width = 6, textOutput(ns("CaseCapacity")), br(),
                 textOutput(ns("Referral")), br(),
                 uiOutput(ns("FTERef")), br(),
                 uiOutput(ns("CurrentFTE")), br(), 
                 uiOutput(ns("RefFTEequ"))
          )),
      
      
      tabBox(title = h4(strong("Cases Pending")), height = 350, id = "tabBox_casesPending",
             tabPanel("Summary Plot View",  plotlyOutput(ns("casesPending"), height = 250)),
             tabPanel("Summary Table View", DT::dataTableOutput(ns("casesPendingTable"))),
             tabPanel("Raw Data Table", div(DT::dataTableOutput(ns("raw_casesPendingTable")), style = "font-size:80%")))),
    
    fluidRow(
      tabBox(title = h4(strong("Cases Terminated")), height = 300, id = "tabBox_casesPending",
             tabPanel("Summary Plot View",  plotlyOutput(ns("casesTerminated"), height = 250)),
             tabPanel("Summary Table View", DT::dataTableOutput(ns("casesTerminatedTable"))),
             tabPanel("Raw Data Table", div(DT::dataTableOutput(ns("raw_casesTerminatedTable")), style = "font-size:80%"))),
      tabBox(title = h4(strong("Cases Monitored")), height = 300, id = "tabBox_casesPending",
             tabPanel("Summary Plot View",  plotlyOutput(ns("casesMonitored"), height = 250)),
             tabPanel("Summary Table View", DT::dataTableOutput(ns("casesMonitoredTable"))),
             tabPanel("Raw Data Table", div(DT::dataTableOutput(ns("raw_casesMonitoredTable")), style = "font-size:80%")))
    )
    
  )
}


eachRegion <- function(input,output, session, regionalData){
  pending <- reactive({
    filter(regionalData, `Status Code` %in% c("Pending", "Pending Consent")) %>%
      mutate(`Pending Status` = as.factor(case_when(`Days Pending` >= 365 ~ "Active > 365", 
                                                    TRUE ~ "Active < 365")))  })
  
  pendingTotal <- reactive({pending()%>%
      nrow()
  }) 
  
  PendingOneyear<- reactive({
    pending() %>%
      filter(`Days Pending` >= 365) %>%
      nrow()
  })
  
  Pendingless<-reactive({
    pending()%>%
      filter(`Days Pending`<= 365)%>%
      nrow()
  })
  
  pendingPlot <- reactive({
    req(pending())
    programTotals <- pending() %>%
      group_by(Program) %>%
      summarize(`Program Total` = n())
    
    pendingPlot <- pending() %>%
      group_by(`Pending Status`, Program) %>%
      summarize(Total = n()) %>%
      left_join(programTotals, by = "Program") %>%
      pivot_wider(names_from = `Pending Status`, values_from = Total) %>%
      # in case nothing in either category
      rowwise() %>%
      mutate(`Active > 365` = ifelse("Active > 365" %in% names(.), `Active > 365`, 0),
             `Active < 365` = ifelse("Active < 365" %in% names(.), `Active < 365`, 0))
    return(pendingPlot)
  })
  
  terminated <- reactive({
    filter(regionalData, 
           `Term Date` >= (pullDate %m-% months(12)), # filter within 12 months from pull date
           `Status Code` %in% c("Terminated")) %>%
      group_by(Program) %>%
      mutate(`Program Total` = n())  })
  
  
  terminatedTotal<- reactive({
    terminated()%>%
      nrow()
  })
  
  terminatedPlot <- reactive({
    req(terminated())
    terminated() %>%
      distinct(Program, .keep_all = TRUE) %>%
      dplyr::select(Program, `Program Total`)  })
  
  monitored <- reactive({
    filter(regionalData, `Status Code` %in% c("Effective")) %>%
      group_by(Program) %>%
      mutate(`Program Total` = n()) })
  
  
  monitoredTotal<- reactive({
    monitored() %>%
      nrow()
  })
  
  
  monitoredPlot <- reactive({
    req(monitored())
    monitored() %>%
      distinct(Program, .keep_all = TRUE) %>%
      dplyr::select(Program, `Program Total`)  })
  
  
  
  ### Lucy's Boxes
  
  output$pendingcases <- renderText({
    paste0("Pending Cases: ", pendingTotal())
    
  })
  
  
  output$pendingcasesover365<-renderText({
    paste0("Pending Cases > 365 days: ", PendingOneyear())
    
  })
  
  output$pendingcasesless365<- renderText({
    paste0("Pending Cases < 365 days: ", Pendingless())
  })
  
  
  output$MonitoredCases<- renderText({
    paste0("Monitored Cases: ", monitoredTotal())
    
  })
  
  output$TerminatedCases<- renderText({
    paste0("Terminated Cases: ", terminatedTotal())
  })
  
  #######
  
  
  output$casesPending <- renderPlotly({
    req(pendingPlot())
    if(nrow(filter(pendingPlot(), !is.na(Program))) > 0 ){
      plot_ly(data = pendingPlot(), x = ~Program, y = ~`Active < 365`, type = 'bar', name = 'Active < 365',
              hoverinfo="text",text=~paste(sep="<br>",
                                           paste("Program Name: ",Program),
                                           paste("Category Count: ",`Active < 365`),
                                           paste("Program Total: ", `Program Total`))) %>%
        add_trace(y = ~`Active > 365`, name = "Active > 365",
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Program Name: ",Program),
                                               paste("Category Count: ",`Active > 365`),
                                               paste("Program Total: ", `Program Total`))) %>%
        layout(height = 250,
               yaxis = list(title = 'Count'),
               xaxis = list(title = 'Program Name'), 
               barmode = 'stack')
    } else {
      NULL
    }
    
  })
  
  output$casesPendingTable <- DT::renderDataTable({
    req(pendingPlot())
    DT::datatable(pendingPlot(), rownames = FALSE, options = list(dom = 't'))  })
  
  output$raw_casesPendingTable <- DT::renderDataTable({
    req(pending())
    DT::datatable(dplyr::select(pending(), `Facility Name`:Comments), rownames = FALSE, extensions = 'Buttons',  
                  options = list(dom = 'Bft', scrollX = TRUE, scrollY = "125px", autoWidth = TRUE,
                                 buttons=list('copy',
                                              list(extend='csv',filename=paste('pendingConsent_',unique(pending()$`Facility Region`),'_',
                                                                               unique(pending()$pullDate),'.csv',sep=''))))) %>%
      DT::formatStyle("Comments","white-space"="nowrap")  })
  
  
  output$casesTerminated <- renderPlotly({
    req(terminatedPlot())
    plot_ly(data = terminatedPlot()) %>%
      add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Program Total: ", `Program Total`))) %>%
      layout(height = 250,
             yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Program Name'))  })
  
  output$casesTerminatedTable <- DT::renderDataTable({
    req(terminatedPlot())
    DT::datatable(terminatedPlot(), rownames = FALSE, options = list(dom = 't'))  })
  
  output$raw_casesTerminatedTable <- DT::renderDataTable({
    req(terminated())
    DT::datatable(dplyr::select(terminated(), `Facility Name`:Comments), rownames = FALSE, extensions = 'Buttons',  
                  options = list(dom = 'Bft', scrollX = TRUE, scrollY = "125px", autoWidth = TRUE,
                                 buttons=list('copy',
                                              list(extend='csv',filename=paste('terminatedConsent_',unique(terminated()$`Facility Region`),'_',
                                                                               unique(terminated()$pullDate),'.csv',sep=''))))) %>%
      DT::formatStyle("Comments","white-space"="nowrap") })
  
  output$casesMonitored <- renderPlotly({
    req(terminatedPlot())
    plot_ly(data = monitoredPlot()) %>%
      add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Program Total: ", `Program Total`))) %>%
      layout(height = 250,
             yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Program Name')) })
  
  output$casesMonitoredTable <- DT::renderDataTable({
    req(monitoredPlot())
    DT::datatable(monitoredPlot(), rownames = FALSE, options = list(dom = 't'))  })
  
  output$raw_casesMonitoredTable <- DT::renderDataTable({
    req(monitored())
    DT::datatable(dplyr::select(monitored(), `Facility Name`:Comments), rownames = FALSE, extensions = 'Buttons', 
                  options = list(dom = 'Bft', scrollX = TRUE, scrollY = "125px", autoWidth = TRUE,
                                 buttons=list('copy',
                                              list(extend='csv',filename=paste('monitoredConsent_',unique(monitored()$`Facility Region`),'_',
                                                                               unique(monitored()$pullDate),'.csv',sep=''))))) %>%
      DT::formatStyle("Comments","white-space"="nowrap") })
  
}


orgchart<-function(input,output, session, OrgData, Referral, regionalData){
  
  pending <- reactive({
    filter(regionalData, `Status Code` %in% c("Pending", "Pending Consent")) %>%
      mutate(`Pending Status` = as.factor(case_when(`Days Pending` >= 365 ~ "Active > 365", 
                                                    TRUE ~ "Active < 365")))  })
  
  manager<-reactive({
    filter(OrgData, Position %in% c("Manager", "Manager/Vacant","Director", "Program Coordinator", "Team Leader/Vacant"))
  })
  
  Specialist<- reactive({
    filter(OrgData, `Position` %in% c("Enforcement Specialist", "Enforcement Specialist/Vacant", "Administrative and Staff Assistant", "Administrative and Staff Assistant/Vacant"))
  })
  
  
  CaseCap<-reactive({
    OrgData%>%
      summarise("RegionalCase"=sum(`Capacity`))
  })
  
  pendingTotal <- reactive({pending()%>%
      nrow()
  }) 
  
  Current<-reactive({
    format(round(((CaseCap()- pendingTotal())/ 15), 2), nsmall = 2)
    
  })
  
  output$CaseCapacity <- renderText({
    paste0("Regional Case Capacity: ", CaseCap())
    
  })
  
  output$Referral<- renderText({
    
    paste0("Average Referral Rate: ", Referral$`Referral Rate`)
    
  })
  
  RefFTE<-reactive({
    
    format(round(((CaseCap()- Referral$`Referral Rate`)/ 15),2),nsmall=2)
    
  })
  
  output$FTERef<- renderUI({
    if(RefFTE() >= 0.00){
      
      a <- paste0("<span style=color:#1E90FF>", "Referral FTE Deficit: ",  RefFTE(), "</span>")
      
    }else{
      a <- paste0("<span style=color:#ff5733>", "Referral FTE Deficit: ",  RefFTE(), "</span>")
    }
    
    HTML(a)
    
  })
  output$RefFTEequ<-renderUI({
    HTML(paste0(em("Referral Deficit Eq. "),"(", CaseCap(), "-", Referral$`Referral Rate`, ")", "/ ",  15, "= ", RefFTE(), br(),
                em( "Current Deficit Eq. " ), "(",  CaseCap(), "- ", pendingTotal(), ")", "/ ", 15, "= ", Current()))
  })
  
  output$CurrentFTE<-renderUI({
    
    if(Current() >= 0.00){
      
      b <- paste0("<span style=color:#1E90FF>", "Current FTE Deficit: ",  Current(), "</span>")
      
    }else{
      b <- paste0("<span style=color:#ff5733>", "Current FTE Deficit: ",  Current(), "</span>")
    }
    
    HTML(b)
    
  })
  
  output$Manager <- renderUI({  
    lapply(1:length(manager()$`Position number`), function(i) {
      
      valueBox(value=
                 tags$p(paste0(manager()$`Position`[i], "- ", manager()$`Position number`[i]), style="font-size: 14px;"), 
               subtitle =HTML(paste0("Capacity: ", manager()$`Capacity`[i])),color="olive",  width=3)
      
    })
    
  })
  
  output$Specialists<- renderUI({
    lapply(1:length(Specialist()$`Position number`), function(i) { 
      valueBox(value=tags$p(paste0(Specialist()$`Position`[i], "- ",
                                   Specialist()$`Position number`[i]),style = "font-size: 13px;"),
               subtitle=HTML(paste0("Capacity: ", Specialist()$`Capacity`[i])),
               color = "teal", #here display number1 one by one like name 
               width = 3)
    } )
  })
  
  
}
