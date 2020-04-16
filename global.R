library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)

source('modules/cashCivilChargesModule.R')

# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}


# Bring in test dataset, had to manually convert from .xls (Logi pull default) to .csv and delete header matter
# Pull was from April 14, 2020 to Jan 1, 2019
dat <- read_csv('LogiPulls/01012019_04142020.csv') %>%
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
                                                               "Other", "Groundwater") ~ "Other", 
                                       `Program Name` %in% c("UST", "AST") ~ "AST/UST", 
                                       TRUE ~ as.character(`Program Name`)))) %>%
  # add date of pull and calculate days Pending
  mutate(pullDate = Sys.Date(),
         `Days Pending` = trunc(time_length(interval(`NOV Date`, pullDate), unit = 'day')))


# tabItem module
eachRegionUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Workload Summary", height = 300),
      tabBox(title = h4(strong("Cases Pending")), height = 300, id = "tabBox_casesPending",
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
  
  pendingPlot <- reactive({
    req(pending())
    programTotals <- pending() %>%
      group_by(Program) %>%
      summarize(`Program Total` = n())
    
    pendingPlot <- pending() %>%
      group_by(`Pending Status`, Program) %>%
      summarize(Total = n()) %>%
      left_join(programTotals, by = "Program") %>%
      pivot_wider(names_from = `Pending Status`, values_from = Total)
    return(pendingPlot)
  })
  
  terminated <- reactive({
    filter(regionalData, 
           `Term Date` >= (pullDate %m-% months(12)), # filter within 12 months from pull date
           `Status Code` %in% c("Terminated")) %>%
      group_by(Program) %>%
      mutate(`Program Total` = n())  })
  
  terminatedPlot <- reactive({
    req(terminated())
    terminated() %>%
      distinct(Program, .keep_all = TRUE) %>%
      dplyr::select(Program, `Program Total`)  })
  
  monitored <- reactive({
    filter(regionalData, `Status Code` %in% c("Effective")) %>%
      group_by(Program) %>%
      mutate(`Program Total` = n()) })
  
  
  monitoredPlot <- reactive({
    req(monitored())
    monitored() %>%
      distinct(Program, .keep_all = TRUE) %>%
      dplyr::select(Program, `Program Total`)  })
  
  output$casesPending <- renderPlotly({
    req(pendingPlot())
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
      layout(yaxis = list(title = 'Count'),
             xaxis = list(title = 'Program Name'), 
             barmode = 'stack')
  })
  
  output$casesPendingTable <- DT::renderDataTable({
    req(pendingPlot())
    DT::datatable(pendingPlot(), rownames = FALSE, options = list(dom = 't'))  })
  
  output$raw_casesPendingTable <- DT::renderDataTable({
    req(pending())
    DT::datatable(pending(), rownames = FALSE, extensions = 'Buttons',  
                  options = list(dom = 'Bft', scrollX = TRUE, scrollY = "125px", autoWidth = TRUE,
                                 buttons=list('copy',
                                              list(extend='csv',filename=paste('pendingConsent_',unique(pending()$`Facility Region`),'_',
                                                                               unique(pending()$pullDate),'.csv',sep='')))))  })
  
  
  output$casesTerminated <- renderPlotly({
    req(terminatedPlot())
    plot_ly(data = terminatedPlot()) %>%
      add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Program Total: ", `Program Total`))) %>%
      layout(yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Program Name'))  })
  
  output$casesTerminatedTable <- DT::renderDataTable({
    req(terminatedPlot())
    DT::datatable(terminatedPlot(), rownames = FALSE, options = list(dom = 't'))  })
  
  output$raw_casesTerminatedTable <- DT::renderDataTable({
    req(terminated())
    DT::datatable(terminated(), rownames = FALSE, extensions = 'Buttons',  
                  options = list(dom = 'Bft', scrollX = TRUE, scrollY = "125px", autoWidth = TRUE,
                                 buttons=list('copy',
                                              list(extend='csv',filename=paste('terminatedConsent_',unique(terminated()$`Facility Region`),'_',
                                                                               unique(terminated()$pullDate),'.csv',sep='')))))  })
  
  output$casesMonitored <- renderPlotly({
    req(terminatedPlot())
    plot_ly(data = monitoredPlot()) %>%
      add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Program Total: ", `Program Total`))) %>%
      layout(yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Program Name')) })
  
  output$casesMonitoredTable <- DT::renderDataTable({
    req(monitoredPlot())
    DT::datatable(monitoredPlot(), rownames = FALSE, options = list(dom = 't'))  })
  
  output$raw_casesMonitoredTable <- DT::renderDataTable({
    req(monitored())
    DT::datatable(monitored(), rownames = FALSE, extensions = 'Buttons', 
                  options = list(dom = 'Bft', scrollX = TRUE, scrollY = "125px", autoWidth = TRUE,
                                 buttons=list('copy',
                                              list(extend='csv',filename=paste('monitoredConsent_',unique(monitored()$`Facility Region`),'_',
                                                                               unique(monitored()$pullDate),'.csv',sep='')))))  })
  
}