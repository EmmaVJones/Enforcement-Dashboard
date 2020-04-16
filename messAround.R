library(tidyverse)
library(plotly)
library(lubridate)
#library(readxl)

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

# PRO example, cases pending plot
PRO <- filter(dat, `Facility Region` == "PRO")

pending <- filter(PRO, `Status Code` %in% c("Pending", "Pending Consent")) %>%
  mutate(`Pending Status` = as.factor(case_when(`Days Pending` >= 365 ~ "Active > 365", 
                                                TRUE ~ "Active < 365")))

programTotals <- pending %>%
  group_by(Program) %>%
  summarize(`Program Total` = n())

pendingPlot <- pending %>%
  group_by(`Pending Status`, Program) %>%
  summarize(Total = n()) %>%
  left_join(programTotals, by = "Program") %>%
  pivot_wider(names_from = `Pending Status`, values_from = Total)



plot_ly(data = pendingPlot, x = ~Program, y = ~`Active < 365`, type = 'bar', name = 'Active < 365',
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

# table beneath
pendingPlot %>%
  pivot_longer(-Program, names_to = "Pending Status", values_to = "Total") %>% 
  pivot_wider(names_from = Program, values_from = Total) %>%
  DT::datatable(rownames = FALSE, options = list(dom = 't'))









# PRO example, cases monitored plot

monitored <- filter(PRO, `Status Code` %in% c("Effective")) %>%
  group_by(Program) %>%
  mutate(`Program Total` = n())
# save detailed info for drill down later?

monitored %>%
  distinct(Program, .keep_all = TRUE) %>%
  dplyr::select(Program, `Program Total`) %>%
  plot_ly() %>%
  add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Program Name: ",Program),
                                         paste("Program Total: ", `Program Total`))) %>%
  layout(yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name'))

monitored %>%
  distinct(Program, .keep_all = TRUE) %>%
  dplyr::select(Program, `Program Total`) %>%
  plot_ly() %>%
  add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
            text = ~`Program Total`, 
            #texttemplate ='%{y:.2s}', 
            textposition = 'outside',
            #hoverinfo = 'text',
            hovertemplate = paste("Program Name: %{x}"),
            showlegend = FALSE)%>%
            #hovertemplate = ~paste(sep="<br>",
            #                       paste("Program Name: ",Program),
            #                       paste("Program Total: ", `Program Total`))) %>%
            
            #hoverinfo="text",text=~paste(sep="<br>",
            #                             paste("Program Name: ",Program),
            #                             paste("Program Total: ", `Program Total`))) %>%
  layout(yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name')) 





## Cases terminated

terminated <- filter(PRO, 
                    `Term Date` >= (pullDate %m-% months(12)), # filter within 12 months from pull date
                    `Status Code` %in% c("Terminated")) %>%
  group_by(Program) %>%
  mutate(`Program Total` = n())
# save detailed info for drill down later?


terminated %>%
  distinct(Program, .keep_all = TRUE) %>%
  dplyr::select(Program, `Program Total`) %>%
  plot_ly() %>%
  add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Program Name: ",Program),
                                         paste("Program Total: ", `Program Total`))) %>%
  layout(yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name'))






## Statewide View- mess with plots and metrics to best tell top level story


#### Civil charges by region

civilCharges <- dat %>%
  group_by(`Facility Region`) %>%
  mutate(`Regional Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE))
# save detailed info for drill down later?

civilCharges %>%
  distinct(`Facility Region`, .keep_all = TRUE) %>%
  plot_ly() %>%
  add_trace(x = ~`Facility Region`, y = ~`Regional Total Cash Civil Charges`, type = 'bar', name = 'Regional Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region Name: ",`Facility Region`),
                                         paste("Regional Total: $", 
                                               prettyNum(`Regional Total Cash Civil Charges`, 
                                                         big.mark = ',',scientific=FALSE)))) %>%
  layout(yaxis = list(title = 'Total (Dollars)',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Region Name'))

# drill down
filter(civilCharges, `Facility Region` == 'BRRO') %>% # this is chosen by user interactively
  group_by(Program) %>%
  mutate(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>%
  distinct(`Facility Region`, .keep_all = TRUE) %>%
  #summarize(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>%
  plot_ly() %>%
  add_trace(x = ~Program, y = ~`Program Total Cash Civil Charges`, type = 'bar', name = 'Regional Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Program Name: ",Program),
                                         paste("Regional Total: $", 
                                               prettyNum(`Program Total Cash Civil Charges`, 
                                                         big.mark = ',',scientific=FALSE)))) %>%
  layout(yaxis = list(title = 'Total (Dollars)',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name'))


# build into drill down
library(shiny)

# Cash Totals drill down plot
cashTotalsUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(plotlyOutput(ns("cashPlot")), 
        uiOutput(ns("back")),
        verbatimTextOutput(ns('test')))
  )
}

cashTotals <- function(input, output, session, dat){
  selections <- reactiveVal()
  
  output$test <- renderPrint({event_data("plotly_click")$x})
  
  civilCharges <- reactive({
    dat %>%
      group_by(`Facility Region`) %>%
      mutate(`Regional Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) })
  
  # show cash totals by region by default, but if there is a selected region
  # show cash totals by program within that region
  output$cashPlot <- renderPlotly({
    nSelections <- length(selections())
    if (nSelections == 0) {
      civilCharges() %>%
        distinct(`Facility Region`, .keep_all = TRUE) %>%
        plot_ly() %>%
        add_trace(x = ~`Facility Region`, y = ~`Regional Total Cash Civil Charges`, type = 'bar', name = 'Regional Total',
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Region Name: ",`Facility Region`),
                                               paste("Regional Total: $", 
                                                     prettyNum(`Regional Total Cash Civil Charges`, 
                                                               big.mark = ',',scientific=FALSE)))) %>%
        layout(yaxis = list(title = 'Total (Dollars)',
                            tickformat=',d'), # make integer y axis
               xaxis = list(title = 'Region Name'))
    } else {
      filter(civilCharges(), `Facility Region` %in% selections()) %>% # this is chosen by user interactively
        group_by(Program) %>%
        mutate(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>%
        distinct(`Facility Region`, .keep_all = TRUE) %>%
        #summarize(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>% # can't just summarize or will lose region name
        plot_ly() %>%
        add_trace(x = ~Program, y = ~`Program Total Cash Civil Charges`, type = 'bar', name = 'Regional Total',
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Region Name: ",`Facility Region`),
                                               paste("Program Name: ",Program),
                                               paste("Program Total: $", 
                                                     prettyNum(`Program Total Cash Civil Charges`, 
                                                               big.mark = ',',scientific=FALSE)))) %>%
        layout(yaxis = list(title = 'Total (Dollars)',
                            tickformat=',d'), # make integer y axis
               xaxis = list(title = 'Program Name'))
      
    }
    
    
  })
  
  observeEvent(event_data("plotly_click"), {
    new <- event_data("plotly_click")$x
    old <- selections()
    selections(c(old, new))
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(selections())) 
      actionButton(session$ns("clear"), "Back", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, selections(NULL))
}

ui <- fluidPage(
  cashTotalsUI('cashMoney'))

server <- function(input, output, session) {
  callModule(cashTotals, 'cashMoney', dat)
}


shinyApp(ui, server)
    


ui <- fluidPage(plotlyOutput("cashPlot"), uiOutput("back"))

server <- function(input, output, session) {
  
  selections <- reactiveVal()
  
  output$test <- renderPrint({event_data("plotly_click")$x})
  
  civilCharges <- reactive({
    dat %>%
    group_by(`Facility Region`) %>%
    mutate(`Regional Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) })
  
  # show cash totals by region by default, but if there is a selected region
  # show cash totals by program within that region
  output$cashPlot <- renderPlotly({
    nSelections <- length(selections())
    if (nSelections == 0) {
      civilCharges() %>%
        distinct(`Facility Region`, .keep_all = TRUE) %>%
        plot_ly() %>%
        add_trace(x = ~`Facility Region`, y = ~`Regional Total Cash Civil Charges`, type = 'bar', name = 'Regional Total',
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Region Name: ",`Facility Region`),
                                               paste("Regional Total: $", 
                                                     prettyNum(`Regional Total Cash Civil Charges`, 
                                                               big.mark = ',',scientific=FALSE)))) %>%
        layout(yaxis = list(title = 'Total (Dollars)',
                            tickformat=',d'), # make integer y axis
               xaxis = list(title = 'Region Name'))
    } else {
      filter(civilCharges(), `Facility Region` %in% selections()) %>% # this is chosen by user interactively
        group_by(Program) %>%
        mutate(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>%
        distinct(`Facility Region`, .keep_all = TRUE) %>%
        #summarize(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>% # can't just summarize or will lose region name
        plot_ly() %>%
        add_trace(x = ~Program, y = ~`Program Total Cash Civil Charges`, type = 'bar', name = 'Regional Total',
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Region Name: ",`Facility Region`),
                                               paste("Program Name: ",Program),
                                               paste("Program Total: $", 
                                                     prettyNum(`Program Total Cash Civil Charges`, 
                                                               big.mark = ',',scientific=FALSE)))) %>%
        layout(yaxis = list(title = 'Total (Dollars)',
                            tickformat=',d'), # make integer y axis
               xaxis = list(title = 'Program Name'))
      
    }
    
    
  })
  
  observeEvent(event_data("plotly_click"), {
    new <- event_data("plotly_click")$x
    old <- selections()
    selections(c(old, new))
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(selections())) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, selections(NULL))
}

shinyApp(ui, server)

