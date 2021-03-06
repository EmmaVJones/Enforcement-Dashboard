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