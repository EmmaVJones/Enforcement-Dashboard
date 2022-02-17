# Modules to view statewide by region and then by program


pendingStatewideRegionUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Pending By Region"), height = 400,
        plotlyOutput(ns("pendingPlot"))
    )
  )
}


pendingStatewideRegion <- function(input, output, session, fullDataPull){
  output$pendingPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull %>%
      filter(`Status Code` %in% c("Pending", "Pending Consent", "Pending APA")) %>%
      mutate(`Pending Status` = as.factor(case_when(`Days Pending` >= 365 ~ "Active > 365", 
                                                    TRUE ~ "Active < 365"))) %>%
      group_by(`Facility Region`, `Pending Status`) %>%
      summarize(`Pending Category Count` = n()) %>%
      pivot_wider(names_from = `Pending Status`, values_from =`Pending Category Count`) %>%
      ungroup()%>%
      bind_rows(tibble(`Facility Region` = NA, `Active < 365` = NA, `Active > 365` = NA)) %>% # add in template in case one group not there
      group_by(`Facility Region`) %>%
      mutate(`Total Pending` = sum(`Active < 365`, `Active > 365`, na.rm = TRUE)) %>%
      plot_ly() %>%
      add_trace(x = ~`Facility Region`, y = ~`Active < 365`, type = 'bar', name = 'Active < 365',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Region Name: ",`Facility Region`),
                                             paste("Active < 365: ",`Active < 365`),
                                             paste("Regional Total: ", `Total Pending`))) %>%
      add_trace(x = ~`Facility Region`, y = ~`Active > 365`, name = "Active > 365",
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Region Name: ",`Facility Region`),
                                             paste("Active > 365: ",`Active > 365`),
                                             paste("Regional Total: ", `Total Pending`))) %>%
      layout(height = 300,
             yaxis = list(title = 'Count'),
             xaxis = list(title = 'Region Name'), 
             barmode = 'stack')
  })
}

pendingStatewideMediaUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Pending By Media"), height = 400,
        plotlyOutput(ns("pendingPlot"))
    )
  )
}


pendingStatewideMedia <- function(input, output, session, fullDataPull){
  output$pendingPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull %>%
      filter(`Status Code` %in% c("Pending", "Pending Consent", "Pending APA")) %>%
      mutate(`Pending Status` = as.factor(case_when(`Days Pending` >= 365 ~ "Active > 365", 
                                                    TRUE ~ "Active < 365"))) %>%
      group_by(Program, `Pending Status`) %>%
      summarize(`Pending Category Count` = n()) %>%
      pivot_wider(names_from = `Pending Status`, values_from =`Pending Category Count`) %>%
      ungroup()%>%
      bind_rows(tibble(Program = NA, `Active < 365` = NA, `Active > 365` = NA)) %>% # add in template in case one group not there
      group_by(Program) %>%
      mutate(`Total Pending` = sum(`Active < 365`, `Active > 365`, na.rm = TRUE)) %>%
      plot_ly() %>%
      add_trace(x = ~Program, y = ~`Active < 365`, type = 'bar', name = 'Active < 365',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Active < 365: ",`Active < 365`),
                                             paste("Program Total: ", `Total Pending`))) %>%
      add_trace(x = ~Program, y = ~`Active > 365`, name = "Active > 365",
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Active > 365: ",`Active > 365`),
                                             paste("Program Total: ", `Total Pending`))) %>%
      layout(height = 300,
             yaxis = list(title = 'Count'),
             xaxis = list(title = 'Program Name'), 
             barmode = 'stack')
  })
}





monitoredStatewideRegionUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Monitored By Region"), height = 400,
        plotlyOutput(ns("monitoredPlot"))
    )
  )
}


monitoredStatewideRegion <- function(input, output, session, fullDataPull){
  output$monitoredPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull%>%
      filter(`Status Code` %in% c("Effective")) %>%
      group_by(`Facility Region`) %>% 
      summarize(`Region Total` = n()) %>%
      plot_ly() %>%
      add_trace(x = ~`Facility Region`, y = ~`Region Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Region Name: ",`Facility Region`),
                                             paste("Region Total: ", `Region Total`))) %>%
      layout(height = 300,
             yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Region Name'))
  })
}


monitoredStatewideMediaUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Monitored By Media"), height = 400,
        plotlyOutput(ns("monitoredPlot"))
    )
  )
}


monitoredStatewideMedia <- function(input, output, session, fullDataPull){
  output$monitoredPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull %>%
      filter(`Status Code` %in% c("Effective")) %>%
      group_by(Program) %>% #View()
      summarize(`Program Total` = n()) %>% # View()
      plot_ly() %>%
      add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Program Total: ", `Program Total`))) %>%
      layout(height = 300,
             yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Program Name'))  })
}







enforcementActionsStatewideRegionUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Enforcement Actions By Region"), height = 400,
        plotlyOutput(ns("enforcementActionsPlot"))
    )
  )
}


enforcementActionsStatewideRegion <- function(input, output, session, fullDataPull){
  output$enforcementActionsPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull%>%
      filter(`Status Code` %in% c("Effective")) %>%
      group_by(`Facility Region`) %>% 
      summarize(`Region Total` = n()) %>%
      plot_ly() %>%
      add_trace(x = ~`Facility Region`, y = ~`Region Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Region Name: ",`Facility Region`),
                                             paste("Region Total: ", `Region Total`))) %>%
      layout(height = 300,
             yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Region Name'))
  })
}


enforcementActionsStatewideMediaUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Enforcement Actions By Media"), height = 400,
        plotlyOutput(ns("enforcementActionsPlot"))
    )
  )
}


enforcementActionsStatewideMedia <- function(input, output, session, fullDataPull){
  output$enforcementActionsPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull %>%
      filter(`Status Code` %in% c("Effective")) %>%
      group_by(Program) %>% #View()
      summarize(`Program Total` = n()) %>% # View()
      plot_ly() %>%
      add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Program Total: ", `Program Total`))) %>%
      layout(height = 300,
             yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Program Name'))  })
}










terminatedStatewideRegionUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Terminated (In The Last 12 Months) By Region"), height = 400,
        plotlyOutput(ns("terminatedPlot"))
    )
  )
}


terminatedStatewideRegion <- function(input, output, session, fullDataPull){
  output$terminatedPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull %>%
      filter(`Term Date` >= (pullDate %m-% months(12)), # filter within 12 months from pull date
             `Status Code` %in% c("Terminated")) %>%
      group_by(`Facility Region`) %>% 
      summarize(`Region Total` = n()) %>%
      plot_ly() %>%
      add_trace(x = ~`Facility Region`, y = ~`Region Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Region Name: ",`Facility Region`),
                                             paste("Region Total: ", `Region Total`))) %>%
      layout(height = 300,
             yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Region Name'))
  })
}





terminatedStatewideMediaUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Terminated (In The Last 12 Months) By Media"), height = 400,
        plotlyOutput(ns("terminatedPlot"))
    )
  )
}


terminatedStatewideMedia <- function(input, output, session, fullDataPull){
  output$terminatedPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull %>%
      filter(`Term Date` >= (pullDate %m-% months(12)) & # filter within 12 months from pull date
               `Status Code` %in% c("Terminated")) %>%
      group_by(Program) %>% # View()
      summarize(`Program Total` = n()) %>%# View()
      plot_ly() %>%
      add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Program Total: ", `Program Total`))) %>%
      layout(height = 300,
             yaxis = list(title = 'Count',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Program Name'))
    
    
  })
}




## New cash civil charges modules


cashTotalsRegionUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cash Civil Charges By Region"), height = 400,
        plotlyOutput(ns("cashPlot"))
    )
  )
}


cashTotalsRegion <- function(input, output, session, fullDataPull){
  output$cashPlot <- renderPlotly({
    req(fullDataPull)
    civilCharges <- fullDataPull %>%
      group_by(`Facility Region`) %>%
      mutate(`Regional Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE))
    # basic plot
    civilCharges %>%
      distinct(`Facility Region`, .keep_all = TRUE) %>% #View()
      plot_ly() %>%
      add_trace(x = ~`Facility Region`, y = ~`Regional Total Cash Civil Charges`, type = 'bar', name = 'Regional Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Region Name: ",`Facility Region`),
                                             paste("Regional Total: $", 
                                                   prettyNum(`Regional Total Cash Civil Charges`, 
                                                             big.mark = ',',scientific=FALSE)))) %>%
      layout(height = 275,
             yaxis = list(title = 'Total (Dollars)',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Region Name'))
  })
}





cashTotalsMediaUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cash Civil Charges By Media"), height = 400,
        plotlyOutput(ns("cashPlot"))
    )
  )
}


cashTotalsMedia <- function(input, output, session, fullDataPull){
  output$cashPlot <- renderPlotly({
    req(fullDataPull)
    civilCharges <- fullDataPull %>%
      group_by(Program) %>%
      mutate(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE))
    # basic plot
    civilCharges %>%
      distinct(Program, .keep_all = TRUE) %>% #View()
      plot_ly() %>%
      add_trace(x = ~Program, y = ~`Program Total Cash Civil Charges`, type = 'bar', name = 'Program Total',
                hoverinfo="text",text=~paste(sep="<br>",
                                             paste("Program Name: ",Program),
                                             paste("Program Total: $", 
                                                   prettyNum(`Program Total Cash Civil Charges`, 
                                                             big.mark = ',',scientific=FALSE)))) %>%
      layout(height = 275,
             yaxis = list(title = 'Total (Dollars)',
                          tickformat=',d'), # make integer y axis
             xaxis = list(title = 'Program Name'))
    
    
  })
}












FTEdeficitStatewideUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Referral Rate FTE Deficit by Region"), height = 400,
        plotlyOutput(ns("referralRatePlot"))),
    box(title = strong("Current FTE Deficit by Region"), height = 400,
        plotlyOutput(ns("currentPlot")))
  )
}


FTEdeficitStatewide <- function(input, output, session, fullDataPull, orgData, referral){
  statewide <- reactive({
    req(fullDataPull)
    fullDataPull %>%
      filter(`Status Code` %in% c("Pending", "Pending Consent", "Pending APA")) %>%
      group_by(`Facility Region`) %>%
      summarise(`Pending Total` = n()) %>%
      left_join(orgData %>%
                  group_by(Region) %>%
                  summarise(`Region Capacity` = sum(Capacity)), by = c('Facility Region' = 'Region')) %>%
      left_join(referral, by = c('Facility Region' = 'Region')) %>%
      mutate(`Referral FTE Deficit` = (`Region Capacity` - `Referral Rate`)/15 , 
             `Current FTE Deficit` = (`Region Capacity` - `Pending Total`)/15, 
             `Current FTE Deficit Color` = ifelse(`Current FTE Deficit` < 0, "#ff5733","#1E90FF"))  })
  
  output$referralRatePlot <- renderPlotly({
    req(statewide())
    plot_ly(data = statewide(), x = ~`Facility Region`, y = ~`Referral FTE Deficit`, type = 'bar',
            color = ~`Referral FTE Deficit` < 0, colors = c("#1E90FF", "#ff5733"), name = ~ifelse(`Referral FTE Deficit` < 0, "< 0", "> 0"),
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region: ",`Facility Region`),
                                         paste("Referral FTE Deficit: ",format(`Referral FTE Deficit`, digits = 2)))) %>%
      layout(height = 300,
             yaxis = list(title = 'Referral FTE Deficit'),
             xaxis = list(title = 'Region'))  })
  
  output$currentPlot <- renderPlotly({
    req(statewide())
    plot_ly(data = statewide(), x = ~`Facility Region`, y = ~`Current FTE Deficit`, type = 'bar', 
            #color = ~`Current FTE Deficit` < 0, colors = c("#1E90FF", "#ff5733"), name = ~ifelse(`Current FTE Deficit` < 0, "< 0", "> 0"),
            #color = ~`Current FTE Deficit Color`, colors = ~`Current FTE Deficit Color`,
            color = ~`Current FTE Deficit` < 0, colors = c("#1E90FF", "#ff5733"),# ~`Current FTE Deficit Color`,
            name = ~ifelse(`Current FTE Deficit` < 0, "< 0", "> 0"),
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region: ",`Facility Region`),
                                         paste("Current FTE Deficit: ",format(`Current FTE Deficit`, digits = 2)))) %>%
      layout(height = 300,
             yaxis = list(title = 'Current FTE Deficit'),
             xaxis = list(title = 'Region'))   })
}







cFTEdeficitStatewideUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Current FTE Deficit by Region"), height = 400,
        plotlyOutput(ns("currentPlot"))
    )
  )
}