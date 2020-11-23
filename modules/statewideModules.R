# Modules to view statewide by region and then by program


pendingStatewideUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Pending By Region"), height = 400,
        plotlyOutput(ns("pendingPlot"))
    )
  )
}


pendingStatewide <- function(input, output, session, fullDataPull){
  output$pendingPlot <- renderPlotly({
    req(fullDataPull)
    fullDataPull %>%
      filter(`Status Code` %in% c("Pending", "Pending Consent")) %>%
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


monitoredStatewideUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Monitored By Region"), height = 400,
        plotlyOutput(ns("monitoredPlot"))
    )
  )
}


monitoredStatewide <- function(input, output, session, fullDataPull){
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

terminatedStatewideUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = strong("Cases Terminated By Region"), height = 400,
        plotlyOutput(ns("terminatedPlot"))
    )
  )
}


terminatedStatewide <- function(input, output, session, fullDataPull){
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
      filter(`Status Code` %in% c("Pending", "Pending Consent")) %>%
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
            color = ~`Current FTE Deficit Color`, colors = ~`Current FTE Deficit Color`,
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