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




# Pending counts by region
dat %>%
  filter(`Status Code` %in% c("Pending", "Pending Consent")) %>%
  mutate(`Pending Status` = as.factor(case_when(`Days Pending` >= 365 ~ "Active > 365", 
                                                TRUE ~ "Active < 365"))) %>%
  group_by(`Facility Region`, `Pending Status`) %>%
  summarize(`Pending Category Count` = n()) %>%
  pivot_wider(names_from = `Pending Status`, values_from =`Pending Category Count`) %>%
  ungroup()%>%
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
  layout(height = 250,
         yaxis = list(title = 'Count'),
         xaxis = list(title = 'Region Name'), 
         barmode = 'stack')


# monitored counts by region
dat %>%
  filter(`Status Code` %in% c("Effective")) %>%
  group_by(`Facility Region`) %>% 
  summarize(`Region Total` = n()) %>%
  plot_ly() %>%
  add_trace(x = ~`Facility Region`, y = ~`Region Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region Name: ",`Facility Region`),
                                         paste("Region Total: ", `Region Total`))) %>%
  layout(yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Region Name'))


# terminated counts by region
dat %>%
  filter(`Term Date` >= (pullDate %m-% months(12)), # filter within 12 months from pull date
         `Status Code` %in% c("Terminated")) %>%
  group_by(`Facility Region`) %>% 
  summarize(`Region Total` = n()) %>%
  plot_ly() %>%
  add_trace(x = ~`Facility Region`, y = ~`Region Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region Name: ",`Facility Region`),
                                         paste("Region Total: ", `Region Total`))) %>%
  layout(yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Region Name'))




# monitored by region

View(dat %>% 
  filter(`Status Code` %in% c("Effective")) %>%
  group_by(`Facility Region`) %>% 
  mutate(`Program Total` = n()) %>%
  distinct(`Facility Region`, .keep_all = TRUE))
  
dat %>%
  filter(`Status Code` %in% c("Effective")) %>%
  group_by(`Facility Region`) %>% 
  mutate(`Program Total` = n()) %>%
  distinct(`Facility Region`, .keep_all = TRUE)%>%
  plot_ly() %>%
  add_trace(x = ~`Facility Region`, y = ~`Program Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region: ",`Facility Region`),
                                         paste("Program Total: ", `Program Total`))) %>%
  layout(yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name'))








################################################################################################################



# FTE deficit calculations




regionalData <- filter(dat, `Facility Region` == "TRO")

OrgData <- filter(OrgData, `Region` == "TRO")

Referral <- filter(Referral, `Region`=="TRO")

pending <- filter(regionalData, `Status Code` %in% c("Pending", "Pending Consent")) %>%
  mutate(`Pending Status` = as.factor(case_when(`Days Pending` >= 365 ~ "Active > 365", 
                                                TRUE ~ "Active < 365"))) 


CaseCap <- OrgData%>%
  summarise("RegionalCase"=sum(`Capacity`))

pendingTotal <- pending %>%
  nrow()

Current <- format(round(((CaseCap- pendingTotal)/ 15), 2), nsmall = 2)

RefFTE<- format(round(((CaseCap- Referral$`Referral Rate`)/ 15),2),nsmall=2)


###############################################################################

# statewide comparison

statewideFTEbreakdown <- dat %>%
  filter(`Status Code` %in% c("Pending", "Pending Consent")) %>%
  group_by(`Facility Region`) %>%
  summarise(`Pending Total` = n()) %>%
  left_join(OrgData %>%
              group_by(Region) %>%
              summarise(`Region Capacity` = sum(Capacity)), by = c('Facility Region' = 'Region')) %>%
  left_join(Referral, by = c('Facility Region' = 'Region')) %>%
  mutate(`Referral FTE Deficit` = (`Region Capacity` - `Referral Rate`)/15 , 
         `Current FTE Deficit` = (`Region Capacity` - `Pending Total`)/15)

# stacked, might be a bit much for users
plot_ly(data = statewideFTEbreakdown, x = ~`Facility Region`, y = ~`Referral FTE Deficit`, type = 'bar', name = 'Referral FTE Deficit',
        hoverinfo="text",text=~paste(sep="<br>",
                                     paste("Region: ",`Facility Region`),
                                     paste("Referral FTE Deficit: ",`Referral FTE Deficit`))) %>%
  add_trace(y = ~`Current FTE Deficit`, name = "Current FTE Deficit",
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region: ",`Facility Region`),
                                         paste("Current FTE Deficit: ",`Current FTE Deficit`))) %>%
  layout(height = 250,
         yaxis = list(title = 'Referral FTE Deficit'),
         xaxis = list(title = 'Region'), 
         barmode = 'stack')


# color bars by good/bad

plot_ly(data = statewideFTEbreakdown, x = ~`Facility Region`, y = ~`Referral FTE Deficit`, type = 'bar',
        color = ~`Referral FTE Deficit` < 0, colors = c("#1E90FF", "#ff5733"), name = ~ifelse(`Referral FTE Deficit` < 0, "< 0", "> 0"),
        hoverinfo="text",text=~paste(sep="<br>",
                                     paste("Region: ",`Facility Region`),
                                     paste("Referral FTE Deficit: ",`Referral FTE Deficit`))) %>%
  layout(height = 250,
         yaxis = list(title = 'Referral FTE Deficit'),
         xaxis = list(title = 'Region'))


plot_ly(data = statewideFTEbreakdown, x = ~`Facility Region`, y = ~`Current FTE Deficit`, type = 'bar', 
        color = ~`Current FTE Deficit` < 0, colors = c("#1E90FF", "#ff5733"), name = ~ifelse(`Current FTE Deficit` < 0, "< 0", "> 0"),
        hoverinfo="text",text=~paste(sep="<br>",
                                     paste("Region: ",`Facility Region`),
                                     paste("Current FTE Deficit: ",`Current FTE Deficit`))) %>%
  layout(height = 250,
         yaxis = list(title = 'Current FTE Deficit'),
         xaxis = list(title = 'Region')) 



