
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(pool)
library(config)
library(dbplyr)

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


# Bring in hard data
OrgData<- read_csv('LogiPulls/OrgCharts.csv') # mostly static, needs to be reviewed annually by enf program for accuracy

Referral<- read_csv('LogiPulls/ReferralRates.csv') # mostly static, needs to be reviewed annually by enf program for accuracy
# this could be calculated in time with guidance from enforcement program



#### Production Environment For testing
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)



# Bring in live data from ODS
# Facilities Data (for joining)
enfFacilities <- pool %>% tbl(in_schema("enf",  "Enf_Facilities_View")) %>%
  as_tibble()

# get start of year filter information
startOfYear <- as.Date(paste0(year(Sys.Date())-1, '-01-01')) # use 2019 as starting point

# Case data
dat <- pool %>% tbl(in_schema("enf",  "Enf_enforcement_Cases_View")) %>%
 # filter(between(Enc_Executed_Date, !! startOfYear, !! Sys.Date()) |
#           between(Enc_Nov_Date, !! startOfYear, !! Sys.Date()) |
#           between(Enc_Terminated_Date, !! startOfYear, !! Sys.Date()) ) %>%
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
# 

### Statewide View

# Cases Pending by Region
dat %>%
  filter(`Status Code` %in% c("Pending", "Pending Consent","Pending APA")) %>%
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


# Cases Pending by Media
dat %>%
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



# Cases Monitored by Region
dat %>%
  filter(`Status Code` %in% c("Effective")) %>%
  group_by(`Facility Region`) %>% #View()
  summarize(`Region Total` = n()) %>% # View()
  plot_ly() %>%
  add_trace(x = ~`Facility Region`, y = ~`Region Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region Name: ",`Facility Region`),
                                         paste("Region Total: ", `Region Total`))) %>%
  layout(height = 300,
         yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Region Name'))


# Cases Monitored by Media
dat %>%
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
         xaxis = list(title = 'Program Name'))

## Enforcement Actions by Region
dat %>%
  group_by(`Facility Region`) %>% #View()
  summarize(`Region Total` = n()) %>% # View()
  plot_ly() %>%
  add_trace(x = ~`Facility Region`, y = ~`Region Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region Name: ",`Facility Region`),
                                         paste("Region Total: ", `Region Total`))) %>%
  layout(height = 300,
         yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Region Name'))


## Enforcement Actions by Media
dat %>%
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
         xaxis = list(title = 'Program Name'))








## Cases Terminated By Region (in the last 12 months from pull date)
dat %>%
  filter(`Term Date` >= (pullDate %m-% months(12)) & # filter within 12 months from pull date
           `Status Code` %in% c("Terminated")) %>%
  group_by(`Facility Region`) %>% # View()
  summarize(`Region Total` = n()) %>%# View()
  plot_ly() %>%
  add_trace(x = ~`Facility Region`, y = ~`Region Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region Name: ",`Facility Region`),
                                         paste("Region Total: ", `Region Total`))) %>%
  layout(height = 300,
         yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Region Name'))


## Cases Terminated By Media (in the last 12 months from pull date)
dat %>%
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




## Cash civil charges by Region module
civilCharges <- dat %>%
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
# dialed in Plot
selections <- c('BRRO','NRO', 'PRO', 'SWRO', 'TRO', 'VRO')[1]

filter(civilCharges, `Facility Region` %in% selections) %>% # this is chosen by user interactively
  group_by(Program) %>%
  mutate(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>%
  distinct(`Facility Region`, .keep_all = TRUE) %>% #View()
  #summarize(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>% # can't just summarize or will lose region name
  plot_ly() %>%
  add_trace(x = ~Program, y = ~`Program Total Cash Civil Charges`, type = 'bar', name = 'Regional Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region Name: ",`Facility Region`),
                                         paste("Program Name: ",Program),
                                         paste("Program Total: $", 
                                               prettyNum(`Program Total Cash Civil Charges`, 
                                                         big.mark = ',',scientific=FALSE)))) %>%
  layout(height = 250,
         yaxis = list(title = 'Total (Dollars)',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name'))






## Cash civil charges by Media module
civilCharges <- dat %>%
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
# dialed in Plot
selections <- c('Air', 'AST/UST', 'Other', 'VPDES', 'VWP', 'Waste')[1]

filter(civilCharges, Program %in% selections) %>% # this is chosen by user interactively
  group_by(`Facility Region`) %>%
  mutate(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>%
  distinct(`Facility Region`, .keep_all = TRUE) %>% #View()
  #summarize(`Program Total Cash Civil Charges` = sum(`Cash Civil Charge`, na.rm = TRUE)) %>% # can't just summarize or will lose region name
  plot_ly() %>%
  add_trace(x = ~`Facility Region`, y = ~`Program Total Cash Civil Charges`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Region Name: ",Program),
                                         paste("Program Name: ",Program),
                                         paste("Program Total: $", 
                                               prettyNum(`Program Total Cash Civil Charges`, 
                                                         big.mark = ',',scientific=FALSE)))) %>%
  layout(height = 250,
         yaxis = list(title = 'Total (Dollars)',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name'))











# ### Statewide Module: Referral Rate FTE Deficit by Region
# ## dataset used by Referral Rate FTE Deficit by Region and Current FTE Deficit by Region
# statewide <- dat %>% #fullDataPull %>%
#     filter(`Status Code` %in% c("Pending", "Pending Consent", "Pending APA")) %>%
#     group_by(`Facility Region`) %>%
#     summarise(`Pending Total` = n()) %>%
#     left_join(OrgData %>% #orgData %>%
#                 group_by(Region) %>%
#                 summarise(`Region Capacity` = sum(Capacity)), by = c('Facility Region' = 'Region')) %>%
#     left_join(Referral,#referral,
#               by = c('Facility Region' = 'Region')) %>%
#     mutate(`Referral FTE Deficit` = (`Region Capacity` - `Referral Rate`)/15 , 
#            `Current FTE Deficit` = (`Region Capacity` - `Pending Total`)/15, 
#            `Current FTE Deficit Color` = ifelse(`Current FTE Deficit` < 0, "#ff5733","#1E90FF"))
# 
# ### Statewide Module: Referral Rate FTE Deficit by Region Plot
# plot_ly(data = statewide, x = ~`Facility Region`, y = ~`Referral FTE Deficit`, type = 'bar',
#           color = ~`Referral FTE Deficit` < 0, colors = c("#1E90FF", "#ff5733"), name = ~ifelse(`Referral FTE Deficit` < 0, "< 0", "> 0"),
#           hoverinfo="text",text=~paste(sep="<br>",
#                                        paste("Region: ",`Facility Region`),
#                                        paste("Referral FTE Deficit: ",format(`Referral FTE Deficit`, digits = 2)))) %>%
#     layout(height = 300,
#            yaxis = list(title = 'Referral FTE Deficit'),
#            xaxis = list(title = 'Region'))  

# ### Statewide Module: Current FTE Deficit By Region Plot
# plot_ly(data = statewide, x = ~`Facility Region`, y = ~`Current FTE Deficit`, type = 'bar', 
#           #color = ~`Current FTE Deficit` < 0, colors = c("#1E90FF", "#ff5733"), name = ~ifelse(`Current FTE Deficit` < 0, "< 0", "> 0"),
#         #color = ~`Current FTE Deficit Color`, colors = ~`Current FTE Deficit Color`,
#         color = ~`Current FTE Deficit` < 0, colors = c("#1E90FF", "#ff5733"),# ~`Current FTE Deficit Color`,
#           name = ~ifelse(`Current FTE Deficit` < 0, "< 0", "> 0"),
#           hoverinfo="text",text=~paste(sep="<br>",
#                                        paste("Region: ",`Facility Region`),
#                                        paste("Current FTE Deficit: ",format(`Current FTE Deficit`, digits = 2)))) %>%
#     layout(height = 300,
#            yaxis = list(title = 'Current FTE Deficit'),
#            xaxis = list(title = 'Region'))  
# 


### Region Module 

regionalData <- filter(dat, `Facility Region` == "BRRO") # PRO



######## Workload summary box
pending <-  filter(regionalData, `Status Code` %in% c("Pending", "Pending Consent", "Pending APA")) %>%
    mutate(`Pending Status` = as.factor(case_when(`Days Pending` >= 365 ~ "Active > 365", 
                                                  TRUE ~ "Active < 365")))  

pendingTotal <- pending%>%
    nrow()


PendingOneyear<- pending %>%
    filter(`Days Pending` >= 365) %>%
    nrow()


Pendingless<- pending%>%
    filter(`Days Pending`<= 365)%>%
    nrow()


## Cases Pending Box
programTotals <- pending %>%
    group_by(Program) %>%
    summarize(`Program Total` = n())
  
# Pending plot
pendingPlot <- pending %>%
    group_by(`Pending Status`, Program) %>%
    summarize(Total = n()) %>%
    left_join(programTotals, by = "Program") %>%
    pivot_wider(names_from = `Pending Status`, values_from = Total) %>%
    # in case nothing in either category
    rowwise() %>%
    mutate(`Active > 365` = ifelse("Active > 365" %in% names(.), `Active > 365`, 0),
           `Active < 365` = ifelse("Active < 365" %in% names(.), `Active < 365`, 0))
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
  layout(height = 250,
         yaxis = list(title = 'Count'),
         xaxis = list(title = 'Program Name'), 
         barmode = 'stack')


# Cases Terminate Box
terminated <- filter(regionalData, 
         `Term Date` >= (pullDate %m-% months(12)), # filter within 12 months from pull date
         `Status Code` %in% c("Terminated")) %>%
    group_by(Program) %>%
    mutate(`Program Total` = n()) 


terminatedTotal<- terminated %>%
    nrow()


terminatedPlot <- terminated %>%
    distinct(Program, .keep_all = TRUE) %>%
    dplyr::select(Program, `Program Total`)  


plot_ly(data = terminatedPlot) %>%
  add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Program Name: ",Program),
                                         paste("Program Total: ", `Program Total`))) %>%
  layout(height = 250,
         yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name'))



### Cases Monitored Box
monitored <- filter(regionalData, `Status Code` %in% c("Effective")) %>%
    group_by(Program) %>%
    mutate(`Program Total` = n()) 


monitoredTotal<-  monitored %>%
    nrow()


monitoredPlot <- monitored %>%
    distinct(Program, .keep_all = TRUE) %>%
    dplyr::select(Program, `Program Total`) 

plot_ly(data = monitoredPlot) %>%
  add_trace(x = ~Program, y = ~`Program Total`, type = 'bar', name = 'Program Total',
            hoverinfo="text",text=~paste(sep="<br>",
                                         paste("Program Name: ",Program),
                                         paste("Program Total: ", `Program Total`))) %>%
  layout(height = 250,
         yaxis = list(title = 'Count',
                      tickformat=',d'), # make integer y axis
         xaxis = list(title = 'Program Name'))
