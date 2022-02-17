# pull data from production environment

### Production Environment
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQL Server Native Client 11.0", 
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# Enforcement data available
#con <- dbConnect(odbc::odbc(), .connection_string = "driver={SQL Server};server={WSQ04151};database={ODS_test};trusted_connection=true")
#dbListTables(con)
#"Enf_Admin_Proceed_View" "Enf_ECM_Docs_View" "Enf_enforcement_Cases_View" "Enf_Facilities_View" 
#"Enf_Respon_Parties_View"  "Enf_Settle_Details_View"

adminProceed <- pool %>% tbl(in_schema("enf", "Enf_Admin_Proceed_View")) %>%
  as_tibble()
ECMdocs <- pool %>% tbl(in_schema("enf", "Enf_ECM_Docs_View")) %>%
  as_tibble()
enfCases <- pool %>% tbl(in_schema("enf", "Enf_enforcement_Cases_View")) %>%
  as_tibble()
enfFacilities <- pool %>%  tbl(in_schema("enf",  "Enf_Facilities_View")) %>%
  as_tibble()
respParties <- pool %>% tbl(in_schema("enf", "Enf_Respon_Parties_View")) %>%
  as_tibble()
settlementDetails <- pool %>% tbl(in_schema("enf", "Enf_Settle_Details_View")) %>%
  as_tibble()



# most information needed comes from enfCases and facility name is joined by Efc_ID
# get start of year filter information
startOfYear <- as.Date(paste0(year(Sys.Date()), '-01-01'))


dat1 <- pool %>% tbl("Enf_enforcement_Cases_View") %>%
  filter(#between(Enc_Executed_Date, !! startOfYear, !! Sys.Date()) &
           between(Enc_Nov_Date, !! startOfYear, !! Sys.Date()) ) %>%# &
           #between(Enc_Terminated_Date, !! startOfYear, !! Sys.Date()) ) %>%
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


# check data is correct
test <- filter(dat1, `EA Number` %in% dat$`EA Number`)
