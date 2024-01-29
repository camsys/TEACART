### This file is a temporary HELPER file. It will be deleted by end of project
### The functions below will be moved to the top of the global server function
### It reads in 2 types of data: 1. Raw Data (not editable by user) and 
### 2. User Inputs (editable by user). It then creates the R objects needed for the processing scripts

### LOAD ----------------------------------
library(tidyverse)
library(readxl)

### HELPER FUNCTIONS -----------------------------------------

# Define the mapping from column1 values to groups
veh_types_mapping <- c("Passenger Cars" ~ "Light Duty Vehicles", 
                       "Light Duty Trucks" ~ "Light Duty Vehicles", 
                       "Medium Duty Trucks" ~ "Medium/Heavy Duty Vehicles", 
                       "Heavy Duty Trucks" ~ "Medium/Heavy Duty Vehicles")

ev_forecast_mapping <- c("AEO Baseline" ~ "AEO_Tech_Frac",
                         "ACC" ~ "ACC_Tech_Frac",
                         "ACC II" ~ "ACCII_Tech_Frac",
                         "ACC II + ACT" ~ "ACCACT_Tech_Frac",
                         "Custom" ~ "Custom")

veh_subtype_to_fuel_type_mapping <- c("Gasoline ICE" ~ "Gasoline",
                                      "EV100" ~ "Electricity",
                                      "EV200" ~ "Electricity",
                                      "EV300" ~ "Electricity",
                                      "EtOH" ~ "Gasoline",
                                      "Diesel ICE" ~ "Diesel",
                                      "CNG/LNG/LPG" ~ "Natural Gas",
                                      "SI HEV on Gas" ~ "Gasoline",
                                      "SI PHEV 10" ~ "Gasoline",
                                      "SI PHEV 40" ~ "Gasoline",
                                      "FCV" ~ "Electricity",
                                      "CNG" ~ "Natural Gas",
                                      "EV" ~ "Electricity",
                                      "Diesel PHEV" ~ "Diesel",
                                      "Gasoline PHEV" ~ "Gasoline",
                                      "LPG" ~ "LPG")
PHEV_fuel_types <- c("SI PHEV 10","SI PHEV 40","Diesel PHEV","Gasoline PHEV")
ev_fuel_types <- c("EV100","EV200","EV300","SI PHEV 10","SI PHEV 40", "FCV", "EV", "Gasoline PHEV", "Diesel PHEV")

get_horizon_years <- function(df, my_rv) { ### converts from horizon_year_1 to 2025 for example. Gui 1/22/24
  if (!("year" %in% colnames(df))) {stop("Year not a column of this dataframe")}
  df %>%
    mutate(year = case_match(year, 
                             "horizon_year_1" ~ my_rv[["Baseline"]][["horizon_year_1"]],
                             "horizon_year_2" ~ my_rv[["Baseline"]][["horizon_year_1"]],
                             "horizon_year_3" ~ my_rv[["Baseline"]][["horizon_year_1"]]))
}

convert_to_nested_list <- function(df){ 
  # Create an empty list
  my_list <- list()
  
  num_primary_keys <- get_num_primary_keys(df)
  
  # Get number of primary keys, adding a 2 for the transit column,
  num_primary_keys <- if(nrow(df) == 1 | num_primary_keys == 1){num_primary_keys + 1}else{num_primary_keys + 2}
  
  # Get the names of the primary key columns
  primary_keys <- names(df)[1:num_primary_keys]
  
  # Recursive function to create nested lists
  create_nested_list <- function(df, keys){
    # Base case: if there's only one key left, create a named vector
    if(length(keys) == 1){
      named_vector <- setNames(as.list(df[1, -(1:(length(primary_keys)-1))]), names(df)[-(1:(length(primary_keys)-1))])
      return(named_vector)
    }
    # Recursive case: create a list for each unique value of the first key
    else{
      nested_list <- list()
      for(i in unique(df[[keys[1]]])){
        nested_list[[as.character(i)]] <- create_nested_list(df[df[[keys[1]]] == i, ], keys[-1])
      }
      return(nested_list)
    }
  }
  
  # Populate the list
  my_list <- create_nested_list(df, primary_keys)
  
  return(my_list)
}

get_num_primary_keys <- function(df){
  # Get the column names
  cols <- names(df)
  
  # Check each combination of columns
  for(i in seq_along(cols)){
    combinations <- combn(cols, i, simplify = FALSE)
    for(comb in combinations){
      # If the number of unique rows for this combination of columns is equal to the number of rows in the data frame,
      # then this combination of columns can act as a primary key
      if(nrow(df[ , comb]) == nrow(unique(df[ , comb]))){
        return(length(comb))
      }
    }
  }
  
  # If no combination of columns can act as a primary key, return NULL
  return(NULL)
}

### READ and PROCESS RAW DATA ------------------------- 
# At very end of project we should consider switching to RData file to minimize processing/startup time

#State Population ----
#State Population for years 2020, 2030, 2050 - need to project inbetween years
State_Populations <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "State_Populations")

#have to add the 2050 year somehow for approx to work
for (var in unique(State_Populations$state)){
  temp_state <- State_Populations %>% 
    filter(state == var) %>%
    add_row(state = var, 
            year = 2050, 
            population = (.$population[.$year == 2040] - .$population[.$year == 2030]) + .$population[.$year == 2040]) %>% 
    filter(year == 2050)
  State_Populations = rbind(State_Populations,temp_state)
}

State_Populations <-
  expand(State_Populations, state, year = rep(2020:2050)) %>%
  left_join(State_Populations, by = c("state", "year")) %>%
  group_by(state) %>%
  mutate(population = approx(year, population, xout = year)$y) %>% 
  mutate(population_growth = population / population[year == 2020] - 1) %>%
  group_by(year) %>% 
  mutate(population_pct_of_national = population / sum(population)) %>% #SL: I think this is being misused
  ungroup()

#Finsih State Population Processing

#Processing VMT Info----

##This is the percentage of LDV VMT and Heavy Duty Vehicles that occur on the NHS for each state: 2021-2050
NHS_VMT <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "NHS_VMT", range = "A4:R57") %>%
    select(state, LDV_pct_on_NHS, TRK_pct_on_NHS)

##This is the VMT for each state from 2021-2050 - raw number not percentage
VMT_State_Allocation_raw <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "VMT_State_Allocation")

VMT_State_Allocation <- right_join(State_Populations,VMT_State_Allocation_raw, by = c('year','state')) %>%
  group_by(year) %>% 
  mutate(us_vmt = sum(state_vmt)) %>%
  mutate(state_vmt_pct_of_national = state_vmt/us_vmt) %>% #dividing by two removes US Total
  ungroup()
  
##this has millions of vehicles for the whole US by year for veh/vehsubtype 
Stock_Type_Tech_BASE <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Stock_Type_Tech_BASE") %>%
  pivot_longer(cols = !c(veh_type, veh_subtype), names_to = "year", values_to = "stock_millions") %>%
  mutate(year = as.integer(year))

##This has VMT by veh_type (Assuming all subtypes combined) for 2021 - 2050 with 2020 added as zeros
AEO_VMT_Base <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "AEO_VMT_Base") #Original Tool Does not include 2020 Numbers - coded as zero
###this adds VMT per million vehicles aka miles traveled by each veh_type each year 2021-2050 I'M Chaging the Name here!!!
AEO_VMT <- AEO_VMT_Base %>% 
  left_join(
    summarize(Stock_Type_Tech_BASE, "total_stock_millions" = sum(stock_millions), .by = c(veh_type, year)),
            by = join_by(veh_type, year)
    ) %>%
  mutate(#VMT_per_veh = VMT_AEO / total_stock_millions, #find this in the Stock_Type object
         veh_supertype = case_match(veh_type, !!!veh_types_mapping))

#This is a duplicate of the above - original tool had this in the Stock_Type_Tech_Base tab which is why I think it got added twice
Stock_Type <- AEO_VMT %>%
  group_by(veh_supertype, year) %>%
  summarize("total_VMT" = sum(VMT_AEO, na.rm = T), "total_stock_millions" = sum(total_stock_millions, na.rm = T), .groups = "drop") %>%
  mutate("MT_per_vehtype" = total_VMT / total_stock_millions) %>% #was VMT_per_veh and now is MT_per_vhetype
  arrange(desc(veh_supertype))

#Other Data Sources
HPMS <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "HPMS")

Fuel_Econs <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Fuel_Econs")

State_Prices <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "State_Prices")

Electricity_EmRate <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Electricity_EmRate")

Bike_Ped <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Bike_Ped")

NTD_Service <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "NTD_Service")

Fuel_Factors <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Fuel_Factors")
Fuel_Factors_Baselines <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Fuel_Factors_Baselines")
Fuel_Factors_Revision <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Fuel_Factors_Revision") # SL ADDED NEED TO WORK TO COMBINE THESE TWO
Fuel_Factors_Weighted_raw <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Fuel_Factors_Weighted")

#I realized this was misatributing the weighting to heavy duty trucks instead of medium duty see the excel tool
# Fuel_Factors_Weighted_raw <-
  # Fuel_Factors %>% # Different veh types will need different weighting strategies, will likely need to make reactive
  # group_by(veh_type) %>%
  # summarize(NOx_g_per_veh_mi_avg = sum(hd_weight * NOx_g_per_veh_mi, na.rm = T),
  #           PM25_exhaust_avg = sum(hd_weight * PM25_exhaust, na.rm = T),
  #           PM25_tires_brakes_avg = sum(hd_weight * PM25_tires_brakes, na.rm = T))

EV_Forecast <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "EV_Forecast")

Passenger_Rail_State_Mileage <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Passenger_Rail_State_Mileage")
Passenger_Rail_FuelFactors <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Passenger_Rail_FuelFactors")

Public_Transit_data <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Public_Transit_Mileage")

Freight_Rail_Data <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Freight_Rail_Data")

Warming_Potential <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Warming_Potential")

Transit_Costs <- read_excel(".\\data\\1.Raw_Data.xlsx", sheet = "Transit_Costs")
Transit_Costs <- ### Adds zeroes to states that don't have certain transit modes
  expand(Transit_Costs, state_code, transit_mode) %>%
  left_join(Transit_Costs, by = join_by(state_code, transit_mode)) %>%
  replace_na(list(total_cost_veh_operations = 0, total_cost_veh_maintainance = 0, total_cost_fuel_lube = 0, total_cost_om = 0))

# used to rename headers and units during table rendering
references <- read_excel(".\\data\\2.User_Inputs.xlsx",
                         sheet ="References",
                         col_names = TRUE)

references_vector <- setNames(references$field, references$description)
rev_references_vector <- setNames(references$description, references$field)


# these are used to reset tables in the UI
initial_projects <- read_excel(".\\data\\2.User_Inputs.xlsx",
                               sheet ="Projects",
                               col_names = TRUE)

initial_assumptions <- read_excel(".\\data\\2.User_Inputs.xlsx",
                                  sheet ="Assumptions",
                                  col_names = TRUE)

initial_costs <- read_excel(".\\data\\2.User_Inputs.xlsx",
                            sheet ="Costs",
                            col_names = TRUE)

initial_advanced <- read_excel(".\\data\\2.User_Inputs.xlsx",
                               sheet ="Advanced",
                               col_names = TRUE)


#Additional Calculations ----

# right now this only grabs "Vision 2022"
Tech_Frac_Vision <-   #you should be calling the reactive version Tech_Frac_Vision()
  Stock_Type_Tech_BASE %>%
  group_by(veh_type, year) %>%
  mutate(aeo_tech_frac = stock_millions / sum(stock_millions)) %>% #renamed to tech_frac_forecast
  ungroup()

#TechFrac object----
#I need to match up the columns with what's above for this one
# GLV 12/21/23: I did the vision 2022 one above, with the helpful code below. We can expand on other options at a later time? - SL, this has been expanded delete this comment when ready
TechFrac <- Stock_Type_Tech_BASE %>% left_join(EV_Forecast, by = c("veh_type", "year")) %>%
  group_by(year, veh_type) %>%
  #Baseline vision 2022 I think aka AEO
  mutate(AEO_Tech_Frac = stock_millions/sum(stock_millions)) %>%
  #select(year, veh_type, fuel_type, AEO_Tech_Frac) %>%
  ungroup() %>%
  mutate(is_ev_type = ifelse(veh_subtype %in% ev_fuel_types,1,0)) %>%
  group_by(year, veh_type, is_ev_type) %>%
  mutate(per_ev_nonev = AEO_Tech_Frac/sum(AEO_Tech_Frac)) %>%
  #ACC Forecasting
  ungroup() %>%
  group_by(year, veh_type) %>%
  mutate(ACC_Tech_Fractemp = percEVstock_ACC*per_ev_nonev*is_ev_type) %>%
  mutate(ACC_Tech_Frac = ifelse(is_ev_type == 0, per_ev_nonev*(1-sum(ACC_Tech_Fractemp)), ACC_Tech_Fractemp)) %>%
  mutate(ACC_Tech_Frac = ifelse(veh_type %in% c("Medium Duty Truck","Heavy Duty Truck"), AEO_Tech_Frac, ACC_Tech_Frac)) %>%
  select(-ACC_Tech_Fractemp) %>%
  ungroup() %>%
  #ACCII Version
  ungroup() %>%
  group_by(year, veh_type) %>%
  mutate(ACCII_Tech_Fractemp = percEVstock_ACCII*per_ev_nonev*is_ev_type) %>%
  mutate(ACCII_Tech_Frac = ifelse(is_ev_type == 0, per_ev_nonev*(1-sum(ACCII_Tech_Fractemp)), ACCII_Tech_Fractemp)) %>%
  mutate(ACCII_Tech_Frac = ifelse(veh_type %in% c("Medium Duty Truck","Heavy Duty Truck"), AEO_Tech_Frac, ACCII_Tech_Frac)) %>%
  select(-ACCII_Tech_Fractemp) %>%
  ungroup() %>%
  #ACC + ACT
  group_by(year, veh_type) %>%
  mutate(ACCACT_Tech_Fractemp = percEVstock_ACCACT*per_ev_nonev*is_ev_type) %>%
  mutate(ACCACT_Tech_Frac = ifelse(is_ev_type == 0, per_ev_nonev*(1-sum(ACCACT_Tech_Fractemp)), ACCACT_Tech_Fractemp)) %>%
  mutate(ACCACT_Tech_Frac = ifelse(veh_type %in% c("Medium Duty Truck","Heavy Duty Truck"), ACCACT_Tech_Frac, ACC_Tech_Frac)) %>%
  select(-ACCACT_Tech_Fractemp) %>%
  ungroup() %>%
  #ACCII + ACT
  mutate(ACCIIACT_Tech_Frac = ifelse(veh_type %in% c("Passenger Car","Light Duty Truck"), ACCII_Tech_Frac, ACCACT_Tech_Frac))

