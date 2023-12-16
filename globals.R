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

convert_to_nested_list <- function(df){ 
  # Create an empty list
  my_list <- list()
  
  # Get number of primary keys, adding a 2 for the transit column
  num_primary_keys <- if(nrow(df) == 1){get_num_primary_keys(df) + 1}else{get_num_primary_keys(df) + 2}
  
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
State_Populations <- read_excel("1.Raw_Data.xlsx", sheet = "State_Populations")
State_Populations <-
expand(State_Populations, state, year = rep(2020:2050)) %>%
  left_join(State_Populations, by = c("state", "year")) %>%
  group_by(state) %>%
  mutate(population = approx(year, population, xout = year)$y,
         growth = population / population[year == 2020] - 1) %>%
  ungroup()

NHS_VMT <- read_excel("1.Raw_Data.xlsx", sheet = "NHS_VMT", range = "A4:R57") %>%
    select(state, LDV_pct_on_NHS, TRK_pct_on_NHS)

VMT_State_Allocation <- read_excel("1.Raw_Data.xlsx", sheet = "VMT_State_Allocation")

Stock_Type_Tech_BASE <- read_excel("1.Raw_Data.xlsx", sheet = "Stock_Type_Tech_BASE") %>%
  pivot_longer(cols = !c(veh_type, veh_subtype), names_to = "year", values_to = "stock_millions") %>%
  mutate(year = as.integer(year))
# VMT_State_Allocation <- ### Can be calculated but just copied in the sheet that Qi put together
# State_Populations %>%
#   left_join(filter(State_VMTs, year == 2021), by = join_by(year, state)) %>%
#   group_by(state) %>%
#   mutate(hmm = (1+growth)*state_vmt[year == 2021])
AEO_VMT <- read_excel("1.Raw_Data.xlsx", sheet = "AEO_VMT_Base") %>%
  left_join(summarize(Stock_Type_Tech_BASE, "total_stock_millions" = sum(stock_millions), .by = c(veh_type, year)),
            by = join_by(veh_type, year)) %>%
  mutate(VMT_per_veh = VMT_AEO / total_stock_millions,
         veh_supertype = case_match(veh_type, !!!veh_types_mapping))
  
Stock_Type <- AEO_VMT %>%
  group_by(veh_supertype, year) %>%
  summarize("total_VMT" = sum(VMT_AEO, na.rm = T), "total_stock_millions" = sum(total_stock_millions, na.rm = T), .groups = "drop") %>%
  mutate("VMT_per_veh" = total_VMT / total_stock_millions) %>%
  arrange(desc(veh_supertype))

HPMS <- read_excel("1.Raw_Data.xlsx", sheet = "HPMS")

Fuel_Econs <- read_excel("1.Raw_Data.xlsx", sheet = "Fuel_Econs")

State_Prices <- read_excel("1.Raw_Data.xlsx", sheet = "State_Prices")

Electricity_EmRate <- read_excel("1.Raw_Data.xlsx", sheet = "Electricity_EmRate")

Bike_Ped <- read_excel("1.Raw_Data.xlsx", sheet = "Bike_Ped")

NTD_Service <- read_excel("1.Raw_Data.xlsx", sheet = "NTD_Service")

Fuel_Factors <- read_excel("1.Raw_Data.xlsx", sheet = "Fuel_Factors")

Transit_Costs <- read_excel("1.Raw_Data.xlsx", sheet = "Transit_Costs")
Transit_Costs <- ### Adds zeroes to states that don't have certain transit modes
  expand(Transit_Costs, state_code, transit_mode) %>%
  left_join(Transit_Costs, by = join_by(state_code, transit_mode)) %>%
  replace_na(list(total_cost_veh_operations = 0, total_cost_veh_maintainance = 0, total_cost_fuel_lube = 0, total_cost_om = 0))




