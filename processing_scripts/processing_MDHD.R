### LOAD -----------------------------------------------------------------------
library(glue)
library(readxl)
library(tidyverse)

### READ -----------------------------------------------------------------------

## strategy OPS - for this strategy it made more sense to have it in table format, see below
# Strategy_Parameters <-
#   read_csv('Data Extracts/Strategy_Parameters.csv') %>% 
#   select(-custom, -strategy) %>% ### strategy is not needed to uniquely identify parameter
#   reshape2::melt() %>%
#   reshape2::acast(list("subcat", "parameters"))

# strategy_params <-
#   read_csv('Data Extracts/Strategy_Parameters.csv') %>%
#   filter(strategy == "Medium and Heavy Duty Vehicle Replacement") %>%
#   select(parameters, default) %>%
#   rename(vehicle_type = parameters, miles_per_vehicle_per_year = default)
# 
# capital_inputs_mdhd <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "Capital Project Inputs", range = "B91:J95",
#              col_names = c("vehicle_type", "fuel_type", "a", "b", "c", "d", "2025", "2030", "2050")) %>%
#   select(-(3:6)) %>%
#   pivot_longer(cols = !(vehicle_type:fuel_type), names_to = "year", values_to = "total_vehicles")
# 
# fuel_factor_mdhd_weighted <- c("nox" = 2.748, "pm25_exhaust" = 0.062, "pm25_tiresBrakes" = 0.012)
# 
# ### MDHD Emissions Rate
# emrate_mdhd_OLD <-
#   read_excel("C:\\Users\\gvendemiatti\\OneDrive - Cambridge Systematics\\R Shiny Migration\\TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "MDHD", range = "B7:E15",
#              col_names = c("vehicle_fuel_type", "2025", "2030", "2050")) %>%
#   separate("vehicle_fuel_type", into = c("vehicle_type", "fuel_type"), sep = ": ") %>%
#   pivot_longer(cols = !(vehicle_type:fuel_type), names_to = "year", values_to = "em_rate")

### CALCULATE ------------------------------------------------------------------

# output <-
#   capital_inputs_mdhd %>%
#   left_join(emrate_mdhd, by = join_by(vehicle_type, fuel_type, year)) %>%
#   left_join(strategy_params, by = join_by(vehicle_type)) %>%
#   left_join(select(filter(emrate_mdhd, fuel_type == "Electric"), -fuel_type), 
#             by = join_by(vehicle_type, year), suffix = c("", "_electric"), keep = F) %>%
#   mutate(
#     affected_annual_VRM = total_vehicles * miles_per_vehicle_per_year,
#     MTCO2_change = -affected_annual_VRM * em_rate / 1000000,
#     added_electricity_emissions = affected_annual_VRM * em_rate_electric / 1000000
#     )
# 
# output_summarized <-
#   output %>%
#   group_by(year) %>%
#   summarize(total_change_MTCO2 = sum(MTCO2_change) + sum(added_electricity_emissions),
#             total_change_direct = sum(MTCO2_change),
#             total_change_electricity = sum(added_electricity_emissions),
#             total_affected_annual_VRM = sum(affected_annual_VRM)) %>%
#   mutate(total_change_mtnox = total_affected_annual_VRM * fuel_factor_mdhd_weighted["nox"] / 1000000,
#          total_change_pm25 = total_affected_annual_VRM * fuel_factor_mdhd_weighted["pm25_exhaust"] / 1000000)

### REACTIVE -----------

# observeEvent(list(EmRate_by_Tech()), { ### uncomment this line and browser and comment below to test!
output_MDHD <- reactive({
  # browser()
  
  veh_replacement <-
    rvs$Assumptions %>% #strategy params
    filter(table == "Medium and Heavy Duty Vehicle Replacement") %>% 
    select(veh_type, unit, value) %>% 
    pivot_wider(names_from = unit, values_from = value)
  
  capital_inputs_mdhd <-
    rvs$Projects %>% 
      filter(table == "Medium and Heavy Duty Vehicle Replacement") %>% 
      select(year, veh_type, fuel_type, unit, value) %>%
      pivot_wider(names_from = unit, values_from = value) %>%
      get_horizon_years(my_rv = rvs)
    
  Fuel_Factors_by_supertype <- Fuel_Factors_Weighted() %>% filter(veh_subtype == "All") %>% select(-veh_subtype) %>% convert_to_nested_list()
  
  emrates <-
    EmRate_by_Tech() %>%
    filter(veh_subtype %in% c("Gasoline ICE", "Diesel ICE", "EV200", "EV"), veh_type != "Passenger Cars") %>%
    filter(year %in% c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) %>%
    mutate(fuel_type = if_else(veh_subtype == "EV200", "EV", str_remove(veh_subtype, " ICE"))) %>%
    select(veh_type, fuel_type, year, emission_rate)
  
  emrates_school_bus <-
    emrates %>% 
    filter(veh_type == "Medium Duty Trucks", fuel_type %in% c("Diesel", "EV")) %>%
    mutate(veh_type = "School Bus")
  
  emrates_all <- bind_rows(emrates, emrates_school_bus)
    
  output_detailed <-
    capital_inputs_mdhd %>%
    left_join(emrates_all, by = join_by(veh_type, fuel_type, year)) %>%
    left_join(veh_replacement, by = join_by(veh_type)) %>% 
    left_join(select(filter(emrates_all, fuel_type == "EV"), -fuel_type),
              by = join_by(veh_type, year), suffix = c("", "_electric"), keep = F) %>%
    mutate(
      affected_annual_VRM = replacement_vehicles * miles_per_veh_per_year,
      MTCO2_change = -affected_annual_VRM * emission_rate / 1000000,
      added_electricity_emissions = affected_annual_VRM * emission_rate_electric / 1000000
      )
    
  output_summarized <-
    output_detailed %>%
    group_by(year) %>%
    summarize(total_change_MTCO2 = sum(MTCO2_change) + sum(added_electricity_emissions),
              total_change_direct = sum(MTCO2_change),
              total_change_electricity = sum(added_electricity_emissions),
              total_affected_annual_VRM = sum(affected_annual_VRM)) %>%
    mutate(total_change_mtnox = total_affected_annual_VRM * Fuel_Factors_by_supertype[["Medium/Heavy Duty Vehicles"]]$NOx_g_per_veh_mi / 1000000,
           total_change_pm25 = total_affected_annual_VRM * Fuel_Factors_by_supertype[["Medium/Heavy Duty Vehicles"]]$PM25_exhaust_per_veh_mi / 1000000)
    
})