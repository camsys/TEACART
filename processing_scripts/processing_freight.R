### LOAD -----------------------------------------------------------------------
# library(glue)
# library(readxl)
# library(tidyverse)

### READ -----------------------------------------------------------------------


## Freight Emissions Rate
# emrate_freight <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "Freight", range = "B4:E4",
#              col_names = c("vehicle_category", "2025", "2030", "2050")) %>%
#   pivot_longer(cols = !(vehicle_category), names_to = "year", values_to = "emrate_mdhd")
# 
# capital_inputs_freight <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "Capital Project Inputs", range = "H111:J111",
#              col_names = c("2025", "2030", "2050")) %>%
#   pivot_longer(cols = everything(), names_to = "year", values_to = "intermodal_investment")
# 
# freight_emission_rate <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "Freight", range = "C7:E7",
#              col_names = c("2025", "2030", "2050")) %>%
#   pivot_longer(cols = everything(), names_to = "year", values_to = "emrate_rail")
# 
# intermodal_investment_factor_truck <- -72103.0
# intermodal_investment_factor_rail <- 1021459
# 
# fuel_factor_mdhd_weighted <- c("nox" = 2.748, "pm25_exhaust" = 0.062, "pm25_tiresBrakes" = 0.012)
# 
# ### CALCULATE ------------------------------------------------------------------
# capital_inputs_freight %>%
#   left_join(select(emrate_freight, -vehicle_category), by = join_by(year)) %>%
#   left_join(freight_emission_rate, by = join_by(year)) %>%
#   mutate(truck_vmt_affected = intermodal_investment * intermodal_investment_factor_truck,
#          rail_ton_mi_affected = intermodal_investment * intermodal_investment_factor_rail,
#          displaced_truck_emissions = truck_vmt_affected * emrate_mdhd / 1000000,
#          added_rail_emissions = rail_ton_mi_affected * emrate_rail / 1000000,
#          total_change_MTCO2 = displaced_truck_emissions + added_rail_emissions,
#          total_change_direct = total_change_MTCO2,
#          total_change_electricity = 0,
#          total_change_mtnox = truck_vmt_affected * fuel_factor_mdhd_weighted["nox"] / 1000000,
#          total_change_pm25 = (truck_vmt_affected * fuel_factor_mdhd_weighted["pm25_exhaust"] +
#            truck_vmt_affected * fuel_factor_mdhd_weighted["pm25_tiresBrakes"]) / 1000000)

### REACTIVE -----
# observe({browser()})
emrate_freight <- reactive({
  EmRate_by_Tech() %>% 
    filter(veh_type %in% c("Medium Duty Trucks", "Heavy Duty Trucks"), str_detect(veh_subtype, "ICE")) %>%
    select(veh_type, veh_subtype, year, emission_rate) %>%
    left_join(VMT_Type_Tech_Conventional_MDHD(), by = join_by(veh_type, veh_subtype, year)) %>%
    mutate(state_emission_rate = emission_rate * state_pct_of_category) %>% 
    filter(!(veh_type == "Heavy Duty Trucks" & veh_subtype == "Gasoline ICE")) %>%
    mutate(veh_supertype = "Medium/Heavy Duty Vehicles", fuel_supertype = "Conventional") %>%
    summarize("emissions_avg" = sum(state_emission_rate), .by = c(veh_supertype, fuel_supertype, year))
})

emissions_avg_rail <- reactive({
  as.numeric(pull(filter(rvs$Advanced, unit == "energy_intensity"), value)) / pull(filter(Fuel_Factors_Baselines, fuel_type == "Diesel", units == "fuel_conversion_BTU"), value) * 
    1000 * pull(filter(Fuel_Factors_Revision, str_detect(veh_subtype, "Diesel ICE")), fuel_carbon_content)[[1]]
})

# observeEvent(list(EmRate_by_Tech(), VMT_Forecast()), { ### uncomment this line and browser and comment below to test!
output_freight <- reactive({
  # req(EmRate_by_Tech(), VMT_Forecast(), Fuel_Factors_Weighted())
  # browser()
  
  capital_inputs <-
    rvs$Projects %>% 
    filter(category == "Freight Intermodal Facilities") %>% 
    select(year, unit, value) %>%
    pivot_wider(names_from = unit, values_from = value) %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    #group_by() %>%
    arrange(year) %>%
    mutate(
      intermodal_investment = case_when(
        year > rvs$Baseline$horizon_year_1 ~ cumsum(intermodal_investment),
        TRUE ~ intermodal_investment)) %>%
    ungroup() 
  
  ## table to return
  capital_inputs %>% 
    #rowwise() %>%
    #mutate(year = rvs$Baseline[[year]]) %>% ### Pulls horizon years
    #ungroup() %>%
    left_join(emrate_freight(), by = join_by(year)) %>%
    mutate(truck_vmt_affected = intermodal_investment * as.numeric(pull(filter(rvs$Advanced, unit == "intermodal_investment_factor_truck"), value)),
           rail_ton_mi_affected = intermodal_investment * as.numeric(pull(filter(rvs$Advanced, unit == "intermodal_investment_factor_rail"), value)),
           displaced_truck_emissions = truck_vmt_affected * emissions_avg / 1000000,
           added_rail_emissions = rail_ton_mi_affected * emissions_avg_rail() / 1000000,
           total_change_MTCO2 = displaced_truck_emissions + added_rail_emissions,
           total_change_direct = total_change_MTCO2,
           total_change_VMT = truck_vmt_affected,
           total_change_electricity = 0,
           total_change_mtnox = truck_vmt_affected * Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]][["NOx_g_per_veh_mi"]] / 1000000,
           total_change_pm25 = (truck_vmt_affected * Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]][["PM25_exhaust_per_veh_mi"]] +
                                  truck_vmt_affected * Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]][["PM25_tires_brakes_per_veh_mi"]]) / 1000000)
})

cost_effectiveness_freight <- reactive({
  tibble(
    GHG = (emrate_freight() %>% filter(year == 2025) %>% pull(emissions_avg)) * as.numeric(pull(filter(rvs$Advanced, unit == "intermodal_investment_factor_truck"), value)) + 
      (emissions_avg_rail() * as.numeric(pull(filter(rvs$Advanced, unit == "intermodal_investment_factor_rail"), value))),
    VMT = as.numeric(pull(filter(rvs$Advanced, unit == "intermodal_investment_factor_truck"), value)),
    NOX = VMT * Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]][["NOx_g_per_veh_mi"]],
    PM25 = VMT * (Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]][["PM25_exhaust_per_veh_mi"]] + 
                    Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]][["PM25_tires_brakes_per_veh_mi"]])
  )
})