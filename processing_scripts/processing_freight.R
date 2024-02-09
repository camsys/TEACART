

intermodal_investment_factor_truck <- -72103.0
intermodal_investment_factor_rail <- 1021459
# fuel_factor_mdhd_weighted <- c("nox" = 2.748, "pm25_exhaust" = 0.062, "pm25_tiresBrakes" = 0.012)
# observeEvent(list(EmRate_by_Tech(), VMT_Forecast()), { ### uncomment this line and browser and comment below to test!
output_freight <- reactive({
  req(EmRate_by_Tech(), VMT_Forecast())
  # browser()
  
  emrate_freight <-
    EmRate_by_Tech() %>% filter(veh_type %in% c("Medium Duty Trucks", "Heavy Duty Trucks"), str_detect(fuel_type, "ICE")) %>%
    mutate(veh_subtype = str_remove(fuel_type, " ICE")) %>%
    select(veh_type, veh_subtype, year, emission_rate) %>%
    left_join(VMT_Type_Tech_Conventional_MDHD(), by = join_by(veh_type, veh_subtype, year)) %>%
    mutate(state_emission_rate = emission_rate * state_pct_of_category) %>%
    filter(!(veh_type == "Heavy Duty Trucks" & veh_subtype == "Gasoline")) %>%
    mutate(veh_supertype = "Medium/Heavy Duty Vehicles", fuel_supertype = "Conventional") %>%
    summarize("emissions_avg" = sum(state_emission_rate), .by = c(veh_supertype, fuel_supertype, year))
  
  emissions_avg_rail <-
    as.numeric(pull(filter(rv$Advanced, unit == "energy_intensity"), value)) / 128500 * 1000 * 
    pull(filter(Fuel_Factors_Revision, str_detect(fuel_type, "Diesel")), fuel_carbon_content)[[1]] #128500 still needs to be added to somewhere....
  
  capital_inputs <-
    rv$Projects %>% 
    filter(category == "Freight Intermodal Facilities") %>% 
    select(year, charge_port_detail, unit, value) %>% ### needs to be renamed DCFC level
    pivot_wider(names_from = unit, values_from = value)
  
  ## table to return
  capital_inputs %>% 
    rowwise() %>%
    mutate(year = rv$Baseline[[year]]) %>% ### Pulls horizon years
    left_join(emrate_freight) %>%
    mutate(truck_vmt_affected = intermodal_investment * intermodal_investment_factor_truck,
           rail_ton_mi_affected = intermodal_investment * intermodal_investment_factor_rail,
           displaced_truck_emissions = truck_vmt_affected * emissions_avg / 1000000,
           added_rail_emissions = rail_ton_mi_affected * emissions_avg_rail / 1000000,
           total_change_MTCO2 = displaced_truck_emissions + added_rail_emissions,
           total_change_direct = total_change_MTCO2,
           total_change_electricity = 0,
           total_change_mtnox = truck_vmt_affected * Fuel_Factors_Weighted[["Heavy Duty Trucks"]][["NOx_g_per_veh_mi_avg"]] / 1000000,
           total_change_pm25 = (truck_vmt_affected * Fuel_Factors_Weighted[["Heavy Duty Trucks"]][["PM25_exhaust_avg"]] +
                                  truck_vmt_affected * Fuel_Factors_Weighted[["Heavy Duty Trucks"]][["PM25_tires_brakes_avg"]]) / 1000000)
})
