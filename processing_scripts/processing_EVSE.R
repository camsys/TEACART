### REACTIVE ----------

emrate_evse <- reactive({
  bind_rows( ### this runs pretty slow - needs to be rewritten Gui 1/22/24
    EmRate_Electric_MDHD() %>% mutate(veh_supertype = "Medium/Heavy Duty Vehicles") %>% rename(emrate_Electric = emrate_category_avg),
    EmRate_Conventional_LDV() %>% mutate(veh_supertype = "Light Duty Vehicles") %>% rename(emrate_Conventional = emrate_category_avg),
    EmRate_Conventional_MDHD() %>% mutate(veh_supertype = "Medium/Heavy Duty Vehicles") %>% rename(emrate_Conventional = emrate_category_avg),
    EmRate_Electric_LDV() %>% mutate(veh_supertype = "Light Duty Vehicles") %>% rename(emrate_Electric = emrate_category_avg) # Qi: This is different, verify with Gui
  )
})

Stock_filtered <- reactive({
  Stock_Type %>% filter(year %in% c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ## annual miles
})

elasticities_by_port_type <- reactive({ #strategy_params_evse
  rvs$Assumptions %>% 
    filter(table == "EV Charging Infrastructure") %>% 
    select(charge_port_detail, unit, value) %>% 
    pivot_wider(names_from = "unit", values_from = "value")
})

# observeEvent(list(EmRate_by_Tech(), VMT_Forecast()), { ### uncomment this line and browser and comment below to test!
output_EVSE <- reactive({
  
  capital_inputs <-
    rvs$Projects %>%
    filter(category == "EV Charging Infrastructure") %>%
    select(year, charge_port_detail, unit, value) %>%
    pivot_wider(names_from = unit, values_from = value) %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    group_by(charge_port_detail) %>%
    arrange(year) %>%
    mutate(
      new_ports = case_when(
        year > rvs$Baseline$horizon_year_1 ~ cumsum(new_ports),
        TRUE ~ new_ports)) %>%
    ungroup() 
  
  CalculatedPorts <- capital_inputs %>% left_join(elasticities_by_port_type(), by = join_by("charge_port_detail")) %>% 
    mutate(calculated_ports = new_ports * veh_sales_elasticity_wrt_ports)
  
  vmt_affected <-
    CalculatedPorts %>% 
    mutate(veh_supertype = if_else(str_detect(charge_port_detail, "General public"), "Light Duty Vehicles", "Medium/Heavy Duty Vehicles")) %>%
    #get_horizon_years(my_rv = rvs) %>%
    select(year, veh_supertype, calculated_ports) %>%
    left_join(x = Stock_filtered(), y = ., by = join_by(veh_supertype, year), relationship = "one-to-many") %>%
    summarize("VMT_affected" = sum(calculated_ports * MT_per_vehtype), .by = c("veh_supertype", "year"))
  
  evse_by_year_supertype <- Stock_filtered() %>% 
    left_join(vmt_affected, by = join_by(veh_supertype, year)) %>%
    left_join(emrate_evse(), by = join_by(veh_supertype, year)) %>%
    mutate(displaced_conventional_emissions = -VMT_affected * emrate_Conventional / 1000000, 
           added_electricity_emissions = VMT_affected * emrate_Electric / 1000000)
  
  evse_by_year_supertype %>% 
    group_by(year) %>%
    summarize(total_change_direct = sum(displaced_conventional_emissions, na.rm = T),
              total_change_electricity = sum(added_electricity_emissions, na.rm = T),
              truck_vmt_affected = sum(unique(VMT_affected[veh_supertype == "Medium/Heavy Duty Vehicles"]), na.rm = T),
              light_vmt_affected = sum(unique(VMT_affected[veh_supertype == "Light Duty Vehicles"]), na.rm = T)) %>%
    mutate(total_change_MTCO2 = total_change_direct + total_change_electricity,
           total_change_mtnox = -(Fuel_Factors_by_supertype()[["Light Duty Vehicles"]]$NOx_g_per_veh_mi*light_vmt_affected + 
                                    Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]]$NOx_g_per_veh_mi * truck_vmt_affected) / 1000000,
           total_change_pm25 = -(Fuel_Factors_by_supertype()[["Light Duty Vehicles"]]$PM25_tires_brakes_per_veh_mi*light_vmt_affected +
                                   Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]]$PM25_exhaust_per_veh_mi * truck_vmt_affected) / 1000000)
})

cost_effectiveness_EVSE <- reactive({
  emrate_diff <-
    emrate_evse() %>% filter(year == input$horizon_year_1) %>% 
    group_by(veh_supertype) %>% 
    summarize(emrate_diff = sum(emrate_Electric, na.rm = T) - sum(emrate_Conventional, na.rm = T))
  
  elasticities_by_port_type() %>%
    mutate(veh_supertype = if_else(str_detect(charge_port_detail, "General public"), "Light Duty Vehicles", "Medium/Heavy Duty Vehicles")) %>%
    left_join(select(filter(Stock_filtered(), year == input$horizon_year_1), veh_supertype, MT_per_vehtype), by = join_by(veh_supertype)) %>%
    left_join(emrate_diff, by = join_by(veh_supertype)) %>%
    left_join(Fuel_Factors_Weighted() %>% filter(veh_subtype == "All") %>% select(-veh_subtype) %>% rename(veh_supertype = veh_type)) %>%
    mutate(GHG = MT_per_vehtype*veh_sales_elasticity_wrt_ports*emrate_diff,
           VMT = 0,
           NOX = MT_per_vehtype*veh_sales_elasticity_wrt_ports*NOx_g_per_veh_mi,
           PM25 = MT_per_vehtype*veh_sales_elasticity_wrt_ports*PM25_exhaust_per_veh_mi,
           ACTIVE = 0) %>%
    rename(total_change_gGHG = GHG,
           total_change_VMT = VMT,
           total_change_gnox = NOX,
           total_change_gpm25 = PM25,
           total_change_newtrips = ACTIVE)
})
