output_TransitService <- reactive({
 
  # get the follow values from the Fuel_Factors_Revision,
  fuelconv_ditoga <- Fuel_Factors_Revision$fuel_conversion[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disblend <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disCH4 <-  Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disN20 <-  Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelconv_cfCNGtoGas <- Fuel_Factors_Revision$fuel_conversion[Fuel_Factors_Revision$veh_subtype == 'CNG/LNG/LPG' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
  # fuelfact_cng <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'CNG/LNG/LPG' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
  fuelfact_cng <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "CNG" & Fuel_Factors_Baselines$units == "fuel_carbon_content"]
  
  fuelfact_cngN20 <- Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'CNG' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  
  fuelfact_gasblend <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'Gasoline ICE' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
  fuelfact_gasCH4 <- Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'SI HEV on Gas' & Fuel_Factors_Revision$veh_type == 'Light-Duty Trucks']
  fuelfact_gasN20 <-Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'SI PHEV 40' & Fuel_Factors_Revision$veh_type == 'Light-Duty Trucks']
  fuelconv_kwHtoga <- Fuel_Factors_Revision$electricity_conversion[Fuel_Factors_Revision$veh_subtype == 'SI PHEV 40' & Fuel_Factors_Revision$veh_type == 'Light-Duty Trucks']
  fuelfact_cngCH4 <- Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'CNG' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factordisbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factorCNGbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factorgas_medduty_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factordisloc_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  
  
  fuel_factordisbus_PM25 <- Fuel_Factors_Weighted_raw$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted_raw$veh_type == 'Bus'& Fuel_Factors_Weighted_raw$veh_subtype == 'Diesel']
  fuel_factorCNGbus_PM25 <- Fuel_Factors_Weighted_raw$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted_raw$veh_type == 'Bus'& Fuel_Factors_Weighted_raw$veh_subtype == 'CNG']
  fuel_factorgas_medduty_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factorCNGbus_PM25TB <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factordisloc_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
 # Fuel_Factors_Baselines$units
  
  #fuel_factordisloc_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  # get the electricity emission rate
  elect_emrate <- electricity_emrate() %>% group_by(year) %>%
    summarise(electricity_carbon_content =  unique(electricity_carbon_content))
  
  # get the emrate (use CO2e_millions)
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  #use CO2e_millions for emrate
  # use base_impf for emrate_by_Tech
  
  rail_factors <- passenger_rail_fuel_factors() %>% distinct()
  # replace prail with Electric_LR_CO2eq, this is exact same as Electric_HR_CO2eq
  # replace cmtrail with Electric_CR_CO2eq
  # for cmtrail_dis, use Diesel_CR_CO2eq
  #browser()
  # get assumptions input

  
  # get captial project tables: 
  Inputs_busprior <- rvs$Projects[rvs$Projects$table_no_ui == 5,] %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    group_by(area_type, fuel_type,transit_mode) %>%
    arrange(year) %>%
    mutate(
      value = case_when(
        year > rvs$Baseline$horizon_year_1 ~ cumsum(value),
        TRUE ~ value)) %>%
    ungroup() %>%
    select_if(~ any(!is.na(.))) %>%
    left_join(emrate_by_tech_ldv, by = 'year') |> 
    #rename(bus_prioirty_mile = value) %>% 
    select(-table_no_ui,-unit,-category, -table)
  

  Inputs_transit <-  rvs$Projects[rvs$Projects$table_no_ui %in% c(2,3,6),] %>%#, 4),] %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    group_by(table,area_type, fuel_type,transit_mode) %>%
    arrange(year) %>%
    mutate(
      value = case_when(
        year > rvs$Baseline$horizon_year_1 ~ cumsum(value),
        TRUE ~ value)) %>%
    ungroup() %>%
    select_if(~ any(!is.na(.))) %>%
    #rename(VOMS = value) %>% 
    #left_join(.,Capital_Project_Inputs_publicTrans, by = 'year') %>%
    left_join(elect_emrate, by = as.character('year')) %>% # the variable to use: electricity_carbon_content
    left_join(select(rail_factors,year, Electric_LR_CO2eq,Electric_CR_CO2eq,Diesel_CR_CO2eq), by = 'year') %>%
    left_join(emrate_by_tech_ldv, by = 'year')

  Assumptions_transitservice <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Revenue Mile Per Vehicle',] %>%
    select(area_type,transit_mode,value)|>
    rename(avg_vrm = value)
  
  Assumptions_transitservice2 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Average Passenger-Mile Per Vehicle',] %>%
    select(area_type,transit_mode,value)|>
    rename(avg_pax_mi_per_veh_mi = value)
  
  
  Assumptions_transitservice3 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Prior drive mode share of new riders',] %>%
    select(area_type,transit_mode,value)|>
    rename(prior_auto_mode_share = value)
    
  Assumptions_transitservice4 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Bus Priority Factors',] %>%
    filter_all(any_vars(!is.na(.)))
  
  Assumptions_transitservice5 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'On-Road Vehicle Fuel Economy',] %>%
    select(transit_mode, fuel_type,value) |> 
    rename(veh_fuel_economy = value)
  
  Assumptions_transitservice6 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Average Trip Length',] %>%
    select(area_type,transit_mode,value) |> 
    rename(avg_trip_miles = value)
  
  if(rvs$Baseline$land_use_factor == 1){lu_factor <- rvs$Assumptions[rvs$Assumptions$transit_category == "Land Use Multiplier" & !is.na(rvs$Assumptions$transit_category),"value"][[1]]} else {lu_factor =1}
  
  
  transitservice_output <- Inputs_transit %>%
    left_join(Assumptions_transitservice, by = c("area_type","transit_mode")) |>
    left_join(Assumptions_transitservice2,by = c('area_type','transit_mode')) |>
    #rename(pax_mi_fact = value) %>%
    left_join(Assumptions_transitservice3,by = c('area_type','transit_mode')) |>
    #rename(mode_fact = value) %>% 
    # start calculate vmt change
    mutate(add_vrm = value * avg_vrm,
           total_change_VMT = add_vrm *  -avg_pax_mi_per_veh_mi * prior_auto_mode_share,
           total_change_VMT = ifelse(transit_mode == "Demand Response", total_change_VMT, total_change_VMT*lu_factor)) |> 
    mutate(merge_col = paste0(transit_mode, ": ",fuel_type)) |>
    left_join(Assumptions_transitservice5, by = c( 'transit_mode','fuel_type')) |>
    #rename(fuel_econ = value) %>%
    mutate(allyear_emrate = case_when(merge_col == 'Bus: Diesel' ~ 1/veh_fuel_economy * fuelconv_ditoga * fuelfact_disblend*1000 + fuelfact_disCH4 +  fuelfact_disN20,
                                      merge_col == 'Bus: CNG' ~ 1/veh_fuel_economy * fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                      merge_col == 'Demand Response: Gasoline' ~ 1/veh_fuel_economy * fuelfact_gasblend * 1000 + fuelfact_gasCH4 + fuelfact_gasN20,
                                      merge_col == 'Demand Response: CNG' ~ 1/veh_fuel_economy *fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                      merge_col == 'Commuter Rail: Diesel' ~ Diesel_CR_CO2eq / avg_pax_mi_per_veh_mi,
                                      TRUE ~ 0),
           
           onroad_elect_emrate = case_when(merge_col %in% c('Bus: Electric','Demand Response: Electric') ~ 1/veh_fuel_economy *fuelconv_kwHtoga * electricity_carbon_content,
                                           merge_col %in% c('Light Rail / Streetcar: Electric','Heavy Rail: Electric')~ Electric_LR_CO2eq /avg_pax_mi_per_veh_mi,
                                           merge_col %in% c('Commuter Rail: Electric') ~ Electric_CR_CO2eq / avg_pax_mi_per_veh_mi,
                                           TRUE ~ 0 )
           ) |>
    mutate(displaced_auto = total_change_VMT*CO2e_millions/1000000,
           added_transit = add_vrm * (allyear_emrate + onroad_elect_emrate)/1000000) |>#end of adding columns from the On-Road Vehicle Emissions Rate (g CO2e per mile)


    ## need to join the average trip length parameters for increased fixed route service
    left_join(Assumptions_transitservice6, by = c('transit_mode','area_type')) |>
    #rename(trip_len = value) |>
    ## calculate the trip for increased fixed route service.
    mutate(total_change_newtrips = add_vrm*avg_pax_mi_per_veh_mi/avg_trip_miles/365) |>#end of calculate the new trips
    #  calcualte teh total change NOx %>%
    mutate(total_change_mtnox_auto = total_change_VMT * fuel_factorNox * base_impf/1000000,
           total_change_mtnox_transit = case_when(
             fuel_type == "Diesel" & category == 	"Public Transportation: Rail" ~ add_vrm * fuel_factordisloc_NOX/1000000,
             fuel_type == "Diesel" ~ add_vrm * fuel_factordisbus_NOX/1000000,
             fuel_type == "CNG" ~ add_vrm * fuel_factorCNGbus_NOX/1000000,
             fuel_type == "Gasoline" ~ add_vrm * fuel_factorgas_medduty_NOX/1000000,
             fuel_type == "Electric" ~ 0
             ),
           total_change_pm25_auto = total_change_VMT* (fuel_factorPMe*base_impf + fuel_factorPMtb)/1000000,
           total_change_pm25_transit = case_when(
             fuel_type == "Diesel" & category == 	"Public Transportation: Rail" ~ add_vrm * fuel_factordisloc_PM25/1000000,
             fuel_type == "Diesel" ~ add_vrm * fuel_factordisbus_PM25/1000000,
             fuel_type == "CNG" ~ add_vrm * fuel_factorCNGbus_PM25/1000000,
             fuel_type == "Gasoline" ~ add_vrm * fuel_factorgas_medduty_PM25/1000000,
             fuel_type == "Electric" ~ 0
           )
           ) %>%
    mutate(total_change_pm25_transit = ifelse(category == "Public Transportation: Rail", total_change_pm25_transit, total_change_pm25_transit + add_vrm * fuel_factorCNGbus_PM25TB/1000000)) |> 
    mutate(total_change_MTCO2  =  displaced_auto + added_transit,
           total_change_mtnox = total_change_mtnox_auto + total_change_mtnox_transit,
           total_change_pm25 = total_change_pm25_auto + total_change_pm25_transit)
  
  Assumptions_transitservice4 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Bus Priority Factors',] %>%
    filter_all(any_vars(!is.na(.)))
  for(unit in unique(Assumptions_transitservice4$unit)){
    assign(unit, Assumptions_transitservice4$value[Assumptions_transitservice4$unit == unit][[1]])
  }
  
  avg_pax_mi_per_veh_mi <- Assumptions_transitservice2$avg_pax_mi_per_veh_mi[Assumptions_transitservice2$area_type == "Urban" & Assumptions_transitservice2$transit_mode    == "Bus"][[1]]
  prior_auto_mode_share <- Assumptions_transitservice3$prior_auto_mode_share[Assumptions_transitservice3$area_type == "Urban" & Assumptions_transitservice3$transit_mode    == "Bus"][[1]]
  bus_priority_output <- Inputs_busprior |> 
    mutate(voms = value * routes_affected * daily_buses_per_route * route_hours_affected_pct * weekday_annualization) |> 
    mutate(total_change_VMT = -1 *voms * bus_priority_travel_time_change * bus_elasticity_trav_time * avg_pax_mi_per_veh_mi * prior_auto_mode_share,
           total_change_MTCO2 = total_change_VMT * CO2e_millions / 1000000) |> 
    mutate(total_change_mtnox = total_change_VMT * fuel_factorNox * base_impf/1000000,
           total_change_pm25 = total_change_VMT * (fuel_factorPMe * base_impf + fuel_factorPMtb)/1000000) |> 
    mutate(total_change_newtrips = 0)
    
  #browser()
  
  final_output <- rbind(
    transitservice_output |>
      group_by(year) %>%
      dplyr::summarise(across(c(total_change_VMT, total_change_MTCO2, total_change_mtnox, total_change_pm25,total_change_newtrips), ~sum(.x))),
    bus_priority_output |> 
      group_by(year) |> 
      dplyr::summarise(across(c(total_change_VMT, total_change_MTCO2, total_change_mtnox, total_change_pm25,total_change_newtrips), ~sum(.x)))
  ) |> 
    group_by(year) |> 
    dplyr::summarise(across(c(total_change_VMT, total_change_MTCO2, total_change_mtnox, total_change_pm25,total_change_newtrips), ~sum(.x)))
  #browser()
  return(final_output)
  
  # sum(transitservice_output$total_change_pm25_auto[transitservice_output$year == 2025]) + bus_priority_output$total_change_pm25[bus_priority_output$year == 2025]
  # sum(transitservice_output$total_change_pm25_transit[transitservice_output$year == 2025]) 
  # 
  # sum(transitservice_output$total_change_pm25_transit[transitservice_output$year == 2025 & transitservice_output$transit_mode == "Bus"]) 
  # transitservice_output[transitservice_output$year == 2025 & transitservice_output$transit_mode == "Bus",] |> View()
  # 
  # sum(transitservice_output$total_change_pm25_transit[transitservice_output$year == 2025 & transitservice_output$transit_mode == "Demand Response"]) 
  # 
  # 
  # sum(transitservice_output$total_change_pm25_transit[transitservice_output$year == 2025 & !(transitservice_output$transit_mode %in% c("Bus","Demand Response"))]) 
  
  })
   
cost_output_transitservice <- reactive({
  # get the follow values from the Fuel_Factors_Revision,
  fuelconv_ditoga <- Fuel_Factors_Revision$fuel_conversion[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disblend <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disCH4 <-  Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disN20 <-  Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelconv_cfCNGtoGas <- Fuel_Factors_Revision$fuel_conversion[Fuel_Factors_Revision$veh_subtype == 'CNG/LNG/LPG' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
  # fuelfact_cng <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'CNG/LNG/LPG' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
  fuelfact_cng <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "CNG" & Fuel_Factors_Baselines$units == "fuel_carbon_content"]
  
  fuelfact_cngN20 <- Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'CNG' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  
  fuelfact_gasblend <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'Gasoline ICE' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
  fuelfact_gasCH4 <- Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'SI HEV on Gas' & Fuel_Factors_Revision$veh_type == 'Light-Duty Trucks']
  fuelfact_gasN20 <-Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'SI PHEV 40' & Fuel_Factors_Revision$veh_type == 'Light-Duty Trucks']
  fuelconv_kwHtoga <- Fuel_Factors_Revision$electricity_conversion[Fuel_Factors_Revision$veh_subtype == 'SI PHEV 40' & Fuel_Factors_Revision$veh_type == 'Light-Duty Trucks']
  fuelfact_cngCH4 <- Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'CNG' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factordisbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factorCNGbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factorgas_medduty_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factordisbus_PM25 <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factorCNGbus_PM25 <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factorgas_medduty_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  
  
  
  fuel_factorCNGbus_PM25TB <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factorgas_PM25TB <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factorMHD_PM25TB <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-/Heavy-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factordisloc_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factordisloc_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  
  # get the electricity emission rate
  elect_emrate <- electricity_emrate() %>% group_by(year) %>%
    summarise(electricity_carbon_content =  unique(electricity_carbon_content))
  
  # get the emrate (use CO2e_millions)
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  #use CO2e_millions for emrate
  # use base_impf for emrate_by_Tech
  
  rail_factors <- passenger_rail_fuel_factors() %>% distinct()
  # replace prail with Electric_LR_CO2eq, this is exact same as Electric_HR_CO2eq
  # replace cmtrail with Electric_CR_CO2eq
  # for cmtrail_dis, use Diesel_CR_CO2eq
  #browser()
  # get assumptions input
  
  
  # get captial project tables: 
  Inputs_busprior <- rvs$Projects[rvs$Projects$table_no_ui == 5,] %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    group_by(area_type, fuel_type,transit_mode) %>%
    arrange(year) %>%
    ungroup() %>%
    select_if(~ any(!is.na(.))) %>%
    select(-value) |> 
    filter(year == rvs$Baseline$horizon_year_1) |> 
    left_join(emrate_by_tech_ldv, by = 'year') |> 
    #rename(bus_prioirty_mile = value) %>% 
    select(-table_no_ui,-unit,-category)
  
  
  Inputs_transit <-  rvs$Projects[rvs$Projects$table_no_ui %in% c(2,3,6),] %>%#, 4),] %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    group_by(table,area_type, fuel_type,transit_mode) %>%
    arrange(year) %>%
    ungroup() %>%
    select_if(~ any(!is.na(.))) %>%
    select(-value) |> filter(year == rvs$Baseline$horizon_year_1) |> 
    left_join(elect_emrate, by = as.character('year')) %>% # the variable to use: electricity_carbon_content
    left_join(select(rail_factors,year, Electric_LR_CO2eq,Electric_CR_CO2eq,Diesel_CR_CO2eq), by = 'year') %>%
    left_join(emrate_by_tech_ldv, by = 'year')
  
  Assumptions_transitservice <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Revenue Mile Per Vehicle',] %>%
    select(area_type,transit_mode,value)|>
    rename(avg_vrm = value)
  
  Assumptions_transitservice2 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Average Passenger-Mile Per Vehicle',] %>%
    select(area_type,transit_mode,value)|>
    rename(avg_pax_mi_per_veh_mi = value)
  
  
  Assumptions_transitservice3 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Prior drive mode share of new riders',] %>%
    select(area_type,transit_mode,value)|>
    rename(prior_auto_mode_share = value)
  
  Assumptions_transitservice4 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Bus Priority Factors',] %>%
    filter_all(any_vars(!is.na(.)))
  
  Assumptions_transitservice5 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'On-Road Vehicle Fuel Economy',] %>%
    select(transit_mode, fuel_type,value) |> 
    rename(veh_fuel_economy = value)
  
  Assumptions_transitservice6 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Average Trip Length',] %>%
    select(area_type,transit_mode,value) |> 
    rename(avg_trip_miles = value)
  
  if(rvs$Baseline$land_use_factor == 1){lu_factor <- rvs$Assumptions[rvs$Assumptions$transit_category == "Land Use Multiplier" & !is.na(rvs$Assumptions$transit_category),"value"][[1]]} else {lu_factor =1}
  
  #browser()
  transitservice_output <- Inputs_transit %>%
    ungroup() |> 
    left_join(Assumptions_transitservice, by = c("area_type","transit_mode")) |>
    left_join(Assumptions_transitservice2,by = c('area_type','transit_mode')) |>
    #rename(pax_mi_fact = value) %>%
    left_join(Assumptions_transitservice3,by = c('area_type','transit_mode')) |>
    #rename(mode_fact = value) %>% 
    # start calculate vmt change
    mutate(total_change_VMT_transit = avg_vrm,
           total_change_VMT_auto = total_change_VMT_transit *  -avg_pax_mi_per_veh_mi * prior_auto_mode_share,
           total_change_VMT_auto = ifelse(transit_mode == "Demand Response", total_change_VMT_auto, total_change_VMT_auto*lu_factor)) |> 
    mutate(merge_col = paste0(transit_mode, ": ",fuel_type)) |>
    left_join(Assumptions_transitservice5, by = c( 'transit_mode','fuel_type')) |>
    #rename(fuel_econ = value) %>%
    mutate(allyear_emrate = case_when(merge_col == 'Bus: Diesel' ~ 1/veh_fuel_economy * fuelconv_ditoga * fuelfact_disblend*1000 + fuelfact_disCH4 +  fuelfact_disN20,
                                      merge_col == 'Bus: CNG' ~ 1/veh_fuel_economy * fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                      merge_col == 'Demand Response: Gasoline' ~ 1/veh_fuel_economy * fuelfact_gasblend * 1000 + fuelfact_gasCH4 + fuelfact_gasN20,
                                      merge_col == 'Demand Response: CNG' ~ 1/veh_fuel_economy *fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                      merge_col == 'Commuter Rail: Diesel' ~ Diesel_CR_CO2eq / avg_pax_mi_per_veh_mi,
                                      TRUE ~ 0),
           
           onroad_elect_emrate = case_when(merge_col %in% c('Bus: Electric','Demand Response: Electric') ~ 1/veh_fuel_economy *fuelconv_kwHtoga * electricity_carbon_content,
                                           merge_col %in% c('Light Rail / Streetcar: Electric','Heavy Rail: Electric')~ Electric_LR_CO2eq /avg_pax_mi_per_veh_mi,
                                           merge_col %in% c('Commuter Rail: Electric') ~ Electric_CR_CO2eq / avg_pax_mi_per_veh_mi,
                                           TRUE ~ 0 )
    ) |>
    ungroup() |> 
    mutate(total_change_gGHG_auto = total_change_VMT_auto*CO2e_millions,
           total_change_gGHG_transit = total_change_VMT_transit * (allyear_emrate + onroad_elect_emrate),
           total_change_gGHG  =  total_change_gGHG_auto + total_change_gGHG_transit) |>
    left_join(Assumptions_transitservice6, by = c('transit_mode','area_type')) |>
    ## calculate the trip for increased fixed route service.
    mutate(total_change_newtrips = total_change_VMT_transit*avg_pax_mi_per_veh_mi/avg_trip_miles/365) |>#end of calculate the new trips
    ##  calculate the total change NOx %>%
    mutate(total_change_gnox_auto = total_change_VMT_auto * fuel_factorNox * base_impf,
           total_change_gnox_transit = case_when(
             fuel_type == "Diesel" & category == 	"Public Transportation: Rail" ~ total_change_VMT_transit * fuel_factordisloc_NOX,
             fuel_type == "Diesel" ~ total_change_VMT_transit * fuel_factordisbus_NOX,
             fuel_type == "CNG" ~ total_change_VMT_transit * fuel_factorCNGbus_NOX,
             fuel_type == "Gasoline" ~ total_change_VMT_transit * fuel_factorgas_medduty_NOX,
             fuel_type == "Electric" ~ 0
           ),
           total_change_gpm25_auto = total_change_VMT_auto * (fuel_factorPMe*base_impf + fuel_factorPMtb),
           total_change_gpm25_transit = case_when(
             fuel_type == "Diesel" & category == 	"Public Transportation: Rail" ~ total_change_VMT_transit * fuel_factordisloc_PM25,
             fuel_type == "Diesel" ~ total_change_VMT_transit * (fuel_factordisbus_PM25+fuel_factorCNGbus_PM25TB),
             fuel_type == "CNG" ~ total_change_VMT_transit * (fuel_factorCNGbus_PM25 + fuel_factorCNGbus_PM25TB),
             fuel_type == "Gasoline" ~ total_change_VMT_transit * (fuel_factorgas_medduty_PM25+fuel_factorgas_PM25TB),
             fuel_type == "Electric" ~ total_change_VMT_transit * fuel_factorMHD_PM25TB
           )
    ) %>%
    # mutate(total_change_gpm25_transit = ifelse(category!= "Public Transportation: Rail", total_change_gpm25_transit, total_change_gpm25_transit + total_change_VMT_transit * fuel_factorCNGbus_PM25TB)) |> 
    mutate(  total_change_VMT = ifelse(category == "Public Transportation: Rail", total_change_VMT_auto, 
                                       total_change_VMT_transit + total_change_VMT_auto),
           total_change_gnox = total_change_gnox_auto + total_change_gnox_transit,
           total_change_gpm25 = ifelse(fuel_type == "Electric" & category == 	"Public Transportation: Rail",
                                       total_change_gpm25_auto,
                                       total_change_gpm25_auto + total_change_gpm25_transit))|> 
    group_by(table, transit_mode, area_type, fuel_type) |> 
    dplyr::summarise(across(c(total_change_gGHG, total_change_VMT, total_change_gnox, total_change_gpm25,total_change_newtrips), ~sum(.x))) |> 
    ungroup()
  
  for(unit in unique(Assumptions_transitservice4$unit)){
    assign(unit, Assumptions_transitservice4$value[Assumptions_transitservice4$unit == unit][[1]])
  }
  
  avg_pax_mi_per_veh_mi <- Assumptions_transitservice2$avg_pax_mi_per_veh_mi[Assumptions_transitservice2$area_type == "Urban" & Assumptions_transitservice2$transit_mode    == "Bus"][[1]]
  prior_auto_mode_share <- Assumptions_transitservice3$prior_auto_mode_share[Assumptions_transitservice3$area_type == "Urban" & Assumptions_transitservice3$transit_mode    == "Bus"][[1]]
  avg_trip_miles = Assumptions_transitservice6$avg_trip_miles[Assumptions_transitservice6$area_type == "Urban" & Assumptions_transitservice6$transit_mode    == "Bus"][[1]]
 
   bus_priority_output <- Inputs_busprior |> 
    mutate(voms = routes_affected * daily_buses_per_route * route_hours_affected_pct * weekday_annualization) |> 
    mutate(total_change_VMT = -1 *voms * bus_priority_travel_time_change * bus_elasticity_trav_time * avg_pax_mi_per_veh_mi * prior_auto_mode_share,
           total_change_gGHG = total_change_VMT * CO2e_millions) |> 
    mutate(total_change_gnox = total_change_VMT * fuel_factorNox * base_impf,
           total_change_gpm25 = total_change_VMT * (fuel_factorPMe * base_impf + fuel_factorPMtb),
           total_change_newtrips = total_change_VMT/prior_auto_mode_share/avg_trip_miles/365) |> 
     mutate(transit_mode = "Bus",
            area_type = "All",
            fuel_type = "-") |> 
     group_by(table, transit_mode, area_type, fuel_type) |> 
     dplyr::summarise(across(c(total_change_gGHG, total_change_VMT, total_change_gnox, total_change_gpm25,total_change_newtrips), ~sum(.x)))
   
   output_transitservice_cost <- rbind(bus_priority_output,transitservice_output) 
   
  #browser()
  return(output_transitservice_cost)
})
