output_transitservice_cuts <- reactive({
  #browser()
  # observeEvent(input$state_input,{
  
  # browser()
  # req(EmRate_by_Tech())
  # req(VMT_Type_Tech_Base())
  # req(rvs)
  # req(CO2e_Category_Averages())
  # req(Fuel_Factors_Weighted())
  # get the follow values from the Fuel_Factors_Revision,
  fuelconv_ditoga <- Fuel_Factors_Revision$fuel_conversion[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disblend <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disCH4 <-  Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelfact_disN20 <-  Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium-Duty Trucks']
  fuelconv_cfCNGtoGas <- Fuel_Factors_Revision$fuel_conversion[Fuel_Factors_Revision$veh_subtype == 'CNG/LNG/LPG' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
  fuelfact_cng <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'CNG/LNG/LPG' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
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
  fuel_factorgas_medduty_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factorCNGbus_PM25TB <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factordisloc_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factordisloc_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  # get the electricity emission rate
  elect_emrate <- electricity_emrate() %>% group_by(year) %>%
    summarise(electricity_carbon_content =  unique(electricity_carbon_content))
  
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  
  rail_factors <- passenger_rail_fuel_factors() %>% distinct()

  ntd_data <- NTD_Service[NTD_Service$state == input$state_input,c("transit_mode","area_type","total_vehicle_rev_miles","total_miles_per_veh")] |> 
    filter(transit_mode %in% c("Bus","Demand Response"))
  temp <- passenger_rail_miles() |> 
    filter(year == input$base_year) |> 
    select(-state,-year) |>
    pivot_longer(cols = where(is.numeric),values_to = "total_vehicle_rev_miles",
                 names_to = "transit_mode") |> 
    mutate(transit_mode = case_when(#transit_mode == "amtrak_miles" ~ "Amtrak",
                                    transit_mode == "lightrail_miles" ~ "Light Rail",
                                    transit_mode == "heavyrail_miles" ~ "Heavy Rail",
                                    transit_mode == "commuterrail_miles" ~ "Commuter Rail",
                                    TRUE ~ "FILTER")) |> 
    filter(transit_mode != "FILTER") |> 
    mutate(area_type = "All", 
           total_miles_per_veh = NA)
  ntd_data <- rbind(ntd_data,temp)
  
  Assumptions_transitservice2 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Average Passenger-Mile Per Vehicle',] %>%
    filter_all(any_vars(!is.na(.))) |> 
    select(area_type, transit_mode, value) |>
    rename(avg_pax_mi_per_veh_mi = value)
  Assumptions_transitservice2$transit_mode[Assumptions_transitservice2$transit_mode == "Light Rail / Streetcar"] <- "Light Rail"
  
  Assumptions_transitservice3 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Prior drive mode share of new riders',] %>%
    filter_all(any_vars(!is.na(.))) |> 
    select(area_type, transit_mode, value) |>
    rename(prior_auto_mode_share = value)
  Assumptions_transitservice3$transit_mode[Assumptions_transitservice3$transit_mode == "Light Rail / Streetcar"] <- "Light Rail"
  
  Assumptions_transitservice4 <- rvs$Assumptions[rvs$Assumptions$transit_category == 'Bus Priority Factors',] %>%
    filter_all(any_vars(!is.na(.))) |> 
    select()
  
  Assumptions_transitservice5 <- rvs$Assumptions[rvs$Assumptions$transit_category == 'On-Road Vehicle Fuel Economy',] %>%
    filter_all(any_vars(!is.na(.))) |> 
    select('transit_mode','fuel_type','value') |> 
    rename(fuel_econ = value)
    
  
  Assumptions_transitservice6 <- rvs$Assumptions[rvs$Assumptions$transit_category == 'Average Trip Length',] %>%
    filter_all(any_vars(!is.na(.))) |> 
    select('transit_mode','area_type','value') |> 
    rename(avg_trip_len = value)
  Assumptions_transitservice6$transit_mode[Assumptions_transitservice6$transit_mode == "Light Rail / Streetcar"] <- "Light Rail"

  transit_cuts_user_inputs <- rvs$Projects[rvs$Projects$table_no_ui == 15,] %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    group_by(area_type, fuel_type, transit_mode) %>%
    arrange(year) %>%
    mutate(value = case_when(year > rvs$Baseline$horizon_year_1 ~ cumsum(value)/100, #NOTE: the inputs should be percentages not fractions
                             TRUE ~ value/100)) %>%
    ungroup() %>%
    select_if(~ any(!is.na(.))) %>%
    rename(perc_service_cut = value) %>%
    select(-table_no_ui,-unit,-category, -table) |> 
    left_join(ntd_data) |> 
    mutate(dVRM = -1*perc_service_cut*total_vehicle_rev_miles) |> 
    left_join(Assumptions_transitservice2) |> left_join(Assumptions_transitservice3) |> 
    mutate(dVMT = -1*dVRM*avg_pax_mi_per_veh_mi*prior_auto_mode_share) |> 
    left_join(emrate_by_tech_ldv) |> 
    left_join(select(rail_factors,year,Diesel_CR_CO2eq,Diesel_HR_CO2eq,Diesel_LR_CO2eq), by = 'year') %>%
    mutate(fuel_type = case_when(transit_mode == "Bus" ~ "Diesel",
                                 transit_mode == "Demand Response"~"Gasoline",
                                 TRUE ~ "Diesel"
                                 )) |> 
    mutate(merge_col = paste0(transit_mode, ": ",fuel_type)) %>%
    left_join(Assumptions_transitservice5) |>
    mutate(allyear_emrate = case_when(merge_col == 'Bus: Diesel' ~ 1/fuel_econ * fuelconv_ditoga * fuelfact_disblend*1000 + fuelfact_disCH4 +  fuelfact_disN20,
                                      #merge_col == 'Bus: CNG' ~ 1/fuel_econ * fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                      merge_col == 'Demand Response: Gasoline' ~ 1/fuel_econ * fuelfact_gasblend * 1000 + fuelfact_gasCH4 + fuelfact_gasN20,
                                      #merge_col == 'Demand Response: CNG' ~ 1/fuel_econ *fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                      merge_col == 'Commuter Rail: Diesel' ~ Diesel_CR_CO2eq / avg_pax_mi_per_veh_mi,
                                      merge_col == 'Heavy Rail: Diesel' ~ Diesel_HR_CO2eq / avg_pax_mi_per_veh_mi,
                                      merge_col == 'Light Rail: Diesel' ~ Diesel_LR_CO2eq / avg_pax_mi_per_veh_mi)) |> 
    mutate(total_change_MTCO2  =  dVMT * CO2e_millions/1000000 + dVRM * allyear_emrate/1000000) %>%
mutate(remove_auto_MTCO2 = dVMT * CO2e_millions/1000000,
       remove_tranist_MTCO2 = dVRM * allyear_emrate/1000000) |>
    mutate(total_change_mtnox = dVMT * fuel_factorNox * base_impf,
           total_change_mtnox = case_when(merge_col == 'Bus: Diesel' ~ total_change_mtnox + dVRM * fuel_factordisbus_NOX,
                                          merge_col == 'Demand Response: Gasoline' ~ total_change_mtnox + dVRM * fuel_factorgas_medduty_NOX,
                                          merge_col == 'Heavy Rail: Diesel' ~ total_change_mtnox + dVRM * fuel_factordisloc_NOX,
                                          merge_col == 'Light Rail: Diesel' ~ total_change_mtnox + dVRM * fuel_factordisloc_NOX,
                                          merge_col == 'Commuter Rail: Diesel' ~ total_change_mtnox + dVRM * fuel_factordisloc_NOX
                                          ),
           total_change_pm25 = dVMT * (fuel_factorPMe * base_impf + fuel_factorPMtb), 
           total_change_pm25 = case_when(merge_col == 'Bus: Diesel' ~ total_change_pm25 + dVRM * fuel_factordisbus_PM25,
                                         merge_col == 'Demand Response: Gasoline' ~ total_change_pm25 + dVRM * fuel_factorgas_medduty_PM25,
                                         merge_col == 'Heavy Rail: Diesel' ~ total_change_pm25 + dVRM * fuel_factordisloc_PM25,
                                         merge_col == 'Light Rail: Diesel' ~ total_change_pm25 + dVRM * fuel_factordisloc_PM25,
                                         merge_col == 'Commuter Rail: Diesel' ~ total_change_pm25 + dVRM * fuel_factordisloc_PM25
           )) |> 
    left_join(Assumptions_transitservice6) |>
    mutate(total_new_trips = dVRM * avg_pax_mi_per_veh_mi/avg_trip_len/365)
  
  transit_cuts_output <-transit_cuts_user_inputs |> 
    group_by(year) |> 
    summarise(
      total_change_MTCO2 = sum(total_change_MTCO2,na.rm = TRUE),
      total_change_VMT = sum(dVMT,na.rm = TRUE),
      total_chage_newtrips =sum(total_new_trips,na.rm = TRUE),
      total_change_electricity = 0,
      total_change_direct = 0,
      total_change_upstream = 0,
      total_change_mtnox = sum(total_change_mtnox,na.rm = TRUE)/1000000,
      total_change_pm25 = sum(total_change_pm25,na.rm = TRUE)/1000000  
    )
  #browser()
  return(transit_cuts_output)
  
})

#this output is exactly the same as transit service increase cost output
cost_output_transitservice_cuts <- reactive({
  
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  co2emrate <- emrate_by_tech_ldv$CO2e_millions[emrate_by_tech_ldv$year== rvs$Baseline$horizon_year_1]
  
  # get values from Fuel_Factors_Weighted()
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  
  #fuel_factordisbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  
  fuel_factorCNGbus_PM25 <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factorCNGbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factorCNGbus_PM25TB <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  
  fuel_factordisbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factorgas_medduty_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  
  fuel_factordisbus_PM25 <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factorgas_medduty_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factorgas_medduty_PM25TB <-Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  
  fuel_factordisloc_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factordisloc_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  
  Assumptions_transitservice <- rvs$Assumptions[rvs$Assumptions$transit_category == 'Bus Priority Factors',] %>%
    filter_all(any_vars(!is.na(.)))
  
  transitservice_base <- output_TransitService() %>%
    filter(year == input$horizon_year_1) %>%
    slice(1:(n() - 4)) %>%
    filter(table != 'Fleet Electrification') %>%
    select(-'category',-'table_no_ui', -contains("total_"), - 'VOMS', -'unit',-'year',-'merge_col')
  
  if(rvs$Baseline$land_use_factor == 1){lu_factor <- rvs$Assumptions[rvs$Assumptions$transit_category == "Land Use Multiplier" & !is.na(rvs$Assumptions$transit_category),"value"][[1]]} else {lu_factor =1}
  
  output_transitservice_cost <- transitservice_base %>%
    mutate(allyear_emrate = ifelse(allyear_emrate != 0, allyear_emrate, onroad_elect_emrate)) %>%
    mutate(total_change_gGHG = avg_vrm * allyear_emrate + (avg_vrm * pax_mi_fact * mode_fact * -CO2e_millions),
           total_change_VMT = ifelse(grepl("Rail",transit_mode), - avg_vrm * mode_fact * pax_mi_fact * lu_factor,
                                     - avg_vrm * mode_fact * pax_mi_fact * lu_factor + avg_vrm),
           total_change_gnox = case_when(fuel_type == 'CNG' ~ -avg_vrm * mode_fact * pax_mi_fact * 
                                           fuel_factorNox * base_impf +(avg_vrm * fuel_factorCNGbus_NOX),
                                         fuel_type == 'Electric' ~ -avg_vrm * mode_fact * pax_mi_fact * fuel_factorNox * base_impf,
                                         fuel_type == 'Diesel' & transit_mode != 'Commuter Rail' ~ -avg_vrm * mode_fact * pax_mi_fact * 
                                           fuel_factorNox * base_impf +(avg_vrm * fuel_factordisbus_NOX),
                                         fuel_type == 'Gasoline' ~ -avg_vrm * mode_fact * pax_mi_fact * 
                                           fuel_factorNox * base_impf +(avg_vrm * fuel_factorgas_medduty_NOX),
                                         fuel_type == 'Diesel' & transit_mode == 'Commuter Rail' ~ -avg_vrm * mode_fact * pax_mi_fact * 
                                           fuel_factorNox * base_impf +(avg_vrm * fuel_factordisloc_NOX)),
           total_change_gpm25 = case_when(fuel_type == 'CNG' ~ -avg_vrm * mode_fact * 
                                            pax_mi_fact * fuel_factorPMe * base_impf +(-avg_vrm * mode_fact * pax_mi_fact * fuel_factorPMtb) +
                                            (avg_vrm * (fuel_factorCNGbus_PM25 + fuel_factorCNGbus_PM25TB)),
                                          fuel_type == 'Electric'  & table != 'Public Transportation: Rail (VOMS)' ~ -avg_vrm * mode_fact * 
                                            pax_mi_fact * fuel_factorPMe * base_impf +(-avg_vrm * mode_fact * pax_mi_fact * fuel_factorPMtb) +
                                            (avg_vrm * fuel_factorCNGbus_PM25TB),
                                          fuel_type == 'Electric'  & table == 'Public Transportation: Rail (VOMS)' ~ -avg_vrm * mode_fact * 
                                            pax_mi_fact * fuel_factorPMe * base_impf +(-avg_vrm * mode_fact * pax_mi_fact * fuel_factorPMtb),
                                          fuel_type == 'Diesel'& table != 'Public Transportation: Rail (VOMS)' ~ -avg_vrm * mode_fact * 
                                            pax_mi_fact * fuel_factorPMe * base_impf +(-avg_vrm * mode_fact * pax_mi_fact * fuel_factorPMtb) +
                                            (avg_vrm * (fuel_factorCNGbus_PM25TB + fuel_factordisbus_PM25)),
                                          fuel_type == 'Diesel' & table == 'Public Transportation: Rail (VOMS)' ~ -avg_vrm * mode_fact * 
                                            pax_mi_fact * fuel_factorPMe * base_impf +(-avg_vrm * mode_fact * pax_mi_fact * fuel_factorPMtb) +
                                            (avg_vrm * fuel_factordisloc_PM25),
                                          fuel_type == 'Gasoline' ~ -avg_vrm * mode_fact * 
                                            pax_mi_fact * fuel_factorPMe * base_impf +(-avg_vrm * mode_fact * pax_mi_fact * fuel_factorPMtb) +
                                            (avg_vrm * (fuel_factorgas_medduty_PM25 + fuel_factorgas_medduty_PM25TB))
           ),
           total_change_newtrips = avg_vrm * pax_mi_fact / trip_len / 365) %>% #mutate(table_name = paste0(transit_mode,": ",area_type,": ", fuel_type)) %>%
    
    add_row(table = 'Bus Priority',
            #table_name = 'Bus Priority',
            transit_mode = "Bus",
            area_type = "All",
            fuel_type = "All",
            total_change_gGHG = -prod(Assumptions_transitservice$value, na.rm = TRUE) * unique(.$CO2e_millions) * 
              unique(.$mode_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus'])*
              unique(.$pax_mi_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus']),
            total_change_VMT = -prod(Assumptions_transitservice$value, na.rm = TRUE) *
              unique(.$mode_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus'])*
              unique(.$pax_mi_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus']),
            total_change_gnox = -prod(Assumptions_transitservice$value, na.rm = TRUE) *
              unique(.$mode_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus'])*
              unique(.$pax_mi_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus']) * fuel_factorNox * unique(.$base_impf),
            total_change_gpm25 = -prod(Assumptions_transitservice$value, na.rm = TRUE) *
              unique(.$mode_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus'])*
              unique(.$pax_mi_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus']) * fuel_factorPMe * unique(.$base_impf) + 
              -prod(Assumptions_transitservice$value, na.rm = TRUE) *
              unique(.$mode_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus'])*
              unique(.$pax_mi_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus']) * fuel_factorPMtb,
            total_change_newtrips = prod(Assumptions_transitservice$value, na.rm = TRUE) *
              unique(.$mode_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus'])*
              unique(.$pax_mi_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus'])/unique(.$mode_fact[.$area_type == 'Urban' & .$transit_mode == 'Bus'])/
              unique(.$trip_len [.$area_type  == 'Urban' & .$transit_mode == 'Bus'])/365) %>%
    select_if(~all(!is.na(.)))
  
  
  return(output_transitservice_cost)
})

