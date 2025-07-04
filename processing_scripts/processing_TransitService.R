output_TransitService <- reactive({

   # observeEvent(input$state_input,{
  
  # browser()
  # req(EmRate_by_Tech())
  # req(VMT_Type_Tech_Base())
  # req(rvs)
  # req(CO2e_Category_Averages())
  # req(Fuel_Factors_Weighted())
  
  # # functions
  getvmtdiff <- function(year){

    return(unique(transit_service_base$bus_prioirty_mile[transit_service_base$year == year]) *  #ok
             Assumptions_transitservice4$value[Assumptions_transitservice4$unit == 'routes_affected'] *   #ok
             Assumptions_transitservice4$value[Assumptions_transitservice4$unit == 'daily_buses_per_route'] *  #ok
             Assumptions_transitservice4$value[Assumptions_transitservice4$unit == 'route_hours_affected_pct']* #ok
             Assumptions_transitservice4$value[Assumptions_transitservice4$unit == 'weekday_annualization']*  #ok
             Assumptions_transitservice2$value[Assumptions_transitservice2$area_type == 'Urban'& Assumptions_transitservice2$transit_mode == 'Bus']*  # correct
             Assumptions_transitservice4$value[Assumptions_transitservice4$unit == 'bus_priority_travel_time_change' ]*  # ok
             Assumptions_transitservice4$value[Assumptions_transitservice4$unit == 'bus_elasticity_trav_time']*  #ok
             -Assumptions_transitservice3$value[Assumptions_transitservice3$area_type == 'Urban'& Assumptions_transitservice3$transit_mode == 'Bus'])
  }

  getdisplacedAuto <- function(year_selected){
    if(rvs$Baseline$land_use_factor == 1){lu_factor <- rvs$Assumptions[rvs$Assumptions$transit_category == "Land Use Multiplier" & !is.na(rvs$Assumptions$transit_category),"value"][[1]]} else {lu_factor =1}
    
    transit_service_base <- transit_service_base %>%
      left_join(Assumptions_transitservice2,by = c('area_type','transit_mode')) %>%
      rename(pax_mi_fact = value) %>%
      left_join(Assumptions_transitservice3,by = c('area_type','transit_mode')) %>%
      rename(mode_fact = value) %>% 
      # start calculate vmt change
      mutate(add_vrm = VOMS * avg_vrm,
             total_change_VMT = add_vrm *  -pax_mi_fact * mode_fact * lu_factor) %>%
      mutate_if(is.numeric, list(~replace_na(., 0))) %>%
      filter(if_any(everything(), ~!is.na(.)))

    return(unique(getvmtdiff(year_selected)*
                    transit_service_base$CO2e_millions[transit_service_base$table == 'Public Transportation: Rail (VOMS)' & transit_service_base$year == year_selected]/1000000))
  }
  
  
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
  fuel_factorgas_medduty_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium-Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factorCNGbus_PM25TB <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
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

  # get assumptions input
  Assumptions_transitservice <- rvs$Assumptions[rvs$Assumptions$transit_category == 'Revenue Mile Per Vehicle',] %>%
    filter_all(any_vars(!is.na(.)))
  
  # get captial project tables: 
  Capital_Project_Inputs_publicTrans <- rvs$Projects[rvs$Projects$table_no_ui == 5,] %>%
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
    rename(bus_prioirty_mile = value) %>% 
    select(-table_no_ui,-unit,-category, -table)
  

  Capital_Project_Inputs_transit <-  rvs$Projects[rvs$Projects$table_no_ui %in% c(2,3,4,6),] %>%
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
    rename(VOMS = value) %>% 
    left_join(.,Capital_Project_Inputs_publicTrans, by = 'year') %>%
    left_join(elect_emrate, by = as.character('year')) %>% # the variable to use: electricity_carbon_content
    left_join(select(rail_factors,year, Electric_LR_CO2eq,Electric_CR_CO2eq,Diesel_CR_CO2eq), by = 'year') %>%
    left_join(emrate_by_tech_ldv, by = 'year')

  cate_list <- c("Transit: Increased Fixed Route Service (VOMS)", 
                 "Transit: Increased Demand Response Service (VOMS)", 
                 "Public Transportation: Rail (VOMS)" )
  transit_service_base <- Capital_Project_Inputs_transit %>% filter(table == 'Fleet Electrification') %>%
    mutate(avg_vrm = 0)
  temp <- data.frame()
  
  for (i in cate_list) {
    temp <- Capital_Project_Inputs_transit %>%
      filter(table == i) %>%
      left_join(select(Assumptions_transitservice, transit_mode,area_type,value), 
                by = c('area_type', 'transit_mode')
      ) %>% rename(avg_vrm = value)
    
    transit_service_base <- rbind(transit_service_base, temp)
    
  }
  
  Assumptions_transitservice2 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Average Passenger-Mile Per Vehicle',] %>%
    select_if(~ any(!is.na(.))) %>%
    select(-table_no_ui,-unit,-category, - table, - transit_category,-fuel_type) 
    
  
  Assumptions_transitservice3 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Prior drive mode share of new riders',] %>%
    select_if(~ any(!is.na(.))) %>% # the Prior drive mode share of new riders rows
  select(-table_no_ui,-unit,-category, - table, - transit_category,-fuel_type)
    
  Assumptions_transitservice4 <- rvs$Assumptions[rvs$Assumptions$transit_category == 'Bus Priority Factors',] %>%
    filter_all(any_vars(!is.na(.)))
  
  Assumptions_transitservice5 <- rvs$Assumptions[rvs$Assumptions$transit_category == 'On-Road Vehicle Fuel Economy',] %>%
    filter_all(any_vars(!is.na(.)))
  
  Assumptions_transitservice6 <- rvs$Assumptions[rvs$Assumptions$transit_category == 'Average Trip Length',] %>%
    filter_all(any_vars(!is.na(.)))
  
  
  transitservice_output <- transit_service_base %>%
    left_join(Assumptions_transitservice2,by = c('area_type','transit_mode')) %>%
    rename(pax_mi_fact = value) %>%
    left_join(Assumptions_transitservice3,by = c('area_type','transit_mode')) %>%
    rename(mode_fact = value) %>% 
    # start calculate vmt change
    mutate(add_vrm = VOMS * avg_vrm,
           total_change_VMT = add_vrm *  -pax_mi_fact * mode_fact) %>%
    # replace(is.na(.),0) %>%
    filter(if_any(everything(), ~!is.na(.))) %>%
    add_row(category = 'Total: Bus Priority Treatment VMT change',
            year = c(rvs$Baseline$horizon_year_1,rvs$Baseline$horizon_year_2,rvs$Baseline$horizon_year_3),
            total_change_VMT = c(getvmtdiff(rvs$Baseline$horizon_year_1),
                                 getvmtdiff(rvs$Baseline$horizon_year_2),
                                 getvmtdiff(rvs$Baseline$horizon_year_3))) %>%
     # add the net CO2 emission change
     ## need to join the On-Road Vehicle Fuel Economy (mpgge) section from the paramters 
     mutate(merge_col = paste0(transit_mode, ": ",fuel_type)) %>%
    left_join(select(Assumptions_transitservice5,value,transit_mode,fuel_type), by = c( 'transit_mode','fuel_type')) %>%
    rename(fuel_econ = value) %>%
    mutate(allyear_emrate = case_when(merge_col == 'Bus: Diesel' ~ 1/fuel_econ * fuelconv_ditoga * fuelfact_disblend*1000 + fuelfact_disCH4 +  fuelfact_disN20,
                                      merge_col == 'Bus: CNG' ~ 1/fuel_econ * fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                      merge_col == 'Demand Response: Gasoline' ~ 1/fuel_econ * fuelfact_gasblend * 1000 + fuelfact_gasCH4 + fuelfact_gasN20,
                                      merge_col == 'Demand Response: CNG' ~ 1/fuel_econ *fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                      merge_col == 'Commuter Rail: Diesel' ~ Diesel_CR_CO2eq / pax_mi_fact),
           onroad_elect_emrate = case_when(merge_col %in% c('Bus: Electric','Demand Response: Electric') ~ 1/fuel_econ *fuelconv_kwHtoga * electricity_carbon_content,
                                           merge_col %in% c('Light Rail / Streetcar: Electric','Heavy Rail: Electric')~ Electric_LR_CO2eq /pax_mi_fact,
                                           merge_col %in% c('Commuter Rail: Electric') ~ Electric_CR_CO2eq / pax_mi_fact)) %>% #end of adding columns from the On-Road Vehicle Emissions Rate (g CO2e per mile)
    mutate(total_change_MTCO2  =  case_when(merge_col %in% c('Bus: Diesel', 'Bus: CNG', 'Demand Response: Gasoline','Demand Response: CNG') ~
                                            total_change_VMT * CO2e_millions/1000000 + add_vrm * allyear_emrate/1000000,
                                          merge_col %in% c('Commuter Rail: Diesel') ~ add_vrm * allyear_emrate/1000000,
                                          merge_col %in% c('Bus: Electric','Demand Response: Electric') ~
                                            total_change_VMT * CO2e_millions/1000000 + add_vrm * onroad_elect_emrate /1000000,
                                          merge_col %in% c('Commuter Rail: Electric','Light Rail / Streetcar: Electric','Heavy Rail: Electric') ~
                                            add_vrm * onroad_elect_emrate /1000000)) %>%
    add_row(category = 'Totals: Displaced Auto',
            year = c(rvs$Baseline$horizon_year_1,rvs$Baseline$horizon_year_2,rvs$Baseline$horizon_year_3),
            total_change_MTCO2 = c(getdisplacedAuto(rvs$Baseline$horizon_year_1),
                                 getdisplacedAuto(rvs$Baseline$horizon_year_2),
                                 getdisplacedAuto(rvs$Baseline$horizon_year_3))) %>%
    add_row(category = 'Rail Displaced Auto',
            year = c(rvs$Baseline$horizon_year_1,rvs$Baseline$horizon_year_2,rvs$Baseline$horizon_year_3),
            total_change_MTCO2 = c(sum(.$total_change_VMT[.$table == 'Public Transportation: Rail (VOMS)' & .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE)*
              unique(na.omit(.$CO2e_millions[.$table == 'Public Transportation: Rail (VOMS)' & .$year == rvs$Baseline$horizon_year_1]))/1000000,
                                 sum(.$total_change_VMT[.$table == 'Public Transportation: Rail (VOMS)' & .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE)*
                unique(na.omit(.$CO2e_millions[.$table == 'Public Transportation: Rail (VOMS)' & .$year == rvs$Baseline$horizon_year_2]))/1000000,
              sum(.$total_change_VMT[.$table == 'Public Transportation: Rail (VOMS)' & .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE)*
                unique(na.omit(.$CO2e_millions[.$table == 'Public Transportation: Rail (VOMS)' & .$year == rvs$Baseline$horizon_year_3]))/1000000)) %>%#end of calculate co2 emission
    #  calculate the total trips:
    ## need to join the average trip length parameters for increased fixed route service
    left_join(select(Assumptions_transitservice6,value,transit_mode,area_type), by = c('transit_mode','area_type')) %>%
    rename(trip_len = value) %>%
    ## calculate the trip for increased fixed route service.
    mutate(total_newtrips = add_vrm*pax_mi_fact/trip_len/365) %>% #end of calculate the new trips
    #  calcualte teh total change NOx %>%
    mutate(total_change_mtnox = 0,
           total_change_pm25 = 0) %>% # placeholders
    add_row(category = "Totals: Total Change NOx & PM2.5",
            year = c(rvs$Baseline$horizon_year_1,rvs$Baseline$horizon_year_2,rvs$Baseline$horizon_year_3),
            total_change_mtnox = c((sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_1], na.rm = TRUE) * fuel_factorNox* unique(na.omit(.$base_impf[.$year == rvs$Baseline$horizon_year_1])) +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'Diesel' & .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factordisbus_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'CNG' & .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factorCNGbus_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'Gasoline' & .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factorgas_medduty_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'CNG' & .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factorCNGbus_NOX)/1000000 +
                                   sum(.$add_vrm[.$table == 'Public Transportation: Rail (VOMS)' & .$fuel_type == 'Diesel'  & .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factordisloc_NOX/1000000,
                                 (sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_2], na.rm = TRUE) * fuel_factorNox* unique(na.omit(.$base_impf[.$year == rvs$Baseline$horizon_year_2])) +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'Diesel' & .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factordisbus_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'CNG' & .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factorCNGbus_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'Gasoline' & .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factorgas_medduty_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'CNG' & .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factorCNGbus_NOX)/1000000 +
                                   sum(.$add_vrm[.$table == 'Public Transportation: Rail (VOMS)' & .$fuel_type == 'Diesel'  & .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factordisloc_NOX/1000000,
                                 (sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_3], na.rm = TRUE) * fuel_factorNox* unique(na.omit(.$base_impf[.$year == rvs$Baseline$horizon_year_3])) +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'Diesel' & .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factordisbus_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'CNG' & .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factorCNGbus_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'Gasoline' & .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factorgas_medduty_NOX +
                                    sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'CNG' & .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factorCNGbus_NOX)/1000000 +
                                   sum(.$add_vrm[.$table == 'Public Transportation: Rail (VOMS)' & .$fuel_type == 'Diesel'  & .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factordisloc_NOX/1000000),
            total_change_pm25 = c((sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_1],na.rm = TRUE)*fuel_factorPMe*unique(na.omit(.$base_impf[.$year == rvs$Baseline$horizon_year_1])) +
                                     sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factorPMtb +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'Diesel' & .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factordisbus_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'CNG'& .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factorCNGbus_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'Gasoline' & .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factorgas_medduty_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'CNG'& .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factorCNGbus_PM25 +
                                     sum(.$add_vrm[.$table %in% c('Transit: Increased Fixed Route Service (VOMS)', 'Transit: Increased Demand Response Service (VOMS)')& .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factorCNGbus_PM25TB)/1000000 +
                                    sum(.$add_vrm[.$table == 'Public Transportation: Rail (VOMS)' & .$fuel_type == 'Diesel'& .$year == rvs$Baseline$horizon_year_1],na.rm = TRUE) * fuel_factordisloc_PM25/1000000,
                                  (sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_2],na.rm = TRUE)*fuel_factorPMe*unique(na.omit(.$base_impf[.$year == rvs$Baseline$horizon_year_2])) +
                                     sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factorPMtb +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'Diesel' & .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factordisbus_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'CNG'& .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factorCNGbus_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'Gasoline' & .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factorgas_medduty_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'CNG'& .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factorCNGbus_PM25 +
                                     sum(.$add_vrm[.$table %in% c('Transit: Increased Fixed Route Service (VOMS)', 'Transit: Increased Demand Response Service (VOMS)')& .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factorCNGbus_PM25TB)/1000000 +
                                    sum(.$add_vrm[.$table == 'Public Transportation: Rail (VOMS)' & .$fuel_type == 'Diesel'& .$year == rvs$Baseline$horizon_year_2],na.rm = TRUE) * fuel_factordisloc_PM25/1000000,
                                  (sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_3],na.rm = TRUE)*fuel_factorPMe*unique(na.omit(.$base_impf[.$year == rvs$Baseline$horizon_year_3])) +
                                     sum(.$total_change_VMT[.$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factorPMtb +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'Diesel' & .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factordisbus_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Fixed Route Service (VOMS)' & .$fuel_type == 'CNG'& .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factorCNGbus_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'Gasoline' & .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factorgas_medduty_PM25 +
                                     sum(.$add_vrm[.$table == 'Transit: Increased Demand Response Service (VOMS)' & .$fuel_type == 'CNG'& .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factorCNGbus_PM25 +
                                     sum(.$add_vrm[.$table %in% c('Transit: Increased Fixed Route Service (VOMS)', 'Transit: Increased Demand Response Service (VOMS)')& .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factorCNGbus_PM25TB)/1000000 +
                                    sum(.$add_vrm[.$table == 'Public Transportation: Rail (VOMS)' & .$fuel_type == 'Diesel'& .$year == rvs$Baseline$horizon_year_3],na.rm = TRUE) * fuel_factordisloc_PM25/1000000)) %>%
    mutate_if(is.numeric, list(~replace_na(., 0)))
  
  return(transitservice_output)
  
})
   
cost_output_transitservice <- reactive({
  if(rvs$Baseline$land_use_factor == 1){lu_factor <- rvs$Assumptions[rvs$Assumptions$transit_category == "Land Use Multiplier" & !is.na(rvs$Assumptions$transit_category),"value"][[1]]} else {lu_factor =1}
  
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
