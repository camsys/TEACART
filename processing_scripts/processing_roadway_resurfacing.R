output_roadway_resurf <- reactive({
  #browser()
  ff_weighted_temp <- Fuel_Factors_Weighted()
  
  NOx_LDV <- ff_weighted_temp$NOx_g_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_exhaust <-ff_weighted_temp$PM25_exhaust_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_tirebrakes <-ff_weighted_temp$PM25_tires_brakes_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  
  
  ptco2_temp <- pollutant_t_CO2ratio()
  NOx_CO2_ratio <- ptco2_temp$NOx_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
  NOx_CO2_ratio_heavy <- ptco2_temp$NOx_CO2_ratio[ptco2_temp$veh_supertype == "Medium-/Heavy-Duty Vehicles"]
  PM25_CO2_ratio <- ptco2_temp$PM25_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
  PM25_CO2_ratio_heavy <- ptco2_temp$PM25_CO2_ratio[ptco2_temp$veh_supertype == "Medium-/Heavy-Duty Vehicles"]
  
  temp_em_df_sub <- e_emmissions_apportionment() %>% ungroup()
  
  percent_truck_traffic <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$unit == "truck_traffic_pct" & rvs$Assumptions$road_class == "Freeway"] #slchanged
  
  TIST_Delta_Fuel_Costs <- -0.795
  Gas_Price <- 3.18
  value_mi_per_gal <- 19.20
  value_kg_per_gal <- 8.10
  em_rate_weight <- value_kg_per_gal/value_mi_per_gal*1000
  ghg_per_m <- value_kg_per_gal*(TIST_Delta_Fuel_Costs/25/Gas_Price*1000)*1000/322.5
  cost_per_lane_mile <- rvs$Costs$value[rvs$Costs$table_no_ui == 15]
  emrate_by_tech <- CO2e_Category_Averages() |> 
    mutate(veh_pivot = case_when(veh_supertype == "Light-Duty Vehicles" ~ "light", 
                                 veh_supertype == "Medium-/Heavy-Duty Vehicles" ~ "heavy", 
                                 TRUE ~ "oops")) |>
    pivot_wider(names_from = veh_pivot, values_from = CO2e_millions) |> group_by(year) |> 
    summarise(across(where(is.numeric), ~sum(.x, na.rm = T))) |>
    mutate(weighted_avg = light*(1-percent_truck_traffic)+heavy*percent_truck_traffic) |> 
    mutate(d2014 = weighted_avg/em_rate_weight) |> 
    mutate(ghg_per_lane_mile = d2014*ghg_per_m*cost_per_lane_mile/1000000)
  
  resurf <- rvs$Projects[rvs$Projects$table_no_ui == 15,] %>% #slchanged
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    group_by(area_type, fuel_type, transit_mode) %>%
    arrange(year) %>%
    mutate(value = case_when(year > rvs$Baseline$horizon_year_1 ~ cumsum(value)#/100  ## Qi note: this entry is mile not percent, shouldn't divide by 100
                             , #NOTE: the inputs should be percentages not fractions
                             TRUE ~ value
                             #/100
                             )) %>%
    ungroup() %>%
    select_if(~ any(!is.na(.))) %>% left_join(emrate_by_tech) |>
    left_join(temp_em_df_sub) |> 
    mutate(total_change_MTCO2 = value*ghg_per_lane_mile) |> 
    mutate(total_change_VMT = 0,
           total_change_newtrips = 0,
           total_change_electricity = total_change_MTCO2*electricity_per_em,
           total_change_direct = total_change_MTCO2-total_change_electricity) |> 
    mutate(total_change_mtnox = total_change_direct*percent_truck_traffic*NOx_CO2_ratio_heavy+total_change_direct*(1-percent_truck_traffic)*NOx_CO2_ratio,
           total_change_pm25 = total_change_direct*percent_truck_traffic*PM25_CO2_ratio_heavy+total_change_direct*(1-percent_truck_traffic)*PM25_CO2_ratio) 
  
  resurf_output <-resurf |> 
    group_by(year) |> 
    summarise(
      total_change_MTCO2 = sum(total_change_MTCO2,na.rm = TRUE),
      total_change_VMT =0,
      total_change_newtrips = sum(total_change_newtrips,na.rm = TRUE),
      total_change_electricity = sum(total_change_electricity),
      total_change_direct = sum(total_change_direct),
      total_change_upstream = 0,
      total_change_mtnox = sum(total_change_mtnox,na.rm = TRUE),
      total_change_pm25 = sum(total_change_pm25,na.rm = TRUE)  
    )
  #browser()
  return(resurf_output)
})

cost_output_roadway_resurf <- reactive({
  #browser()
  ff_weighted_temp <- Fuel_Factors_Weighted()
  
  NOx_LDV <- ff_weighted_temp$NOx_g_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_exhaust <-ff_weighted_temp$PM25_exhaust_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_tirebrakes <-ff_weighted_temp$PM25_tires_brakes_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  
  
  ptco2_temp <- pollutant_t_CO2ratio()
  NOx_CO2_ratio <- ptco2_temp$NOx_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
  NOx_CO2_ratio_heavy <- ptco2_temp$NOx_CO2_ratio[ptco2_temp$veh_supertype == "Medium-/Heavy-Duty Vehicles"]
  PM25_CO2_ratio <- ptco2_temp$PM25_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
  PM25_CO2_ratio_heavy <- ptco2_temp$PM25_CO2_ratio[ptco2_temp$veh_supertype == "Medium-/Heavy-Duty Vehicles"]
  
  temp_em_df_sub <- e_emmissions_apportionment() %>% ungroup()
  
  percent_truck_traffic <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$unit == "truck_traffic_pct" & rvs$Assumptions$road_class == "Freeway"] #slchanged
  
  TIST_Delta_Fuel_Costs <- -0.795
  Gas_Price <- 3.18
  value_mi_per_gal <- 19.20
  value_kg_per_gal <- 8.10
  em_rate_weight <- value_kg_per_gal/value_mi_per_gal*1000
  ghg_per_m <- value_kg_per_gal*(TIST_Delta_Fuel_Costs/25/Gas_Price*1000)*1000/322.5
  cost_per_lane_mile <- rvs$Costs$value[rvs$Costs$table_no_ui == 15]
  emrate_by_tech <- CO2e_Category_Averages() |> 
    mutate(veh_pivot = case_when(veh_supertype == "Light-Duty Vehicles" ~ "light", 
                                 veh_supertype == "Medium-/Heavy-Duty Vehicles" ~ "heavy", 
                                 TRUE ~ "oops")) |>
    pivot_wider(names_from = veh_pivot, values_from = CO2e_millions) |> group_by(year) |> 
    summarise(across(where(is.numeric), ~sum(.x, na.rm = T))) |>
    mutate(weighted_avg = light*(1-percent_truck_traffic)+heavy*percent_truck_traffic) |> 
    mutate(d2014 = weighted_avg/em_rate_weight) |> 
    mutate(ghg_per_lane_mile = d2014*ghg_per_m*cost_per_lane_mile/1000000) |> 
    filter(year == rvs$Baseline$horizon_year_1)
  temp <- output_roadway_resurf() |>  filter(year == rvs$Baseline$horizon_year_1)
  
  # nox_factor = temp$total_change_mtnox/temp$total_change_direct
  # pm25_factor = temp$total_change_pm25/temp$total_change_direct
  # if(temp$total_change_direct == 0){
  #   nox_factor = 0.001054894 #defaul maryland values in case there is no user inputs
  #   pm25_factor = 0.00002329709939
  # }
  
  #browser()
  
  resurf_fin <- data.frame(table = c("Roadway Resurfacing"),
                              #year =  input$horizon_year_1,
                              total_change_gGHG = emrate_by_tech[["ghg_per_lane_mile"]]*1000000,
                              total_change_VMT = 0,
                              total_change_newtrips = 0) |> 
    mutate(#total_change_gnox = total_change_gGHG*nox_factor,
           #total_change_gpm25 = total_change_gGHG*pm25_factor,
      total_change_gnox = total_change_gGHG*NOx_CO2_ratio,
      total_change_gpm25 = total_change_gGHG*PM25_CO2_ratio)
  return(resurf_fin)
})