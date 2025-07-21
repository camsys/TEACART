output_land_use <- reactive({
  #browser()
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  
  ff_weighted_temp <- Fuel_Factors_Weighted()
  NOx_LDV <- ff_weighted_temp$NOx_g_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_exhaust <-ff_weighted_temp$PM25_exhaust_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_tirebrakes <-ff_weighted_temp$PM25_tires_brakes_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  
  
  ptco2_temp <- pollutant_t_CO2ratio()
  NOx_CO2_ratio <- ptco2_temp$NOx_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
  PM25_CO2_ratio <- ptco2_temp$PM25_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
  
  temp_em_df_sub <- e_emmissions_apportionment() %>% ungroup()
  
  
  assumptions <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 16,] %>% #slchanged
    filter_all(any_vars(!is.na(.))) |> 
    select(element,unit, value)
  land_use_factor_vmt <- -1/rvs$Costs$value[rvs$Costs$unit == "per_hh_shifted"]*assumptions$value[assumptions$element == "People per household"]*(assumptions$value[assumptions$element == "VMT per capita: Suburban (High VMT)"] - assumptions$value[assumptions$element == "VMT per capita: Urban (Low VMT)"])
  rezone_factor_vmt_residential <- assumptions$value[assumptions$element == "Elasticity of VMT w/r/t residential density"] * assumptions$value[assumptions$element == "Urban density"] * assumptions$value[assumptions$element == "VMT per household"] * assumptions$value[assumptions$element == "Urban density"]/assumptions$value[assumptions$element == "Suburban density"]
  rezone_factor_vmt_job <- 2*assumptions$value[assumptions$element == "Elasticity of VMT w/r/t job density"] * assumptions$value[assumptions$element == "Work trip length"] * assumptions$value[assumptions$element == "Work annualization factor"] * assumptions$value[assumptions$element == "Employees per acre at 1.0 FAR"]
  rezone_factor_vmt_high <- assumptions$value[assumptions$element == "TOD density - higher"] *  assumptions$value[assumptions$element == "VMT per household"] * assumptions$value[assumptions$element == "TOD density - higher"] / assumptions$value[assumptions$element == "Suburban density"] * assumptions$value[assumptions$element == "Elasticity of VMT w/r/t residential density"] +  
    2*assumptions$value[assumptions$element == "Work trip length"]*assumptions$value[assumptions$element == "Work annualization factor"] * assumptions$value[assumptions$element == "Elasticity of VMT w/r/t job density"] * assumptions$value[assumptions$element == "Employees per acre at 1.0 FAR"]
  rezone_factor_vmt_mod <- assumptions$value[assumptions$element == "TOD density - lower"] *  assumptions$value[assumptions$element == "VMT per household"] * assumptions$value[assumptions$element == "TOD density - lower"] / assumptions$value[assumptions$element == "Suburban density"] * assumptions$value[assumptions$element == "Elasticity of VMT w/r/t residential density"] +  
    2*assumptions$value[assumptions$element == "Work trip length"]*assumptions$value[assumptions$element == "Work annualization factor"] * assumptions$value[assumptions$element == "Elasticity of VMT w/r/t job density"] * assumptions$value[assumptions$element == "Employees per acre at 1.0 FAR"]
 
   temp <- rvs$Projects[rvs$Projects$table_no_ui == 16,] %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    group_by(land_use, unit) %>%
    arrange(year) %>%
    mutate(value = case_when(year > rvs$Baseline$horizon_year_1 ~ cumsum(value), #NOTE: the inputs should be percentages not fractions
                             TRUE ~ value)) |> 
    select(year, land_use, unit, value)

  rezone <- temp |> 
    left_join(emrate_by_tech_ldv) |> 
    left_join(temp_em_df_sub) %>% 
    mutate(total_change_VMT = case_when(land_use == "Increase Residential Density" ~ value*rezone_factor_vmt_residential,
                                        land_use == "Increase Job Density" ~ value*rezone_factor_vmt_job,
                                        land_use == "Mixed-Use TOD (moderate intensity)" ~ value*rezone_factor_vmt_mod,
                                        land_use == "Mixed-Use TOD (higher intensity)" ~ value*rezone_factor_vmt_high,
                                        land_use ==  "Land Use Incentives" ~ value*1000000*land_use_factor_vmt,
                                        TRUE ~ 0)) |> 
    mutate(total_change_MTCO2 = total_change_VMT*CO2e_millions/1000000,
           total_change_electricity = total_change_MTCO2*electricity_per_em,
           total_change_direct = total_change_MTCO2-total_change_electricity) |> 
    mutate(total_change_mtnox = total_change_VMT*NOx_LDV*base_impf /1000000,
           total_change_pm25 = (total_change_VMT*base_impf*PM25_LDV_exhaust+total_change_VMT*PM25_LDV_tirebrakes)/1000000) 
  
  rezone_fin <- rezone |>   group_by(year) %>% 
    summarise(
      total_change_MTCO2 = sum(total_change_MTCO2,na.rm = TRUE),
      total_change_VMT = sum(total_change_VMT,na.rm = TRUE),
      total_change_newtrips =0,
      total_change_electricity = sum(total_change_electricity,na.rm = TRUE),
      total_change_direct = sum(total_change_direct,na.rm = TRUE),
      total_change_upstream = 0,
      total_change_mtnox = sum(total_change_mtnox,na.rm = TRUE),
      total_change_pm25 = sum(total_change_pm25,na.rm = TRUE)
    ) 
  return(rezone_fin)
})

cost_output_land_use <- reactive({
  #browser()
  
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  
  ff_weighted_temp <- Fuel_Factors_Weighted()
  NOx_LDV <- ff_weighted_temp$NOx_g_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_exhaust <-ff_weighted_temp$PM25_exhaust_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_tirebrakes <-ff_weighted_temp$PM25_tires_brakes_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  
  
  ptco2_temp <- pollutant_t_CO2ratio()
  NOx_CO2_ratio <- ptco2_temp$NOx_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
  PM25_CO2_ratio <- ptco2_temp$PM25_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
  
  temp_em_df_sub <- e_emmissions_apportionment() %>% ungroup()
  
  
  assumptions <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 16,] %>% #slchanged
    filter_all(any_vars(!is.na(.))) |> 
    select(element,unit, value)
  land_use_factor_vmt <- -1000000/rvs$Costs$value[rvs$Costs$unit == "per_hh_shifted"]*assumptions$value[assumptions$element == "People per household"]*(assumptions$value[assumptions$element == "VMT per capita: Suburban (High VMT)"] - assumptions$value[assumptions$element == "VMT per capita: Urban (Low VMT)"])
 rezone_factor_vmt_residential <- 18*assumptions$value[assumptions$element == "Elasticity of VMT w/r/t residential density"] *  assumptions$value[assumptions$element == "VMT per household"] 
  rezone_factor_vmt_job <- 2*assumptions$value[assumptions$element == "Elasticity of VMT w/r/t job density"] * assumptions$value[assumptions$element == "Work trip length"] * assumptions$value[assumptions$element == "Work annualization factor"] * assumptions$value[assumptions$element == "Employees per acre at 1.0 FAR"]
  rezone_factor_vmt_high <- 18*25*assumptions$value[assumptions$element == "VMT per household"] * assumptions$value[assumptions$element == "Elasticity of VMT w/r/t residential density"] +  
    150*2*assumptions$value[assumptions$element == "Work trip length"]*assumptions$value[assumptions$element == "Work annualization factor"] * assumptions$value[assumptions$element == "Elasticity of VMT w/r/t job density"] * assumptions$value[assumptions$element == "Employees per acre at 1.0 FAR"]
  rezone_factor_vmt_mod <- 18*15*assumptions$value[assumptions$element == "VMT per household"] * assumptions$value[assumptions$element == "Elasticity of VMT w/r/t residential density"] +  
    100*2*assumptions$value[assumptions$element == "Work trip length"]*assumptions$value[assumptions$element == "Work annualization factor"] * assumptions$value[assumptions$element == "Elasticity of VMT w/r/t job density"] * assumptions$value[assumptions$element == "Employees per acre at 1.0 FAR"]
  
  #browser()
  land_use_base <- data.frame(table = c("Land Use (Smart Growth) Incentives"#, 
                                              #"Increased Residential Density",
                                              #"Increased Job Density",
                                              #"Mixed-Use TOD (higher intensity)",
                                              #"Mixed-Use TOD (moderate intensity)"
                                        ),
                                    year =  input$horizon_year_1,
                              total_change_gGHG = 1,
                              total_change_VMT = c(land_use_factor_vmt),#,rezone_factor_vmt_residential,rezone_factor_vmt_job,rezone_factor_vmt_high,rezone_factor_vmt_mod),
                              total_change_gnox = 1, 
                              total_change_gpm25 = 1,
                              total_change_newtrips = 0) |> 
    left_join(emrate_by_tech_ldv) |> 
    mutate(total_change_gGHG = total_change_VMT*CO2e_millions) |>
    mutate(total_change_gnox = total_change_VMT*NOx_LDV*base_impf,
           total_change_gpm25 = (total_change_VMT*base_impf*PM25_LDV_exhaust+total_change_VMT*PM25_LDV_tirebrakes)) |> 
    select(-c(year, veh_supertype, CO2e_millions, base_impf, delay_impf))
  return(land_use_base)
  
})
