
## strategy 3: Micro
# Capital_Project_Inputs_Micro <- read.csv('./Data Extracts/Capital_Project_Inputs_Micro.csv') %>%
#   mutate(unit_2050 = unit_2025  + unit_2030  + unit_2050,
#          unit_2030  = unit_2025  + unit_2030 ) %>%  # calculate cumulative value
#   pivot_longer(cols = !(Program.Type:Unit), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'unit') %>%
#   left_join(.,emrate, by = 'year') %>%
#   left_join(.,emrate_by_tech, by = 'year')
# 
# ## load possible tables: 
# Strategy_Parameters <- read.csv('./Data Extracts/Strategy_Parameters.csv') %>%
#   mutate(selected_value = ifelse(!is.na(custom), custom,default))
# 
# Micro_base <- Capital_Project_Inputs_Micro %>%
#   mutate(Ebike_cost = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'E-bike cost'],
#          subsidy_cover = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Subsidy coverage (%)'],
#          weekly_trip = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike trips per week'],
#          trip_len = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Average trip length'],
#          auto_share = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Prior auto mode share'])%>%
#   mutate(total_vmt_change = unit * weekly_trip * -trip_len * auto_share*52,
#          total_CO2_change = total_vmt_change * emrate/1000000,
#          total_NOx_change = total_vmt_change* emrate_by_tech *ldv_weightedNOX/1000000,
#          total_PM25_change = (total_vmt_change * ldv_weightedPM25 * emrate_by_tech + total_vmt_change * ldv_weightedPM25TB)/1000000,
#          total_newtrips = weekly_trip  / 7 * unit
#   )  # end of Micro strategy

output_micro <- reactive({
#  observeEvent(input$state_input,{
  

    emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light Duty Vehicles')
    
    Capital_Project_Inputs_Micro <- rvs$Projects[rvs$Projects$table_no_ui == 8,] %>% #table 8 is micro in Projects
      mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                              year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                              year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
      arrange(year) %>%
      mutate(
        value = case_when(
          year > rvs$Baseline$horizon_year_1 ~ cumsum(value),
          TRUE ~ value)) %>%
      left_join(select(emrate_by_tech_ldv,year,CO2e_millions,base_impf), by = 'year') 
    
    Assumptions_micro <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 4,]  # table 4 is micro in Assumptions
    
    
    # get values from Fuel_Factors_Weighted()
    fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
    fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
    fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
    
    micro_output <- Capital_Project_Inputs_Micro %>%
      mutate(Ebike_cost = Assumptions_micro$value[Assumptions_micro$unit == 'e_bike_cost'],
             subsidy_cover = Assumptions_micro$value[Assumptions_micro$unit == 'subsidy_coverage_pct'],
             weekly_trip = Assumptions_micro$value[Assumptions_micro$unit == 'bike_trips_per_week'],
             trip_len = Assumptions_micro$value[Assumptions_micro$unit == 'avg_trip_miles'],
             auto_share = Assumptions_micro$value[Assumptions_micro$unit == 'prior_auto_mode_share'])%>%
      mutate(total_change_VMT = value * weekly_trip * -trip_len * auto_share*52,
             total_change_MTCO2 = total_change_VMT * CO2e_millions/1000000,
             total_change_mtnox = total_change_VMT* base_impf *fuel_factorNox/1000000,
             total_change_pm25 = (total_change_VMT * fuel_factorPMe * base_impf + total_change_VMT * fuel_factorPMtb)/1000000,
             total_newtrips = weekly_trip  / 7 * value
      )  # end of Micro strategy
    
    return(micro_output)
  })


cost_output_micro <- reactive({
  
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light Duty Vehicles')
  Assumptions_micro <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 4,]  # table 4 is micro in Assumptions
  
  # get the desired vars
  bike_per_week <- Assumptions_micro$value[Assumptions_micro$unit == 'bike_trips_per_week']
  avg_triplen <- Assumptions_micro$value[Assumptions_micro$unit == 'avg_trip_miles']
  priauto_share <- Assumptions_micro$value[Assumptions_micro$unit == 'prior_auto_mode_share']
  co2emrate <- emrate_by_tech_ldv$CO2e_millions[emrate_by_tech_ldv$year== rvs$Baseline$horizon_year_1]
  emrate_nox <- emrate_by_tech_ldv$base_impf[emrate_by_tech_ldv$year== rvs$Baseline$horizon_year_1]
  
  # get values from Fuel_Factors_Weighted()
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  
  
  output_micro_cost <- data.frame(
    ebike_subsidy = 'e-bike subsidies',
    total_change_gGHG = bike_per_week * avg_triplen * priauto_share * 52 * -co2emrate,
    total_change_VMT = bike_per_week * avg_triplen * priauto_share * 52,
    total_change_gnox = (bike_per_week * avg_triplen * priauto_share * 52) * fuel_factorNox * emrate_nox,
    total_change_gpm25 = (bike_per_week * avg_triplen * priauto_share * 52) * fuel_factorPMe * emrate_nox + (bike_per_week * avg_triplen * priauto_share * 52) * fuel_factorPMtb,
    total_change_newtrips = bike_per_week / 7
  )
  
  return(output_micro_cost)
})