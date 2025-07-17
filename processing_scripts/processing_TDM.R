
# observe({browser()})
 output_TDM <- reactive({
  #observeEvent(input$state_input,{

  #browser()
  # req(rvs)
  # req(emrate_by_tech_ldv())
  # req(Fuel_Factors_Weighted())
  
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  
  Capital_Project_Inputs_TDM <- rvs$Projects[rvs$Projects$table_no_ui == 7,] %>% #table 7 is tdm in Projects
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    arrange(year) %>%
    mutate(
      value = case_when(
        year > rvs$Baseline$horizon_year_1 ~ cumsum(value),
        TRUE ~ value)) %>%
    left_join(select(emrate_by_tech_ldv,year,CO2e_millions,base_impf), by = 'year') 
  
  Assumptions_tdm <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 7,] # table 7 is TDM in Assumptions#slchanged
  
  # get values from Fuel_Factors_Weighted()
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  
  
  tdm_output <- Capital_Project_Inputs_TDM %>%
    mutate(wrkVMT_change = Assumptions_tdm$value[Assumptions_tdm$unit == 'avg_reduction_in_drive_along_pct'],
           wrktrip_len = Assumptions_tdm$value[Assumptions_tdm$unit == 'avg_work_trip_miles'],
           wrk_annualization = Assumptions_tdm$value[Assumptions_tdm$unit == 'annualization_factor'])%>%
    mutate(total_change_VMT = -value * wrkVMT_change * wrktrip_len * wrk_annualization*2,
           total_change_MTCO2 = total_change_VMT * CO2e_millions/1000000,
           total_change_mtnox = total_change_VMT* base_impf *fuel_factorNox/1000000,
           total_change_pm25 = (total_change_VMT * fuel_factorPMe * base_impf + total_change_VMT * fuel_factorPMtb)/1000000 )  # end of TDM strategy
  
  return(tdm_output)
})
 
 
 cost_output_TDM <- reactive({
   
   emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  Assumptions_tdm <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 7,]  # table 7 is TDM in Assumptions#slchanged
   
   # get the desired vars
   co2emrate <- emrate_by_tech_ldv$CO2e_millions[emrate_by_tech_ldv$year== rvs$Baseline$horizon_year_1]
   emrate_nox <- emrate_by_tech_ldv$base_impf[emrate_by_tech_ldv$year== rvs$Baseline$horizon_year_1]
   
   # get values from Fuel_Factors_Weighted()
   fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
   fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
   fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
   
   
   output_TDM_cost <- data.frame(
     comm_tripreduce = 'commuter trip reduction program',
     total_change_gGHG = -prod(Assumptions_tdm$value, na.rm = TRUE) * co2emrate * 2,
     total_change_VMT = -prod(Assumptions_tdm$value, na.rm = TRUE) *2, # why *2 here? ask Ben
     total_change_gnox = -prod(Assumptions_tdm$value, na.rm = TRUE) *2 * fuel_factorNox * emrate_nox,
     total_change_gpm25 =  -prod(Assumptions_tdm$value, na.rm = TRUE) *2 * fuel_factorPMe * emrate_nox + -prod(Assumptions_tdm$value, na.rm = TRUE) *2 * fuel_factorPMtb,
     total_change_newtrips = 0
   )
   
   return(output_TDM_cost)
 })
