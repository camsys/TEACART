
output_bikped <- reactive({
  #observeEvent(input$state_input,{
  
   
   #browser()
  req(EmRate_by_Tech())
  req(VMT_Type_Tech_Base())
  req(rvs)
  req(CO2e_Category_Averages())
  req(Fuel_Factors_Weighted())
  
  # preprocess the strategy parameters
  Assumptions_bikdped <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 1,] 
  
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  
  # calculate the Displaced Auto mile/yr
  Bicycle_and_Pedestrian_base <- Bike_Ped %>% 
    mutate(
      annual_displaced_auto_miles = 
        case_when(area_type == 'Core' ~ -(daily_new_bicyclists*
                                            Assumptions_bikdped$value[Assumptions_bikdped$element == 'Core ( greater than 10,000 ppsm)']*
                                            Assumptions_bikdped$value[Assumptions_bikdped$element == 'Bike'] +
                                            daily_new_walkers * Assumptions_bikdped$value[Assumptions_bikdped$element == 'Walk']*
                                            Assumptions_bikdped$value[Assumptions_bikdped$element == 'Core ( greater than 10,000 ppsm)'])*Assumptions_bikdped$value[Assumptions_bikdped$unit == 'annualization_factor'],
                                                  area_type == 'Urban' ~  -(daily_new_bicyclists*Assumptions_bikdped$value[Assumptions_bikdped$element == 'Urban (4,000 - 10,000 ppsm)']*
                                                                              Assumptions_bikdped$value[Assumptions_bikdped$element == 'Bike'] +
                                                                              daily_new_walkers * Assumptions_bikdped$value[Assumptions_bikdped$element == 'Walk']*
                                                                              Assumptions_bikdped$value[Assumptions_bikdped$element == 'Urban (4,000 - 10,000 ppsm)'])*
                                                    Assumptions_bikdped$value[Assumptions_bikdped$unit == 'annualization_factor'],
                                                  area_type == 'Suburban' ~  -(daily_new_bicyclists*Assumptions_bikdped$value[Assumptions_bikdped$element == 'Suburban (500 - 4,000 ppsm)']*
                                                                                 Assumptions_bikdped$value[Assumptions_bikdped$element == 'Bike'] +
                                                                                 daily_new_walkers * Assumptions_bikdped$value[Assumptions_bikdped$element == 'Walk']*
                                                                                 Assumptions_bikdped$value[Assumptions_bikdped$element == 'Suburban (500 - 4,000 ppsm)'])*
                                                    Assumptions_bikdped$value[Assumptions_bikdped$unit == 'annualization_factor'],
                                                  area_type == 'Rural' ~  -(daily_new_bicyclists*Assumptions_bikdped$value[Assumptions_bikdped$element == 'Rural ( less than 500 ppsm)']*
                                                                              Assumptions_bikdped$value[Assumptions_bikdped$element == 'Bike'] +
                                                                              daily_new_walkers * Assumptions_bikdped$value[Assumptions_bikdped$element == 'Walk']*
                                                                              Assumptions_bikdped$value[Assumptions_bikdped$element == 'Rural ( less than 500 ppsm)'])*
                                                    Assumptions_bikdped$value[Assumptions_bikdped$unit == 'annualization_factor']
    )) 
  
  captial_project_input_bkped <- rvs$Projects[rvs$Projects$table_no_ui == 1,] %>%
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                     year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                     year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
  group_by(area_type, facility_type) %>%
  arrange(year) %>%
  mutate(
    value = case_when(
      year > rvs$Baseline$horizon_year_1 ~ cumsum(value),
      TRUE ~ value)) %>%
  ungroup() %>%
      left_join(select(emrate_by_tech_ldv,year,CO2e_millions,base_impf), by = 'year') 
  
  ## table to return
  bike_ped_output <- Bicycle_and_Pedestrian_base %>% merge(.,captial_project_input_bkped, by = c('facility_type','area_type'), all = TRUE) %>% 
    mutate(total_change_VMT = annual_displaced_auto_miles * value,
           total_change_MTCO2 = total_change_VMT * CO2e_millions/1000000,
           total_change_newtrips = value * (daily_new_bicyclists + daily_new_walkers),
           total_change_mtnox = total_change_VMT * fuel_factorNox *base_impf /1000000,   # need replace fuel_factorNox
           total_change_pm25 = (total_change_VMT*fuel_factorPMe * base_impf  + total_change_VMT * fuel_factorPMtb)/1000000  #need replace fuel_factorPMtb
    ) # end of bike and ped strategy
  return(bike_ped_output)
}
)

# observeEvent(input$state_input,{
#   browser()
# })

cost_output_bikeped <- reactive({
  Assumptions_bikdped <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 1,] 
  
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light-Duty Vehicles')
  
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light-Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  
  # calculate the Displaced Auto mile/yr
  Bicycle_and_Pedestrian_base <- Bike_Ped %>% 
    mutate(annual_displaced_auto_miles = case_when(area_type == 'Core' ~ -(daily_new_bicyclists*Assumptions_bikdped$value[Assumptions_bikdped$element == 'Core ( greater than 10,000 ppsm)']*
                                                                             Assumptions_bikdped$value[Assumptions_bikdped$element == 'Bike'] +
                                                                             daily_new_walkers * Assumptions_bikdped$value[Assumptions_bikdped$element == 'Walk']*
                                                                             Assumptions_bikdped$value[Assumptions_bikdped$element == 'Core ( greater than 10,000 ppsm)'])*
                                                     Assumptions_bikdped$value[Assumptions_bikdped$unit == 'annualization_factor'],
                                                   area_type == 'Urban' ~  -(daily_new_bicyclists*Assumptions_bikdped$value[Assumptions_bikdped$element == 'Urban (4,000 - 10,000 ppsm)']*
                                                                               Assumptions_bikdped$value[Assumptions_bikdped$element == 'Bike'] +
                                                                               daily_new_walkers * Assumptions_bikdped$value[Assumptions_bikdped$element == 'Walk']*
                                                                               Assumptions_bikdped$value[Assumptions_bikdped$element == 'Urban (4,000 - 10,000 ppsm)'])*
                                                     Assumptions_bikdped$value[Assumptions_bikdped$unit == 'annualization_factor'],
                                                   area_type == 'Suburban' ~  -(daily_new_bicyclists*Assumptions_bikdped$value[Assumptions_bikdped$element == 'Suburban (500 - 4,000 ppsm)']*
                                                                                  Assumptions_bikdped$value[Assumptions_bikdped$element == 'Bike'] +
                                                                                  daily_new_walkers * Assumptions_bikdped$value[Assumptions_bikdped$element == 'Walk']*
                                                                                  Assumptions_bikdped$value[Assumptions_bikdped$element == 'Suburban (500 - 4,000 ppsm)'])*
                                                     Assumptions_bikdped$value[Assumptions_bikdped$unit == 'annualization_factor'],
                                                   area_type == 'Rural' ~  -(daily_new_bicyclists*Assumptions_bikdped$value[Assumptions_bikdped$element == 'Rural ( less than 500 ppsm)']*
                                                                               Assumptions_bikdped$value[Assumptions_bikdped$element == 'Bike'] +
                                                                               daily_new_walkers * Assumptions_bikdped$value[Assumptions_bikdped$element == 'Walk']*
                                                                               Assumptions_bikdped$value[Assumptions_bikdped$element == 'Rural ( less than 500 ppsm)'])*
                                                     Assumptions_bikdped$value[Assumptions_bikdped$unit == 'annualization_factor']
    )) 
  
  output_bikeped_cost <- Bicycle_and_Pedestrian_base %>%
    mutate(total_change_gGHG = annual_displaced_auto_miles * emrate_by_tech_ldv$CO2e_millions[emrate_by_tech_ldv$year == input$horizon_year_1],  # this number is little bit off dur to discrepencies in emrate by tech ldv
           total_change_VMT = annual_displaced_auto_miles,
           total_change_gnox = annual_displaced_auto_miles * emrate_by_tech_ldv$base_impf[emrate_by_tech_ldv$year == input$horizon_year_1] * fuel_factorNox,
           total_change_gpm25 = annual_displaced_auto_miles * emrate_by_tech_ldv$base_impf[emrate_by_tech_ldv$year == input$horizon_year_1] * fuel_factorPMe + 
             fuel_factorPMtb * annual_displaced_auto_miles,
           total_change_newtrips = daily_new_bicyclists  + daily_new_walkers )
  
  return(output_bikeped_cost)
})

