
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
           total_newtrips = value * (daily_new_bicyclists + daily_new_walkers),
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

# 
# total_change_VMT = VMTperLaneMile*300*VMT_elasticity,
# total_change_gGHG = (VMTperLaneMile*300*existing_lanes*.5)*(minutes_delay_saved_perVMT/60)*road_class_delay_emrate+VMTperLaneMile*300*VMT_elasticity*light_duty_automobile_emrate, #I think this eq is wrong in excel
# total_change_gnox = total_change_MTCO2*NOx_LDV,
# total_change_gpm25 = total_change_MTCO2*(PM25_LDV_exhaust+PM25_LDV_tirebrakes),
# 


#library(dplyr)
# library(tidyverse)

# ## strategy 1: bicycle and pedestrian:
# Strategy_Parameters <- read.csv('./Data Extracts/Strategy_Parameters.csv')
# NTD_Service <- read.csv('./Data Extracts//NTD_Service.csv')
# #HPMS_Data <- read.csv('./Data Extracts//HPMS_Data.csv')
# captial_project_input_bkped <- read.csv('./Data Extracts//Capital_Project_Inputs_Bike&Ped.csv')
# Bicycle_and_Pedestrian <- read.csv('./Data Extracts//Bicycle_and_Pedestrian.csv')  #use as the base for Bicycle and pedestrian strategy
# 
# 
# ## need input from EmRate_by_Tech,
# year <- c('2025','2030','2050')
# emrate <-c(310.271806608129,288.308700192269,256.867458798161)
# emrate <- data.frame(year,emrate)
# 
# emrate_by_tech <-c(0.948769678674915,0.88160943732177,0.785466327121931)
# emrate_by_tech <- data.frame(year,emrate_by_tech)
# 
# ## the following three are from the local pollutants, - check with Seth
# fuel_factorNox <- 0.234684146669504
# fuel_factorPMe <- 0.00510190278963835
# fuel_factorPMtb <- 0.004
# 
# # preprocess the strategy parameters
# Strategy_Parameters <- Strategy_Parameters %>%
#   mutate(selected_value = ifelse(!is.na(custom), custom,default))
# 
# # calculate the Displaced Auto mile/yr
# Bicycle_and_Pedestrian_base <- Bicycle_and_Pedestrian %>%
#   mutate(annual_displaced_auto_miles = case_when(grepl("core urban",per_new_facility_mile, ignore.case = TRUE) ~ -(daily_new_bicyclists*
#                                                                                                                      Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Core (>10,000 ppsm)']*
#                                                                                                                      Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike'] +
#                                                                                                                      daily_new_walkers * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Walk']*
#                                                                                                                      Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Core (>10,000 ppsm)'])*
#                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'Bicycle and Pedestrian Data'],
#                                                  grepl("- urban",per_new_facility_mile, ignore.case = TRUE) ~  -(daily_new_bicyclists*
#                                                                                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Urban (4,000 - 10,000 ppsm)']*
#                                                                                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike'] +
#                                                                                                                    daily_new_walkers * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Walk']*
#                                                                                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Urban (4,000 - 10,000 ppsm)'])*
#                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'Bicycle and Pedestrian Data'],
#                                                  grepl("suburban",per_new_facility_mile, ignore.case = TRUE) ~  -(daily_new_bicyclists*
#                                                                                                                     Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Suburban (500 - 4,000 ppsm)']*
#                                                                                                                     Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike'] +
#                                                                                                                     daily_new_walkers * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Walk']*
#                                                                                                                     Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Suburban (500 - 4,000 ppsm)'])*
#                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'Bicycle and Pedestrian Data'],
#                                                  grepl("rural",per_new_facility_mile, ignore.case = TRUE) ~  -(daily_new_bicyclists*
#                                                                                                                  Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Rural (<500 ppsm)']*
#                                                                                                                  Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike'] +
#                                                                                                                  daily_new_walkers * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Walk']*
#                                                                                                                  Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Rural (<500 ppsm)'])*
#                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'Bicycle and Pedestrian Data']
#   ))
# ## process the captial project Inputs for bicycle and Pedestrian
# captial_project_input_bkped <- captial_project_input_bkped %>%
#   mutate(facilitiesmile_2050 = facilitiesmile_2025 + facilitiesmile_2030 + facilitiesmile_2050,
#          facilitiesmile_2030 = facilitiesmile_2025 + facilitiesmile_2030) %>%
#   pivot_longer(cols = !(facility_type:area_type), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'facilitiesmile') %>%
#   left_join(.,emrate_by_tech, by = 'year') %>%
#   left_join(.,emrate, by = 'year')
# 
# Bicycle_and_Pedestrian_base <- Bicycle_and_Pedestrian_base %>% merge(.,captial_project_input_bkped, by.x = 'per_new_facility_mile', by.y = "facility_type", all = TRUE) %>%
#   mutate(total_vmt_change = annual_displaced_auto_miles * facilitiesmile,
#          total_CO2_change = total_vmt_change * emrate/1000000,
#          total_newtrips = facilitiesmile * (daily_new_bicyclists + daily_new_walkers),
#          total_NOx_change = total_vmt_change * fuel_factorNox *emrate_by_tech/1000000,
#          total_PM25_change = (total_vmt_change*fuel_factorPMe * emrate_by_tech + total_vmt_change * fuel_factorPMtb)/1000000
#   ) # end of bike and ped strategy

# output_bikeped <-  reactive({
#   req(EmRate_by_Tech(), eemrate(), rvs)
#
# })