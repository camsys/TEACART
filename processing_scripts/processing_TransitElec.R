# library(dplyr)
# library(tidyverse)
# 
# 
# 
# 
# 
# ## Strategy 5: Transit Electrification
# 
# 
# # fuelfactors:
# fuelfact_MHD_NOx <- 2.747851577        
# fuelfact_MHD_PM25 <- 0.0618538615272493
# 
# fuelconv_ditoga <- 0.892857142857143  # $C$71
# fuelfact_disblend <- 9.4 #C$9
# fuelfact_disCH4 <- 0.2375 #f$18
# fuelfact_disN20 <- 12.8438 #G$18
# fuelconv_cfCNGtoGas <-123.57     # C$73
# fuelfact_cng <- 0.05444  # C$10
# fuelfact_cngN20 <- 0.298  # G$20
# fuelfact_gasblend <- 7.94 #C$8
# fuelfact_gasCH4 <- 0.2 #F$14
# fuelfact_gasN20 <- 0.387 #G$14
# fuelconv_kwHtoga <- 33.9777387229057  # C$70
# 
# year <- c('2025','2030','2050')
# elect_emrate <- c(119.107002758621,95.2856022068966,0)
# elect_emrate <- data.frame(year,elect_emrate)
# 
# Strategy_Parameters <- read.csv('./Data Extracts/Strategy_Parameters.csv') %>%
#   mutate(selected_value = ifelse(!is.na(custom), custom,default))
# 
# Capital_Project_Inputs_transit_elec <- read.csv('./Data Extracts/Capital_Project_Inputs_transit_elec.csv') %>%
#   mutate(VOMS_2050 = VOMS_2025 + VOMS_2030 + VOMS_2050,
#          VOMS_2030 = VOMS_2025 + VOMS_2030) %>% # calculate cumulative value
#   pivot_longer(cols = !(category:typefull), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'VOMS') %>%
### Qi at this point, 
#   left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'Average pax-mi per vehicle-mile (load factor)', c('parameters','selected_value')], by = c('typefull' = 'parameters')) %>%
#   left_join(.,elect_emrate, by = 'year') %>%
#   rename(pax_mi_fact = selected_value) %>% 
#   mutate(merge_col = paste0(veh_type, ": ",vehicle_fuel_type)) %>%
#   left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'On-Road Vehicle Fuel Economy (mpgge)', c('parameters','selected_value')], by = c( 'merge_col' = 'parameters')) %>%
#   rename(fuel_econ = selected_value) %>%
#   mutate(allyear_emrate = case_when(merge_col == 'Bus: Diesel' ~ 1/fuel_econ * fuelconv_ditoga * fuelfact_disblend*1000 + fuelfact_disCH4 +  fuelfact_disN20,
#                                     merge_col == 'Bus: CNG' ~ 1/fuel_econ * fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
#                                     merge_col == 'Demand Response: Gasoline' ~ 1/fuel_econ * fuelfact_gasblend * 1000 + fuelfact_gasCH4 + fuelfact_gasN20,
#                                     merge_col == 'Demand Response: CNG' ~ 1/fuel_econ *fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20),
#          onroad_elect_emrate = case_when(merge_col %in% c('Bus: Electric','Demand Response: Electric') ~ 1/fuel_econ *fuelconv_kwHtoga * elect_emrate)) %>%
#   left_join(., Strategy_Parameters[Strategy_Parameters$subcat == 'Vehicle Revenue Mile per Vehicle', c('parameters','selected_value')], by = c('typefull' = 'parameters')) %>%
#   rename(avg_vrm = selected_value) %>%
#   mutate(affected_VRM = VOMS * avg_vrm,
#          ## calculate CO2 change
#          total_CO2_change = -affected_VRM * allyear_emrate/1000000) %>%
#   add_row(category = 'Total: Bus CO2 change', 
#           year = c('2025','2030','2050'),
#           total_CO2_change = c(sum(.$affected_VRM[grepl("Bus",.$typefull) & .$year == '2025'])*na.omit(.$onroad_elect_emrate[.$merge_col == 'Bus: Electric' & .$year == '2025']) /1000000,
#                                sum(.$affected_VRM[grepl("Bus",.$typefull) & .$year == '2030'])*na.omit(.$onroad_elect_emrate[.$merge_col == 'Bus: Electric' & .$year == '2030']) /1000000,
#                                sum(.$affected_VRM[grepl("Bus",.$typefull) & .$year == '2050'])*na.omit(.$onroad_elect_emrate[.$merge_col == 'Bus: Electric' & .$year == '2050']) /1000000)) %>%
#   add_row(category = 'Total: Demand Response change CO2 change',
#           year = c('2025','2030','2050'),
#           total_CO2_change = c(sum(.$affected_VRM[grepl("Demand Response",.$typefull) & .$year == '2025'])*na.omit(.$onroad_elect_emrate[.$merge_col == 'Demand Response: Electric' & .$year == '2025'])/1000000,
#                                sum(.$affected_VRM[grepl("Demand Response",.$typefull) & .$year == '2030'])*na.omit(.$onroad_elect_emrate[.$merge_col == 'Demand Response: Electric' & .$year == '2030'])/1000000,
#                                sum(.$affected_VRM[grepl("Demand Response",.$typefull) & .$year == '2050'])*na.omit(.$onroad_elect_emrate[.$merge_col == 'Demand Response: Electric' & .$year == '2050'])/1000000)) %>%
#   # calcualte total NOX change, this is calcualted as a sum
#   mutate(total_NOx_change = 0,
#          total_PM25_change = 0) %>% # placeholders
#   add_row(category = "Totals: Total Change NOx PM25",
#           year = c('2025','2030','2050'),
#           total_NOx_change = c(-sum(.$affected_VRM[.$year == '2025'], na.rm = TRUE)*fuelfact_MHD_NOx/1000000,
#                                -sum(.$affected_VRM[.$year == '2030'], na.rm = TRUE)*fuelfact_MHD_NOx/1000000,
#                                -sum(.$affected_VRM[.$year == '2050'], na.rm = TRUE)*fuelfact_MHD_NOx/1000000),
#           total_PM25_change = c(-sum(.$affected_VRM[.$year == '2025'], na.rm = TRUE)*fuelfact_MHD_PM25/1000000,
#                                 -sum(.$affected_VRM[.$year == '2030'], na.rm = TRUE)*fuelfact_MHD_PM25/1000000,
#                                 -sum(.$affected_VRM[.$year == '2050'], na.rm = TRUE)*fuelfact_MHD_PM25/1000000)) %>%
#   mutate_if(is.numeric, list(~replace_na(., 0)))
# #end of transit service strategy calculation
# 


output_transitElec <- reactive({
#  observeEvent(input$state_input,{
  
    #browser()
    # req(EmRate_by_Tech())
    # req(VMT_Type_Tech_Base())
    # req(rvs)
    # req(CO2e_Category_Averages())
    # req(Fuel_Factors_Weighted())
    
    # collect need variables
    
    fuelfact_MHD_NOx <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium/Heavy Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']        
    fuelfact_MHD_PM25 <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium/Heavy Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']

    # get the follow values from the Fuel_Factors_Revision,
    fuelconv_ditoga <- Fuel_Factors_Revision$fuel_conversion[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium Duty Trucks']
    fuelfact_disblend <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium Duty Trucks']
    fuelfact_disCH4 <-  Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium Duty Trucks']
    fuelfact_disN20 <-  Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'Diesel ICE' & Fuel_Factors_Revision$veh_type == 'Medium Duty Trucks']
    fuelconv_cfCNGtoGas <- Fuel_Factors_Revision$fuel_conversion[Fuel_Factors_Revision$veh_subtype == 'CNG/LNG/LPG' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
    fuelfact_cng <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'CNG/LNG/LPG' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
    fuelfact_cngN20 <- Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'CNG' & Fuel_Factors_Revision$veh_type == 'Medium Duty Trucks']
    fuelfact_gasblend <- Fuel_Factors_Revision$fuel_carbon_content[Fuel_Factors_Revision$veh_subtype == 'Gasoline ICE' & Fuel_Factors_Revision$veh_type == 'Passenger Cars']
    fuelfact_gasCH4 <- Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'SI HEV on Gas' & Fuel_Factors_Revision$veh_type == 'Light Duty Trucks']
    fuelfact_gasN20 <-Fuel_Factors_Revision$fuel_N20_CO2eq_per_mile[Fuel_Factors_Revision$veh_subtype == 'SI PHEV 40' & Fuel_Factors_Revision$veh_type == 'Light Duty Trucks']  
    fuelconv_kwHtoga <- Fuel_Factors_Revision$electricity_conversion[Fuel_Factors_Revision$veh_subtype == 'SI PHEV 40' & Fuel_Factors_Revision$veh_type == 'Light Duty Trucks']  
    fuelfact_cngCH4 <- Fuel_Factors_Revision$fuel_CH4_CO2e_per_mile[Fuel_Factors_Revision$veh_subtype == 'CNG' & Fuel_Factors_Revision$veh_type == 'Medium Duty Trucks']
    
    # get the electricity emission rate
    elect_emrate <- electricity_emrate() %>% group_by(year) %>%
      summarise(electricity_carbon_content =  unique(electricity_carbon_content))
    
    # get assumptions input
    Assumptions_transitelect <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Average Passenger-Mile Per Vehicle',] %>%
      select_if(~ any(!is.na(.)))

    Assumptions_transitelect2 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'On-Road Vehicle Fuel Economy',] %>%
      select_if(~ any(!is.na(.)))
    
    Assumptions_transitelect3 <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_category == 'Revenue Mile Per Vehicle',] %>%
      select_if(~ any(!is.na(.))) %>%
      select(-table_no_ui,-unit,-category, -table, -transit_category)
    
    
        # get projects input
    Capital_Project_Inputs_transit_elec <- rvs$Projects[rvs$Projects$table_no_ui == 4 | (rvs$Projects$table_no_ui == 3 & rvs$Projects$fuel_type == 'Electric'),] %>%
      filter(!(fuel_type == 'Diesel' & transit_mode == 'Demand Response')) %>%
      add_row(
        fuel_type = "Electric",
        year = c('horizon_year_1', 'horizon_year_2', 'horizon_year_3'),
        transit_mode = 'Bus'
      ) %>%
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
      rename(replacement_vehicles = value) %>% 
      select(-table_no_ui,-unit,-category, -table)
     
   
    transitelect_output <- Capital_Project_Inputs_transit_elec %>%
      left_join(select(Assumptions_transitelect,area_type,transit_mode,value),by = c('area_type','transit_mode')) %>%
      rename(avg_pax_mi_per_veh_mi = value) %>% 
    left_join(elect_emrate, by = c('year')) %>% # the variable to use: electricity_carbon_content
      left_join(select(Assumptions_transitelect2,fuel_type,transit_mode,value),by = c('fuel_type','transit_mode')) %>%
      rename(fuel_econ = value) %>% 
      # create a merge field
      mutate(merge_col = paste0(transit_mode, ": ",fuel_type)) %>%
      mutate(allyear_emrate = case_when(merge_col == 'Bus: Diesel' ~ 1/fuel_econ * fuelconv_ditoga * fuelfact_disblend*1000 + fuelfact_disCH4 +  fuelfact_disN20,
                                        merge_col == 'Bus: CNG' ~ 1/fuel_econ * fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
                                        merge_col == 'Demand Response: Gasoline' ~ 1/fuel_econ * fuelfact_gasblend * 1000 + fuelfact_gasCH4 + fuelfact_gasN20,
                                        merge_col == 'Demand Response: CNG' ~ 1/fuel_econ *fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20),
             onroad_elect_emrate = case_when(merge_col %in% c('Bus: Electric','Demand Response: Electric') ~ 1/fuel_econ *fuelconv_kwHtoga * electricity_carbon_content))%>%
      #mutate(onroad_all_rate = ifelse(!is.na(allyear_emrate), allyear_emrate,onroad_elect_emrate)) %>%
      left_join(select(Assumptions_transitelect3,area_type,transit_mode,value), by = c('transit_mode','area_type')) %>%
      rename(avg_vrm = value) %>%
      mutate(affected_VRM = ifelse(fuel_type != 'Electric', replacement_vehicles * avg_vrm,0),
                      ## calculate CO2 change
             total_change_MTCO2 = -affected_VRM * allyear_emrate/1000000) %>%
      add_row(area_type = 'All',
              transit_mode = 'Bus',
              year = c(rvs$Baseline$horizon_year_1,rvs$Baseline$horizon_year_2,rvs$Baseline$horizon_year_3),
              total_change_MTCO2 = c(sum(.$affected_VRM[.$transit_mode == 'Bus' & .$year == rvs$Baseline$horizon_year_1])*unique(na.omit(.$onroad_elect_emrate[.$merge_col == 'Bus: Electric' & .$year == rvs$Baseline$horizon_year_1])) /1000000,
                                   sum(.$affected_VRM[.$transit_mode == 'Bus' & .$year == rvs$Baseline$horizon_year_2])*unique(na.omit(.$onroad_elect_emrate[.$merge_col == 'Bus: Electric' & .$year == rvs$Baseline$horizon_year_2])) /1000000,
                                   sum(.$affected_VRM[.$transit_mode == 'Bus' & .$year == rvs$Baseline$horizon_year_3])*unique(na.omit(.$onroad_elect_emrate[.$merge_col == 'Bus: Electric' & .$year == rvs$Baseline$horizon_year_3])) /1000000)) %>%
      add_row(area_type = 'All',
              transit_mode = 'Demand Response',
              year = c(rvs$Baseline$horizon_year_1,rvs$Baseline$horizon_year_2,rvs$Baseline$horizon_year_3),
              total_change_MTCO2 = c(sum(.$affected_VRM[.$transit_mode == 'Demand Response' & .$year == rvs$Baseline$horizon_year_1])*unique(na.omit(.$onroad_elect_emrate[.$merge_col == 'Demand Response: Electric' & .$year == rvs$Baseline$horizon_year_1]))/1000000,
                                   sum(.$affected_VRM[.$transit_mode == 'Demand Response' & .$year == rvs$Baseline$horizon_year_2])*unique(na.omit(.$onroad_elect_emrate[.$merge_col == 'Demand Response: Electric' & .$year == rvs$Baseline$horizon_year_2]))/1000000,
                                   sum(.$affected_VRM[.$transit_mode == 'Demand Response' & .$year == rvs$Baseline$horizon_year_3])*unique(na.omit(.$onroad_elect_emrate[.$merge_col == 'Demand Response: Electric' & .$year == rvs$Baseline$horizon_year_3]))/1000000)) %>%
      mutate(total_change_mtnox = -affected_VRM *fuelfact_MHD_NOx/1000000,
             total_change_pm25 = -affected_VRM *fuelfact_MHD_PM25/1000000)
    
    return(transitelect_output)
    
    })

# observeEvent(input$state_input,{
#   browser()
# })


cost_output_transitselect<- reactive({

  fuel_factorCNGbus_PM25 <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']

  fuel_factorCNGbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']

  fuel_factordisbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']

  fuel_factorgas_medduty_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']

  fuel_factorgas_medduty_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']

  Assumptions_transitservice <- rvs$Assumptions[rvs$Assumptions$transit_category == 'Bus Priority Factors',] %>%
    filter_all(any_vars(!is.na(.)))
  
  transitelect_base <- output_transitElec() %>%
    filter(year == input$horizon_year_1) %>%
    filter(!(is.na(fuel_type))) %>%
    select( -contains("total_"))

  
  output_transitelect_cost <- transitelect_base %>%
    mutate(total_change_gGHG = case_when(transit_mode == 'Bus' ~-(avg_vrm * (allyear_emrate-unique(.$onroad_elect_emrate[merge_col == 'Bus: Electric']))),
                                         transit_mode == 'Demand Response' ~ -(avg_vrm * (allyear_emrate-unique(.$onroad_elect_emrate[merge_col == 'Demand Response: Electric'])))),
           total_change_VMT = 0,
           total_change_gnox = case_when(fuel_type == 'Diesel'~ -avg_vrm * fuel_factordisbus_NOX,
                                         fuel_type == 'CNG' ~ -avg_vrm * fuel_factorCNGbus_NOX,
                                         fuel_type == 'Gasoline' ~ -avg_vrm * fuel_factorgas_medduty_NOX),
           total_change_gpm25 = case_when(fuel_type == 'Diesel'~ - avg_vrm * fuel_factordisbus_PM25,
                                          fuel_type == 'CNG' ~ -avg_vrm * fuel_factorCNGbus_PM25,
                                          fuel_type == 'Gasoline' ~ -avg_vrm * fuel_factorgas_medduty_PM25),
           total_change_newtrips = 0) %>% 
    filter(fuel_type != 'Electric') %>%
    select_if(~all(!is.na(.))) %>%
    mutate(table_name = paste0(transit_mode,": ",fuel_type,": ", area_type)) %>%
    select(contains("total_"),table_name)
  
  
  return(output_transitelect_cost)
})
