library(dplyr)
library(tidyverse)

# strategy 2: Transit Service Expansion

# 
# 
# ##the calculation of CO2 change will need input from the Baseline parameters, I will use hardcoded numbers for now, once combined the work, we can lookup the number.There numbers are used in the On-Road Vehicle Emissions Rate (g CO2e per mile).
# fuelconv_ditoga <- 0.892857142857143  # $C$71
# fuelconv_kwHtoga <- 33.9777387229057  # C$70
# fuelconv_cfCNGtoGas <-123.57     # C$73
# ## also factors from the fuel factors tab
# fuelfact_disCH4 <- 0.2375 #f$18
# fuelfact_cngCH4 <- 105   #f$20
# fuelfact_gasblend <- 7.94 #C$8
# fuelfact_disblend <- 9.4 #C$9
# fuelfact_cng <- 0.05444  # C$10
# fuelfact_disN20 <- 12.8438 #G$18
# fuelfact_cngN20 <- 0.298  # G$20
# fuelfact_gasCH4 <- 0.2 #F$14
# fuelfact_gasN20 <- 0.387 #G$14
# 
# year <- c('2025','2030','2050')
# emrate <-c(310.271806608129,288.308700192269,256.867458798161)
# emrate <- data.frame(year,emrate)
# 
# elect_emrate <- c(119.107002758621,95.2856022068966,0)
# elect_emrate <- data.frame(year,elect_emrate)
# 
# emrate_by_tech <-c(0.948769678674915,0.88160943732177,0.785466327121931)
# emrate_by_tech <- data.frame(year,emrate_by_tech)
# 
# 
# prail <- c(29.67536144,23.7402891558906,0)
# prail <- data.frame(year,prail)
# 
# 
# cmtrail <- c(55.2103756699422,44.1683005359538,0)
# cmtrail <- data.frame(year,cmtrail)
# cmtrail_dis <-116.963992146663
# 
# #### Fuel Factor: Local Polluant
# ldv_weightedNOX <- 0.234684146669504 #C$36
# disbus_NOX <-  6.47421906690381  #C$38
# CNGbus_NOX <- 3.45416817736355 #C$39
# gas_medduty_NOX <- 1.63066666666667 #C$35
# disloc_NOX <- 0.0647421906690381  #C$40
# 
# ldv_weightedPM25 <- 0.005101903 #D$36
# disbus_PM25 <-  0.129072681337541  #D$38
# CNGbus_PM25 <- 0.0261338680041088 #D$39
# gas_medduty_PM25 <- 0.0413333333333333 #D$35
# disloc_PM25 <- 0.00129072681337541  #D$40
# ldv_weightedPM25TB <- 0.004  #E$36
# CNGbus_PM25TB <- 0.0116397943776201 #E$39
# 
# 
# # functions
# getvmtdiff <- function(year){
#   
#   return(unique(transit_service_base$bus_prioirty_mile[transit_service_base$year == year]) * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == '# Routes affected'] *
#            Strategy_Parameters$selected_value[Strategy_Parameters$parameters == '# Daily buses per route'] *
#            Strategy_Parameters$selected_value[Strategy_Parameters$parameters == '% Route-hours affected']*
#            Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Weekday annualization' & Strategy_Parameters$subcat == 'Bus Priority Factors']*
#            Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bus: Urban' & Strategy_Parameters$subcat == 'Average pax-mi per vehicle-mile (load factor)']*
#            Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bus priority % travel time change' ]*
#            Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bus ridership elasticity w/r/t travel time']*
#            -Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bus: Urban' & Strategy_Parameters$subcat == 'Prior drive mode share of new riders'])
# }
# 
# getdisplacedAuto <- function(year_selected){
#   transit_service_base <- transit_service_base %>% 
#     left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'Average pax-mi per vehicle-mile (load factor)', c('parameters','selected_value')], by = c('typefull' = 'parameters')) %>%
#     rename(pax_mi_fact = selected_value) %>% 
#     left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'Prior drive mode share of new riders', c('parameters','selected_value')], by = c('typefull' = 'parameters')) %>%
#     rename(mode_fact = selected_value) %>%
#     # start calculate vmt change
#     mutate(add_vrm = VOMS * avg_vrm,
#            total_vmt_change = add_vrm *  -pax_mi_fact * mode_fact) %>%
#     mutate_if(is.numeric, list(~replace_na(., 0))) %>%
#     filter(if_any(everything(), ~!is.na(.)))
#   
#   return(unique(sum(transit_service_base$total_vmt_change[transit_service_base$change_category == 'Rail' & transit_service_base$year == year_selected])*transit_service_base$emrate[transit_service_base$change_category == 'Rail' & transit_service_base$year == year_selected]/1000000))
# }
# 
# ## load possible tables: 
# Strategy_Parameters <- read.csv('./Data Extracts/Strategy_Parameters.csv') %>%
#   mutate(selected_value = ifelse(!is.na(custom), custom,default))
# 
# Capital_Project_Inputs_publicTrans <- read.csv('./Data Extracts/Capital_Project_Inputs_publicTrans.csv') %>%
#   mutate(unit_2050 = unit_2025  + unit_2030  + unit_2050,
#          unit_2030 = unit_2025  + unit_2030 ) %>%  # calculate cumulative value %>%
#   select(-'unit',-'expanded_bus_priority') %>%
#   pivot_longer(cols = everything(), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'bus_prioirty_mile')
# 
# Capital_Project_Inputs_transit <- read.csv('./Data Extracts/Capital_Project_Inputs_transit.csv') %>%
#   mutate(VOMS_2050 = VOMS_2025 + VOMS_2030 + VOMS_2050,
#          VOMS_2030 = VOMS_2025 + VOMS_2030) %>%  # calculate cumulative value
#   pivot_longer(cols = !(category:typefull), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'VOMS') %>% 
#   left_join(.,Capital_Project_Inputs_publicTrans, by = 'year') %>%
#   left_join(.,elect_emrate, by = 'year') %>%
#   left_join(.,cmtrail, by = 'year') %>%
#   left_join(.,prail, by = 'year')%>%
#   left_join(.,emrate, by = 'year') %>%
#   left_join(.,emrate_by_tech, by = 'year')
# 
# 
# 
# 
# cate_list <- c("Increased Fixed Route Service", "Increased Demand Response Service","Rail")
# transit_service_base <- Capital_Project_Inputs_transit %>% filter(change_category == 'Fleet Electrification') %>%
#   mutate(avg_vrm = 0)
# temp <- data.frame()
# for ( i in cate_list) {
#   temp <- Capital_Project_Inputs_transit %>% 
#     filter(change_category == i) %>%
#     left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'Vehicle Revenue Mile per Vehicle', c('parameters','selected_value')], by = c('typefull' = 'parameters')) %>%
#     rename(avg_vrm = selected_value)
#   transit_service_base = rbind(transit_service_base, temp)
# }
# 
# transit_service_base <- transit_service_base %>% 
#   left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'Average pax-mi per vehicle-mile (load factor)', c('parameters','selected_value')], by = c('typefull' = 'parameters')) %>%
#   rename(pax_mi_fact = selected_value) %>% 
#   left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'Prior drive mode share of new riders', c('parameters','selected_value')], by = c('typefull' = 'parameters')) %>%
#   rename(mode_fact = selected_value) %>%
#   # start calculate vmt change
#   mutate(add_vrm = VOMS * avg_vrm,
#          total_vmt_change = add_vrm *  -pax_mi_fact * mode_fact) %>%
#   replace(is.na(.),0) %>%
#   filter(if_any(everything(), ~!is.na(.))) %>%
#   add_row(category = 'Total: Bus Priority Treatment VMT change',
#           year = c('2025','2030','2050'),
#           total_vmt_change = c(getvmtdiff('2025'),getvmtdiff('2030'),getvmtdiff('2050'))) %>%
#   # add the net CO2 emission change
#   ## need to join the On-Road Vehicle Fuel Economy (mpgge) section from the paramters 
#   mutate(merge_col = paste0(veh_type, ": ",vehicle_fuel_type)) %>%
#   left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'On-Road Vehicle Fuel Economy (mpgge)', c('parameters','selected_value')], by = c( 'merge_col' = 'parameters')) %>%
#   rename(fuel_econ = selected_value) %>%
#   mutate(allyear_emrate = case_when(merge_col == 'Bus: Diesel' ~ 1/fuel_econ * fuelconv_ditoga * fuelfact_disblend*1000 + fuelfact_disCH4 +  fuelfact_disN20,
#                                     merge_col == 'Bus: CNG' ~ 1/fuel_econ * fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
#                                     merge_col == 'Demand Response: Gasoline' ~ 1/fuel_econ * fuelfact_gasblend * 1000 + fuelfact_gasCH4 + fuelfact_gasN20,
#                                     merge_col == 'Demand Response: CNG' ~ 1/fuel_econ *fuelconv_cfCNGtoGas * fuelfact_cng * 1000 + fuelfact_cngCH4 + fuelfact_cngN20,
#                                     merge_col == 'Commuter rail: Diesel' ~ cmtrail_dis / pax_mi_fact),
#          onroad_elect_emrate = case_when(merge_col %in% c('Bus: Electric','Demand Response: Electric') ~ 1/fuel_econ *fuelconv_kwHtoga * elect_emrate,
#                                          merge_col %in% c('Light rail/streetcar: Electric','Heavy rail: Electric')~ prail /pax_mi_fact,
#                                          merge_col %in% c('Commuter rail: Electric') ~ cmtrail / pax_mi_fact)) %>% #end of adding columns from the On-Road Vehicle Emissions Rate (g CO2e per mile)
#   mutate(total_CO2_change  =  case_when(merge_col %in% c('Bus: Diesel', 'Bus: CNG', 'Demand Response: Gasoline','Demand Response: CNG') ~ 
#                                           total_vmt_change * emrate/1000000 + add_vrm * allyear_emrate/1000000,
#                                         merge_col %in% c('Commuter rail: Diesel') ~ add_vrm * allyear_emrate/1000000,
#                                         merge_col %in% c('Bus: Electric','Demand Response: Electric') ~ 
#                                           total_vmt_change * emrate/1000000 + add_vrm * onroad_elect_emrate /1000000,
#                                         merge_col %in% c('Commuter rail: Electric','Light rail/streetcar: Electric','Heavy rail: Electric') ~
#                                           add_vrm * onroad_elect_emrate /1000000)) %>%
#   add_row(veh_type = 'Displaced Auto',
#           year = c('2025','2030','2050'),
#           total_CO2_change = c(getdisplacedAuto('2025'),getdisplacedAuto('2030'),getdisplacedAuto('2050'))) %>%
#   add_row(category = 'Totals: Displaced Auto', 
#           year = c('2025','2030','2050'),
#           total_CO2_change = c(sum(.$total_vmt_change[.$category == 'Total: Bus Priority Treatment VMT change' & .$year == '2025'],na.rm = TRUE)*
#                                  na.omit(unique(.$emrate[.$change_category == 'Rail' & .$year == '2025']))/1000000,
#                                sum(.$total_vmt_change[.$category == 'Total: Bus Priority Treatment VMT change' & .$year == '2030'],na.rm = TRUE)*
#                                  na.omit(unique(.$emrate[.$change_category == 'Rail' & .$year == '2030']))/1000000,
#                                sum(.$total_vmt_change[.$category == 'Total: Bus Priority Treatment VMT change' & .$year == '2050'],na.rm = TRUE)*
#                                  na.omit(unique(.$emrate[.$change_category == 'Rail' & .$year == '2050']))/1000000)) %>%#end of calculate co2 emission 
#   #  calculate the total trips: 
#   ## need to join the average trip length parameters for increased fixed route service
#   left_join(.,Strategy_Parameters[Strategy_Parameters$subcat == 'Average Trip Length (mi)', c('parameters','selected_value')], by = c('typefull' = 'parameters')) %>%
#   rename(trip_len = selected_value) %>%
#   ## calculate the trip for increased fixed route service. 
#   mutate(total_newtrips = add_vrm*pax_mi_fact/trip_len/365) %>% #end of calculate the new trips
#   #  calcualte teh total change NOx %>%
#   mutate(total_NOx_change = 0,
#          total_PM25_change = 0) %>% # placeholders
#   add_row(category = "Totals: Total Change NOx & PM2.5",
#           year = c('2025','2030','2050'),
#           total_NOx_change = c((sum(.$total_vmt_change[.$year == '2025'], na.rm = TRUE) * ldv_weightedNOX* unique(na.omit(.$emrate_by_tech[.$year == '2025'])) + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'Diesel' & .$year == '2025'],na.rm = TRUE) * disbus_NOX +
#                                   sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'CNG' & .$year == '2025'],na.rm = TRUE) * CNGbus_NOX + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'Gasoline' & .$year == '2025'],na.rm = TRUE) * gas_medduty_NOX + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'CNG' & .$year == '2025'],na.rm = TRUE) * CNGbus_NOX)/1000000 + 
#                                  sum(.$add_vrm[.$change_category == 'Rail' & .$vehicle_fuel_type == 'Diesel'  & .$year == '2025'],na.rm = TRUE) * disloc_NOX/1000000,
#                                (sum(.$total_vmt_change[.$year == '2030'], na.rm = TRUE) * ldv_weightedNOX* unique(na.omit(.$emrate_by_tech[.$year == '2030'])) + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'Diesel' & .$year == '2030'],na.rm = TRUE) * disbus_NOX +
#                                   sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'CNG' & .$year == '2030'],na.rm = TRUE) * CNGbus_NOX + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'Gasoline' & .$year == '2030'],na.rm = TRUE) * gas_medduty_NOX + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'CNG' & .$year == '2030'],na.rm = TRUE) * CNGbus_NOX)/1000000 + 
#                                  sum(.$add_vrm[.$change_category == 'Rail' & .$vehicle_fuel_type == 'Diesel'  & .$year == '2030'],na.rm = TRUE) * disloc_NOX/1000000,
#                                (sum(.$total_vmt_change[.$year == '2050'], na.rm = TRUE) * ldv_weightedNOX* unique(na.omit(.$emrate_by_tech[.$year == '2050'])) + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'Diesel' & .$year == '2050'],na.rm = TRUE) * disbus_NOX +
#                                   sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'CNG' & .$year == '2050'],na.rm = TRUE) * CNGbus_NOX + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'Gasoline' & .$year == '2050'],na.rm = TRUE) * gas_medduty_NOX + 
#                                   sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'CNG' & .$year == '2050'],na.rm = TRUE) * CNGbus_NOX)/1000000 + 
#                                  sum(.$add_vrm[.$change_category == 'Rail' & .$vehicle_fuel_type == 'Diesel'  & .$year == '2050'],na.rm = TRUE) * disloc_NOX/1000000),
#           total_PM25_change = c((sum(.$total_vmt_change[.$year == '2025'],na.rm = TRUE)*ldv_weightedPM25*unique(na.omit(.$emrate_by_tech[.$year == '2025'])) +
#                                    sum(.$total_vmt_change[.$year == '2025'],na.rm = TRUE) * ldv_weightedPM25TB + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'Diesel' & .$year == '2025'],na.rm = TRUE) * disbus_PM25 +
#                                    sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'CNG'& .$year == '2025'],na.rm = TRUE) * CNGbus_PM25 + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'Gasoline' & .$year == '2025'],na.rm = TRUE) * gas_medduty_PM25 + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'CNG'& .$year == '2025'],na.rm = TRUE) * CNGbus_PM25 + 
#                                    sum(.$add_vrm[.$change_category %in% c('Increased Fixed Route Service', 'Increased Demand Response Service')& .$year == '2025'],na.rm = TRUE) * CNGbus_PM25TB)/1000000 + 
#                                   sum(.$add_vrm[.$change_category == 'Rail' & .$vehicle_fuel_type == 'Diesel'& .$year == '2025'],na.rm = TRUE) * disloc_PM25/1000000,
#                                 (sum(.$total_vmt_change[.$year == '2030'],na.rm = TRUE)*ldv_weightedPM25*unique(na.omit(.$emrate_by_tech[.$year == '2030'])) +
#                                    sum(.$total_vmt_change[.$year == '2030'],na.rm = TRUE) * ldv_weightedPM25TB + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'Diesel' & .$year == '2030'],na.rm = TRUE) * disbus_PM25 +
#                                    sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'CNG'& .$year == '2030'],na.rm = TRUE) * CNGbus_PM25 + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'Gasoline' & .$year == '2030'],na.rm = TRUE) * gas_medduty_PM25 + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'CNG'& .$year == '2030'],na.rm = TRUE) * CNGbus_PM25 + 
#                                    sum(.$add_vrm[.$change_category %in% c('Increased Fixed Route Service', 'Increased Demand Response Service')& .$year == '2030'],na.rm = TRUE) * CNGbus_PM25TB)/1000000 + 
#                                   sum(.$add_vrm[.$change_category == 'Rail' & .$vehicle_fuel_type == 'Diesel'& .$year == '2030'],na.rm = TRUE) * disloc_PM25/1000000,
#                                 (sum(.$total_vmt_change[.$year == '2050'],na.rm = TRUE)*ldv_weightedPM25*unique(na.omit(.$emrate_by_tech[.$year == '2050'])) +
#                                    sum(.$total_vmt_change[.$year == '2050'],na.rm = TRUE) * ldv_weightedPM25TB + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'Diesel' & .$year == '2050'],na.rm = TRUE) * disbus_PM25 +
#                                    sum(.$add_vrm[.$change_category == 'Increased Fixed Route Service' & .$vehicle_fuel_type == 'CNG'& .$year == '2050'],na.rm = TRUE) * CNGbus_PM25 + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'Gasoline' & .$year == '2050'],na.rm = TRUE) * gas_medduty_PM25 + 
#                                    sum(.$add_vrm[.$change_category == 'Increased Demand Response Service' & .$vehicle_fuel_type == 'CNG'& .$year == '2050'],na.rm = TRUE) * CNGbus_PM25 + 
#                                    sum(.$add_vrm[.$change_category %in% c('Increased Fixed Route Service', 'Increased Demand Response Service')& .$year == '2050'],na.rm = TRUE) * CNGbus_PM25TB)/1000000 + 
#                                   sum(.$add_vrm[.$change_category == 'Rail' & .$vehicle_fuel_type == 'Diesel'& .$year == '2050'],na.rm = TRUE) * disloc_PM25/1000000)) %>%
#   mutate_if(is.numeric, list(~replace_na(., 0)))
# 
# #end of transit service strategy calculation


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
    transit_service_base <- transit_service_base %>%
      left_join(Assumptions_transitservice2,by = c('area_type','transit_mode')) %>%
      rename(pax_mi_fact = value) %>%
      left_join(Assumptions_transitservice3,by = c('area_type','transit_mode')) %>%
      rename(mode_fact = value) %>% 
      # start calculate vmt change
      mutate(add_vrm = VOMS * avg_vrm,
             total_change_VMT = add_vrm *  -pax_mi_fact * mode_fact) %>%
      mutate_if(is.numeric, list(~replace_na(., 0))) %>%
      filter(if_any(everything(), ~!is.na(.)))

    return(unique(getvmtdiff(year_selected)*
                    transit_service_base$CO2e_millions[transit_service_base$table == 'Public Transportation: Rail (VOMS)' & transit_service_base$year == year_selected]/1000000))
  }
  
  
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
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factordisbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factorCNGbus_NOX <-Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factorgas_medduty_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factordisbus_PM25 <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factorCNGbus_PM25 <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factorgas_medduty_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Medium Duty Trucks'& Fuel_Factors_Weighted()$veh_subtype == 'Gasoline/Diesel']
  fuel_factorCNGbus_PM25TB <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Bus'& Fuel_Factors_Weighted()$veh_subtype == 'CNG']
  fuel_factordisloc_PM25 <-Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  fuel_factordisloc_NOX <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Locomotives'& Fuel_Factors_Weighted()$veh_subtype == 'Diesel']
  # get the electricity emission rate
  elect_emrate <- electricity_emrate() %>% group_by(year) %>%
    summarise(electricity_carbon_content =  unique(electricity_carbon_content))
  
  # get the emrate (use CO2e_millions)
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light Duty Vehicles')
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
  # 
  # #end of transit service strategy calculation
  
  # check <- transitservice_output %>% group_by(year) %>%
  #   summarise(total_change_pm25 = sum(total_change_pm25),
  #             total_change_VMT = sum(total_change_VMT),
  #             total_change_MTCO2 = sum(total_change_MTCO2),
  #             total_newtrips = sum(total_newtrips),
  #             total_change_mtnox = sum(total_change_mtnox))
  
  
  return(transitservice_output)
  
})
   
   
   
