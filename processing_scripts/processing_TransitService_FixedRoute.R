library(dplyr)
library(tidyverse)

#Notes: 
#VOMS - Vehicles Operated in Maximum Service (VOMS)
#VRM - Vehicle Revenue Miles
#Fixed Route Bus = FR
#Demand Response = DR
#Bus Priority = BP
#Rail = RR
observeEvent(input$state_input, {
  #browser()


# INPUTS ------------------------------------------------------------------
#projects
  project_df_input_FR <- make_project_table_cumulative(rvs$Projects,
                                                    table_no = 2,
                                                    cols = c('area_type','fuel_type','transit_mode')) %>%
    get_horizon_years(my_rv = rvs)
  
  project_df_input_DR <- make_project_table_cumulative(rvs$Projects,
                                                          table_no = 3,
                                                          cols = c('area_type','fuel_type','transit_mode')) %>%
    get_horizon_years(my_rv = rvs)
  
  project_df_input_RR <- make_project_table_cumulative(rvs$Projects,
                                                       table_no = 6,
                                                       cols = c('area_type','fuel_type','transit_mode')) %>%
    get_horizon_years(my_rv = rvs) %>%
    mutate(area_type = "All")
  
  project_df_input <- rbind(project_df_input_FR, project_df_input_DR) %>% rbind(project_df_input_RR)
  
  #Factors
  #VRM Table
  vrm<-rvs$Assumptions[rvs$Assumptions$table_no_ui ==2 & rvs$Assumptions$unit == "rev_mi_per_veh",c("area_type","transit_mode","value")] %>%
    rename(VRM_per_Veh = value)
  
  temp_out <- project_df_input %>% left_join(vrm) %>% View()
})
# strategy 2: Transit Service Expansion

##the calculation of CO2 change will need input from the Baseline parameters, I will use hardcoded numbers for now, once combined the work, we can lookup the number.There numbers are used in the On-Road Vehicle Emissions Rate (g CO2e per mile).
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
# ## also factors from Electricity_EmRate
# # elect_emrate2025 <- 119.107002758621
# # elect_emrate2030 <- 95.2856022068966
# # elect_emrate2050 <- 0
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
# ## also factors from Passenger Rail
# # prail_2025 <- 29.67536144 # same for both heavy and light rail 
# # prail_2030 <- 23.7402891558906 
# # prail_2050 <-  0
# 
# prail <- c(29.67536144,23.7402891558906,0)
# prail <- data.frame(year,prail)
# 
# # cmtrail_2025 <- 55.2103756699422
# # cmtrail_2030 <- 44.1683005359538
# # cmtrail_2050 <- 0
# 
# cmtrail <- c(55.2103756699422,44.1683005359538,0)
# cmtrail <- data.frame(year,cmtrail)
# 
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
# 
# 
