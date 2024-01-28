### LOAD -----------------------------------------------------------------------
library(glue)
library(readxl)
library(tidyverse)

### READ -----------------------------------------------------------------------

## strategy OPS
# Strategy_Parameters <-
#   read_csv('Data Extracts/Strategy_Parameters.csv')  |>  
#   select(-custom, -strategy) |>  ### strategy is not needed to uniquely identify parameter
#   reshape2::melt() |> 
#   reshape2::acast(list("subcat", "parameters"))
# 
# capital_inputs_ops <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "Capital Project Inputs", range = "B84:J87", 
#              col_names = c("improvement_type", "facility_area_type", "a", "b", "c", "d", "2025", "2030", "2050")) |> 
#   select(-(3:6)) |> 
#   pivot_longer(cols = !(improvement_type:facility_area_type), names_to = "year", values_to = "total_signals")
observeEvent(input$state_input, {
  
  browser()
  
EmRate_by_Tech <- EmRate_by_Tech() %>%
  mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>% 
  select(year, 
         veh_type, veh_subtype, apportionment, uses_electiricity,
         veh_supertype, emission_rate)

VMT_Type_Tech_Base <- VMT_Type_Tech_Base()  %>%
  mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>%
  select(year, veh_type, veh_subtype,
         veh_supertype, mmt_by_subtype)

temp_em_df_delay_improvement <- left_join(EmRate_by_Tech, VMT_Type_Tech_Base, by = c("year","veh_type","veh_subtype", "veh_supertype")) %>% 
  group_by(year, veh_supertype) %>%
  summarise(cat_avg = sum(emission_rate*mmt_by_subtype))

# temp_em_df <- left_join(EmRate_by_Tech, VMT_Type_Tech_Base, by = c("year","veh_type","veh_subtype", "veh_supertype")) %>% 
#   group_by(year, veh_supertype) %>%
#   summarise(cat_avg = sum(emission_rate*mmt_by_subtype, na.rm = TRUE))

temp_em_df <- CO2e_Category_Averages()

temp_em_df_sub <- e_emmissions_apportionment() %>% select(-veh_supertype)


#inputs ------

#user inputs
project_df_input <- make_project_table_cumulative(rvs$Projects,
                                                  table_no = 14,
                                                  cols = c('area_type','road_class'),
                                                  years_list = c(rvs$Baseline$horizon_year_1,
                                                                 rvs$Baseline$horizon_year_2,
                                                                 rvs$Baseline$horizon_year_3))

#hardcode inputs - I wonder if these should be part of the assumptions? or Capital Inputs?
car_gallons_hour_delay = 0.4 # this is a hardcoded unmutable (hu) input
truck_gallons_hour_delay = 1.7 # this is a hu input

#fuel factors inputs
gasoline_CO2_kg_per_gallon = Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type =="Gasoline" & Fuel_Factors_Baselines$units == "fuel_carbon_content"] #7.94 #this is a hu input from Fuel Factors tab
diesel_CO2_kg_per_gallon = Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type =="Diesel" & Fuel_Factors_Baselines$units == "fuel_carbon_content"] #9.4 #this is a hu input from Fuel Factors tab

ff_weighted_temp <- Fuel_Factors_Weighted()
NOx_LDV <- ff_weighted_temp$NOx_g_per_veh_mi[ff_weighted_temp$veh_type=="Light Duty Vehicles"]
PM25_LDV_exhaust <-ff_weighted_temp$PM25_exhaust_per_veh_mi[ff_weighted_temp$veh_type=="Light Duty Vehicles"]
PM25_LDV_tirebrakes <-ff_weighted_temp$PM25_tires_brakes_per_veh_mi[ff_weighted_temp$veh_type=="Light Duty Vehicles"]

ptco2_temp <- pollutant_t_CO2ratio()
NOx_CO2_ratio <- ptco2_temp$NOx_CO2_ratio[ptco2_temp$veh_supertype == "Light Duty Vehicles"]
PM25_CO2_ratio <- ptco2_temp$PM25_CO2_ratio[ptco2_temp$veh_supertype == "Light Duty Vehicles"]

#functions ----

#create dataframe
temp_output <- data.frame(year = c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
temp_output <- expand(temp_output, year, area_type = c("Urban", "Rural"), road_class = c("Principal Arterial", "Freeway"))

#add percent truck traffic
temp_output$percent_truck_traffic <- sapply(temp_output$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x  & rvs$Assumptions$unit == "truck_traffic_pct"])

temp_output$light_duty_automobile_emrate <- sapply(temp_output$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])
temp_output$medium_heavy_duty_truck_emrate <- sapply(temp_output$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])

temp_output$ldv_delay_emrate <- sapply(temp_output$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])*gasoline_CO2_kg_per_gallon*1000*car_gallons_hour_delay
temp_output$mhdv_delay_emrate <- sapply(temp_output$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])*diesel_CO2_kg_per_gallon*1000*truck_gallons_hour_delay
temp_output<-temp_output %>%
  mutate(road_class_delay_emrate = ldv_delay_emrate*(1-percent_truck_traffic)+mhdv_delay_emrate*percent_truck_traffic)

temp_output$ldv_impf <- sapply(temp_output$year, function(x) temp_em_df$base_impf [temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])


temp_output$VMT_elasticity <- sapply(temp_output$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x & rvs$Assumptions$unit == "VMT_elasticity_lane_mi"])
temp_output$traveltime_elasticity <- sapply(temp_output$area_type, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x & rvs$Assumptions$unit == "VMT_elasticity_trav_time"])

#need to change user input to principal arterial
tspeed_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "travel_speed_mph"]}
temp_output$tspeed = apply(temp_output, 1, tspeed_fun, c1 = "area_type", c2 = "road_class")
AADT_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "VMT_per_lane_mile"]}
temp_output$VMTperLaneMile = apply(temp_output, 1, AADT_fun, c1 = "area_type", c2 = "road_class")

project_signal <- make_project_table_cumulative(rvs$Projects[rvs$Projects$unit == "new_retimed_signal",],
                              table_no = 9, 
                              cols = c("area_type","road_class"), 
                              years_list = c(rvs$Baseline$horizon_year_1,
                                             rvs$Baseline$horizon_year_2,
                                             rvs$Baseline$horizon_year_3))

project_roundabout <- make_project_table_cumulative(rvs$Projects[rvs$Projects$unit == "new_roundabouts",],
                                                table_no = 9, 
                                                cols = c("area_type","road_class"),  
                                                years_list = c(rvs$Baseline$horizon_year_1,
                                                               rvs$Baseline$horizon_year_2,
                                                               rvs$Baseline$horizon_year_3))

### FUNCTIONS ------------------------------------------------------------------

get_vmt_change <- function(total_signals, aadt_at_signal, induced_elasticity){
    new_volume <- aadt_at_signal + (aadt_at_signal * corridor_travel_time_change * induced_elasticity)
    return(total_signals*(new_volume-aadt_at_signal)*corridor_length/signals_per_mile*365)
    # return(new_volume)
}

get_CO2_from_delay_reduction <- function(total_signals, aadt_at_signal, travel_speed, pct_truck_traffic, year){
  # browser()
  new_corridor_travel_speed <- travel_speed / (1+corridor_travel_time_change)
  travel_time_per_veh_change <- -(corridor_length/travel_speed)+
    (corridor_length/new_corridor_travel_speed)
  C02_per_hour_delay <- (1.7*fuel_factor_diesel*1000) * emrate_improvement_factor_mediumHeavy[year] * pct_truck_traffic + ### MIGHT NEED TO UPDATE TO MEDIUM HEAVY
    (0.4*fuel_factor_gasoline*1000) * emrate_improvement_factor_light[year] * (1-pct_truck_traffic)
  daily_total_reduction <- aadt_at_signal * travel_time_per_veh_change
  return(total_signals * daily_total_reduction * C02_per_hour_delay * 365 / 1000000)
  # return(daily_total_reduction)
  # return((1.7*fuel_factor_diesel*1000) * emrate_improvement_factor_light_2025)
}

get_CO2_from_VMT <- function(vmt_change, year){
  return(vmt_change*emrate_light[year]/1000000)
}

output <-
  capital_inputs_ops |> 
  mutate(VMT_CHANGE = if_else(improvement_type != "New or retimed signal", NA,
                              get_vmt_change(total_signals = total_signals, 
                                             aadt_at_signal = if_else(str_detect(facility_area_type, "Urban"),
                                                                      Strategy_Parameters["Average AADT", "Principal Arterial - Urban"],
                                                                      Strategy_Parameters["Average AADT", "Principal Arterial - Rural"]),
                                             induced_elasticity = if_else(str_detect(facility_area_type, "Urban"),
                                                                          Strategy_Parameters["Induced Travel Elasticities", "VMT w/r/t travel time (urban)"],
                                                                          Strategy_Parameters["Induced Travel Elasticities", "VMT w/r/t travel time (rural)"])))
  ) |> 
  mutate(C02_change_delay = if_else(improvement_type != "New or retimed signal", NA,
                                    get_CO2_from_delay_reduction(total_signals = total_signals, year = year,
                                                                 aadt_at_signal = if_else(str_detect(facility_area_type, "Urban"),
                                                                                          Strategy_Parameters["Average AADT", "Principal Arterial - Urban"],
                                                                                          Strategy_Parameters["Average AADT", "Principal Arterial - Rural"]),
                                                                 travel_speed = if_else(str_detect(facility_area_type, "Urban"), 
                                                                                        Strategy_Parameters["Travel Speed (mph)", "Principal Arterial - Urban"],
                                                                                        Strategy_Parameters["Travel Speed (mph)", "Principal Arterial - Rural"]),
                                                                 pct_truck_traffic = Strategy_Parameters["Percent Truck Traffic (%)", "Arterial"])) 
  ) |> 
  mutate(C02_change_VMT = if_else(improvement_type != "New or retimed signal", NA,
                                  get_CO2_from_VMT(vmt_change = VMT_CHANGE, year = year)),
         "MTCO2_change" = if_else(improvement_type == "New or retimed signal", 
                                  C02_change_VMT + C02_change_delay,
                                  if_else(str_detect(facility_area_type, "Urban"),
                                          total_signals * Strategy_Parameters["Average AADT", "Principal Arterial - Urban"] * 365 * roundabout_effect / 1000,
                                          total_signals * Strategy_Parameters["Average AADT", "Principal Arterial - Rural"] * 365 * roundabout_effect / 1000))
  ) |> 
  mutate(C02_change_delay = if_else(improvement_type == "New or retimed signal", C02_change_delay, MTCO2_change)) ## THIS IS A DUPLICATE BUT SIMPLIFIES CALCULATION

output_summarized <-
  output |> 
  group_by(year) |> 
  dplyr::summarize(total_change_VMT = sum(VMT_CHANGE, na.rm = T),
            total_change_MTCO2 = sum(MTCO2_change, na.rm = T),
            total_CO2_change_delay = sum(C02_change_delay, na.rm = T)) |> 
  mutate(total_change_mtnox = total_change_VMT * fuel_factor_ldv_weighted["nox"] * emrate_improvement_factor_ldv[year] / 1000000 + 
           total_CO2_change_delay * emrate_nox_ratio,
         total_change_pm25 = (total_change_VMT * fuel_factor_ldv_weighted["pm25_exhaust"] * 
                                emrate_improvement_factor_ldv[year] + 
                                total_change_VMT * 
                                fuel_factor_ldv_weighted["pm25_tiresBrakes"]) / 1000000 + total_CO2_change_delay * emrate_pm25_ratio,
         total_change_electricity = total_change_MTCO2 * electricity_pct_of_emissions[year],
         total_change_direct = total_change_MTCO2 - total_change_electricity)

### WIDE FORMAT (OLD) ----------------------------------------------------------
# capital_inputs_ops %>%
#   mutate(across(c("cuml_2025", "cuml_2030", "cuml_2050"), 
#                 ~if_else(improvement_type != "New or retimed signal", NA,
#                          get_vmt_change(total_signals = .x, 
#                                         aadt_at_signal = if_else(str_detect(facility_area_type, "Urban"),
#                                                                  Strategy_Parameters["Average AADT", "Principal Arterial - Urban"],
#                                                                  Strategy_Parameters["Average AADT", "Principal Arterial - Rural"]),
#                                         induced_elasticity = if_else(str_detect(facility_area_type, "Urban"),
#                                                                      Strategy_Parameters["Induced Travel Elasticities", "VMT w/r/t travel time (urban)"],
#                                                                      Strategy_Parameters["Induced Travel Elasticities", "VMT w/r/t travel time (rural)"]))),
#                 .names = "vmt_change_{.col}")
#   ) %>%
#   mutate(delay_reduction)

})