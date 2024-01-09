#load packages - delete in final deployment --------
library(dplyr)
library(tidyr)
library(stringr)

observeEvent(input$state_input, {
  
  browser()
#set inputs - delete in final deployment -------

#these should be pulled from EmRate Tech
#light_duty_automobile_emrate_2025 = 311
#light_duty_automobile_emrate_2030 = 290
#light_duty_automobile_emrate_2050 = 257
EmRate_by_Tech <- EmRate_by_Tech() %>%  
  mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>% View()
  select(year, 
         veh_type, fuel_type,
         veh_supertype, emission_rate)

VMT_Type_Tech_Base <- VMT_Type_Tech_Base()  %>% 
  mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>%
  select(year, veh_type, veh_subtype,
         veh_supertype, mmt_by_type)

temp_em_df_delay_improvement <- left_join(EmRate_by_Tech, VMT_Type_Tech_Base, by = c("year","veh_type","fuel_type"="veh_subtype", "veh_supertype")) %>% 
  group_by(year, veh_supertype) %>%
  summarise(cat_avg = sum(emission_rate*mmt_by_type))
  
temp_em_df <- left_join(EmRate_by_Tech, VMT_Type_Tech_Base, by = c("year","veh_type","fuel_type"="veh_subtype", "veh_supertype")) %>% 
  group_by(year, veh_supertype) %>%
  summarise(cat_avg = sum(emission_rate*mmt_by_type, na.rm = TRUE))

#light_duty_automobile_emrate = list(hz1 = temp_em_df$cat_avg[temp_em_df$year == rvs$Baseline$horizon_year_1 & temp_em_df$veh_supertype == "Light Duty Vehicles"],
#                                    hz2 = temp_em_df$cat_avg[temp_em_df$year == rvs$Baseline$horizon_year_2 & temp_em_df$veh_supertype == "Light Duty Vehicles"],
#                                    hz3 = temp_em_df$cat_avg[temp_em_df$year == rvs$Baseline$horizon_year_3 & temp_em_df$veh_supertype == "Light Duty Vehicles"])
# medium_heavy_duty_truck_emrate = list(hz1 = temp_em_df$cat_avg[temp_em_df$year == rvs$Baseline$horizon_year_1 & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"],
#                                       hz2 = temp_em_df$cat_avg[temp_em_df$year == rvs$Baseline$horizon_year_2 & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"],
#                                       hz3 = temp_em_df$cat_avg[temp_em_df$year == rvs$Baseline$horizon_year_3 & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])

#inputs ------

#user inputs
project_df_input <- rvs$Projects[rvs$Projects$table_no_ui == 14, c("year", "area_type", "road_class", "value")] %>%
  mutate(
    year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                      year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                      year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3))

#hardcode inputs
car_gallons_hour_delay = 0.4 # this is a hardcoded unmutable (hu) input
truck_gallons_hour_dealy = 1.7 # this is a hu input

#fuel factors inputs
gasoline_CO2_kg_per_gallon = Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type =="Gasoline" & Fuel_Factors_Baselines$units == "fuel_carbon_content"] #7.94 #this is a hu input from Fuel Factors tab
diesel_CO2_kg_per_gallon = Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type =="Diesel" & Fuel_Factors_Baselines$units == "fuel_carbon_content"] #9.4 #this is a hu input from Fuel Factors tab

#functions ----

calculate_roadway_expansion_emmision_rates_per_hours_delays <- 
  function(
    car_emrate, #this is from EmRate by Tech
    truck_emrate, #this is from EmRate by Tech
    percent_truck_traffic = .18, #this changes whether or not it's freeway or arterial
    
    car_gallons_hour_delay = 0.4, # this is a hardcoded unmutable (hu) input
    truck_gallons_hour_dealy = 1.7, # this is a hu input
    gasoline_CO2_kg_per_gallon = 7.94, #this is a hu input from Fuel Factors tab
    diesel_CO2_kg_per_gallon = 9.4 #this is a hu input from Fuel Factors tab
  ){
    delay_emrate = car_emrate*car_gallons_hour_delay*7.94*1000*(1-percent_truck_traffic) + truck_emrate*truck_gallons_hour_delay*7.94*1000*(percent_truck_traffic)
    return(delay_emrate)
  }

calculate_MT_CO2e_change <- function(
    total_lane_miles = list(y2025=1,y2035=2,y2050=3),
    VMTperLaneMile, #roadway depedent
    VMT_elasticity, #roadway depedent - I HAVE BIG BEN Q! WHY DOES THIS OFTEN EVALUATE TO ZERO FOR VMT CHANGE
    base_speed, #roadway depedent
    CO2em_per_hour_delay, #roadway dependent use first fucntion
    tspeed, #roadway dependent
    existing_lanes = 6 #roadway dependent
){
  annual_VMTperLaneMile = VMTperLaneMile*300
  minutes_delay_saved_perVMT = 0.2*(1-VMT_elasticity)/(1-0.67)
  
  minutes_per_mile_base = 60/base_speed
  minutes_per_mile_new = minutes_delay_saved_perVMT - minutes_per_mile_base
  
  new_speed = 1/(minutes_per_mile_new*60) #in mph
  speed_change = new_speed - base_speed
  
  VMT_change = lapply(total_lane_miles,
                      function(x) VMT_elasticity*annual_VMTperLaneMile*x)
  VMT_increase = lapply(VMT_change,
                        function(x) tspeed*x) # could I combine these two lapply?
  delay_reduction = lapply(total_lane_miles, 
                      function(x) -((annual_VMTperLaneMile*existing_lanes*x/2)*(minutes_delay_saved_perVMT/60)*CO2em_per_hour_delay)/1000000)
  net_CO2_change = list(y2025 = VMT_increase$y2025 + delay_reduction$y2025,
                        y2030 = VMT_increase$y2030 + delay_reduction$y2030,
                        y2050 = VMT_increase$y2050 + delay_reduction$y2050)
  
}

temp <- data.frame(year = c(rvs$Baseline$base_year, rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
temp <- expand(temp, year, area_type = c("Urban", "Rural"), road_class = c("Principal Arterial", "Freeway"))
temp1<-temp #%>% mutate(percent_truck_traffic = rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == road_class  & rvs$Assumptions$unit == "truck_traffic_pct"])
temp1 <- temp
temp1$percent_truck_traffic <- sapply(temp1$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x  & rvs$Assumptions$unit == "truck_traffic_pct"])
temp1$light_duty_automobile_emrate <- sapply(temp1$year, function(x) temp_em_df$cat_avg[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])
temp1$medium_heavy_duty_truck_emrate <- sapply(temp1$year, function(x) temp_em_df$cat_avg[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])
temp1$VMT_elasticity <- sapply(temp1$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x & rvs$Assumptions$unit == "VMT_elasticity_lane_mi"])
temp1$traveltime_elasticity <- sapply(temp1$area_type, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x & rvs$Assumptions$unit == "VMT_elasticity_trav_time"])
#need to change user input to principal arterial
tspeed_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "travel_speed_mph"]}
temp1$tspeed = apply(temp1, 1, tspeed_fun, c1 = "area_type", c2 = "road_class")
AADT_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "VMT_per_lane_mile"]}
temp1$VMTperLaneMile = apply(temp1, 1, AADT_fun, c1 = "area_type", c2 = "road_class")

temp1 %>% 
  mutate(delay_emrate = light_duty_automobile_emrate*car_gallons_hour_delay*7.94*1000*(1-percent_truck_traffic) + medium_heavy_duty_truck_emrate*truck_gallons_hour_delay*7.94*1000*(percent_truck_traffic),
         annual_VMTperLaneMile = VMTperLaneMile*300,
         minutes_delay_saved_perVMT = 0.2*(1-VMT_elasticity)/(1-0.67),
         minutes_per_mile_base = 60/tspeed,
         minutes_per_mile_new = minutes_delay_saved_perVMT - minutes_per_mile_base,
         new_speed = 1/(minutes_per_mile_new*60), #in mph
         speed_change = new_speed - tspeed) %>%
  left_join(project_df_input) %>%
  mutate(total_change_VMT = VMT_elasticity*annual_VMTperLaneMile*value,
         VMT_increase = tspeed*total_change_VMT,
         delay_reduction = -((annual_VMTperLaneMile*existing_lanes*x/2)*(minutes_delay_saved_perVMT/60)*CO2em_per_hour_delay)/1000000,
         
         total_change_MTCO2 = VMT_increase + delay_reduction,
         total_change_direct = total_change_MTCO2,
         total_change_electricity = 0,
         total_change_upstream = 0,
         total_change_mtnox = truck_vmt_affected * Fuel_Factors_Weighted[["Heavy Duty Trucks"]][["NOx_g_per_veh_mi_avg"]] / 1000000,
         total_change_pm25 = (truck_vmt_affected * Fuel_Factors_Weighted[["Heavy Duty Trucks"]][["PM25_exhaust_avg"]] +
                                truck_vmt_affected * Fuel_Factors_Weighted[["Heavy Duty Trucks"]][["PM25_tires_brakes_avg"]]) / 1000000)



})




