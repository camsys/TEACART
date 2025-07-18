#Emissions Output
output_RoadwayExp <- reactive({
  
#set inputs - delete in final deployment -------

#these should be pulled from EmRate Tech
#light_duty_automobile_emrate_2025 = 311
#light_duty_automobile_emrate_2030 = 290
#light_duty_automobile_emrate_2050 = 257
  
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

temp_em_df_sub <- e_emmissions_apportionment() %>% ungroup()


#inputs ------

#user inputs
project_df_input <- make_project_table_cumulative(rvs$Projects,
                                                  table_no = 14,
                                                  cols = c('area_type','road_class')) %>%
  mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                          year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                          year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) 

#hardcode inputs - I wonder if these should be part of the assumptions? or Capital Inputs?
car_gallons_hour_delay = 0.4 # this is a hardcoded unmutable (hu) input
truck_gallons_hour_delay = 1.7 # this is a hu input

existing_lanes_df <- data.frame(area_type = c("Rural","Rural","Urban","Urban"),
                                road_class = c("Principal Arterial","Freeway","Principal Arterial","Freeway"),
                                existing_lanes = c(4,6,4,6)) 
#fuel factors inputs
gasoline_CO2_kg_per_gallon = Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type =="Gasoline" & Fuel_Factors_Baselines$units == "fuel_carbon_content"] #7.94 #this is a hu input from Fuel Factors tab
diesel_CO2_kg_per_gallon = Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type =="Diesel" & Fuel_Factors_Baselines$units == "fuel_carbon_content"] #9.4 #this is a hu input from Fuel Factors tab

ff_weighted_temp <- Fuel_Factors_Weighted()
NOx_LDV <- ff_weighted_temp$NOx_g_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
PM25_LDV_exhaust <-ff_weighted_temp$PM25_exhaust_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
PM25_LDV_tirebrakes <-ff_weighted_temp$PM25_tires_brakes_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]

ptco2_temp <- pollutant_t_CO2ratio()
NOx_CO2_ratio <- ptco2_temp$NOx_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]
PM25_CO2_ratio <- ptco2_temp$PM25_CO2_ratio[ptco2_temp$veh_supertype == "Light-Duty Vehicles"]

#functions ----

#create dataframe
temp_output <- data.frame(year = c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
temp_output <- expand(temp_output, year, area_type = c("Urban", "Rural"), road_class = c("Principal Arterial", "Freeway"))

#add percent truck traffic
temp_output$percent_truck_traffic <- sapply(temp_output$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$road_class == x  & rvs$Assumptions$unit == "truck_traffic_pct"]) #slchanged

temp_output$light_duty_automobile_emrate <- sapply(temp_output$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Light-Duty Vehicles"])
temp_output$medium_heavy_duty_truck_emrate <- sapply(temp_output$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium-/Heavy-Duty Vehicles"])

temp_output$ldv_delay_emrate <- sapply(temp_output$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Light-Duty Vehicles"])*gasoline_CO2_kg_per_gallon*1000*car_gallons_hour_delay
temp_output$mhdv_delay_emrate <- sapply(temp_output$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium-/Heavy-Duty Vehicles"])*diesel_CO2_kg_per_gallon*1000*truck_gallons_hour_delay
temp_output<-temp_output %>%
  mutate(road_class_delay_emrate = ldv_delay_emrate*(1-percent_truck_traffic)+mhdv_delay_emrate*percent_truck_traffic)

temp_output$ldv_impf <- sapply(temp_output$year, function(x) temp_em_df$base_impf [temp_em_df$year == x & temp_em_df$veh_supertype == "Light-Duty Vehicles"])


temp_output$VMT_elasticity <- sapply(temp_output$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$road_class == x & rvs$Assumptions$unit == "VMT_elasticity_lane_mi"]) #slchanged
temp_output$traveltime_elasticity <- sapply(temp_output$area_type, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == x & rvs$Assumptions$unit == "VMT_elasticity_trav_time"]) #slchanged

#need to change user input to principal arterial
tspeed_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "travel_speed_mph"]} #slchanged
temp_output$tspeed = apply(temp_output, 1, tspeed_fun, c1 = "area_type", c2 = "road_class")
AADT_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "VMT_per_lane_mile"]} #slchanged
temp_output$VMTperLaneMile = apply(temp_output, 1, AADT_fun, c1 = "area_type", c2 = "road_class")

temp_output <- temp_output %>% left_join(existing_lanes_df) %>% 
  mutate(annual_VMTperLaneMile = VMTperLaneMile*300,
         minutes_delay_saved_perVMT = 0.2*(1-VMT_elasticity)/(1-0.67),
         minutes_per_mile_base = 60/tspeed,
         minutes_per_mile_new = minutes_per_mile_base - minutes_delay_saved_perVMT,
         new_speed = 1/(minutes_per_mile_new)*60, #in mph
         speed_change = minutes_per_mile_new - minutes_per_mile_base) %>%
  mutate(year = as.character(year)) %>%
  left_join(project_df_input %>% mutate(year = as.character(year))) %>% 
  
  left_join(temp_em_df_sub %>% mutate(year = as.character(year))) %>% 

  mutate(total_change_VMT = VMT_elasticity*annual_VMTperLaneMile*value,
         
         VMT_increase = light_duty_automobile_emrate*total_change_VMT/1000000,#I Have a Ben Q about this why is travel speed used? Itseems like a mistake
         delay_reduction = -((annual_VMTperLaneMile*existing_lanes*value/2)*(minutes_delay_saved_perVMT/60)*road_class_delay_emrate)/1000000,
         
         total_change_MTCO2 = VMT_increase + delay_reduction,
         total_change_electricity = total_change_MTCO2*electricity_per_em,
         total_change_direct = total_change_MTCO2-total_change_electricity) %>% 
  mutate(total_change_mtnox = total_change_VMT*NOx_LDV*ldv_impf/1000000 + delay_reduction*NOx_CO2_ratio,
         total_change_pm25 = (total_change_VMT*ldv_impf*PM25_LDV_exhaust+total_change_VMT*PM25_LDV_tirebrakes)/1000000 + delay_reduction*PM25_CO2_ratio) 

temp_output_fin<-temp_output%>%
  group_by(year) %>% 
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

return(temp_output_fin)
})

#Cost Output
cost_output_RoadwayExp <- reactive({
  
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
  
  temp_em_df <- CO2e_Category_Averages()
  
  #inputs ------
  
  #hardcode inputs - I wonder if these should be part of the assumptions? or Capital Inputs?
  car_gallons_hour_delay = 0.4 # this is a hardcoded unmutable (hu) input
  truck_gallons_hour_delay = 1.7 # this is a hu input
  
  existing_lanes_df <- data.frame(area_type = c("Rural","Rural","Urban","Urban"),
                                  road_class = c("Principal Arterial","Freeway","Principal Arterial","Freeway"),
                                  existing_lanes = c(4,6,4,6)) 
  #fuel factors inputs
  gasoline_CO2_kg_per_gallon = Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type =="Gasoline" & Fuel_Factors_Baselines$units == "fuel_carbon_content"] #7.94 #this is a hu input from Fuel Factors tab
  diesel_CO2_kg_per_gallon = Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type =="Diesel" & Fuel_Factors_Baselines$units == "fuel_carbon_content"] #9.4 #this is a hu input from Fuel Factors tab
  
  ff_weighted_temp <- Fuel_Factors_Weighted()
  
  NOx_LDV <- ff_weighted_temp$NOx_g_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_exhaust <-ff_weighted_temp$PM25_exhaust_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  PM25_LDV_tirebrakes <-ff_weighted_temp$PM25_tires_brakes_per_veh_mi[ff_weighted_temp$veh_type=="Light-Duty Vehicles"]
  
  #create dataframe
  temp_output <- data.frame(year = c(rvs$Baseline$horizon_year_1)) ### Pulls horizon years
  temp_output <- expand(temp_output, year, area_type = c("Urban", "Rural"), road_class = c("Principal Arterial", "Freeway"))
  
  #add percent truck traffic
  temp_output$percent_truck_traffic <- sapply(temp_output$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$road_class == x  & rvs$Assumptions$unit == "truck_traffic_pct"]) #slchanged
  
  temp_output$light_duty_automobile_emrate <- sapply(temp_output$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Light-Duty Vehicles"])
  #temp_output$medium_heavy_duty_truck_emrate <- sapply(temp_output$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])
  
  temp_output$ldv_delay_emrate <- sapply(temp_output$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Light-Duty Vehicles"])*gasoline_CO2_kg_per_gallon*1000*car_gallons_hour_delay
  temp_output$mhdv_delay_emrate <- sapply(temp_output$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium-/Heavy-Duty Vehicles"])*diesel_CO2_kg_per_gallon*1000*truck_gallons_hour_delay
  temp_output<-temp_output %>%
    mutate(road_class_delay_emrate = ldv_delay_emrate*(1-percent_truck_traffic)+mhdv_delay_emrate*percent_truck_traffic)

  temp_output$VMT_elasticity <- sapply(temp_output$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$road_class == x & rvs$Assumptions$unit == "VMT_elasticity_lane_mi"]) #slchanged

  AADT_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "VMT_per_lane_mile"]} #slchanged
  temp_output$VMTperLaneMile = apply(temp_output, 1, AADT_fun, c1 = "area_type", c2 = "road_class")
  
  temp_output <- temp_output %>% left_join(existing_lanes_df) %>% 
    mutate(minutes_delay_saved_perVMT = 0.2*(1-VMT_elasticity)/(1-0.67),
           total_change_VMT = VMTperLaneMile*300*VMT_elasticity,
           total_change_gGHG = (VMTperLaneMile*300*existing_lanes*.5)*(minutes_delay_saved_perVMT/60)*road_class_delay_emrate+VMTperLaneMile*300*VMT_elasticity*light_duty_automobile_emrate, #I think this eq is wrong in excel
           total_change_gnox = total_change_VMT*NOx_LDV,
           total_change_gpm25 = total_change_VMT*(PM25_LDV_exhaust+PM25_LDV_tirebrakes),
           total_change_newtrips = 0,
           unit = "new_lane") %>%
    select(year, area_type, road_class,total_change_gGHG, total_change_VMT, total_change_gnox, total_change_gpm25, total_change_newtrips)
  
  return(temp_output)
})

