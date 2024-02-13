

output_OPS <- reactive({
  
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

temp_em_df_sub <- e_emmissions_apportionment()


#inputs ------

#hardcode inputs - I wonder if these should be part of the assumptions? or Capital Inputs?
car_gallons_hour_delay = 0.4 # this is a hardcoded unmutable (hu) input
truck_gallons_hour_delay = 1.7 # this is a hu input
sample_corridor_length <- 1
signals_per_mile <- 2
corridor_travel_time_change <- -0.12
roundabout_effect <- -0.0665628668793441


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
temp_output_signal <- data.frame(year = c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
temp_output_signal <- expand(temp_output_signal, year, area_type = c("Urban", "Rural"), road_class = c("Principal Arterial"))

temp_output_roundabout <- data.frame(year = c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
temp_output_roundabout <- expand(temp_output_roundabout, year, area_type = c("Urban", "Rural"), road_class = c("Principal Arterial"))

#add percent truck traffic
temp_output_signal$percent_truck_traffic <- sapply(temp_output_signal$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x  & rvs$Assumptions$unit == "truck_traffic_pct"])

temp_output_signal$light_duty_automobile_emrate <- sapply(temp_output_signal$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])
temp_output_signal$medium_heavy_duty_truck_emrate <- sapply(temp_output_signal$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])

temp_output_signal$ldv_delay_emrate <- sapply(temp_output_signal$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])*gasoline_CO2_kg_per_gallon*1000*car_gallons_hour_delay
temp_output_signal$mhdv_delay_emrate <- sapply(temp_output_signal$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])*diesel_CO2_kg_per_gallon*1000*truck_gallons_hour_delay
temp_output_signal<-temp_output_signal %>%
  mutate(road_class_delay_emrate = ldv_delay_emrate*(1-percent_truck_traffic)+mhdv_delay_emrate*percent_truck_traffic)

temp_output_signal$ldv_impf <- sapply(temp_output_signal$year, function(x) temp_em_df$base_impf [temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])


temp_output_signal$VMT_elasticity <- sapply(temp_output_signal$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x & rvs$Assumptions$unit == "VMT_elasticity_lane_mi"])
temp_output_signal$traveltime_elasticity <- sapply(temp_output_signal$area_type, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x & rvs$Assumptions$unit == "VMT_elasticity_trav_time"])

#need to change user input to principal arterial
tspeed_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "travel_speed_mph"]}
temp_output_signal$tspeed = apply(temp_output_signal, 1, tspeed_fun, c1 = "area_type", c2 = "road_class")
temp_output_signal <- temp_output_signal %>% mutate(new_speed = tspeed/(1+corridor_travel_time_change))
temp_output_signal <- temp_output_signal %>% mutate(change_speed_per_veh = -(sample_corridor_length/tspeed)+(sample_corridor_length/new_speed))

AADT_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "avg_AADT"]}
temp_output_signal$AADT = apply(temp_output_signal, 1, AADT_fun, c1 = "area_type", c2 = "road_class")
temp_output_roundabout$AADT = apply(temp_output_roundabout, 1, AADT_fun, c1 = "area_type", c2 = "road_class")


temp_output_signal <- temp_output_signal %>% mutate(delay_reduction = AADT*change_speed_per_veh)
temp_output_signal <- temp_output_signal %>% mutate(new_AADT = AADT+AADT*corridor_travel_time_change*traveltime_elasticity)

project_signal <- make_project_table_cumulative(rvs$Projects[rvs$Projects$unit == "new_retimed_signal",],
                              table_no = 9, 
                              cols = c("area_type","road_class"), 
                              years_list = c(rvs$Baseline$horizon_year_1,
                                             rvs$Baseline$horizon_year_2,
                                             rvs$Baseline$horizon_year_3))

#project_signal$value <- c(10,20,30,10,20,30)

temp_output_signal <- temp_output_signal %>%
  left_join(project_signal) %>% 
  mutate(total_change_VMT = value*(new_AADT-AADT)*(sample_corridor_length/signals_per_mile)*365,
         CO2e_from_delay = delay_reduction*value*road_class_delay_emrate*365/1000000,
         CO2e_from_vmt = total_change_VMT*light_duty_automobile_emrate/1000000,
         total_change_MTCO2 = CO2e_from_delay+CO2e_from_vmt) %>%
  mutate(total_change_mtnox = total_change_VMT*NOx_LDV*ldv_impf/1000000 + CO2e_from_delay*NOx_CO2_ratio,
         total_change_pm25 = (total_change_VMT*ldv_impf*PM25_LDV_exhaust+total_change_VMT*PM25_LDV_tirebrakes)/1000000 + CO2e_from_delay*PM25_CO2_ratio) %>%
  select(year, area_type, road_class, total_change_VMT, total_change_MTCO2,total_change_mtnox,total_change_pm25)
  


project_roundabout <- make_project_table_cumulative(rvs$Projects[rvs$Projects$unit == "new_roundabouts",],
                                                table_no = 9, 
                                                cols = c("area_type","road_class"),  
                                                years_list = c(rvs$Baseline$horizon_year_1,
                                                               rvs$Baseline$horizon_year_2,
                                                               rvs$Baseline$horizon_year_3)) 

#project_roundabout$value <- c(10,20,30,10,20,30)

temp_output_roundabout <- temp_output_roundabout %>%
  left_join(project_roundabout) %>%
  mutate(total_change_MTCO2 = AADT*365*value*roundabout_effect/1000) %>%
  mutate(total_change_mtnox = total_change_MTCO2*NOx_CO2_ratio,
         total_change_pm25 =  total_change_MTCO2*PM25_CO2_ratio) %>%
  mutate(total_change_VMT = 0) %>%
  select(year, area_type, road_class, total_change_VMT, total_change_MTCO2,total_change_mtnox,total_change_pm25)

fin_output <- rbind(temp_output_roundabout, temp_output_signal)  %>%
  ungroup() %>%
  group_by(year) %>% 
  dplyr::summarise(across(c(total_change_VMT, total_change_MTCO2, total_change_mtnox, total_change_pm25), ~sum(.x)))

#View(fin_output)

return(fin_output)
})

#observeEvent(input$state_input, {browser()})

output_cost_OPS <- reactive({
  
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
  
  temp_em_df_sub <- e_emmissions_apportionment()
  
  
  #inputs ------

  
  #hardcode inputs - I wonder if these should be part of the assumptions? or Capital Inputs?
  car_gallons_hour_delay = 0.4 # this is a hardcoded unmutable (hu) input
  truck_gallons_hour_delay = 1.7 # this is a hu input
  sample_corridor_length <- 1
  signals_per_mile <- 2
  corridor_travel_time_change <- -0.12
  roundabout_effect <- -0.0665628668793441
  
  
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
  #temp_output_signal <- data.frame(year = c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
  temp_output_signal <- data.frame(year = c(rvs$Baseline$horizon_year_1,rvs$Baseline$horizon_year_1),area_type = c("Urban", "Rural"), road_class = c("Principal Arterial","Principal Arterial"))
  
  #temp_output_roundabout <- data.frame(year = c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
  temp_output_roundabout <- data.frame(year = c(rvs$Baseline$horizon_year_1,rvs$Baseline$horizon_year_1),area_type = c("Urban", "Rural"), road_class = c("Principal Arterial","Principal Arterial"))
  
  #add percent truck traffic
  temp_output_signal$percent_truck_traffic <- sapply(temp_output_signal$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x  & rvs$Assumptions$unit == "truck_traffic_pct"])
  
  temp_output_signal$light_duty_automobile_emrate <- sapply(temp_output_signal$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])
  temp_output_signal$medium_heavy_duty_truck_emrate <- sapply(temp_output_signal$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])
  
  temp_output_signal$ldv_delay_emrate <- sapply(temp_output_signal$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])*gasoline_CO2_kg_per_gallon*1000*car_gallons_hour_delay
  temp_output_signal$mhdv_delay_emrate <- sapply(temp_output_signal$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])*diesel_CO2_kg_per_gallon*1000*truck_gallons_hour_delay
  temp_output_signal<-temp_output_signal %>%
    mutate(road_class_delay_emrate = ldv_delay_emrate*(1-percent_truck_traffic)+mhdv_delay_emrate*percent_truck_traffic)
  
  temp_output_signal$ldv_impf <- sapply(temp_output_signal$year, function(x) temp_em_df$base_impf [temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])
  
  
  temp_output_signal$VMT_elasticity <- sapply(temp_output_signal$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x & rvs$Assumptions$unit == "VMT_elasticity_lane_mi"])
  temp_output_signal$traveltime_elasticity <- sapply(temp_output_signal$area_type, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x & rvs$Assumptions$unit == "VMT_elasticity_trav_time"])
  
  #need to change user input to principal arterial
  tspeed_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "travel_speed_mph"]}
  temp_output_signal$tspeed = apply(temp_output_signal, 1, tspeed_fun, c1 = "area_type", c2 = "road_class")
  temp_output_signal <- temp_output_signal %>% mutate(new_speed = tspeed/(1+corridor_travel_time_change))
  temp_output_signal <- temp_output_signal %>% mutate(change_speed_per_veh = -(sample_corridor_length/tspeed)+(sample_corridor_length/new_speed))
  
  AADT_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "avg_AADT"]}
  temp_output_signal$AADT = apply(temp_output_signal, 1, AADT_fun, c1 = "area_type", c2 = "road_class")
  temp_output_roundabout$AADT = apply(temp_output_roundabout, 1, AADT_fun, c1 = "area_type", c2 = "road_class")
  
  
  temp_output_signal <- temp_output_signal %>% mutate(delay_reduction = AADT*change_speed_per_veh)
  temp_output_signal <- temp_output_signal %>% mutate(new_AADT = AADT+AADT*corridor_travel_time_change*traveltime_elasticity)
  #note for BEN the cost output refers to the wrong Daily total delay reduction (hours)
  temp_output_signal <- temp_output_signal %>%
    mutate(total_change_VMT = (new_AADT-AADT)*(sample_corridor_length/signals_per_mile)*365,
           CO2e_from_delay = delay_reduction*road_class_delay_emrate*365,
           CO2e_from_vmt = total_change_VMT*light_duty_automobile_emrate,
           total_change_gGHG = CO2e_from_delay+CO2e_from_vmt) %>%
    #BEN: In the excel sheet there is a negative in front of the total_change_VMT? Also why isn't it time 365 when the delay emissions is in the g GHG calc
    #BEN: Why is the pm25 not applying to the ldv impf for both exaust and brake fuel factor in the excel 
    mutate(total_change_gnox = -1*total_change_VMT*NOx_LDV*ldv_impf + delay_reduction*road_class_delay_emrate*NOx_CO2_ratio,
           total_change_gpm25 = -1*(total_change_VMT*ldv_impf*PM25_LDV_exhaust+total_change_VMT*PM25_LDV_tirebrakes) + CO2e_from_delay*PM25_CO2_ratio) %>%
    
    mutate(total_daily_active = total_change_VMT/365) %>% 
    select(year, area_type, road_class, total_change_VMT, total_change_gGHG,total_change_gnox,total_change_gpm25) %>%
    mutate(cap_proj_type = "New or retimed signal")
  
  ghg_base <- temp_output_signal$total_change_gGHG[temp_output_signal$year == rvs$Baseline$horizon_year_1 & temp_output_signal$area_type == "Urban"] %>% as.numeric()
  vmt_base <- temp_output_signal$total_change_VMT[temp_output_signal$year == rvs$Baseline$horizon_year_1 & temp_output_signal$area_type == "Urban"] %>% as.numeric()
  nox_base <- temp_output_signal$total_change_gnox[temp_output_signal$year == rvs$Baseline$horizon_year_1 & temp_output_signal$area_type == "Urban"] %>% as.numeric()
  pmg25_base <- temp_output_signal$total_change_gpm25[temp_output_signal$year == rvs$Baseline$horizon_year_1 & temp_output_signal$area_type == "Urban"] %>% as.numeric()
  
  #Note for ben this is weird in the excel what's up
  temp_output_roundabout <- temp_output_roundabout %>%
    mutate(total_change_gGHG = AADT*365*roundabout_effect*1000) %>%
    mutate(total_change_gnox = total_change_gGHG*(nox_base/ghg_base),
           total_change_gpm25 =  total_change_gGHG*(pmg25_base/ghg_base)) %>%
    mutate(total_change_VMT = total_change_gGHG*(vmt_base/ghg_base)) %>%
    select(year, area_type, road_class, total_change_VMT, total_change_gGHG,total_change_gnox,total_change_gpm25) %>%
    mutate(cap_proj_type = "New roundabouts")
  
  fin_output <- rbind(temp_output_roundabout, temp_output_signal)  %>%
    ungroup() %>%
    group_by(year) %>% 
    mutate(total_change_newtrips = -1*total_change_VMT/365)
  
  return(fin_output)
})
