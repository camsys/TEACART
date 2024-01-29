
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
                                                  table_no = 9,
                                                  cols = c('area_type','road_class','unit'),
                                                  years_list = c(rvs$Baseline$horizon_year_1,
                                                                 rvs$Baseline$horizon_year_2,
                                                                 rvs$Baseline$horizon_year_3))
project_df_input$value <- 10

#hardcode inputs - I wonder if these should be part of the assumptions? or Capital Inputs?
car_gallons_hour_delay = 0.4 
truck_gallons_hour_delay = 1.7 

sample_corridor_length = 1 # this only applies to signals
signals_per_mile = 2 #this only applies to signals
corridor_travel_time_change = -.12 #this only applies to signals



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

#create dataframe for signal-----
temp_signal <- data.frame(year = c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
temp_signal <- expand(temp_signal, year, area_type = c("Urban", "Rural"), road_class = c("Principal Arterial"))

project_signal <- make_project_table_cumulative(projects_df = rvs$Projects[rvs$Projects$unit == "new_retimed_signal",],
                                                table_no = 9, 
                                                cols = c("area_type","road_class"), 
                                                years_list = c(rvs$Baseline$horizon_year_1,
                                                               rvs$Baseline$horizon_year_2,
                                                               rvs$Baseline$horizon_year_3))
project_signal$value <- c(10,20,30,10,20,30)
temp_signal <- temp_signal %>% left_join(project_signal)

#need to change user input to principal arterial
tspeed_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "travel_speed_mph"]}
temp_signal$tspeed_base = apply(temp_signal, 1, tspeed_fun, c1 = "area_type", c2 = "road_class")
temp_signal$tspeed_chng = temp_signal$tspeed_base/(1+corridor_travel_time_change)

AADT_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "avg_AADT"]}
temp_signal$avg_AADT = apply(temp_signal, 1, AADT_fun, c1 = "area_type", c2 = "road_class")

temp_signal$traveltime_elasticity <- sapply(temp_signal$area_type, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x & rvs$Assumptions$unit == "VMT_elasticity_trav_time"])


temp_signal$percent_truck_traffic <- sapply(temp_signal$road_class, function(x) rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$road_class == x  & rvs$Assumptions$unit == "truck_traffic_pct"])

temp_signal$light_duty_automobile_emrate <- sapply(temp_signal$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])
#temp_output$medium_heavy_duty_truck_emrate <- sapply(temp_output$year, function(x) temp_em_df$CO2e_millions[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])

temp_signal$ldv_delay_emrate <- sapply(temp_signal$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Light Duty Vehicles"])*gasoline_CO2_kg_per_gallon*1000*car_gallons_hour_delay
temp_signal$mhdv_delay_emrate <- sapply(temp_signal$year, function(x) temp_em_df$delay_impf[temp_em_df$year == x & temp_em_df$veh_supertype == "Medium/Heavy Duty Vehicles"])*diesel_CO2_kg_per_gallon*1000*truck_gallons_hour_delay
temp_signal<-temp_signal %>%
  mutate(road_class_delay_emrate = ldv_delay_emrate*(1-percent_truck_traffic)+mhdv_delay_emrate*percent_truck_traffic)


temp_signal<-temp_signal %>% 
  mutate(
  ttime_change = -(sample_corridor_length/tspeed_base)+(sample_corridor_length/tspeed_chng),
  delay_reduction = avg_AADT*ttime_change,
  new_VMT = avg_AADT+(avg_AADT*corridor_travel_time_change*traveltime_elasticity),
  VMT_change = value*(new_VMT-avg_AADT)*(sample_corridor_length/signals_per_mile)*365
  ) %>%
  mutate(
    co2_from_delay = value*delay_reduction*road_class_delay_emrate*365/1000000,
    total_VMT_increase = light_duty_automobile_emrate*VMT_change/1000000,
    total_CO2e_icnrease = co2_from_delay+total_VMT_increase
  )

#create dataframe for roundabout ----
temp_roundabout <- data.frame(year = c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ### Pulls horizon years
temp_roundabout <- expand(temp_roundabout, year, area_type = c("Urban", "Rural"), road_class = c("Principal Arterial"))


project_roundabout <- make_project_table_cumulative(rvs$Projects[rvs$Projects$unit == "new_roundabouts",],
                                                table_no = 9, 
                                                cols = c("area_type","road_class"),  
                                                years_list = c(rvs$Baseline$horizon_year_1,
                                                               rvs$Baseline$horizon_year_2,
                                                               rvs$Baseline$horizon_year_3))
project_roundabout$value <- c(10,20,30,10,20,30)

temp_roundabout <- temp_roundabout %>% left_join(project_roundabout)
temp_roundabout$roundabout_effect <- -0.0665628668793441
AADT_fun <- function(x, c1, c2){rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$area_type == x[c1] & rvs$Assumptions$road_class == x[c2] & rvs$Assumptions$unit == "avg_AADT"]}
temp_roundabout$avg_AADT = apply(temp_roundabout, 1, AADT_fun, c1 = "area_type", c2 = "road_class")
temp_roudabout <- temp_roundabout %>%
  mutate(total_change_CO2e = roundabout_effect*value*avg_AADT*365)

fin <- rbind(temp_signal %>% select(),
             temp_roundabout %>% select())

return(fin)
})

output_OPS_cost <- reactive({
  
  project_signal <- make_project_table_cumulative(projects_df = rvs$Projects[rvs$Projects$unit == "new_retimed_signal",],
                                                  table_no = 9, 
                                                  cols = c("area_type","road_class"), 
                                                  years_list = c(rvs$Baseline$horizon_year_1,
                                                                 rvs$Baseline$horizon_year_2,
                                                                 rvs$Baseline$horizon_year_3))
  project_signal$value <- c(10,20,30,10,20,30)
  
  project_roundabout <- make_project_table_cumulative(rvs$Projects[rvs$Projects$unit == "new_roundabouts",],
                                                      table_no = 9, 
                                                      cols = c("area_type","road_class"),  
                                                      years_list = c(rvs$Baseline$horizon_year_1,
                                                                     rvs$Baseline$horizon_year_2,
                                                                     rvs$Baseline$horizon_year_3))
  project_roundabout$value <- c(10,20,30,10,20,30)
  
})

