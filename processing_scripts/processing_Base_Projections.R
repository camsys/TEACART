
#intermediate values ----------------------------------------------------------
observeEvent(input$state_input, {
  #browser()
  req(input$state_input)
  #print("UPDATING: Intermediate Values")
  #Fuel Costs
  #print("fuel costs start")
  rvs$Costs$value[rvs$Costs$table_no_ui==18 & #slchanged
                    rvs$Costs$fuel_type == "Gasoline" &
                    rvs$Costs$unit == "dollars_per_gallon"] <- State_Prices$gas_regular[State_Prices$state == input$state_input]
  rvs$Costs$value[rvs$Costs$table_no_ui==18 &#slchanged
                    rvs$Costs$fuel_type == "Diesel" &
                    rvs$Costs$unit == "dollars_per_gallon"] <- State_Prices$gas_diesel[State_Prices$state == input$state_input]
  rvs$Costs$value[rvs$Costs$table_no_ui==18 &#slchanged
                    rvs$Costs$fuel_type == "Electricity" &
                    rvs$Costs$unit == "dollars_per_kwh"] <- State_Prices$electricity_retail_avg[State_Prices$state == input$state_input]/100
  #Assumptions
  #print("average aadt start")
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == "Urban" & #slchanged
                          rvs$Assumptions$road_class == "Principal Arterial" &
                          rvs$Assumptions$unit == "avg_AADT"] <-
    HPMS$AADT[HPMS$state == input$state_input & HPMS$road_class == "Other Principal Arterial" & HPMS$area_type == "Urban"]

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == "Rural" & #slchanged
                          rvs$Assumptions$road_class == "Principal Arterial" &
                          rvs$Assumptions$unit == "avg_AADT"] <-
    HPMS$AADT[HPMS$state == input$state_input & HPMS$road_class == "Other Principal Arterial" & HPMS$area_type == "Rural"]
  #print("vmt per lane mile")
  
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == "Rural" & #slchanged
                          rvs$Assumptions$road_class == "Principal Arterial" &
                          rvs$Assumptions$unit == "VMT_per_lane_mile"] <-
    HPMS$VMT_perln[HPMS$state == input$state_input & HPMS$road_class == "Other Principal Arterial" & HPMS$area_type == "Rural"]

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == "Rural" & #slchanged
                          rvs$Assumptions$road_class == "Freeway" &
                          rvs$Assumptions$unit == "VMT_per_lane_mile"] <-
    HPMS$VMT_perln[HPMS$state == input$state_input & HPMS$road_class == "Interstate" & HPMS$area_type == "Rural"]

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == "Urban" & #slchanged
                          rvs$Assumptions$road_class == "Principal Arterial" &
                          rvs$Assumptions$unit == "VMT_per_lane_mile"] <-
    HPMS$VMT_perln[HPMS$state == input$state_input & HPMS$road_class == "Other Principal Arterial" & HPMS$area_type == "Urban"]

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 9 & rvs$Assumptions$area_type == "Urban" & #slchanged
                          rvs$Assumptions$road_class == "Freeway" &
                          rvs$Assumptions$unit == "VMT_per_lane_mile"] <-
    HPMS$VMT_perln[HPMS$state == input$state_input & HPMS$road_class == "Interstate" & HPMS$area_type == "Urban"]

  # Passenger Miles -------
  #browser()
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Urban" &
                          rvs$Assumptions$transit_mode == "Bus" &
                          rvs$Assumptions$unit == "avg_pax_mi_per_veh_mi"] <-
    ifelse(!isTruthy(NTD_Service$load_factor[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"]),
           NTD_Service$load_factor[NTD_Service$state == "United States" &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"],
           NTD_Service$load_factor[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"])
  #print(1)
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Rural" &
                          rvs$Assumptions$transit_mode == "Bus" &
                          rvs$Assumptions$unit == "avg_pax_mi_per_veh_mi"] <-
    ifelse(!isTruthy(NTD_Service$load_factor[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"]),
           NTD_Service$load_factor[NTD_Service$state == "United States" &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"],
           NTD_Service$load_factor[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"])
  #print(2)
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Urban" &
                          rvs$Assumptions$transit_mode == "Demand Response" &
                          rvs$Assumptions$unit == "avg_pax_mi_per_veh_mi"] <-
    ifelse(!isTruthy(NTD_Service$load_factor[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"]),
           NTD_Service$load_factor[NTD_Service$state == "United States" &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"],
           NTD_Service$load_factor[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"])
  #print(3)
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Rural" &
                          rvs$Assumptions$transit_mode == "Demand Response" &
                          rvs$Assumptions$unit == "avg_pax_mi_per_veh_mi"] <-
    ifelse(!isTruthy(NTD_Service$load_factor[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"]),
           NTD_Service$load_factor[NTD_Service$state == "United States" &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"],
           NTD_Service$load_factor[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"])
  #print(4)
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Commuter Bus" &
                          rvs$Assumptions$unit == "avg_pax_mi_per_veh_mi"] <-
    ifelse(!isTruthy(NTD_Service$load_factor[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Bus"]),
           NTD_Service$load_factor[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Commuter Bus"],
           NTD_Service$load_factor[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Bus"])
  #print(5)
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Light Rail / Streetcar" &
                          rvs$Assumptions$unit == "avg_pax_mi_per_veh_mi"] <-
    ifelse(!isTruthy(NTD_Service$load_factor[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Light Rail"]),
           NTD_Service$load_factor[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Light Rail"],
           NTD_Service$load_factor[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Light Rail"])
  #print(6)
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Heavy Rail" &
                          rvs$Assumptions$unit == "avg_pax_mi_per_veh_mi"] <-
    ifelse(!isTruthy(NTD_Service$load_factor[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Heavy Rail"]),
           NTD_Service$load_factor[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Heavy Rail"],
           NTD_Service$load_factor[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Heavy Rail"])
  #print(7)
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Commuter Rail" &
                          rvs$Assumptions$unit == "avg_pax_mi_per_veh_mi"] <-
    ifelse(!isTruthy(NTD_Service$load_factor[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Rail"]),
           NTD_Service$load_factor[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Commuter Rail"],
           NTD_Service$load_factor[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Rail"])

  # Avg. Trip Length (mi) ----------
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Urban" &
                          rvs$Assumptions$transit_mode == "Bus" &
                          rvs$Assumptions$unit == "avg_trip_miles"] <-
    ifelse(!isTruthy(NTD_Service$avg_trip_length[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"]),
           NTD_Service$avg_trip_length[NTD_Service$state == "United States" &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"],
           NTD_Service$avg_trip_length[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Rural" &
                          rvs$Assumptions$transit_mode == "Bus" &
                          rvs$Assumptions$unit == "avg_trip_miles"] <-
    ifelse(!isTruthy(NTD_Service$avg_trip_length[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"]),
           NTD_Service$avg_trip_length[NTD_Service$state == "United States" &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"],
           NTD_Service$avg_trip_length[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Urban" &
                          rvs$Assumptions$transit_mode == "Demand Response" &
                          rvs$Assumptions$unit == "avg_trip_miles"] <-
    ifelse(!isTruthy(NTD_Service$avg_trip_length[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"]),
           NTD_Service$avg_trip_length[NTD_Service$state == "United States" &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"],
           NTD_Service$avg_trip_length[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"])
  
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Rural" &
                          rvs$Assumptions$transit_mode == "Demand Response" &
                          rvs$Assumptions$unit == "avg_trip_miles"] <-
    ifelse(!isTruthy(NTD_Service$avg_trip_length[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"]),
           NTD_Service$avg_trip_length[NTD_Service$state == "United States" &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"],
           NTD_Service$avg_trip_length[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Commuter Bus" &
                          rvs$Assumptions$unit == "avg_trip_miles"] <-
    ifelse(!isTruthy(NTD_Service$avg_trip_length[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Bus"]),
           NTD_Service$avg_trip_length[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Commuter Bus"],
           NTD_Service$avg_trip_length[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Bus"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Light Rail / Streetcar" &
                          rvs$Assumptions$unit == "avg_trip_miles"] <-
    ifelse(!isTruthy(NTD_Service$avg_trip_length[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Light Rail"]),
           NTD_Service$avg_trip_length[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Light Rail"],
           NTD_Service$avg_trip_length[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Light Rail"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Heavy Rail" &
                          rvs$Assumptions$unit == "avg_trip_miles"] <-
    ifelse(!isTruthy(NTD_Service$avg_trip_length[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Heavy Rail"]),
           NTD_Service$avg_trip_length[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Heavy Rail"],
           NTD_Service$avg_trip_length[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Heavy Rail"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Commuter Rail" &
                          rvs$Assumptions$unit == "avg_trip_miles"] <-
    ifelse(!isTruthy(NTD_Service$avg_trip_length[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Rail"]),
           NTD_Service$avg_trip_length[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Commuter Rail"],
           NTD_Service$avg_trip_length[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Rail"])
  
  # Average Revenue Miles -------
  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Urban" &
                          rvs$Assumptions$transit_mode == "Bus" &
                          rvs$Assumptions$unit == "rev_mi_per_veh"] <-
    ifelse(!isTruthy(NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"]),
           NTD_Service$total_miles_per_veh[NTD_Service$state == "United States" &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"],
           NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Bus"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Rural" &
                          rvs$Assumptions$transit_mode == "Bus" &
                          rvs$Assumptions$unit == "rev_mi_per_veh"] <-
    ifelse(!isTruthy(NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"]),
           NTD_Service$total_miles_per_veh[NTD_Service$state == "United States" &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"],
           NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Bus"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Urban" &
                          rvs$Assumptions$transit_mode == "Demand Response" &
                          rvs$Assumptions$unit == "rev_mi_per_veh"] <-
    ifelse(!isTruthy(NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"]),
           NTD_Service$total_miles_per_veh[NTD_Service$state == "United States" &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"],
           NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Urban" & NTD_Service$transit_mode == "Demand Response"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "Rural" &
                          rvs$Assumptions$transit_mode == "Demand Response" &
                          rvs$Assumptions$unit == "rev_mi_per_veh"] <-
    ifelse(!isTruthy(NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"]),
           NTD_Service$total_miles_per_veh[NTD_Service$state == "United States" &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"],
           NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input &  NTD_Service$area_type == "Rural" & NTD_Service$transit_mode == "Demand Response"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Commuter Bus" &
                          rvs$Assumptions$unit == "rev_mi_per_veh"] <-
    ifelse(!isTruthy(NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Bus"]),
           NTD_Service$total_miles_per_veh[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Commuter Bus"],
           NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Bus"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Light Rail / Streetcar" &
                          rvs$Assumptions$unit == "rev_mi_per_veh"] <-
    ifelse(!isTruthy(NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Light Rail"]),
           NTD_Service$total_miles_per_veh[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Light Rail"],
           NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Light Rail"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Heavy Rail" &
                          rvs$Assumptions$unit == "rev_mi_per_veh"] <-
    ifelse(!isTruthy(NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Heavy Rail"]),
           NTD_Service$total_miles_per_veh[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Heavy Rail"],
           NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Heavy Rail"])

  rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$area_type == "All" &
                          rvs$Assumptions$transit_mode == "Commuter Rail" &
                          rvs$Assumptions$unit == "rev_mi_per_veh"] <-
    ifelse(!isTruthy(NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Rail"]),
           NTD_Service$total_miles_per_veh[NTD_Service$state == "United States" & NTD_Service$transit_mode == "Commuter Rail"],
           NTD_Service$total_miles_per_veh[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Rail"])


  #Costs
  print("per VRM fuel costs start")
  #browser()
  
  rvs$Costs$value[rvs$Costs$table_no_ui == 2 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type == "Diesel" &
                    rvs$Costs$transit_mode == "Bus" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    (1/rvs$Assumptions$value[rvs$Assumptions$unit == "veh_fuel_economy" &
                               rvs$Assumptions$fuel_type == "Diesel" &
                               rvs$Assumptions$transit_mode == "Bus" &
                               rvs$Assumptions$table_no_ui == 2]) *
    (Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" & Fuel_Factors_Baselines$units == "fuel_conversion_gasoline_equivalent"])*
    (rvs$Costs$value[rvs$Costs$table_no_ui == 18 & rvs$Costs$fuel_type == "Diesel" & rvs$Costs$unit == "dollars_per_gallon"])*inflate_values(2020) #slchanged
#print("VRM 1")
  rvs$Costs$value[rvs$Costs$table_no_ui == 0 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type == "Diesel" &
                    rvs$Costs$transit_mode == "Commuter Bus" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    (1/rvs$Assumptions$value[rvs$Assumptions$unit == "veh_fuel_economy" &
                               rvs$Assumptions$fuel_type == "Diesel" &
                               rvs$Assumptions$transit_mode == "Bus" &
                               rvs$Assumptions$table_no_ui == 2]) *
    (Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" & Fuel_Factors_Baselines$units == "fuel_conversion_gasoline_equivalent"])*
    (rvs$Costs$value[rvs$Costs$table_no_ui == 18 & rvs$Costs$fuel_type == "Diesel" & rvs$Costs$unit == "dollars_per_gallon"])*inflate_values(2020) #slchanged
  #print("VRM 2")
  rvs$Costs$value[rvs$Costs$table_no_ui == 2 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type == "CNG" &
                    rvs$Costs$transit_mode == "Bus" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    (1/rvs$Assumptions$value[rvs$Assumptions$unit == "veh_fuel_economy" &
                               rvs$Assumptions$fuel_type == "CNG" &
                               rvs$Assumptions$transit_mode == "Bus" &
                               rvs$Assumptions$table_no_ui == 2]) *
    (rvs$Costs$value[rvs$Costs$table_no_ui == 18 & rvs$Costs$fuel_type == "CNG" & rvs$Costs$unit == "dollars_per_gge"])*inflate_values(2020)#slchanged
  #print("VRM 3")
  rvs$Costs$value[rvs$Costs$table_no_ui == 2 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type == "Electric" &
                    rvs$Costs$transit_mode == "Bus" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    (1/rvs$Assumptions$value[rvs$Assumptions$unit == "veh_fuel_economy" &
                               rvs$Assumptions$fuel_type == "Electric" &
                               rvs$Assumptions$transit_mode == "Bus" &
                               rvs$Assumptions$table_no_ui == 2]) *
    (Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "electricity" & Fuel_Factors_Baselines$units == "fuel_conversion_gasoline_equivalent"])*
    (rvs$Costs$value[rvs$Costs$table_no_ui == 18 & rvs$Costs$fuel_type == "Electricity" & rvs$Costs$unit == "dollars_per_kwh"])*inflate_values(2020)#slchanged
  #print("VRM 4")
  rvs$Costs$value[rvs$Costs$table_no_ui == 3 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type == "Gasoline" &
                    rvs$Costs$transit_mode == "Demand Response" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    (1/rvs$Assumptions$value[rvs$Assumptions$unit == "veh_fuel_economy" &
                               rvs$Assumptions$fuel_type == "Gasoline" &
                               rvs$Assumptions$transit_mode == "Demand Response" &
                               rvs$Assumptions$table_no_ui == 2]) *
    (rvs$Costs$value[rvs$Costs$table_no_ui == 18 & rvs$Costs$fuel_type == "Gasoline" & rvs$Costs$unit == "dollars_per_gallon"])*inflate_values(2020)#slchanged
  #print("VRM 5")
  rvs$Costs$value[rvs$Costs$table_no_ui == 3 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type == "CNG" &
                    rvs$Costs$transit_mode == "Demand Response" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    (1/rvs$Assumptions$value[rvs$Assumptions$unit == "veh_fuel_economy" &
                               rvs$Assumptions$fuel_type == "CNG" &
                               rvs$Assumptions$transit_mode == "Demand Response" &
                               rvs$Assumptions$table_no_ui == 2]) *
    (rvs$Costs$value[rvs$Costs$table_no_ui == 18 & rvs$Costs$fuel_type == "CNG" & rvs$Costs$unit == "dollars_per_gge"])*inflate_values(2020)#slchanged
  #print("VRM 6")
  rvs$Costs$value[rvs$Costs$table_no_ui == 3 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type == "Electric" &
                    rvs$Costs$transit_mode == "Demand Response" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    (1/rvs$Assumptions$value[rvs$Assumptions$unit == "veh_fuel_economy" &
                               rvs$Assumptions$fuel_type == "Electric" &
                               rvs$Assumptions$transit_mode == "Demand Response" &
                               rvs$Assumptions$table_no_ui == 2]) *
    (Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "electricity" & Fuel_Factors_Baselines$units == "fuel_conversion_gasoline_equivalent"])*
    (rvs$Costs$value[rvs$Costs$table_no_ui == 18 & rvs$Costs$fuel_type == "Electricity" & rvs$Costs$unit == "dollars_per_kwh"])*inflate_values(2020)#slchanged
  #print("VRM 7")
  per_VRM_fuel_cost_electric_lr <- Transit_Costs$total_cost_fuel_lube[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Light Rail"]/
    NTD_Service$total_vehicle_rev_miles[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Light Rail"]
  rvs$Costs$value[rvs$Costs$table_no_ui == 6 & #slchanged
                    rvs$Costs$fuel_type == "Electric" &
                    rvs$Costs$transit_mode == "Light Rail / Streetcar" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    ifelse(!isTruthy(per_VRM_fuel_cost_electric_lr),0.030362,per_VRM_fuel_cost_electric_lr)*inflate_values(2019)
  #print("VRM 8")
  per_VRM_fuel_cost_electric_hr <- Transit_Costs$total_cost_fuel_lube[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Heavy Rail"]/
    NTD_Service$total_vehicle_rev_miles[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Heavy Rail"]
  rvs$Costs$value[rvs$Costs$table_no_ui == 6 & #slchanged
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type == "Electric" &
                    rvs$Costs$transit_mode == "Heavy Rail" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    ifelse(!isTruthy(per_VRM_fuel_cost_electric_hr),0.02043808029,per_VRM_fuel_cost_electric_hr)*inflate_values(2019)
  #print("VRM 9")
  per_VRM_fuel_cost_electric_cr <- Transit_Costs$total_cost_fuel_lube[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Commuter Rail"]/
    NTD_Service$total_vehicle_rev_miles[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Rail"]
  rvs$Costs$value[rvs$Costs$table_no_ui == 6 & #slchanged
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type %in% c("Electric","Diesel") &
                    rvs$Costs$transit_mode == "Commuter Rail" &
                    rvs$Costs$unit == "per_VRM_fuel_cost"] <-
    ifelse(!isTruthy(per_VRM_fuel_cost_electric_cr),0.5519780,per_VRM_fuel_cost_electric_cr)*inflate_values(2019)

  #ONM Costs
  #print("average onm start")
  #browser()4.22929370727435
  per_VRM_onm_cost_bus_Diesel <- Transit_Costs$total_cost_om[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Bus"]/Transit_Costs$veh_rev_miles[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Bus"]
  rvs$Costs$value[rvs$Costs$table_no_ui == 2 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type %in% c("Diesel","CNG") &
                    rvs$Costs$transit_mode == "Bus" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
    ifelse(!isTruthy(per_VRM_onm_cost_bus_Diesel),4.22929370727435,4.22929370727435)*inflate_values(2020)
  
  rvs$Costs$value[rvs$Costs$table_no_ui == 2 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type %in% c("Diesel","CNG") &
                    rvs$Costs$transit_mode == "Bus" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
      ifelse(!isTruthy(per_VRM_onm_cost_bus_Diesel),7.6801949,per_VRM_onm_cost_bus_Diesel)*inflate_values(2020)


  rvs$Costs$value[rvs$Costs$table_no_ui == 2 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type %in% c("Electric") &
                    rvs$Costs$transit_mode == "Bus" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
    ifelse(!isTruthy(per_VRM_onm_cost_bus_Diesel),7.6801949*.76,per_VRM_onm_cost_bus_Diesel*.76)*inflate_values(2020)

  rvs$Costs$value[rvs$Costs$table_no_ui == 0 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type %in% c("Diesel") &
                    rvs$Costs$transit_mode == "Commuter Bus" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
    ifelse(!isTruthy(per_VRM_onm_cost_bus_Diesel),7.6801949,per_VRM_onm_cost_bus_Diesel)*inflate_values(2020)

  per_VRM_onm_cost_dr <- Transit_Costs$total_cost_om[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Demand Response"]/Transit_Costs$veh_rev_miles[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Demand Response"]
  rvs$Costs$value[rvs$Costs$table_no_ui == 3 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type %in% c("Gasoline","CNG") &
                    rvs$Costs$transit_mode == "Demand Response" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
    ifelse(!isTruthy(per_VRM_onm_cost_dr),2.3827171,per_VRM_onm_cost_dr)*inflate_values(2020)


  rvs$Costs$value[rvs$Costs$table_no_ui == 2 &
                    #rvs$Costs$area_type == "Urban" &
                    rvs$Costs$fuel_type %in% c("Electric") &
                    rvs$Costs$transit_mode == "Demand Response" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
    ifelse(!isTruthy(per_VRM_onm_cost_dr),2.3827171*.76,per_VRM_onm_cost_dr*.76)*inflate_values(2020)

  per_VRM_onm_cost_electric_lr <- Transit_Costs$total_cost_om[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Light Rail"]/
    NTD_Service$total_vehicle_rev_miles[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Light Rail"]
  rvs$Costs$value[rvs$Costs$table_no_ui == 6 &#slchanged
                    rvs$Costs$fuel_type == "Electric" &
                    rvs$Costs$transit_mode == "Light Rail / Streetcar" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
    ifelse(!isTruthy(per_VRM_onm_cost_electric_lr),12.458012648,per_VRM_onm_cost_electric_lr)*inflate_values(2019)

  per_VRM_onm_cost_electric_hr <- Transit_Costs$total_cost_om[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Heavy Rail"]/
    NTD_Service$total_vehicle_rev_miles[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Heavy Rail"]
  rvs$Costs$value[rvs$Costs$table_no_ui == 6 & #slchanged
                    rvs$Costs$fuel_type == "Electric" &
                    rvs$Costs$transit_mode == "Heavy Rail" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
    ifelse(!isTruthy(per_VRM_onm_cost_electric_hr), 6.720342417,per_VRM_onm_cost_electric_hr)*inflate_values(2019)

  per_VRM_onm_cost_electric_cr <- Transit_Costs$total_cost_om[Transit_Costs$state == input$state_input & Transit_Costs$transit_mode == "Commuter Rail"]/
    NTD_Service$total_vehicle_rev_miles[NTD_Service$state == input$state_input & NTD_Service$transit_mode == "Commuter Rail"]
  rvs$Costs$value[rvs$Costs$table_no_ui == 6 & #slchanged
                    rvs$Costs$fuel_type %in% c("Electric","Diesel") &
                    rvs$Costs$transit_mode == "Commuter Rail" &
                    rvs$Costs$unit == "per_VRM_onm_cost"] <-
    ifelse(!isTruthy(per_VRM_onm_cost_electric_cr),11.008286292,per_VRM_onm_cost_electric_cr)*inflate_values(2019)
  #='Strategy Parameters'!G80*'Strategy Parameters'!G81
  rvs$Costs$value[rvs$Costs$table_no_ui == 8 & rvs$Costs$unit == "e_bike_subsidies"] <- #slchanged
    rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 8 & rvs$Assumptions$element == "E-bike subsidies" & rvs$Assumptions$unit == "e_bike_cost"]*rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 8 & rvs$Assumptions$element == "E-bike subsidies" & rvs$Assumptions$unit == "subsidy_coverage_pct"] #slchanged

  #add it here
  #freight rail growth rate
  #Baseline Paramters - C39
  
  #EV Time Series as well
  
  })

inflate_values <- function(yr){
  df = data.frame(year = c(2012:2023),
  adjustment = c(1.3607,1.3393,1.3185,1.3197,1.3018,1.27,1.2443,1.2253,1.1955,1.179,1.097,1.0309))
  value = df$adjustment[df$year == yr][[1]]
  return(value)
}

#EmRate_By_Tech ---------------------------------------------------------------

#This function calcualte the emrate for electricity based on a linear interpolation
#between base year and the user input net_zero_year
electricity_emrate_projecter <- function(eemrate_df, net_zero_year = 2100){

  eemrate_return <-eemrate_temp<- eemrate_df %>% select(-CO2eq_lbs_MWh) %>%
    mutate(linear_eq = (-1*CO2eq_g_kWh)/(net_zero_year-2021),
           intercept = -1*linear_eq*2021+CO2eq_g_kWh)
  
  for(n in 2022:2100){
    eemrate_return <- rbind(eemrate_return, mutate(eemrate_temp, year = n))
  }
  
  eemrate_return<-eemrate_return %>%
    mutate(electricity_carbon_content = linear_eq*year+intercept)
  
  eemrate_return$electricity_carbon_content[eemrate_return$year >= net_zero_year] <- 0
  eemrate_return_fin<-eemrate_return %>% filter(year <= 2050) %>%
    select(year, electricity_carbon_content) %>%
    expand(nesting(year, electricity_carbon_content), veh_subtype = c("EV100","EV200","EV300","SI PHEV 10","SI PHEV 40", "FCV", "EV", "Gasoline PHEV", "Diesel PHEV"))

  
  return(eemrate_return_fin)
}

#Electricity Emmission Rates for the state
electricity_emrate <- reactive({ #this is electricity emission rate
  #browser()
  req(rvs$Baseline$elec_grid_emissions_net_zero)
  req(rvs$Baseline$state)
  zero_em <- rvs$Baseline$elec_grid_emissions_net_zero
  state_ch <- rvs$Baseline$state
  
  eemrate<-electricity_emrate_projecter(filter(Electricity_EmRate,state == state_ch), net_zero_year = zero_em)
  
  return(eemrate)
})

#EmRate_by_Tech ----
EmRate_by_Tech <- reactive({ #this is emission rate for all vehicles types
 
  #browser()
  input_ff_factors <- rvs$Advanced[rvs$Advanced$table_no_ui == 7,] %>% 
    select(veh_type, value) %>%
    rename(apportionment = value) %>%
    mutate(apportionment = as.numeric(apportionment)) %>% left_join(
    rbind(expand(data.frame(),veh_type = c("Passenger Cars", "Light-Duty Trucks"), veh_subtype = c("SI PHEV 10","SI PHEV 40")),
          expand(data.frame(),veh_type = c("Medium-Duty Trucks", "Heavy-Duty Trucks"), veh_subtype = c("Gasoline PHEV", "Diesel PHEV")))) %>%
    rbind(
      rbind(
        expand(data.frame(),veh_type = c("Passenger Cars", "Light-Duty Trucks"), veh_subtype = c("EV100","EV200","EV300"), apportionment = 1),
        expand(data.frame(),veh_type = c("Medium-Duty Trucks", "Heavy-Duty Trucks"), veh_subtype = c("EV"), apportionment = 1)))
  
  eemrate <- electricity_emrate()
  
  EmRate_by_Tech <- left_join(Fuel_Econs, eemrate) 
  EmRate_by_Tech <- left_join(EmRate_by_Tech, left_join(Fuel_Factors_Revision,input_ff_factors)) %>% 
    select(-c(fuel_conversion_unit, electricity_conversion_unit,fuel_carbon_content_unit))
  
  #fill NAs with zero. Caused by missing factors for certain vehicles. This could 
  #be skipped by using a better NA coding in the csv I think?
  EmRate_by_Tech[is.na(EmRate_by_Tech)]<-0
  
  #Calculate fuel emission rate, electircity emission rate, and combine based
  #on the apportionment for each vehicle type
  #most of these factors are in the fuel factors sheet
  ##SL NOTE:: Something is slightly off wit heavy trucks maybe medium as well less than a .1% difference 
  EmRate_by_Tech <- EmRate_by_Tech %>%
    #use gasoline factor for light-duty EtOH
    mutate(fuel_N20_CO2eq_per_mile = ifelse(veh_type == 'Light-Duty Trucks' & veh_subtype == 'EtOH',
                                            unique(EmRate_by_Tech$fuel_N20_CO2eq_per_mile[EmRate_by_Tech$veh_type == 'Light-Duty Trucks' &
                                                                                            EmRate_by_Tech$veh_subtype == 'Gasoline ICE']),
                                            fuel_N20_CO2eq_per_mile)) %>%
    mutate(fuel_emission_rate = (1/mpg_gasoline_eq) * (1000*fuel_carbon_content * fuel_conversion) + fuel_CH4_CO2e_per_mile+fuel_N20_CO2eq_per_mile) %>%
    mutate(electricity_emission_rate = (1/mpg_gasoline_eq) * electricity_carbon_content * electricity_conversion) %>%
    mutate(emission_rate = (1-apportionment*uses_electiricity)*fuel_emission_rate+apportionment*uses_electiricity*electricity_emission_rate)
  
  return(EmRate_by_Tech)
  
})

#Other Em_Rate tables that are useful
#This is the Category Averages excel rows 49 through 57 for the most part
CO2e_Category_Averages <- reactive({
  
  temp_all<-VMT_Type_Tech_Base() %>%
    group_by(veh_supertype, year) %>%
    mutate(pct_supertype = mmt_by_subtype/sum(mmt_by_subtype)) %>%
    select(veh_supertype, year, veh_type, veh_subtype, pct_supertype)
  
  temp_Conventional_LDV<-VMT_Type_Tech_Conventional_LDV() 
  
  temp_Conventional_MDHD <- VMT_Type_Tech_Conventional_MDHD() 
  
  
  all_cats_temp<-temp_all %>% left_join(EmRate_by_Tech()) %>% 
    select(emission_rate, veh_supertype, year, veh_type, veh_subtype, pct_supertype) %>%
    mutate(CO2e_millions = pct_supertype*emission_rate) %>% 
    group_by(veh_supertype, year) %>% 
    summarise(CO2e_millions = sum(CO2e_millions, na.rm = T)) %>%
    ungroup()
  
  ldv_gas_impf <- temp_Conventional_LDV %>% left_join(EmRate_by_Tech()) %>%
    select(emission_rate, year, veh_type, veh_subtype, state_pct_of_category) %>%
    mutate(CO2e_millions = state_pct_of_category *emission_rate) %>%
    group_by(year) %>% 
    summarise(CO2e_millions = sum(CO2e_millions, na.rm = T))
  ldv_gas_impf <- ldv_gas_impf$CO2e_millions[ldv_gas_impf$year == 2022]
  
  mhdv_conventional_impf <- temp_Conventional_MDHD %>% left_join(EmRate_by_Tech()) %>%
    select(emission_rate, year, veh_type, veh_subtype, state_pct_of_category) %>%
    mutate(CO2e_millions = state_pct_of_category *emission_rate) %>%
    group_by(year) %>%
    summarise(CO2e_millions = sum(CO2e_millions, na.rm = T))
  mhdv_conventional_impf<-mhdv_conventional_impf$CO2e_millions[mhdv_conventional_impf$year == 2021]
  
  ldv_2022 <- all_cats_temp$CO2e_millions[all_cats_temp$year == 2022 & all_cats_temp$veh_supertype == "Light-Duty Vehicles"]
  mhdv_2022 <- all_cats_temp$CO2e_millions[all_cats_temp$year == 2022 & all_cats_temp$veh_supertype == "Medium-/Heavy-Duty Vehicles"]
  
  
  
  all_cats_temp <- all_cats_temp %>% 
    mutate(base_impf = ifelse(veh_supertype == "Light-Duty Vehicles", CO2e_millions/ldv_2022,
                              CO2e_millions/mhdv_2022)) %>%
    mutate(delay_impf = ifelse(veh_supertype == "Light-Duty Vehicles", CO2e_millions/ldv_gas_impf,
                               CO2e_millions/mhdv_conventional_impf))
  
  return(all_cats_temp)
  
})

#This is PHEV Emission Apportionment ex-rows 59 - 86
PHEV_Em_Apportionment <- reactive({
  
  temp_em_rate <- EmRate_by_Tech()
  temp_em_rate <- temp_em_rate %>%
    filter(paste0(veh_type, "-", veh_subtype) %in% c("Passenger Cars-SI PHEV 10",
                                                     "Light-Duty Trucks-SI PHEV 10",
                                                     "Medium-Duty Trucks-Gasoline PHEV",
                                                     "Heavy-Duty Trucks-Gasoline PHEV")) %>%
    select(year, veh_type, apportionment, fuel_emission_rate, electricity_emission_rate) %>%
    mutate(fuel_em_apportioned = fuel_emission_rate*(1-apportionment),
           e_em_apportioned = electricity_emission_rate*apportionment,
           PHEV_elc_per_em = e_em_apportioned/(fuel_em_apportioned+e_em_apportioned))
  
  return(temp_em_rate)
})

#This is the Local Pollutant to CO2 Ratios ex-rows 109 - 115
pollutant_t_CO2ratio <- reactive({
  temp_cat_avg <- CO2e_Category_Averages()
  ldv_2021_co2e <- temp_cat_avg$CO2e_millions[temp_cat_avg$veh_supertype == "Light-Duty Vehicles" & temp_cat_avg$year == 2021]
  mhd_2021_co2e <- temp_cat_avg$CO2e_millions[temp_cat_avg$veh_supertype == "Medium-/Heavy-Duty Vehicles" & temp_cat_avg$year == 2021]
  
  temp_ff <- Fuel_Factors_Weighted()
  temp_ff<-temp_ff[temp_ff$veh_type %in% c("Light-Duty Vehicles", "Medium-/Heavy-Duty Vehicles"),]
  
  temp_ff<-temp_ff %>% 
    rename(veh_supertype = veh_type) %>%
    mutate(NOx_CO2_ratio = ifelse(veh_supertype == "Light-Duty Vehicles", NOx_g_per_veh_mi/ldv_2021_co2e,NOx_g_per_veh_mi/mhd_2021_co2e),
           PM25_CO2_ratio = ifelse(veh_supertype == "Light-Duty Vehicles", PM25_exhaust_per_veh_mi/ldv_2021_co2e,PM25_exhaust_per_veh_mi/mhd_2021_co2e)) %>%
    select(veh_supertype, NOx_CO2_ratio, PM25_CO2_ratio)
  return(temp_ff)
})

#Em_OnRoad_Base
Em_OnRoad_Base <- reactive({
  #SETH I want to change this so electricity emissions are pulled out better. Gotta check what it might effect with the group
  temp_eob<- left_join(VMT_Type_Tech_Base()[,c('veh_type','veh_subtype','year','mmt_by_subtype')],
                       EmRate_by_Tech()[,c('veh_type','veh_subtype','year','emission_rate')]) %>%
    left_join(PHEV_Em_Apportionment()[,c('veh_type','year','PHEV_elc_per_em')])
  
  temp_eob<-temp_eob %>% 
    mutate(fuel_type = case_match(veh_subtype, !!!veh_subtype_to_fuel_type_mapping)) %>%
    mutate(phev_bin = !(veh_subtype %in% PHEV_fuel_types)) %>%
    mutate(uses_electiricity = as.numeric(veh_subtype %in% ev_fuel_types)) %>%
    mutate(emissions = ifelse(phev_bin, mmt_by_subtype*emission_rate, (1-PHEV_elc_per_em)*mmt_by_subtype*emission_rate)) %>%
    mutate(ev_emissions = ifelse(phev_bin,   uses_electiricity*mmt_by_subtype*emission_rate, uses_electiricity*(PHEV_elc_per_em)*mmt_by_subtype*emission_rate))
    
  em_on_road_base<-rbind(
      temp_eob,
      temp_eob %>% filter(!phev_bin) %>% mutate(emissions = ev_emissions) %>% mutate(fuel_type = "Electricity")
    ) %>%
    group_by(veh_type, fuel_type, year) %>%
    summarise(MT_CO2e_direct= sum(emissions,na.rm = T)) %>% 
    left_join(Fuel_Factors_Baselines[Fuel_Factors_Baselines$units == "upstream_life_cycle_factor",c("fuel_type","value")]) %>%
    mutate(MT_CO2e_upstream = value*MT_CO2e_direct) %>% select(-value)
  
  return(em_on_road_base)
  }) #Check if Qi is using this
Em_OnRoad_Base_up <- reactive({
  #browser()
  temp_eob<- left_join(VMT_Type_Tech_Base()[,c('veh_type','veh_subtype','year','mmt_by_subtype')],
                       EmRate_by_Tech()[,c('veh_type','veh_subtype','year','emission_rate')]) %>%
    left_join(PHEV_Em_Apportionment()[,c('veh_type','year','PHEV_elc_per_em')])
  
  temp_eob<-temp_eob %>% 
    mutate(fuel_type = case_match(veh_subtype, !!!veh_subtype_to_fuel_type_mapping)) %>%
    mutate(phev_bin = !(veh_subtype %in% PHEV_fuel_types)) %>%
    mutate(uses_electiricity = as.numeric(veh_subtype %in% ev_fuel_types)) %>%
    mutate(emissions = ifelse(phev_bin, mmt_by_subtype*emission_rate, (1-PHEV_elc_per_em)*mmt_by_subtype*emission_rate)) %>%
    mutate(ev_emissions = ifelse(phev_bin,   uses_electiricity*mmt_by_subtype*emission_rate, uses_electiricity*(PHEV_elc_per_em)*mmt_by_subtype*emission_rate))
  temp_eob$emissions[temp_eob$veh_subtype %in% c("EV","EV100","EV200","EV300")]<-0
  em_on_road_base<-temp_eob %>%
    group_by(veh_type,fuel_type, year) %>%
    summarise(MT_CO2e_direct= sum(emissions),
              MT_CO2e_electricity = sum(ev_emissions)) %>% 
    left_join(Fuel_Factors_Baselines[Fuel_Factors_Baselines$units == "upstream_life_cycle_factor",c("fuel_type","value")]) %>%
    mutate(MT_CO2e_upstream = value*MT_CO2e_direct) %>% select(-value) %>%
    mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>% 
    group_by(veh_supertype, year) %>%
    summarise(MT_CO2e_direct= sum(MT_CO2e_direct, na.rm = T),
              MT_CO2e_electricity = sum(MT_CO2e_electricity, na.rm = T),
              MT_CO2e_upstream = sum(MT_CO2e_upstream, na.rm = T))
  
    #below is for looking at error
  #   group_by(veh_supertype, year, fuel_type, veh_type) |>
  #   mutate(MT_CO2e_direct= sum(MT_CO2e_direct, na.rm = T),
  #   MT_CO2e_electricity = sum(MT_CO2e_electricity, na.rm = T),
  # MT_CO2e_upstream = sum(MT_CO2e_upstream, na.rm = T)) |> filter(#veh_supertype == "Medium-/Heavy-Duty Vehicles",
  #                                                                year %in% c(2021)) |> View()

  return(em_on_road_base)
})
#This is from Em_OnRoad_Base line 82
e_emmissions_apportionment <- reactive({
  temp_e <- Em_OnRoad_Base() %>%
    mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>%
    group_by(veh_supertype, year, fuel_type) %>%
    summarise(electricity_per_em_temp = sum(MT_CO2e_direct)) %>%
    ungroup() %>% group_by(veh_supertype,year) %>%
    mutate(electricity_per_em = electricity_per_em_temp/sum(electricity_per_em_temp)) %>%
    filter(fuel_type == "Electricity") %>%
    filter(veh_supertype == "Light-Duty Vehicles") %>%
    ungroup() %>%
    select(year, electricity_per_em)
  return(temp_e)
})

#Tech_Frac_Vision ---- 
Tech_Frac_Vision <- reactive({ #this is electiric vehicle projections
  
  col <- case_match(rvs$Baseline$veh_elec_baseline, !!!ev_forecast_mapping)
  Tech_Frac_Vision_temp <- TechFrac #dont think i need to reassign this to protect the original names
  
  names(Tech_Frac_Vision_temp)[names(Tech_Frac_Vision_temp) == col] <- "tech_frac_forecast" #this was the old aeo_tech_frac
  Tech_Frac_Vision_temp <- Tech_Frac_Vision_temp %>% select(veh_type, veh_subtype, year, stock_millions, tech_frac_forecast)
  
  return(Tech_Frac_Vision_temp)
})

#passenger rail ----
passenger_rail_miles <- reactive({ #not sure where we need this so I'm leaving it in this indeterminate form for now
  #req('')
  #The data for the amtrak riders is wronge NOTE gonna ask Qi about this
  state_ch <- rvs$Baseline$state
  #browser()
  #passenger rail inputs
  # input_AmTrak_EnergySource <- 'Diesel' #From Fuel Factors/Base inputs #Original in Excel tool to allow custom energy source
  # input_CR_EnergySource <- 'Diesel' #From Fuel Factors/Base inputs
  # input_HR_EnergySource <- 'Electric'#From Fuel Factors/Base inputs
  # input_LR_EnergySource <- 'Electric' #From Fuel Factors/Base inputs
  input_AmTrak_AvgTripLength <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & 
                                                        rvs$Assumptions$transit_mode == "AmTrak" &
                                                        rvs$Assumptions$unit == "avg_trip_miles"] # maybe move this to advanced sorry
  
  
  Passenger_Rail_State_Mileage <- Passenger_Rail_State_Mileage %>% 
    filter(state == state_ch)  %>%
    mutate(amtrak_miles = amtrak_riders*input_AmTrak_AvgTripLength)
  
  Passenger_Rail <- Passenger_Rail_State_Mileage
  for(yr in 2020:2050){
    Passenger_Rail_temp = Passenger_Rail_State_Mileage %>% mutate(year = yr)
    Passenger_Rail = rbind(Passenger_Rail, Passenger_Rail_temp)
  }
  
  return(Passenger_Rail)
  })

passenger_rail_fuel_factors <- reactive({
  
  input_BTU_per_gallon_diesel <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                Fuel_Factors_Baselines$units == "fuel_conversion_BTU"] # also get from advanced oops
  input_BTU_per_kWh <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "electricity" &
                                                      Fuel_Factors_Baselines$units == "fuel_conversion_BTU"] #3414 #From Fuel Factors/Base inputs
  input_Diesel_CO2_kg_per_gallon <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                   Fuel_Factors_Baselines$units == "fuel_carbon_content"] #9.4 #From Fuel Factors
  input_Locomotives_CH4_gCO2eq_per_gallon <- Fuel_Factors$CH4_g_per_gallon[Fuel_Factors$fuel_type=="Diesel" & Fuel_Factors$veh_type=="Locomotives"]*Warming_Potential$GWP[Warming_Potential$Gas == "CH4"]
  input_Locomotives_N20_gCO2eq_per_gallon <- Fuel_Factors$N20_g_per_gallon[Fuel_Factors$fuel_type=="Diesel" & Fuel_Factors$veh_type=="Locomotives"]*Warming_Potential$GWP[Warming_Potential$Gas == "N20"] 
  
  Passenger_Rail_FuelFactors <- Passenger_Rail_FuelFactors %>%
    mutate(Diesel_Amtrak_CO2eq = amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
           Diesel_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
           Diesel_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
           Diesel_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon)) %>%
    left_join(electricity_emrate() %>% select(year, electricity_carbon_content) %>% filter(!duplicated(year)))%>%
    mutate(Electric_Amtrak_CO2eq = amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
           Electric_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
           Electric_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
           Electric_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content)
  
  return(Passenger_Rail_FuelFactors)
})

passenger_rail_emissions <- reactive({
  
  temp<-passenger_rail_miles() %>% 
    left_join(passenger_rail_fuel_factors()) %>%
    mutate(
      amtrak_em_diesel = amtrak_miles*Diesel_Amtrak_CO2eq/1000000,
      amtrak_em_electricity = amtrak_miles*Electric_Amtrak_CO2eq/1000000,
      cr_em_diesel = commuterrail_miles*Diesel_CR_CO2eq/1000000,
      cr_em_electricity = commuterrail_miles*Electric_CR_CO2eq/1000000,
      hr_em_diesel = heavyrail_miles*Diesel_HR_CO2eq/1000000,
      hr_em_electricity = heavyrail_miles*Electric_HR_CO2eq/1000000,
      lr_em_diesel = lightrail_miles*Diesel_LR_CO2eq/1000000,
      lr_em_electricity = lightrail_miles*Electric_LR_CO2eq/1000000
    ) %>% #filter(year == 2021) %>% 
    mutate(
      MT_CO2e_direct = amtrak_em_diesel*(rvs$Advanced$value[rvs$Advanced$table_no_ui == 4&rvs$Advanced$mode_service == "Amtrak"&rvs$Advanced$unit == "energy_source"]=="Diesel")+
        cr_em_diesel*(rvs$Advanced$value[rvs$Advanced$table_no_ui == 4&rvs$Advanced$mode_service == "Commuter Rail"&rvs$Advanced$unit == "energy_source"]=="Diesel")+
        hr_em_diesel*(rvs$Advanced$value[rvs$Advanced$table_no_ui == 4&rvs$Advanced$mode_service == "Heavy Rail"&rvs$Advanced$unit == "energy_source"]=="Diesel")+
        lr_em_diesel*(rvs$Advanced$value[rvs$Advanced$table_no_ui == 4&rvs$Advanced$mode_service == "Light Rail"&rvs$Advanced$unit == "energy_source"]=="Diesel"),
      MT_CO2e_electricity = amtrak_em_electricity*(rvs$Advanced$value[rvs$Advanced$table_no_ui == 4&rvs$Advanced$mode_service == "Amtrak"&rvs$Advanced$unit == "energy_source"]=="Electric")+
        cr_em_electricity*(rvs$Advanced$value[rvs$Advanced$table_no_ui == 4&rvs$Advanced$mode_service == "Commuter Rail"&rvs$Advanced$unit == "energy_source"]=="Electric")+
        hr_em_electricity*(rvs$Advanced$value[rvs$Advanced$table_no_ui == 4&rvs$Advanced$mode_service == "Heavy Rail"&rvs$Advanced$unit == "energy_source"]=="Electric")+
        lr_em_electricity*(rvs$Advanced$value[rvs$Advanced$table_no_ui == 4&rvs$Advanced$mode_service == "Light Rail"&rvs$Advanced$unit == "energy_source"]=="Electric"),
    ) %>%
    select(year,MT_CO2e_direct,MT_CO2e_electricity)
  
  return(temp)
})

#Freight Rail ----
freight_rail_emissions <- reactive({ #not sure where we need this so I'm leaving it in this indeterminate form for now

  state_ch <- rvs$Baseline$state
  #browser()
  #Freight rail inputs
  # input_FR_GrowthRate <- rvs$Advanced$value[rvs$Advanced$table_no_ui == 5 & rvs$Advanced$unit == "growth_rate"] %>% as.numeric()
  input_FR_BTU_per_tonmile <- rvs$Advanced$value[rvs$Advanced$table_no_ui == 5 & rvs$Advanced$unit == "energy_intensity"] %>% as.numeric()
  input_BTU_per_gallon_diesel <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                   Fuel_Factors_Baselines$units == "fuel_conversion_BTU"]
  input_Diesel_CO2_kg_per_gallon <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                      Fuel_Factors_Baselines$units == "fuel_carbon_content"]

  #need to add state filter to save memory
  Freight_Rail <- Freight_Rail_Data %>%
    filter(state == state_ch)
  
  for(yr in 2020:2050){
    fr_temp <- Freight_Rail_Data %>% filter(state == state_ch) %>% mutate(year= yr) %>%
      mutate(FR_million_tonmiles = FR_million_tonmiles*(1+growthrate)^(yr-2019)
             )
    Freight_Rail <- rbind(Freight_Rail, fr_temp)
  }
  
  #NOTE for Ben: The excel sheet seems to not refer to the right state's million ton-miles for this one
  Freight_Rail <- Freight_Rail %>% 
    mutate(em_rate = input_FR_BTU_per_tonmile/input_BTU_per_gallon_diesel*input_Diesel_CO2_kg_per_gallon*1000) %>%
    mutate(MT_CO2e_direct = em_rate*FR_million_tonmiles) %>%
    select(year, MT_CO2e_direct)
  
  return(Freight_Rail)
})

#Public Transit----
public_transit_emissions <- reactive({ #not sure where we need this so I'm leaving it in this indeterminate form for now
 #browser()
  #these are apportionment for each public transit fuel type in baseline parameters
  input_MB_app_diesel<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Bus" & rvs$Advanced$fuel_type == "Diesel"] %>% as.numeric()
  input_MB_app_CNG<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Bus" & rvs$Advanced$fuel_type == "CNG"]%>% as.numeric()
  input_MB_app_Electric<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Bus" & rvs$Advanced$fuel_type == "Electric"]%>% as.numeric()
  
  input_DR_app_gasoline<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Demand Response" & rvs$Advanced$fuel_type == "Gasoline"] %>% as.numeric()#these are apportionment for each public transit fuel type in baseline parameters
  input_DR_app_CNG<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Demand Response" & rvs$Advanced$fuel_type == "CNG"]%>% as.numeric()
  input_DR_app_Electric<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Demand Response" & rvs$Advanced$fuel_type == "Electric"]%>% as.numeric()
  
  
  input_CB_app_diesel <- rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Commuter Bus" & rvs$Advanced$fuel_type == "Diesel"]%>% as.numeric() #these are apportionment for each public transit fueel type in baseline parameters
  input_CB_app_CNG<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Commuter Bus" & rvs$Advanced$fuel_type == "CNG"]%>% as.numeric()
  input_CB_app_Electric<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Commuter Bus" & rvs$Advanced$fuel_type == "Electric"]%>% as.numeric()
  
  # #need to implement the custom or default code ig
  MB_diesel_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Bus" & rvs$Assumptions$fuel_type == "Diesel" & rvs$Assumptions$unit == "veh_fuel_economy"]
  MB_cng_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Bus" & rvs$Assumptions$fuel_type == "CNG" & rvs$Assumptions$unit == "veh_fuel_economy"]
  MB_electric_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Bus" & rvs$Assumptions$fuel_type == "Electric" & rvs$Assumptions$unit == "veh_fuel_economy"]
  DR_gasoline_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Demand Response" & rvs$Assumptions$fuel_type == "Gasoline" & rvs$Assumptions$unit == "veh_fuel_economy"]
  DR_cng_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Demand Response" & rvs$Assumptions$fuel_type == "CNG" & rvs$Assumptions$unit == "veh_fuel_economy"]
  DR_electric_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Demand Response" & rvs$Assumptions$fuel_type == "Electric" & rvs$Assumptions$unit == "veh_fuel_economy"]
  CB_diesel_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Commuter Bus" & rvs$Assumptions$fuel_type == "Diesel" & rvs$Assumptions$unit == "veh_fuel_economy"]
  CB_cng_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Commuter Bus" & rvs$Assumptions$fuel_type == "CNG" & rvs$Assumptions$unit == "veh_fuel_economy"]
  CB_electric_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Commuter Bus" & rvs$Assumptions$fuel_type == "Electric" & rvs$Assumptions$unit == "veh_fuel_economy"]
  
  #MB
  input_Diesel_per_Gasoline_eq <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                      Fuel_Factors_Baselines$units == "fuel_conversion_gasoline_equivalent"] 
  input_Diesel_CO2_kg_per_gallon <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                     Fuel_Factors_Baselines$units == "fuel_carbon_content"] 
  
  input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile <- Fuel_Factors$GWP_CH4_g_per_mi[Fuel_Factors$fuel_type == "Diesel" &
                                                                         Fuel_Factors$veh_type == "Heavy-Duty Trucks"]
  input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile <- Fuel_Factors$GWP_N20_g_per_mi[Fuel_Factors$fuel_type == "Diesel" &
                                                                                     Fuel_Factors$veh_type == "Heavy-Duty Trucks"] 
  
  input_CNG_per_Gasoline_eq <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "CNG" &
                                                             Fuel_Factors_Baselines$units == "fuel_conversion_gasoline_equivalent"] 
  input_CNG_CO2_kg_per_gallon <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "CNG" &
                                                                   Fuel_Factors_Baselines$units == "fuel_carbon_content"] 
  input_HeavyDutyTruck_CNG_CH4_gCO2e_per_mile <- Fuel_Factors$GWP_CH4_g_per_mi[Fuel_Factors$fuel_type == "CNG" &
                                                                                    Fuel_Factors$veh_type == "Heavy-Duty Trucks"]
  input_HeavyDutryTruck_CNG_NOX_gCO2e_per_mile <- Fuel_Factors$GWP_N20_g_per_mi[Fuel_Factors$fuel_type == "CNG" &
                                                                                     Fuel_Factors$veh_type == "Heavy-Duty Trucks"] 
  input_Electricity_per_Gasoline_eq <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "electricity" &
                                                                 Fuel_Factors_Baselines$units == "fuel_conversion_gasoline_equivalent"] 
  input_Gasoline_CO2_kg_per_gallon <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Gasoline" &
                                                                   Fuel_Factors_Baselines$units == "fuel_carbon_content"] 
  input_LightDutyTruck_Gasoline_CH4_gCO2e_per_mile <- Fuel_Factors$GWP_CH4_g_per_mi[Fuel_Factors$fuel_type == "Gasoline" &
                                                                                    Fuel_Factors$veh_type == "Light-Duty Trucks"]
  input_LightDutryTruck_Gasoline_NOX_gCO2e_per_mile <- Fuel_Factors$GWP_N20_g_per_mi[Fuel_Factors$fuel_type == "Gasoline" &
                                                                                     Fuel_Factors$veh_type == "Light-Duty Trucks"] 
  
  input_up_Gasoline <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Gasoline" &
                                                      Fuel_Factors_Baselines$units == "upstream_life_cycle_factor"] 
  input_up_Diesel <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                    Fuel_Factors_Baselines$units == "upstream_life_cycle_factor"] 
  input_up_CNG <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "CNG" &
                                                 Fuel_Factors_Baselines$units == "upstream_life_cycle_factor"] 
    
  state_ch <- rvs$Baseline$state
  
  Public_Transit <- Public_Transit_data %>%
    filter(State == state_ch)
  
  for(yr in 2020:2050){
    Public_Transit_temp = Public_Transit_data %>%filter(State == state_ch)%>% mutate(year = yr) %>% filter(year == yr)
    Public_Transit = rbind(Public_Transit, Public_Transit_temp)
  }
  
  Public_Transit <- Public_Transit %>%
    dplyr::rename(
      cb_revmiles = CB,
      dr_revmiles = DR,
      mb_revmiles = MB)
  
  #browser()
  
  #NOTE FOR BEN: Issue here where the excel sheet is referencing the on-road vehcile economy for Bus: Diesel instead of COmmuter BUs: Diesel in public transit tab
  Public_Transit <- Public_Transit %>%
    left_join(electricity_emrate() %>% select(year, electricity_carbon_content) %>% filter(!duplicated(year))) %>% #the filter duplciated is just removing different vehicle types with the same values
    mutate(MB_diesel_emintensity = (1/MB_diesel_mpgge)*input_Diesel_per_Gasoline_eq*input_Diesel_CO2_kg_per_gallon*1000 + input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile + input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile,
           MB_cng_emintensity = (1/MB_cng_mpgge)*input_CNG_per_Gasoline_eq*input_CNG_CO2_kg_per_gallon*1000 + input_HeavyDutyTruck_CNG_CH4_gCO2e_per_mile + input_HeavyDutryTruck_CNG_NOX_gCO2e_per_mile,
           MB_electric_emintensity = (1/MB_electric_mpgge)*input_Electricity_per_Gasoline_eq*electricity_carbon_content,
           
           DR_gasoline_emintensity = (1/DR_gasoline_mpgge)*input_Gasoline_CO2_kg_per_gallon*1000 + input_LightDutyTruck_Gasoline_CH4_gCO2e_per_mile + input_LightDutryTruck_Gasoline_NOX_gCO2e_per_mile,
           DR_cng_emintensity = (1/DR_cng_mpgge)*input_CNG_per_Gasoline_eq*input_CNG_CO2_kg_per_gallon*1000 + input_HeavyDutyTruck_CNG_CH4_gCO2e_per_mile + input_HeavyDutryTruck_CNG_NOX_gCO2e_per_mile,
           DR_electric_emintensity = (1/DR_electric_mpgge)*input_Electricity_per_Gasoline_eq*electricity_carbon_content,
           
           CB_diesel_emintensity = (1/CB_diesel_mpgge)*input_Diesel_per_Gasoline_eq*input_Diesel_CO2_kg_per_gallon*1000 + input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile + input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile,
           CB_cng_emintensity =  (1/CB_cng_mpgge)*input_CNG_per_Gasoline_eq*input_CNG_CO2_kg_per_gallon*1000 + input_HeavyDutyTruck_CNG_CH4_gCO2e_per_mile + input_HeavyDutryTruck_CNG_NOX_gCO2e_per_mile,
           CB_electric_emintensity =  (1/CB_electric_mpgge)*input_Electricity_per_Gasoline_eq*electricity_carbon_content
           ) %>% 
   # filter(year >= 2021) %>%
    mutate(
      MB_Emissions_Direct = (MB_diesel_emintensity*input_MB_app_diesel+MB_cng_emintensity*input_MB_app_CNG)*mb_revmiles/1000000,
      MB_Emissions_Electricity = (MB_electric_emintensity*input_MB_app_Electric)*mb_revmiles/1000000,
      MB_Emissions_Upstream = (MB_diesel_emintensity*input_MB_app_diesel*input_up_Diesel+MB_cng_emintensity*input_MB_app_CNG*input_up_CNG)*mb_revmiles/1000000,
      
      DR_Emissions_Direct = (DR_gasoline_emintensity*input_DR_app_gasoline+DR_cng_emintensity*input_DR_app_CNG)*dr_revmiles/1000000,
      DR_Emissions_Electricity = (DR_electric_emintensity*input_DR_app_Electric)*dr_revmiles/1000000,
      DR_Emissions_Upstream = (DR_gasoline_emintensity*input_DR_app_gasoline*input_up_Gasoline+DR_cng_emintensity*input_DR_app_CNG*input_up_CNG)*dr_revmiles/1000000,
      
      
      CB_Emissions_Direct = (CB_diesel_emintensity*input_CB_app_diesel+CB_cng_emintensity*input_CB_app_CNG+CB_electric_emintensity*input_CB_app_Electric)*cb_revmiles/1000000,
      CB_Emissions_Electricity = (CB_electric_emintensity*input_CB_app_Electric)*cb_revmiles/1000000,
      CB_Emissions_Upstream = (CB_diesel_emintensity*input_CB_app_diesel*input_up_Diesel+CB_cng_emintensity*input_CB_app_CNG*input_up_CNG)*cb_revmiles/1000000,
    ) %>% 
    mutate(MT_CO2e_direct  = MB_Emissions_Direct+DR_Emissions_Direct+CB_Emissions_Direct,
           MT_CO2e_electricity   = MB_Emissions_Electricity+DR_Emissions_Electricity+CB_Emissions_Electricity,
           MT_CO2e_upstream = MB_Emissions_Upstream+DR_Emissions_Upstream+CB_Emissions_Upstream) %>% #View()
    select(year, MT_CO2e_direct, MT_CO2e_electricity, MT_CO2e_upstream )
  
  return(Public_Transit)
})

#Fuel Factor Weighted ----
Fuel_Factors_Weighted <- reactive({
  
  temp<-VMT_Forecast() %>% 
    filter(year == 2022) %>%
    mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>%
    group_by(veh_supertype) %>%
    mutate(vmt_supertype = sum(state_vmt_AEO)) %>%
    ungroup() %>% group_by(year, veh_type, veh_supertype) %>% 
    summarise(veh_type_per_vmt_supertype = state_vmt_AEO/vmt_supertype) %>%
    ungroup() %>%
    select(-year)
    
  temp_super<-Fuel_Factors_Weighted_raw %>% 
    left_join(temp) %>% filter(!is.na(veh_supertype)) %>% group_by(veh_supertype) %>% 
    filter(!(veh_subtype == 'Gasoline' & veh_type == 'Heavy-Duty Trucks')) %>%
    summarise(across(where(is.numeric), function(x) sum(x*veh_type_per_vmt_supertype))) %>%
    ungroup() %>% select(-veh_type_per_vmt_supertype) %>% rename(veh_type = veh_supertype) %>% mutate(veh_subtype = "All")
  
  Fuel_Factors_Weighted <- rbind(Fuel_Factors_Weighted_raw, temp_super)
  
  return(Fuel_Factors_Weighted)
  })

Fuel_Factors_by_supertype <- reactive({
  Fuel_Factors_Weighted() %>% filter(veh_subtype == "All") %>% select(-veh_subtype) %>% convert_to_nested_list()
})

construction_and_maintenance <- reactive({
 de <- rvs$Advanced$value[rvs$Advanced$table_no_ui == 6 & rvs$Advanced$unit=="direct_emissions"] %>% as.numeric()
 ue <- rvs$Advanced$value[rvs$Advanced$table_no_ui == 6 & rvs$Advanced$unit=="upstream_emissions"] %>% as.numeric()
 join<-CO2e_Category_Averages() %>% filter(veh_supertype == "Medium-/Heavy-Duty Vehicles") %>% select(year,base_impf)
 
 c_m <- data.frame(year = 2021:2050, 
                   de_c = as.numeric(de), 
                   ue_c = as.numeric(ue)) %>%
   left_join(join) %>% 
   mutate(MT_CO2e_direct = de_c*base_impf,
          MT_CO2e_upstream = ue_c*base_impf)
 
 return(c_m)

 })
#Final Baseline Return 
baseline_ghg_forecast <- reactive({
  #browser()
  
  use_e = rvs$Baseline$include_electricity# %>% as.numeric()
  use_up = rvs$Baseline$include_upstream_fuels# %>% as.numeric()
  use_rr = rvs$Baseline$include_rail
  if(use_e %in% c("TRUE","1","Yes","YES")){use_e <- 1} else {use_e <- 0}
  if(use_up %in% c("TRUE","1","Yes","YES")){use_up <- 1} else {use_up <- 0}
  if(use_rr %in% c("TRUE","1","Yes","YES")){use_rr <- 1} else {use_rr <- 0}
    
  #Em_OnRoad_Base_up()
  #public_transit_emissions()
  #passenger_rail_emissions()
  #this is for removing public transit emissions from MD/HD vehicle emissions
  public_transit_subtract <- public_transit_emissions()%>%
    filter(year %in% c(rvs$Baseline$base_year, 
                       rvs$Baseline$horizon_year_1,
                       rvs$Baseline$horizon_year_2,
                       rvs$Baseline$horizon_year_3)) %>%
    mutate(veh_supertype = "Medium-/Heavy-Duty Vehicles") %>%
    mutate(subtract = MT_CO2e_direct + use_e*MT_CO2e_electricity+use_up*MT_CO2e_upstream) %>% #View()
    select(veh_supertype,year, subtract)
 #browser()
  temp<- Em_OnRoad_Base_up() %>%
    filter(year %in% c(rvs$Baseline$base_year, 
                       rvs$Baseline$horizon_year_1,
                       rvs$Baseline$horizon_year_2,
                       rvs$Baseline$horizon_year_3)) %>%
    left_join(public_transit_subtract) |> 
    mutate(subtract = ifelse(is.na(subtract), 0, subtract)) |> 
    mutate(Emissions = MT_CO2e_direct + use_e*MT_CO2e_electricity+use_up*MT_CO2e_upstream - subtract) %>% 
    select(veh_supertype,year, Emissions) %>% #View()
    
    rbind(
      
      public_transit_emissions()%>%
        filter(year %in% c(rvs$Baseline$base_year, 
                           rvs$Baseline$horizon_year_1,
                           rvs$Baseline$horizon_year_2,
                           rvs$Baseline$horizon_year_3)) %>%
        mutate(veh_supertype = "Public Transit") %>%
        mutate(Emissions = MT_CO2e_direct + use_e*MT_CO2e_electricity+use_up*MT_CO2e_upstream) %>% #View()
        select(veh_supertype,year, Emissions)
      
    ) %>%
    rbind(
      
      passenger_rail_emissions() %>%
        filter(year %in% c(rvs$Baseline$base_year, 
                           rvs$Baseline$horizon_year_1,
                           rvs$Baseline$horizon_year_2,
                           rvs$Baseline$horizon_year_3)) %>%
        mutate(veh_supertype = "Passenger Rail") %>%
        mutate(Emissions = (MT_CO2e_direct + use_e*MT_CO2e_electricity)*use_rr)%>% 
        select(veh_supertype,year, Emissions)
      
    ) %>%
    rbind(
      
      freight_rail_emissions()%>%
        filter(year %in% c(rvs$Baseline$base_year, 
                           rvs$Baseline$horizon_year_1,
                           rvs$Baseline$horizon_year_2,
                           rvs$Baseline$horizon_year_3)) %>%
        mutate(veh_supertype = "Freight Rail") %>%
        mutate(Emissions = MT_CO2e_direct*use_rr)%>% 
        select(veh_supertype,year, Emissions)
    ) %>% rbind(
      construction_and_maintenance()%>%
        filter(year %in% c(rvs$Baseline$base_year, 
                           rvs$Baseline$horizon_year_1,
                           rvs$Baseline$horizon_year_2,
                           rvs$Baseline$horizon_year_3)) %>%
        mutate(veh_supertype = "Construction and Maintenance") %>%
        mutate(Emissions = MT_CO2e_direct + use_up*MT_CO2e_upstream) %>% 
        select(veh_supertype,year, Emissions)
    ) %>% 
    pivot_wider(names_from= year, values_from = Emissions)
 
  
 return(temp)
})

baseline_ghg_forecast_all_years <- reactive({
  
  use_e = rvs$Baseline$include_electricity# %>% as.numeric()
  use_up = rvs$Baseline$include_upstream_fuels# %>% as.numeric()
  use_rr = rvs$Baseline$include_rail
  if(use_e %in% c("TRUE","1","Yes","YES")){use_e <- 1} else {use_e <- 0}
  if(use_up %in% c("TRUE","1","Yes","YES")){use_up <- 1} else {use_up <- 0}
  if(use_rr %in% c("TRUE","1","Yes","YES")){use_rr <- 1} else {use_rr <- 0}
  #this is for removing public transit emissions from MD/HD vehicle emissions
  public_transit_subtract <- public_transit_emissions()%>%
    mutate(veh_supertype = "Medium-/Heavy-Duty Vehicles") %>%
    mutate(subtract = MT_CO2e_direct + use_e*MT_CO2e_electricity+use_up*MT_CO2e_upstream) %>% #View()
    select(veh_supertype,year, subtract)
  
  temp<- Em_OnRoad_Base_up() %>%
    left_join(public_transit_subtract) |> 
    mutate(subtract = ifelse(is.na(subtract), 0, subtract)) |> 
    mutate(Emissions = MT_CO2e_direct + use_e*MT_CO2e_electricity+use_up*MT_CO2e_upstream - subtract) %>% 
    select(veh_supertype,year, Emissions) %>%
    
    rbind(
      
      public_transit_emissions()%>%
        #filter(year %in% c(2021, 
        #                   rvs$Baseline$horizon_year_1,
        #                   rvs$Baseline$horizon_year_2,
        #                   rvs$Baseline$horizon_year_3)) %>%
        mutate(veh_supertype = "Public Transit") %>%
        mutate(Emissions = MT_CO2e_direct + use_e*MT_CO2e_electricity+use_up*MT_CO2e_upstream) %>% 
        select(veh_supertype,year, Emissions)
      
    ) %>%
    rbind(
      
      passenger_rail_emissions() %>%
        #filter(year %in% c(2021, 
        #                   rvs$Baseline$horizon_year_1,
        #                   rvs$Baseline$horizon_year_2,
        #                   rvs$Baseline$horizon_year_3)) %>%
        mutate(veh_supertype = "Passenger Rail") %>%
        mutate(Emissions = (MT_CO2e_direct + use_e*MT_CO2e_electricity)*use_rr)%>% 
        select(veh_supertype,year, Emissions)
      
    ) %>%
    rbind(
      
      freight_rail_emissions()%>%
        #filter(year %in% c(2021, 
        #                   rvs$Baseline$horizon_year_1,
        #                   rvs$Baseline$horizon_year_2,
        #                   rvs$Baseline$horizon_year_3)) %>%
        mutate(veh_supertype = "Freight Rail") %>%
        mutate(Emissions = (MT_CO2e_direct)*use_rr)%>% 
        select(veh_supertype,year, Emissions)
    ) %>% rbind(
      construction_and_maintenance()%>%
        #filter(year %in% c(2021, 
        #                   rvs$Baseline$horizon_year_1,
        #                   rvs$Baseline$horizon_year_2,
        #                   rvs$Baseline$horizon_year_3)) %>%
        mutate(veh_supertype = "Construction and Maintenance") %>%
        mutate(Emissions = MT_CO2e_direct + use_up*MT_CO2e_upstream) %>% 
        select(veh_supertype,year, Emissions)
    ) #%>%
    #pivot_wider(names_from= year, values_from = Emissions)
  
  return(temp)
})

output$baseline_line_graph <- renderPlotly({
  req(baseline_ghg_forecast())
  
  
  line_plot(baseline_ghg_forecast())
})

### VMT Tables ----------------
### these tables don't have to be show to the user, but it is helpful to have them as reactive tables

#VMT Type Tech Base ----
VMT_Type_Tech_Base <- reactive({ #this is VMT 
  #browser()

  state_ch <- rvs$Baseline$state
  nhs_ch <- rvs$Baseline$trans_system_scope
  #browser()
  VMT_VehType<-VMT_Forecast() #name is a bit historic probably should be changed
  nhs_vals <- filter(NHS_VMT, state == state_ch)
  tech_frac_temp <- Tech_Frac_Vision()
  
  if(nhs_ch == "NHS Only"){
    
    VMT_Type_Tech_Basetemp <- tech_frac_temp %>% 
      left_join(VMT_VehType, by = c('year','veh_type')) %>%
      mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>%
      mutate(mmt_by_subtype = ifelse(veh_supertype == "Light-Duty Vehicles", 
                                     nhs_vals$LDV_pct_on_NHS[1]*state_vmt_AEO * tech_frac_forecast,
                                     nhs_vals$TRK_pct_on_NHS[1]*state_vmt_AEO * tech_frac_forecast)) 
  } else {
    
    VMT_Type_Tech_Basetemp <- tech_frac_temp %>% 
      left_join(VMT_VehType, by = c('year','veh_type')) %>%
      mutate(mmt_by_subtype = state_vmt_AEO * tech_frac_forecast)
    
  }
  
  return(VMT_Type_Tech_Basetemp)
  
})

VMT_Forecast <- reactive({
  
  AEO_VMT %>%
    left_join(filter(VMT_State_Allocation, state == rvs$Baseline$state) %>% select(year, state_vmt_pct_of_national), 
              by = join_by(year)) %>%
    mutate(state_vmt_AEO = VMT_AEO * state_vmt_pct_of_national) # state VMT forecast 
  
})

### category breakouts needed for EVSE -------
VMT_Type_Tech_MDHD <- reactive({
  VMT_Type_Tech_Base() %>%
    filter(veh_type %in% c("Medium-Duty Trucks", "Heavy-Duty Trucks")) %>% 
    summarize(veh_type, veh_subtype, mmt_by_subtype, state_pct_of_category = mmt_by_subtype / sum(mmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Medium-/Heavy-Duty Vehicles")
})

VMT_Type_Tech_Conventional_LDV <- reactive({
  VMT_Type_Tech_Base() %>%
    filter((veh_type == "Passenger Cars" & veh_subtype == "Gasoline ICE") |
             (veh_type == "Light-Duty Trucks" & veh_subtype == "Gasoline ICE")) %>%
    summarize(veh_type, veh_subtype, mmt_by_subtype, state_pct_of_category = mmt_by_subtype / sum(mmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Conventional LDV")
})

VMT_Type_Tech_Conventional_MDHD <- reactive({
  VMT_Type_Tech_Base() %>%
    filter((veh_type == "Medium-Duty Trucks" & veh_subtype == "Diesel ICE") |
             (veh_type == "Medium-Duty Trucks" & veh_subtype == "Gasoline ICE") | 
             (veh_type == "Heavy-Duty Trucks" & veh_subtype == "Diesel ICE")) %>%
    summarize(veh_type, veh_subtype, mmt_by_subtype, state_pct_of_category = mmt_by_subtype / sum(mmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Conventional MDHD")
})

VMT_Type_Tech_Electric_LDV <- reactive({
  VMT_Type_Tech_Base() %>%
    filter(veh_subtype %in% c("EV100", "EV200", "EV300")) %>%
    summarize(veh_type, veh_subtype, mmt_by_subtype, state_pct_of_category = mmt_by_subtype / sum(mmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Electric LDV")
})

VMT_Type_Tech_Electric_MDHD <- reactive({
  VMT_Type_Tech_Base() %>%
    filter(veh_type %in% c("Medium-Duty Trucks", "Heavy-Duty Trucks") & veh_subtype == "EV") %>% 
    summarize(veh_type, veh_subtype, mmt_by_subtype, state_pct_of_category = mmt_by_subtype / sum(mmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Electric MDHD")
})

### EmRate_by_Tech category average 

EmRate_Conventional_LDV <- reactive({
  EmRate_by_Tech() %>% 
    filter(veh_subtype == "Gasoline ICE", veh_type %in% c("Passenger Cars", "Light-Duty Trucks")) %>%
    select(veh_type, veh_subtype, year, emission_rate) %>%
    left_join(VMT_Type_Tech_Conventional_LDV(), by = join_by(veh_type, veh_subtype, year), relationship = "one-to-one") %>%
    summarize("emrate_category_avg" = weighted.mean(emission_rate, state_pct_of_category), .by = c("veh_category", "year"))
})

EmRate_Conventional_MDHD <- reactive({
  EmRate_by_Tech() %>% 
    filter((veh_type == "Medium-Duty Trucks" & veh_subtype == "Diesel ICE") |
             (veh_type == "Medium-Duty Trucks" & veh_subtype == "Gasoline ICE") | 
             (veh_type == "Heavy-Duty Trucks" & veh_subtype == "Diesel ICE")) %>%
    select(veh_type, veh_subtype, year, emission_rate) %>%
    left_join(VMT_Type_Tech_Conventional_MDHD(), by = join_by(veh_type, veh_subtype, year), relationship = "one-to-one") %>%
    summarize("emrate_category_avg" = weighted.mean(emission_rate, state_pct_of_category), .by = c("veh_category", "year"))
})

EmRate_Electric_LDV <- reactive({ ### excel doesn't grab all electric vehicles - but I think it should be this way?
  a<- EmRate_by_Tech() %>% 
    filter(veh_subtype %in% c("EV100", "EV200", "EV300")) %>%
    select(veh_type, veh_subtype, year, emission_rate) %>%
    left_join(VMT_Type_Tech_Electric_LDV(), by = join_by(veh_type, veh_subtype, year), relationship = "one-to-one") %>%
    summarize("emrate_category_avg" = weighted.mean(emission_rate, state_pct_of_category), .by = c("veh_category", "year"))
})


# electric mdhd # has problem
EmRate_Electric_MDHD <- reactive({
  EmRate_by_Tech() %>% 
    filter(veh_type %in% c("Medium-Duty Trucks", "Heavy-Duty Trucks") & veh_subtype == "EV") %>%
    select(veh_type, veh_subtype, year, emission_rate) %>%
    left_join(VMT_Type_Tech_Electric_MDHD(), by = join_by(veh_type, veh_subtype, year), relationship = "one-to-one") %>%
    summarize("emrate_category_avg" = weighted.mean(emission_rate, state_pct_of_category), .by = c("veh_category", "year"))
})

## Qi: the electricity_carbon_content value is a little bit off, need to check

# Outputs ---------------------------------------------------------------------
scenario_summary_results <- reactive({    #req('')
  #req(reactive_scenario())

  base_year <- rvs$Baseline$base_year
  
  dt <- baseline_ghg_forecast()
  ft <- VMT_Forecast()
  
  dt_emissions_base <- dt %>% ungroup() %>%
    summarise(across(where(is.numeric),~sum(.x,na.rm = T))) %>%
    pivot_longer(cols = everything(), names_to = "year") %>%
    mutate(Scenario = "Baseline")
  
  base_year_1_co2 <- dt_emissions_base$value[dt_emissions_base$year == rvs$Baseline$base_year]
  base_year_2_co2 <- dt_emissions_base$value[dt_emissions_base$year == rvs$Baseline$horizon_year_1]
  base_year_3_co2 <- dt_emissions_base$value[dt_emissions_base$year == rvs$Baseline$horizon_year_2]
  base_year_4_co2 <- dt_emissions_base$value[dt_emissions_base$year == rvs$Baseline$horizon_year_3]
  
  dt_VMT_base <- ft %>% ungroup() %>% filter(year >=2021) %>%
    filter(year %in% c(rvs$Baseline$base_year,
                       rvs$Baseline$horizon_year_1,
                       rvs$Baseline$horizon_year_2,
                       rvs$Baseline$horizon_year_3)) %>%
    group_by(year) %>%
    summarise(total_VMT = sum(state_vmt_AEO,na.rm = T))  %>%
    rename(value = total_VMT) %>% mutate(Scenario = "Baseline") 
  
  base_year_1_VMT <- dt_VMT_base$value[dt_VMT_base$year == rvs$Baseline$base_year]
  base_year_2_VMT <- dt_VMT_base$value[dt_VMT_base$year == rvs$Baseline$horizon_year_1]
  base_year_3_VMT <- dt_VMT_base$value[dt_VMT_base$year == rvs$Baseline$horizon_year_2]
  base_year_4_VMT <- dt_VMT_base$value[dt_VMT_base$year == rvs$Baseline$horizon_year_3]
  
  scen_select <-   reactive_scenario() 
 # scen_select |> mutate(`Grouped Projects` = ifelse(`Grouped Projects` == "Park-and-Ride"),"Park and Ride", `Grouped Projects`))
  strategy_temp <- scenario_sum() %>% left_join(scen_select, by = c("Strategy" = "Grouped Projects")) %>% 
    select('year', Strategy,Scenario1, Scenario2, total_change_newtrips, total_change_mtnox, total_change_pm25, total_change_VMT, total_change_MTCO2)
  
  scen_co2 <- strategy_temp %>% mutate(year = as.character(year)) %>% 
    group_by(year) %>%
    summarise(Scenario1 = sum(total_change_MTCO2*Scenario1,na.rm = T),
              Scenario2 = sum(total_change_MTCO2*Scenario2,na.rm = T)) %>%
    pivot_longer(cols = c(Scenario1,Scenario2), names_to = "Scenario") #%>%
    #left_join(dt_emissions_base %>% rename(add = value) %>% select(-Scenario), by = "year") %>%
    #mutate(value = value + add) %>% 
    #select(-add)
  
  scen_VMT <- strategy_temp %>%
    group_by(year) %>%
    summarise(Scenario1 = sum(total_change_VMT*Scenario1,na.rm=T)/1000000,
              Scenario2 = sum(total_change_VMT*Scenario2,na.rm=T)/1000000) %>%
    pivot_longer(cols = c(Scenario1,Scenario2), names_to = "Scenario")
  
  scen_NOX <- strategy_temp %>%
    group_by(year) %>%
    summarise(Scenario1 = sum(total_change_mtnox*Scenario1,na.rm=T),
              Scenario2 = sum(total_change_mtnox*Scenario2,na.rm=T)) %>%
    pivot_longer(cols = c(Scenario1,Scenario2), names_to = "Scenario")%>% 
    pivot_wider(names_from= year, values_from = value)  %>%
    mutate(table_title = "NOx Reduction (MT)")
  
  scen_NOX[,as.character(rvs$Baseline$base_year)]<-0
  
  scen_PM25 <- strategy_temp %>%
    group_by(year) %>%
    summarise(Scenario1 = sum(total_change_pm25*Scenario1,na.rm=T),
              Scenario2 = sum(total_change_pm25*Scenario2,na.rm=T)) %>%
    pivot_longer(cols = c(Scenario1,Scenario2), names_to = "Scenario")%>% 
    pivot_wider(names_from= year, values_from = value)  %>%
    mutate(table_title = "PM2.5 Reduction (MT)")
  
  scen_PM25[,as.character(rvs$Baseline$base_year)]<-0
  
  scen_NewTrips <- strategy_temp %>%
    group_by(year) %>%
    summarise(Scenario1 = sum(total_change_newtrips*Scenario1,na.rm=T),
              Scenario2 = sum(total_change_newtrips*Scenario2,na.rm=T)) %>%
    pivot_longer(cols = c(Scenario1,Scenario2), names_to = "Scenario")%>% 
    pivot_wider(names_from= year, values_from = value)  %>% 
    mutate(table_title = "New Daily Active Trips")
  
  scen_NewTrips[,as.character(rvs$Baseline$base_year)]<-0
  
  fin_emissions_table <- rbind(dt_emissions_base,scen_co2 %>% mutate(value = ifelse(year == rvs$Baseline$horizon_year_1, base_year_2_co2 + value,
                                                                                           ifelse(year == rvs$Baseline$horizon_year_2, base_year_3_co2 + value,
                                                                                                  ifelse(year == rvs$Baseline$horizon_year_3,base_year_4_co2 + value, NA))))) %>%
    #mutate(value = ifelse(Scenario != "Baseline"&year == rvs$Baseline$base_year, base_year_1_co2,value))
    pivot_wider(names_from= year, values_from = value, values_fill = base_year_1_co2)
  fin_emissions_table <- fin_emissions_table %>% 
    mutate(table_title = "Emissions (MT CO2e)")
  
  fin_emissions_reduction_table <- scen_co2 %>%
    pivot_wider(names_from = year) %>% 
    mutate(table_title = "Emissions Reduction (MT from Baseline)")
  
  fin_emissions_reduction_table[,as.character(rvs$Baseline$base_year)]<-0
  
  
  fin_emissions_per_change_table <- scen_co2 %>% mutate(value = ifelse(year == rvs$Baseline$horizon_year_1, (value)/base_year_2_co2,
                                                                              ifelse(year == rvs$Baseline$horizon_year_2, (value)/base_year_3_co2,
                                                                                     ifelse(year == rvs$Baseline$horizon_year_3,(value)/base_year_4_co2, NA)))) %>%
    pivot_wider(names_from = year) %>% 
    mutate(table_title = "Emissions Reduction (% from Baseline)")
  
  fin_emissions_per_change_table[,as.character(rvs$Baseline$base_year)]<-0
  
  
  fin_vmt_table <- rbind(dt_VMT_base,scen_VMT%>% mutate(value = ifelse(year == rvs$Baseline$horizon_year_1, base_year_2_VMT + value,
                                                                       ifelse(year == rvs$Baseline$horizon_year_2, base_year_3_VMT + value,
                                                                              ifelse(year == rvs$Baseline$horizon_year_3,base_year_4_VMT + value, NA))))) %>%
    #mutate(value = ifelse(Scenario != "Baseline"&year == rvs$Baseline$base_year, base_year_1_co2,value))
    pivot_wider(names_from= year, values_from = value, values_fill = base_year_1_VMT) 
  
  fin_vmt_table <- fin_vmt_table %>%
    mutate(table_title = "VMT (millions)")
  
  fin_vmt_reduction_table <- scen_VMT  %>%
    pivot_wider(names_from = year)%>%
    mutate(table_title = "VMT Reduction (millions from Baseline)")
  
  fin_vmt_reduction_table[,as.character(rvs$Baseline$base_year)]<-0
  #browser()
  fin_vmt_per_change_table <- scen_VMT %>% mutate(value = ifelse(year == rvs$Baseline$base_year, (value)/base_year_1_VMT,
                                                                 ifelse(year == rvs$Baseline$horizon_year_1, (value)/base_year_2_VMT,
                                                                        ifelse(year == rvs$Baseline$horizon_year_2, (value)/base_year_3_VMT,
                                                                               ifelse(year == rvs$Baseline$horizon_year_3,(value)/base_year_4_VMT, NA))))) %>% 
    pivot_wider(names_from = year) %>%
    mutate(table_title = "VMT Reduction (% from Baseline)")
 #browser()
  fin_vmt_per_change_table[,as.character(rvs$Baseline$base_year)]<-0
  
  
  
  fin_table <- rbind(fin_emissions_table, fin_emissions_reduction_table, fin_emissions_per_change_table,
                     fin_vmt_table, fin_vmt_reduction_table, fin_vmt_per_change_table, 
                     scen_NOX, scen_PM25, scen_NewTrips)
  return(fin_table)
  })
