output_roadway_resurf <- reactive({

  
  percent_truck_traffic <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 5 & rvs$Assumptions$unit == "truck_traffic_pct" & rvs$Assumptions$road_class == "Freeway"]
  
  TIST_Delta_Fuel_Costs <- -0.795
  Gas_Price <- 3.18
  value_mi_per_gal <- 19.20
  value_kg_per_gal <- 8.10
  em_rate_weight <- value_kg_per_gal/value_mi_per_gal*1000
  #cost_per_lane_mile <- 
  emrate_by_tech <- CO2e_Category_Averages() |> 
    mutate(d2014 = CO2e_millions/em_rate_weight)
  
  return()
})