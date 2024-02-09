### LOAD -----------------------------------------------------------------------
# library(glue)
# library(readxl)
# library(tidyverse)
# 
# ### READ -----------------------------------------------------------------------
# 
# strategy_params_evse <- 
#   read_excel("TEACART_v1.8_Local_shiny.xlsx", sheet = "EVSE", 
#              range = "B14:C18", col_names = c("port_type", "elasticity_to_sales"))
# 
# ### Freight Emissions Rate
# emrate_light <-
#   read_excel("C:\\Users\\gvendemiatti\\OneDrive - Cambridge Systematics\\R Shiny Migration\\TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "EVSE", range = "B4:E5",
#              col_names = c("vehicle_category", "2025", "2030", "2050")) %>%
#   pivot_longer(cols = !(vehicle_category), names_to = "year", values_to = "emrate")
# 
# emrate_mdhd <-
#   read_excel("C:\\Users\\gvendemiatti\\OneDrive - Cambridge Systematics\\R Shiny Migration\\TEACART_v1.8_Local_shiny.xlsx",
#            sheet = "EVSE", range = "B9:E10",
#            col_names = c("vehicle_category", "2025", "2030", "2050")) %>%
#   pivot_longer(cols = !(vehicle_category), names_to = "year", values_to = "emrate")
# 
# emrate_evse <-
#   bind_rows(emrate_light, emrate_mdhd) %>%
#     separate(vehicle_category, into = c("vehicle_type", "fuel_type"), sep = c(" \\(")) %>%
#     mutate(fuel_type = str_replace(str_remove(fuel_type, "\\)"), "Gas", "Conventional"),
#            vehicle_type = str_replace(str_replace(vehicle_type, "Automobile", "Vehicles"),
#                                       "Medium/Heavy Duty", "Medium and Heavy Duty Trucks")) %>%
#     pivot_wider(names_from = fuel_type, values_from = emrate, names_prefix = "emrate_")
# 
# capital_inputs_evse <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "EVSE", range = "B27:E31", 
#              col_names = c("port_type", "2025", "2030", "2050")) %>%
#   pivot_longer(cols = !(port_type), names_to = "year", values_to = "total_ports")
# 
# annual_miles <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "EVSE", range = "B21:E22", 
#              col_names = c("vehicle_type", "2025", "2030", "2050")) %>%
#   pivot_longer(cols = !(vehicle_type), names_to = "year", values_to = "annual_miles")
# 
# fuel_factor_mdhd_weighted <- c("nox" = 2.748, "pm25_exhaust" = 0.062, "pm25_tiresBrakes" = 0.012)
# fuel_factor_ldv_weighted <- c("nox" = 0.235, "pm25_exhaust" = 0.005, "pm25_tiresBrakes" = 0.004)
# 
# ### HELPFUL FUNCTIONS ----------------------------------------------------------
# get_vmt_change <- function(annual_miles, port_type_impacts, year, vehicle_type){
#   my_vehicle_type <- vehicle_type
#   my_year <- year ### Needed to prevent the confusion of "year" the column and "year" the value
#   if (vehicle_type == "Light Duty Vehicles"){
#     filtered_multiplier_impacts <-
#       port_type_impacts %>%
#       filter(port_type %in% c("Level 2: General public", "DCFC (50kw): General public", "DCFC (150kw): General public"),
#              year == my_year) %>%
#       pull(multiplier_impact)
#   } else {
#     filtered_multiplier_impacts <-
#       port_type_impacts %>%
#       filter(str_detect(port_type, "truck"),
#              year == my_year) %>%
#       pull(multiplier_impact)
#   }
# 
#   return(sum(annual_miles * filtered_multiplier_impacts))
# }

# ### CALCULATE ------------------------------------------------------------------
# port_type_impacts <-
#   capital_inputs_evse %>%
#   left_join(strategy_params_evse, by = join_by(port_type)) %>%
#   mutate(multiplier_impact = total_ports * elasticity_to_sales)
# 
# output <- 
#   annual_miles %>%
#   left_join(emrate_evse, by = join_by(vehicle_type, year)) %>%
#   rowwise() %>% ### very important! Could be removed if function was rewritten to use matrix multiplication
#   mutate(VMT_affected = get_vmt_change(annual_miles = annual_miles, port_type_impacts = port_type_impacts, 
#                                        year = year, vehicle_type = vehicle_type)) %>% ### argument = object/column
#   ungroup() %>%
#   mutate(displaced_conventional_emissions = -VMT_affected * emrate_Conventional / 1000000, 
#          added_electricity_emissions = VMT_affected * emrate_Electric / 1000000)
# 
# output_summarized <-
#   output %>%
#   group_by(year) %>%
#   summarize(total_change_direct = sum(displaced_conventional_emissions), 
#             total_change_electricity = sum(added_electricity_emissions),
#             truck_vmt_affected = sum(VMT_affected[vehicle_type == "Medium and Heavy Duty Trucks"]),
#             light_vmt_affected = sum(VMT_affected[vehicle_type == "Light Duty Vehicles"])) %>%
#   mutate(total_change_nox = (fuel_factor_ldv_weighted["nox"]*light_vmt_affected + fuel_factor_mdhd_weighted["nox"] * truck_vmt_affected) / 1000000,
#          total_change_pm25 = (fuel_factor_ldv_weighted["pm25_exhaust"]*light_vmt_affected + 
#                                 fuel_factor_mdhd_weighted["pm25_exhaust"] * truck_vmt_affected) / 1000000)

### REACTIVE ----------

emrate_evse <- reactive({
  bind_rows( ### this runs pretty slow - needs to be rewritten Gui 1/22/24
    EmRate_Electric_MDHD() %>% mutate(veh_supertype = "Medium/Heavy Duty Vehicles") %>% rename(emrate_Electric = emrate_category_avg),
    EmRate_Conventional_LDV() %>% mutate(veh_supertype = "Light Duty Vehicles") %>% rename(emrate_Conventional = emrate_category_avg),
    EmRate_Conventional_MDHD() %>% mutate(veh_supertype = "Medium/Heavy Duty Vehicles") %>% rename(emrate_Conventional = emrate_category_avg),
    EmRate_Electric_LDV() %>% mutate(veh_supertype = "Light Duty Vehicles") %>% rename(emrate_Electric = emrate_category_avg)
  )
})

Stock_filtered <- reactive({
  Stock_Type %>% filter(year %in% c(rvs$Baseline$horizon_year_1, rvs$Baseline$horizon_year_2, rvs$Baseline$horizon_year_3)) ## annual miles
})

elasticities_by_port_type <- reactive({ #strategy_params_evse
  rvs$Assumptions %>% 
    filter(table == "EV Charging Infrastructure") %>% 
    select(charge_port_detail, unit, value) %>% 
    pivot_wider(names_from = "unit", values_from = "value")
})

# observeEvent(list(EmRate_by_Tech(), VMT_Forecast()), { ### uncomment this line and browser and comment below to test!
output_EVSE <- reactive({
  
  capital_inputs <-
    rvs$Projects %>%
    filter(category == "EV Charging Infrastructure") %>%
    select(year, charge_port_detail, unit, value) %>%
    pivot_wider(names_from = unit, values_from = value)
  
  CalculatedPorts <- capital_inputs %>% left_join(elasticities_by_port_type(), by = join_by("charge_port_detail")) %>% 
    mutate(calculated_ports = new_ports * veh_sales_elasticity_wrt_ports)
  
  vmt_affected <-
    CalculatedPorts %>% 
    mutate(veh_supertype = if_else(str_detect(charge_port_detail, "General public"), "Light Duty Vehicles", "Medium/Heavy Duty Vehicles")) %>%
    get_horizon_years(my_rv = rvs) %>% select(year, veh_supertype, calculated_ports) %>%
    left_join(x = Stock_filtered(), y = ., by = join_by(veh_supertype, year), relationship = "one-to-many") %>%
    summarize("VMT_affected" = sum(calculated_ports * MT_per_vehtype), .by = c("veh_supertype", "year"))
  
  evse_by_year_supertype <- Stock_filtered() %>% 
    left_join(vmt_affected, by = join_by(veh_supertype, year)) %>%
    left_join(emrate_evse(), by = join_by(veh_supertype, year)) %>%
    mutate(displaced_conventional_emissions = -VMT_affected * emrate_Conventional / 1000000, 
           added_electricity_emissions = VMT_affected * emrate_Electric / 1000000)
  
  evse_by_year_supertype %>% 
    group_by(year) %>%
    summarize(total_change_direct = sum(displaced_conventional_emissions, na.rm = T),
              total_change_electricity = sum(added_electricity_emissions, na.rm = T),
              truck_vmt_affected = sum(VMT_affected[veh_supertype == "Medium/Heavy Duty Vehicles"], na.rm = T),
              light_vmt_affected = sum(VMT_affected[veh_supertype == "Light Duty Vehicles"], na.rm = T)) %>%
    mutate(total_change_nox = (Fuel_Factors_by_supertype()[["Light Duty Vehicles"]]$NOx_g_per_veh_mi*light_vmt_affected + 
                                 Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]]$NOx_g_per_veh_mi * truck_vmt_affected) / 1000000,
           total_change_pm25 = (Fuel_Factors_by_supertype()[["Light Duty Vehicles"]]$PM25_tires_brakes_per_veh_mi*light_vmt_affected +
                                  Fuel_Factors_by_supertype()[["Medium/Heavy Duty Vehicles"]]$PM25_exhaust_per_veh_mi * truck_vmt_affected) / 1000000)
})

cost_effectiveness_EVSE <- reactive({
  emrate_diff <-
    emrate_evse() %>% filter(year == 2025) %>% 
    group_by(veh_supertype) %>% 
    summarize(emrate_diff_2025 = sum(emrate_Electric, na.rm = T) - sum(emrate_Conventional, na.rm = T))
  
  elasticities_by_port_type() %>%
    mutate(veh_supertype = if_else(str_detect(charge_port_detail, "General public"), "Light Duty Vehicles", "Medium/Heavy Duty Vehicles")) %>%
    left_join(select(filter(Stock_filtered(), year == 2025), veh_supertype, MT_per_vehtype), by = join_by(veh_supertype)) %>%
    left_join(emrate_diff, by = join_by(veh_supertype)) %>%
    left_join(Fuel_Factors_Weighted() %>% filter(veh_subtype == "All") %>% select(-veh_subtype) %>% rename(veh_supertype = veh_type)) %>%
    mutate(GHG = MT_per_vehtype*veh_sales_elasticity_wrt_ports*emrate_diff_2025,
           VMT = 0,
           NOX = MT_per_vehtype*veh_sales_elasticity_wrt_ports*NOx_g_per_veh_mi,
           PM25 = MT_per_vehtype*veh_sales_elasticity_wrt_ports*PM25_exhaust_per_veh_mi,
           ACTIVE = 0)
})