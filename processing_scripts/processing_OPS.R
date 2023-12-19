### LOAD -----------------------------------------------------------------------
library(glue)
library(readxl)
library(tidyverse)

### READ -----------------------------------------------------------------------

## strategy OPS
Strategy_Parameters <-
  read_csv('Data Extracts/Strategy_Parameters.csv')  |>  
  select(-custom, -strategy) |>  ### strategy is not needed to uniquely identify parameter
  reshape2::melt() |> 
  reshape2::acast(list("subcat", "parameters"))

capital_inputs_ops <-
  read_excel("TEACART_v1.8_Local_shiny.xlsx",
             sheet = "Capital Project Inputs", range = "B84:J87", 
             col_names = c("improvement_type", "facility_area_type", "a", "b", "c", "d", "2025", "2030", "2050")) |> 
  select(-(3:6)) |> 
  pivot_longer(cols = !(improvement_type:facility_area_type), names_to = "year", values_to = "total_signals")

corridor_length <- 1
signals_per_mile <- 2
corridor_travel_time_change <- -0.12
fuel_factor_gasoline <- 7.94
fuel_factor_diesel <- 9.4
fuel_factor_ldv_weighted <- c("nox" = 0.235,
                              "pm25_exhaust" = 0.005,
                              "pm25_tiresBrakes" = 0.004)
roundabout_effect <- -0.0665628668793441
emrate_mediumHeavy_2025 <- 982.773
emrate_mediumHeavy_2030 <- 885.522
emrate_mediumHeavy_2050 <- 738.655
emrate_light <- c("2025" = 311.451,
                  "2030" = 289.950,
                  "2050" = 256.867)
emrate_mediumHeavy <- c("2025" = 982.773,
                        "2030" = 885.522,
                        "2050" = 738.655)
emrate_improvement_factor_light <- c("2025" = 0.9418909,
                                     "2030" = 0.8768672,
                                     "2050" = 0.7768180)
emrate_improvement_factor_mediumHeavy <- c("2025" = 0.9392860,
                                           "2030" = 0.8463381,
                                           "2050" = 0.7059704)
emrate_improvement_factor_ldv <- c("2025" = 0.950296506,
                                   "2030" = 0.884692579,
                                   "2050" = 0.783750527)
emrate_nox_ratio <- 0.000710
emrate_pm25_ratio <- 0.000015424
electricity_pct_of_emissions <- c("2025" = 0.008212732,
                                  "2030" = 0.012639971,
                                  "2050" = 0.006652690)

### REFERENCE EXCEL ------------------------------------------------------------
# H8 = fuel_factor_gasoline
# H9 = fuel_factor_diesel
# C36 = cuml_2025
# C47 = Strategy_Parameters["Average AADT", "Principal Arterial - Urban"]+
#       (Strategy_Parameters["Average AADT", "Principal Arterial - Urban"]*
#       corridor_travel_time_change*Strategy_Parameters["Induced Travel Elasticities", "VMT w/r/t travel time (urban)"])
# C38 = corridor_length
# C39 = signals_per_mile
# C14 = C43 = Strategy_Parameters["Average AADT", "Principal Arterial - Urban"]
# c46 = C28 = Strategy_Parameters["Induced Travel Elasticities", "VMT w/r/t travel time (urban)"]
# C49 = C36*(C47-C43)*(C38/C39)*365
# C45 = C43 * C44
# C44 = -(C38/C40) + (C38/C42)
# C40 = C18 = Strategy_Parameters["Travel Speed (mph)", "Principal Arterial - Rural"]
# C42 = C18/(1+C41)
# C41 = corridor_travel_time_change
# C11 = C9*C23+C8*(1-C23)
# C9 = H9 * emrate_improvement_factor_mediumHeavy_2025
# C23 = Strategy_Parameters["Percent Truck Traffic (%)", "Arterial"]
# C8 = H8 * emrate_improvement_factor_light_2025
# C103 = total_change_vmt

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