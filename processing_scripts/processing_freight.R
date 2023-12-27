### LOAD -----------------------------------------------------------------------
# library(glue)
# library(readxl)
# library(tidyverse)

### READ -----------------------------------------------------------------------


## Freight Emissions Rate
# emrate_freight <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "Freight", range = "B4:E4",
#              col_names = c("vehicle_category", "2025", "2030", "2050")) %>%
#   pivot_longer(cols = !(vehicle_category), names_to = "year", values_to = "emrate_mdhd")
# 
# capital_inputs_freight <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "Capital Project Inputs", range = "H111:J111",
#              col_names = c("2025", "2030", "2050")) %>%
#   pivot_longer(cols = everything(), names_to = "year", values_to = "intermodal_investment")
# 
# freight_emission_rate <-
#   read_excel("TEACART_v1.8_Local_shiny.xlsx",
#              sheet = "Freight", range = "C7:E7",
#              col_names = c("2025", "2030", "2050")) %>%
#   pivot_longer(cols = everything(), names_to = "year", values_to = "emrate_rail")
# 
# intermodal_investment_factor_truck <- -72103.0
# intermodal_investment_factor_rail <- 1021459
# 
# fuel_factor_mdhd_weighted <- c("nox" = 2.748, "pm25_exhaust" = 0.062, "pm25_tiresBrakes" = 0.012)
# 
# ### CALCULATE ------------------------------------------------------------------
# capital_inputs_freight %>%
#   left_join(select(emrate_freight, -vehicle_category), by = join_by(year)) %>%
#   left_join(freight_emission_rate, by = join_by(year)) %>%
#   mutate(truck_vmt_affected = intermodal_investment * intermodal_investment_factor_truck,
#          rail_ton_mi_affected = intermodal_investment * intermodal_investment_factor_rail,
#          displaced_truck_emissions = truck_vmt_affected * emrate_mdhd / 1000000,
#          added_rail_emissions = rail_ton_mi_affected * emrate_rail / 1000000,
#          total_change_MTCO2 = displaced_truck_emissions + added_rail_emissions,
#          total_change_direct = total_change_MTCO2,
#          total_change_electricity = 0,
#          total_change_mtnox = truck_vmt_affected * fuel_factor_mdhd_weighted["nox"] / 1000000,
#          total_change_pm25 = (truck_vmt_affected * fuel_factor_mdhd_weighted["pm25_exhaust"] +
#            truck_vmt_affected * fuel_factor_mdhd_weighted["pm25_tiresBrakes"]) / 1000000)

### REACTIVE -----
observeEvent(EmRate_by_Tech(), {
  #browser()
EmRate_by_Tech() %>% filter(veh_type %in% c("Medium Duty Trucks", "Heavy Duty Trucks"), str_detect(fuel_type, "ICE"))
})


