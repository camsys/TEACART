### LOAD -----------------------------------------------------------------------
library(glue)
library(readxl)
library(tidyverse)

### READ -----------------------------------------------------------------------

## strategy OPS - for this strategy it made more sense to have it in table format, see below
# Strategy_Parameters <-
#   read_csv('Data Extracts/Strategy_Parameters.csv') %>% 
#   select(-custom, -strategy) %>% ### strategy is not needed to uniquely identify parameter
#   reshape2::melt() %>%
#   reshape2::acast(list("subcat", "parameters"))

strategy_params <- 
  read_csv('Data Extracts/Strategy_Parameters.csv') %>%
  filter(strategy == "Medium and Heavy Duty Vehicle Replacement") %>%
  select(parameters, default) %>%
  rename(vehicle_type = parameters, miles_per_vehicle_per_year = default)

capital_inputs_mdhd <-
  read_excel("TEACART_v1.8_Local_shiny.xlsx",
             sheet = "Capital Project Inputs", range = "B91:J95", 
             col_names = c("vehicle_type", "fuel_type", "a", "b", "c", "d", "2025", "2030", "2050")) %>%
  select(-(3:6)) %>%
  pivot_longer(cols = !(vehicle_type:fuel_type), names_to = "year", values_to = "total_vehicles")

fuel_factor_mdhd_weighted <- c("nox" = 2.748, "pm25_exhaust" = 0.062, "pm25_tiresBrakes" = 0.012)

### MDHD Emissions Rate
emrate_mdhd <-
  read_excel("TEACART_v1.8_Local_shiny.xlsx",
             sheet = "MDHD", range = "B7:E15", 
             col_names = c("vehicle_fuel_type", "2025", "2030", "2050")) %>%
  separate("vehicle_fuel_type", into = c("vehicle_type", "fuel_type"), sep = ": ") %>%
  pivot_longer(cols = !(vehicle_type:fuel_type), names_to = "year", values_to = "em_rate")

### CALCULATE ------------------------------------------------------------------

output <-
  capital_inputs_mdhd %>%
  left_join(emrate_mdhd, by = join_by(vehicle_type, fuel_type, year)) %>%
  left_join(strategy_params, by = join_by(vehicle_type)) %>%
  left_join(select(filter(emrate_mdhd, fuel_type == "Electric"), -fuel_type), 
            by = join_by(vehicle_type, year), suffix = c("", "_electric"), keep = F) %>%
  mutate(
    affected_annual_VRM = total_vehicles * miles_per_vehicle_per_year,
    MTCO2_change = -affected_annual_VRM * em_rate / 1000000,
    added_electricity_emissions = affected_annual_VRM * em_rate_electric / 1000000
    )

output_summarized <-
  output %>%
  group_by(year) %>%
  summarize(total_change_MTCO2 = sum(MTCO2_change) + sum(added_electricity_emissions),
            total_change_direct = sum(MTCO2_change),
            total_change_electricity = sum(added_electricity_emissions),
            total_affected_annual_VRM = sum(affected_annual_VRM)) %>%
  mutate(total_change_mtnox = total_affected_annual_VRM * fuel_factor_mdhd_weighted["nox"] / 1000000,
         total_change_pm25 = total_affected_annual_VRM * fuel_factor_mdhd_weighted["pm25_exhaust"] / 1000000)