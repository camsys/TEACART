library(dplyr)
library(tidyverse)


## strategy 3: Micro
Capital_Project_Inputs_Micro <- read.csv('./Data Extracts/Capital_Project_Inputs_Micro.csv') %>%
  mutate(unit_2050 = unit_2025  + unit_2030  + unit_2050,
         unit_2030  = unit_2025  + unit_2030 ) %>%  # calculate cumulative value
  pivot_longer(cols = !(Program.Type:Unit), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'unit') %>%
  left_join(.,emrate, by = 'year') %>%
  left_join(.,emrate_by_tech, by = 'year')

## load possible tables: 
Strategy_Parameters <- read.csv('./Data Extracts/Strategy_Parameters.csv') %>%
  mutate(selected_value = ifelse(!is.na(custom), custom,default))

Micro_base <- Capital_Project_Inputs_Micro %>%
  mutate(Ebike_cost = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'E-bike cost'],
         subsidy_cover = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Subsidy coverage (%)'],
         weekly_trip = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike trips per week'],
         trip_len = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Average trip length'],
         auto_share = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Prior auto mode share'])%>%
  mutate(total_vmt_change = unit * weekly_trip * -trip_len * auto_share*52,
         total_CO2_change = total_vmt_change * emrate/1000000,
         total_NOx_change = total_vmt_change* emrate_by_tech *ldv_weightedNOX/1000000,
         total_PM25_change = (total_vmt_change * ldv_weightedPM25 * emrate_by_tech + total_vmt_change * ldv_weightedPM25TB)/1000000,
         total_newtrips = weekly_trip  / 7 * unit
  )  # end of Micro strategy
