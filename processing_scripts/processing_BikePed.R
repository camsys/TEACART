library(dplyr)
library(tidyverse)

## strategy 1: bicycle and pedestrian: 
Strategy_Parameters <- read.csv('./Data Extracts/Strategy_Parameters.csv')
NTD_Service <- read.csv('./Data Extracts//NTD_Service.csv')
HPMS_Data <- read.csv('./Data Extracts//HPMS_Data.csv')
captial_project_input_bkped <- read.csv('./Data Extracts//Capital_Project_Inputs_Bike&Ped.csv')
Bicycle_and_Pedestrian <- read.csv('./Data Extracts//Bicycle_and_Pedestrian.csv')  #use as the base for Bicycle and pedestrian strategy


## need input from EmRate_by_Tech, 
year <- c('2025','2030','2050')
emrate <-c(310.271806608129,288.308700192269,256.867458798161)
emrate <- data.frame(year,emrate)

emrate_by_tech <-c(0.948769678674915,0.88160943732177,0.785466327121931)
emrate_by_tech <- data.frame(year,emrate_by_tech)

fuel_factorNox <- 0.234684146669504
fuel_factorPMe <- 0.00510190278963835
fuel_factorPMtb <- 0.004

# preprocess the strategy parameters
Strategy_Parameters <- Strategy_Parameters %>%
  mutate(selected_value = ifelse(!is.na(custom), custom,default))

# calculate the Displaced Auto mile/yr
Bicycle_and_Pedestrian_base <- Bicycle_and_Pedestrian %>% 
  mutate(annual_displaced_auto_miles = case_when(grepl("core urban",per_new_facility_mile, ignore.case = TRUE) ~ -(daily_new_bicyclists*
                                                                                                                     Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Core (>10,000 ppsm)']*
                                                                                                                     Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike'] +
                                                                                                                     daily_new_walkers * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Walk']*
                                                                                                                     Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Core (>10,000 ppsm)'])*
                                                   Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'Bicycle and Pedestrian Data'],
                                                 grepl("- urban",per_new_facility_mile, ignore.case = TRUE) ~  -(daily_new_bicyclists*
                                                                                                                   Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Urban (4,000 - 10,000 ppsm)']*
                                                                                                                   Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike'] +
                                                                                                                   daily_new_walkers * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Walk']*
                                                                                                                   Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Urban (4,000 - 10,000 ppsm)'])*
                                                   Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'Bicycle and Pedestrian Data'],
                                                 grepl("suburban",per_new_facility_mile, ignore.case = TRUE) ~  -(daily_new_bicyclists*
                                                                                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Suburban (500 - 4,000 ppsm)']*
                                                                                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike'] +
                                                                                                                    daily_new_walkers * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Walk']*
                                                                                                                    Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Suburban (500 - 4,000 ppsm)'])*
                                                   Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'Bicycle and Pedestrian Data'],
                                                 grepl("rural",per_new_facility_mile, ignore.case = TRUE) ~  -(daily_new_bicyclists*
                                                                                                                 Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Rural (<500 ppsm)']*
                                                                                                                 Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Bike'] +
                                                                                                                 daily_new_walkers * Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Walk']*
                                                                                                                 Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Rural (<500 ppsm)'])*
                                                   Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'Bicycle and Pedestrian Data']
  )) 
## process the captial project Inputs for bicycle and Pedestrian
captial_project_input_bkped <- captial_project_input_bkped %>%
  mutate(facilitiesmile_2050 = facilitiesmile_2025 + facilitiesmile_2030 + facilitiesmile_2050,
         facilitiesmile_2030 = facilitiesmile_2025 + facilitiesmile_2030) %>%
  pivot_longer(cols = !(facility_type:area_type), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'facilitiesmile') %>%
  left_join(.,emrate_by_tech, by = 'year') %>%
  left_join(.,emrate, by = 'year')

Bicycle_and_Pedestrian_base <- Bicycle_and_Pedestrian_base %>% merge(.,captial_project_input_bkped, by.x = 'per_new_facility_mile', by.y = "facility_type", all = TRUE) %>% 
  mutate(total_vmt_change = annual_displaced_auto_miles * facilitiesmile,
         total_CO2_change = total_vmt_change * emrate/1000000,
         total_newtrips = facilitiesmile * (daily_new_bicyclists + daily_new_walkers),
         total_NOx_change = total_vmt_change * fuel_factorNox *emrate_by_tech/1000000,
         total_PM25_change = (total_vmt_change*fuel_factorPMe * emrate_by_tech + total_vmt_change * fuel_factorPMtb)/1000000
  ) # end of bike and ped strategy

