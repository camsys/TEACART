library(dplyr)
library(tidyverse)



# 
# # strategy 4: TDM
# ## values from other tabs:
# year <- c('2025','2030','2050')
# emrate <-c(310.271806608129,288.308700192269,256.867458798161)
# emrate <- data.frame(year,emrate)
# 
# ldv_weightedNOX <- 0.234684146669504 #C$36
# ldv_weightedPM25 <- 0.005101903 #D$36
# ldv_weightedPM25TB <- 0.004  #E$36
# 
# 
# emrate_by_tech <-c(0.948769678674915,0.88160943732177,0.785466327121931)
# emrate_by_tech <- data.frame(year,emrate_by_tech)
# 
# 
# ##
# Capital_Project_Inputs_TDM <- read.csv('./Data Extracts/Capital_Project_Inputs_TDM.csv') %>%
#   mutate(unit_2050 = unit_2025  + unit_2030  + unit_2050,
#          unit_2030  = unit_2025  + unit_2030 ) %>%  # calculate cumulative value
#   pivot_longer(cols = !(Program.Type:Unit), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'unit') %>%
#   left_join(.,emrate, by = 'year') %>%
#   left_join(.,emrate_by_tech, by = 'year')
# 
# 
# Strategy_Parameters <- read.csv('./Data Extracts/Strategy_Parameters.csv') %>%
#   mutate(selected_value = ifelse(!is.na(custom), custom,default))
# 
# 
# TDM_base <- Capital_Project_Inputs_TDM %>%
#   mutate(wrkVMT_change = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Average reduction in drive-alone (%)'],
#          wrktrip_len = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Average work trip length (auto)'],
#          wrk_annualization = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'TDM Data'])%>%
#   mutate(total_vmt_change = -unit * wrkVMT_change * wrktrip_len * wrk_annualization*2,
#          total_CO2_change = total_vmt_change * emrate/1000000,
#          total_NOx_change = total_vmt_change* emrate_by_tech *ldv_weightedNOX/1000000,
#          total_PM25_change = (total_vmt_change * ldv_weightedPM25 * emrate_by_tech + total_vmt_change * ldv_weightedPM25TB)/1000000 )  # end of TDM strategy
# 

 output_TDM <- reactive({
  #observeEvent(input$state_input,{

  browser()
  req(rvs)
  req(emrate_by_tech_ldv())
  req(Fuel_Factors_Weighted())
  
  emrate_by_tech_ldv <- CO2e_Category_Averages() %>% filter(veh_supertype == 'Light Duty Vehicles')
  
  Capital_Project_Inputs_TDM <- rvs$Projects[rvs$Projects$table_no_ui == 7,] %>% #table 7 is tdm in Projects
    mutate(year = case_when(year == "horizon_year_1" ~ rvs$Baseline$horizon_year_1,
                            year == "horizon_year_2" ~ rvs$Baseline$horizon_year_2,
                            year == "horizon_year_3" ~ rvs$Baseline$horizon_year_3)) %>%
    arrange(year) %>%
    mutate(
      value = case_when(
        year > rvs$Baseline$horizon_year_1 ~ cumsum(value),
        TRUE ~ value)) %>%
    left_join(select(emrate_by_tech_ldv,year,CO2e_millions,base_impf), by = 'year') 
  
  Assumptions_tdm <- rvs$Assumptions[rvs$Assumptions$table_no_ui == 3,]  # table 3 is TDM in Assumptions
  
  # get values from Fuel_Factors_Weighted()
  fuel_factorNox <- Fuel_Factors_Weighted()$NOx_g_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMe <- Fuel_Factors_Weighted()$PM25_exhaust_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  fuel_factorPMtb <- Fuel_Factors_Weighted()$PM25_tires_brakes_per_veh_mi[Fuel_Factors_Weighted()$veh_type == 'Light Duty Vehicles'& Fuel_Factors_Weighted()$veh_subtype == 'All']
  
  
  tdm_output <- Capital_Project_Inputs_TDM %>%
    mutate(wrkVMT_change = Assumptions_tdm$value[Assumptions_tdm$unit == 'avg_reduction_in_drive_along_pct'],
           wrktrip_len = Assumptions_tdm$value[Assumptions_tdm$unit == 'avg_work_trip_miles'],
           wrk_annualization = Assumptions_tdm$value[Assumptions_tdm$unit == 'annualization_factor'])%>%
    mutate(total_change_VMT = -value * wrkVMT_change * wrktrip_len * wrk_annualization*2,
           total_change_MTCO2 = total_change_VMT * CO2e_millions/1000000,
           total_change_mtnox = total_change_VMT* base_impf *fuel_factorNox/1000000,
           total_change_pm25 = (total_change_VMT * fuel_factorPMe * base_impf + total_change_VMT * fuel_factorPMtb)/1000000 )  # end of TDM strategy
  
  return(tdm_output)
})
