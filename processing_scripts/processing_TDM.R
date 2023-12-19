library(dplyr)
library(tidyverse)




# strategy 4: TDM
## values from other tabs:
year <- c('2025','2030','2050')
emrate <-c(310.271806608129,288.308700192269,256.867458798161)
emrate <- data.frame(year,emrate)

ldv_weightedNOX <- 0.234684146669504 #C$36
ldv_weightedPM25 <- 0.005101903 #D$36
ldv_weightedPM25TB <- 0.004  #E$36


emrate_by_tech <-c(0.948769678674915,0.88160943732177,0.785466327121931)
emrate_by_tech <- data.frame(year,emrate_by_tech)


##
Capital_Project_Inputs_TDM <- read.csv('./Data Extracts/Capital_Project_Inputs_TDM.csv') %>%
  mutate(unit_2050 = unit_2025  + unit_2030  + unit_2050,
         unit_2030  = unit_2025  + unit_2030 ) %>%  # calculate cumulative value
  pivot_longer(cols = !(Program.Type:Unit), names_to = c("year"),names_pattern = "_(\\d+)", values_to = 'unit') %>%
  left_join(.,emrate, by = 'year') %>%
  left_join(.,emrate_by_tech, by = 'year')


Strategy_Parameters <- read.csv('./Data Extracts/Strategy_Parameters.csv') %>%
  mutate(selected_value = ifelse(!is.na(custom), custom,default))


TDM_base <- Capital_Project_Inputs_TDM %>%
  mutate(wrkVMT_change = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Average reduction in drive-alone (%)'],
         wrktrip_len = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Average work trip length (auto)'],
         wrk_annualization = Strategy_Parameters$selected_value[Strategy_Parameters$parameters == 'Annualization factor' & Strategy_Parameters$strategy == 'TDM Data'])%>%
  mutate(total_vmt_change = -unit * wrkVMT_change * wrktrip_len * wrk_annualization*2,
         total_CO2_change = total_vmt_change * emrate/1000000,
         total_NOx_change = total_vmt_change* emrate_by_tech *ldv_weightedNOX/1000000,
         total_PM25_change = (total_vmt_change * ldv_weightedPM25 * emrate_by_tech + total_vmt_change * ldv_weightedPM25TB)/1000000 )  # end of TDM strategy

