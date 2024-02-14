# 
# observe(
#  { browser()}
# )
#assump_sum <- reactive({

add_columns_if_not_exist <- function(df, column_names) {
  for (col_name in column_names) {
    if (!(col_name %in% names(df))) {
      df[[col_name]] <- 0
    }
  }
  return(df)
}

filter_columns <- function(df, selected_columns, strategy_name) {
  # Ensure that selected_columns are present in the dataframe
  
  df <- add_columns_if_not_exist(df,selected_columns)
  selected_columns <- intersect(selected_columns, names(df))
  
  # Filter the dataframe to include only selected columns
  result_df <- df[, selected_columns, drop = FALSE] %>%

    na.omit() %>% mutate(Strategy = strategy_name) %>%
    group_by(year, Strategy) %>%
    summarise(
      total_newtrips = sum(total_newtrips),
      total_change_mtnox = sum(total_change_mtnox),
      total_change_pm25 = sum(total_change_pm25),
      total_change_VMT = sum(total_change_VMT),
      total_change_MTCO2 = sum(total_change_MTCO2)
    )
}




#observe({
scenario_sum <- reactive({
#browser()

selected_columns <- c("year", "total_newtrips",'total_change_mtnox','total_change_pm25','total_change_VMT','total_change_MTCO2')

bikeped <- filter_columns(output_bikped(),selected_columns,"Bicycle and Pedestrian")
MDHD <- filter_columns(output_MDHD(),selected_columns,"MD/HD Truck Replacement")  
Micro <- filter_columns(output_micro(),selected_columns,"Micromobility")  
pnr <- filter_columns(output_pnr(),selected_columns,"Park and Ride")  
RoadwayExp <- filter_columns(output_RoadwayExp(),selected_columns,"Roadway Expansion")  
TDM <- filter_columns(output_TDM(),selected_columns,"Travel Demand Management")  
transitElec <- filter_columns(output_transitElec(),selected_columns,"Transit Electrification")  
TransitService <- filter_columns(output_TransitService(),selected_columns,"Transit Service Expansion")  
OPS <- filter_columns(output_OPS(),selected_columns,"Traffic Operations")  
EVSE <- filter_columns(output_EVSE(),selected_columns,"Electric Vehicle Charging Infraucture")  
freight <- filter_columns(output_freight(),selected_columns,"Intermodal Freight Investment")  

all_assump <- rbind(bikeped,MDHD,Micro,pnr,RoadwayExp,TDM,transitElec,TransitService,OPS,EVSE,freight)

return(all_assump)
})
# 
# observe({
#   req('')
# temp_sc1 <- data.frame()
# temp_sc2 <- data.frame()
# 
# if(rs[1,2]){
#   temp_sc1 <- filter_columns(output_bikped(),selected_columns,"Bicycle and Pedestrian") %>% rbind(temp_sc1)
# } 
# if(rs[1,3]){
#   temp_sc2 <- filter_columns(output_bikped(),selected_columns,"Bicycle and Pedestrian") %>% rbind(temp_sc2)
# } 
# 
# if(rs[2,2]){
#   temp_sc1 <- filter_columns(output_TransitService(),selected_columns,"Transit Service Expansion")  %>% rbind(temp_sc1)
# }
# if(rs[2,3]){
#   temp_sc2 <- filter_columns(output_TransitService(),selected_columns,"Transit Service Expansion")  %>% rbind(temp_sc2)
# }
# 
# if(rs[3,2]){
#   temp_sc1 <- filter_columns(output_micro(),selected_columns,"Micromobility") %>% rbind(temp_sc1)
# }
# if(rs[3,3]){
#   temp_sc2 <- filter_columns(output_micro(),selected_columns,"Micromobility") %>% rbind(temp_sc2)
# }
# 
# if(rs[4,2]){
#   temp_sc1 <- filter_columns(output_TDM(),selected_columns,"TDM")   %>% rbind(temp_sc1)
# }
# if(rs[4,3]){
#   temp_sc2 <- filter_columns(output_TDM(),selected_columns,"TDM")   %>% rbind(temp_sc2)
# }
#  
# if(rs[5,2]){
#   temp_sc1 <- filter_columns(output_pnr(),selected_columns,"Park and Ride") %>% rbind(temp_sc1)
# }
# if(rs[5,3]){
#   temp_sc2 <- filter_columns(output_pnr(),selected_columns,"Park and Ride") %>% rbind(temp_sc2)
# }
# 
# if(rs[6,2]){
#   temp_sc1 <- filter_columns(output_transitElec(),selected_columns,"Transit Electrification") %>% rbind(temp_sc1)
# }
# if(rs[6,3]){
#   temp_sc2 <- filter_columns(output_transitElec(),selected_columns,"Transit Electrification") %>% rbind(temp_sc2)
# }
# 
# if(rs[7,2]){
#   temp_sc1 <- filter_columns(output_MDHD(),selected_columns,"MD/HD Truck Replacement") %>% rbind(temp_sc1)
# }
# if(rs[7,3]){
#   temp_sc2 <- filter_columns(output_MDHD(),selected_columns,"MD/HD Truck Replacement") %>% rbind(temp_sc2)
# }
# 
# if(rs[8,2]){
#   temp_sc1 <- filter_columns(output_EVSE(),selected_columns,"EV Charging Infrastructure") %>% rbind(temp_sc1)
# }
# if(rs[8,3]){
#   temp_sc2 <- filter_columns(output_EVSE(),selected_columns,"EV Charging Infrastructure") %>% rbind(temp_sc2)
# }
# 
# if(rs[9,2]){
#   temp_sc1 <- filter_columns(output_freight(),selected_columns,"Intermodal Freight Investment") %>% rbind(temp_sc1)
# }
# if(rs[9,3]){
#   temp_sc2 <- filter_columns(output_freight(),selected_columns,"Intermodal Freight Investment") %>% rbind(temp_sc2)
# }
# 
# if(rs[10,2]){
#   temp_sc1 <- filter_columns(output_OPS(),selected_columns,"Traffic Operations") %>% rbind(temp_sc1)
# }
# if(rs[10,3]){
#   temp_sc2 <- filter_columns(output_OPS(),selected_columns,"Traffic Operations") %>% rbind(temp_sc2)
# }
# 
# if(rs[11,2]){
#   temp_sc1 <- filter_columns(output_RoadwayExp(),selected_columns,"Roadway Expansion") %>% rbind(temp_sc1)
# }
# if(rs[11,3]){
#   temp_sc2 <- filter_columns(output_RoadwayExp(),selected_columns,"Roadway Expansion") %>% rbind(temp_sc2)
# }
# 
# if(rs[12,2]){
#   #CUSTOM PROJECTS
#   #output_micro() %>% 
#   #  select(c('year',"total_change_VMT","total_change_MTCO2","total_newtrips","total_change_mtnox","total_change_pm25")) %>%
#   #  mutate(process = "Micromobility")
# }
# if(rs[12,3]){
#   #CUSTOM PROJECTS
#   #output_micro() %>% 
#   #  select(c('year',"total_change_VMT","total_change_MTCO2","total_newtrips","total_change_mtnox","total_change_pm25")) %>%
#   #  mutate(process = "Micromobility")
# }
# 
# dt <- baseline_ghg_forecast()
# ft <- VMT_Forecast()
# 
# dt_emissions_base <- dt %>% ungroup() %>%# select(-veh_supertype) %>%
#   #filter(veh_supertype %in% c("Light Duty Vehicles","Medium/Heavy Duty Trucks")) %>%
#   summarise(across(where(is.numeric),sum))
# dt_VMT_base <- ft %>% ungroup() %>% filter(year >=2021) %>%
#   filter(year %in% c(rvs$Baseline$base_year, 
#                      rvs$Baseline$horizon_year_1,
#                      rvs$Baseline$horizon_year_2,
#                      rvs$Baseline$horizon_year_3)) %>%
#   group_by(year) %>%
#   summarise(total_VMT = sum(state_vmt_AEO,na.rm = T))  %>%
#   pivot_wider(names_from= year, values_from = total_VMT)
# })
