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
    mutate(Strategy = strategy_name) %>%
    group_by(year, Strategy) %>%
    summarise(
      total_newtrips = sum(total_newtrips, na.rm = T),
      total_change_mtnox = sum(total_change_mtnox, na.rm = T),
      total_change_pm25 = sum(total_change_pm25, na.rm = T),
      total_change_VMT = sum(total_change_VMT, na.rm = T),
      total_change_MTCO2 = sum(total_change_MTCO2, na.rm = T)
    ) %>% na.omit()
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

