

observe({
#scenario_sum <- reactive({
browser()

output_bikped() # ok 
output_freight()
output_MDHD() #ok
output_micro() #ok
output_pnr() #ok
output_RoadwayExp()  #ok
output_TDM() #ok
output_transitElec() #ok
output_TransitService() #ok
output_ops()

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





selected_columns <- c("year", "total_newtrips",'total_change_mtnox','total_change_pm25','total_change_VMT','total_change_MTCO2')

bikeped <- filter_columns(output_bikped(),selected_columns,"Bicycle and Pedestrian")
MDHD <- filter_columns(output_MDHD(),selected_columns,"MD/HD Truck Replacement")  
Micro <- filter_columns(output_micro(),selected_columns,"Micromobility")  
pnr <- filter_columns(output_pnr(),selected_columns,"Park and Ride")  
RoadwayExp <- filter_columns(output_RoadwayExp(),selected_columns,"Roadway Expansion")  
TDM <- filter_columns(output_TDM(),selected_columns,"TDM")  
transitElec <- filter_columns(output_transitElec(),selected_columns,"Transit Electrification")  
TransitService <- filter_columns(output_TransitService(),selected_columns,"Transit Service Expansion")  


})

# df_list <- c(output_bikped(),
#              output_EVSE(),
#              #output_freight(),
#              output_MDHD(),
#              output_micro(),
#              output_pnr(),
#              output_RoadwayExp(),
#              output_TDM(),
#              output_transitElec(),
#              output_TransitService()#,
#              #output_OPS
#              )  
#' assumption_values <- list("Bicycle and Pedestrian",
#'                           "EVSE",
#'                           #'Intermodal Freight Investment',
#'                           "Micromobility",
#'                           "MD/HD Truck Replacement",
#'                           "Micromobility",
#'                           "Park and Ride",
#'                           "Roadway Expansion", 
#'                           "Transit Electrification",
#'                           "Transit Service Expansion" #,
#'                           #"Traffic Operations"
#'                           )
#' 
#' combined_df <- combine_all(df_list, selected_columns, assumption_values)

