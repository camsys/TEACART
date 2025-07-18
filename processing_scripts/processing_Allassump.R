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
      total_change_newtrips = sum(total_change_newtrips, na.rm = T),
      total_change_mtnox = sum(total_change_mtnox, na.rm = T),
      total_change_pm25 = sum(total_change_pm25, na.rm = T),
      total_change_VMT = sum(total_change_VMT, na.rm = T),
      total_change_MTCO2 = sum(total_change_MTCO2, na.rm = T)
    ) %>% na.omit()
}

scenario_sum <- reactive({
#browser()
selected_columns <- c("year", "total_change_newtrips",'total_change_mtnox','total_change_pm25','total_change_VMT','total_change_MTCO2')

bikeped <- filter_columns(output_bikped(),selected_columns,"Bicycle and Pedestrian")
MDHD <- filter_columns(output_MDHD(),selected_columns,"MD/HD Truck Replacement")  
Micro <- filter_columns(output_micro(),selected_columns,"Micromobility")  
pnr <- filter_columns(output_pnr(),selected_columns,"Park-and-Ride")  
RoadwayExp <- filter_columns(output_RoadwayExp(),selected_columns,"Roadway Expansion")  %>% mutate(year = as.numeric(year))
TDM <- filter_columns(output_TDM(),selected_columns,"Travel Demand Management") 
transitElec <- filter_columns(output_transitElec(),selected_columns,"Transit Electrification")  
TransitService <- filter_columns(output_TransitService(),selected_columns,"Transit Service Expansion")  
OPS <- filter_columns(output_OPS(),selected_columns,"Traffic Operations")  
EVSE <- filter_columns(output_EVSE(),selected_columns,"Electric Vehicle Charging Infraucture")  
freight <- filter_columns(output_freight(),selected_columns,"Intermodal Freight Investment")  
#browser()
transit_cuts <- filter_columns(output_transitservice_cuts(),selected_columns,"Transit Service Cuts")
land_use <-  filter_columns(output_land_use(),selected_columns,"Land Use")
roadway_resurf <-  filter_columns(output_roadway_resurf(),selected_columns,"Roadway Resurfacing")

all_assump <- rbind(bikeped,MDHD,Micro,pnr,RoadwayExp,TDM,transitElec,TransitService,OPS,EVSE,freight,transit_cuts,land_use,roadway_resurf)

return(all_assump)
})

#########################################################################################################
all_costs <- reactive({
 bikeped <- cost_function(
   ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==1,],
   output_table = cost_output_bikeped(),
   col_sel = c('area_type','cap_proj_type'),
   proj_life = 30,
   #val1_scalar = ,
   #val2_scalar = ,
   style = 'summary'
 ) 

 
 transit_fixed <- cost_function(
   ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==2,],
   output_table = cost_output_transitservice() %>% filter(table == "Transit: Increased Fixed Route Service (VOMS)"),
   col_sel = c('area_type','fuel_type','transit_mode'),
   proj_life = 12,
   scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('area_type','transit_mode','value')] %>% rename("scalar_1" = "value"),
   style = 'summary'
 )
 
 transit_dr <- cost_function(
   ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==3,],
   output_table = cost_output_transitservice() %>% filter(table == "Transit: Increased Demand Response Service (VOMS)"),
   col_sel = c('area_type','fuel_type','transit_mode'),
   proj_life = 12,
   scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('area_type','transit_mode','value')] %>% rename("scalar_1" = "value"),
   style = 'summary'
   )
 
 pub_trans_bus <- cost_function(
   ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==5,], #slchanged
   output_table = cost_output_transitservice() %>% filter(table == "Bus Priority"),
   col_sel = c(),
   proj_life = 5, 
   style =  'summary'
   )
 transit_zeb <- cost_function(
   ini_cost_table =  public_elec_replacement_cost_table(), #%>% filter(table %in% c("Transit: Increased Demand Response Service (VOMS)","Transit: Increased Fixed Route Service (VOMS)")),
   output_table = cost_output_transitselect(),
   col_sel = c('area_type','fuel_type','transit_mode'),
   proj_life = 12,
   #scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('area_type','transit_mode','value')],
   style = 'summary'
   )
 
   pub_trans_rail <- cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==6,], #slchanged
     output_table = cost_output_transitservice() %>% filter(table == "Public Transportation: Rail (VOMS)"),
     col_sel = c('fuel_type','transit_mode'),
     proj_life = 30,
     #BEN: val 1 is only referencing light rail revenue miles 
     scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('transit_mode','value')]%>% rename("scalar_1" = "value"),      
     style = 'summary'
     )
   
   tdm <- cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==7,], #slchanged
     output_table = cost_output_TDM(),
     col_sel = c(),
     proj_life = 1,
     style = 'summary'
     )
   
   micro <- cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==8,], #slchanged
     output_table = cost_output_micro(),
     col_sel = c(),
     proj_life = 6,
     style = 'summary'
     )
   
   traffic_ops <-cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==9,]%>% #slchanged 
       left_join(data.frame(cap_proj_type = c("New roundabouts","New or retimed signal"),
                            proj_life = c(30,5))),
     output_table = output_cost_OPS(),
     col_sel = c('road_class','area_type','cap_proj_type'),
     proj_life = NA,#needs to project lifes actually :(
     style = 'summary'
     )
   
   mhdev<-cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==10,] %>% rename('veh_subtype' = 'fuel_type'), #slchanged
     output_table = cost_effectiveness_MDHD(),
     col_sel = c('veh_type','veh_subtype'),
     proj_life = 12,
     style = 'summary'
     )
   
   pnr<- cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==11,], #slchanged
     output_table = cost_output_pnr(),
     col_sel = c(),
     proj_life = 30,
     style = 'summary'
     )
   
   evsi<-cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==12,], #slchanged
     output_table = cost_effectiveness_EVSE(),
     col_sel = c('charge_port_detail'), #Change to port detail?
     proj_life = 10,
     style = 'summary'
     )
   roadway<- cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==14,], #slchanged
     output_table = cost_output_RoadwayExp(),
     col_sel = c('road_class','area_type'),
     proj_life = 30,#needs to project lifes actually :(
     style = 'summary'
     )
   intermodal<- cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==13,], #slchanged
     output_table = cost_effectiveness_freight(),
     col_sel = c(),
     proj_life = 30,
     style ='summary')
   #browser()
   # transit_cuts<- cost_function(
   #   ini_cost_table =NA,
   #   output_table = cost_output_transitservice_cuts(),
   #   col_sel = c(),
   #   proj_life = 12,
   #   style ='summary')

   roadway_resurf<- cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==15,],
     output_table = cost_output_roadway_resurf(),
     col_sel = c(),
     proj_life = NA,
     style ='summary')
   land_use<- cost_function(
     ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==16,],
     output_table = cost_output_land_use(),
     col_sel = c(),
     proj_life = NA,
     style ='summary')
   # 
   all_costs <- list(bikeped = bikeped,
                     transit_fixed = transit_fixed,
                     transit_dr = transit_dr,
                     pub_trans_bus = pub_trans_bus,
                     transit_zeb = transit_zeb,
                     pub_trans_rail=pub_trans_rail,
                     tdm=tdm,
                     micro=micro,
                     traffic_ops=traffic_ops,
                     mhdev=mhdev,
                     pnr=pnr,
                     evsi=evsi,
                     intermodal=intermodal,
                     roadway=roadway,
                     roadway_resurf = roadway_resurf,
                     land_use = land_use
                     )
   return(all_costs)
       })
