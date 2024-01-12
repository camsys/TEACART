cost_function <- function(ini_cost_table, output_table, cols, proj_life, var1_scalar = NULL, var2_scalar = NULL){
  
}


observeEvent(input$state_input, {
  browser()})


  # #fake inputs
  # tab_no = 12
  # proj_life = 30
  # cols <- c('road_class','area_type')
  # output_table <- cost_output_RoadwayExp()
  # ini_cost_table <- rvs$Costs[rvs$Costs$table_no_ui == tab_no,]

cost_function <- function(ini_cost_table, output_table, cols, proj_life, var1_scalar = NULL, var2_scalar = NULL){
  
  #real input
  cols <- c(cols, 'cost_type')
  
  if('var1' %in% unique(ini_cost_table$cost_type)){
    
    cost_table = ini_cost_table %>% 
      group_by_at(cols) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(names_from = cost_type, values_from = value) %>%
      mutate(var = var1*var1_scalar + var2*var2_scalar) %>%
      select(-c(var1,var2)) %>%
      mutate(annual_cost = cap/proj_life + var)
    
  } else {
    
    cost_table = ini_cost_table %>% 
      group_by_at(cols) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(names_from = cost_type, values_from = value) %>%
      mutate(annual_cost = cap/proj_life + var)
      
  }
  
  sum_fun <- function(x,meas,val,high,med,low,prefix){
    me <- x[meas] %>% as.numeric()
    va <- x[val] %>% as.numeric()
    
    if(me==0){
      return(NA)
    } else if(-1*me > 0 & va < 0){
      return('***')
    } else if(-1*me < 0){
      return(paste0(prefix, " Increase"))
    } else if(-1*me/va > high){
      return("Very High")
    } else if(-1*me/va > med){
      return("High")
    } else if(-1*me/va > low){
      return("Medium")
    } else if(-1*me/va <= low){
      return("Low")
    } else {return("Seth you fool you idiot")}
    
  }

  temp_table <- left_join(output_table, cost_table) %>%
    mutate(MTCO2_per_1m = ifelse(total_change_MTCO2 == 0, NA, -1*total_change_MTCO2/annual_cost),
           VMT_per_1m = ifelse(total_change_VMT  == 0, NA, -1*total_change_VMT/(annual_cost/1000000)),
           nox_per_1m = ifelse(total_change_mtnox  == 0, NA, -1*total_change_mtnox/(annual_cost)),
           pm25_per_1m = ifelse(total_change_pm25  == 0, NA, -1*total_change_pm25/(annual_cost)),
           newtrips_per_1m = ifelse(total_change_newtrips == 0, NA, -1*total_change_newtrips/(annual_cost/1000000)))
  
  temp_table$MTCO2_sum <- apply(temp_table, 1, sum_fun,
                                meas = "total_change_MTCO2",
                                val = "annual_cost",
                                high = 2000,
                                med = 1000,
                                low = 500,
                                prefix = "CO2")
  temp_table$VMT_sum <- apply(temp_table, 1, sum_fun,
                              meas = "total_change_VMT",
                              val = "annual_cost",
                              high = 10000000,
                              med = 5000000,
                              low = 1000000,
                              prefix = "VMT")
  temp_table$nox_sum <- apply(temp_table, 1, sum_fun,
                              meas = "total_change_mtnox",
                              val = "annual_cost",
                              high = 10,
                              med = 5,
                              low = 1,
                              prefix = "NOx")
  temp_table$pm25_sum <- apply(temp_table, 1, sum_fun,
                               meas = "total_change_pm25",
                               val = "annual_cost",
                               high = .1,
                               med = 0.05,
                               low = 0.01,
                               prefix = "PM25")
  temp_table$newtrips_sum <- apply(temp_table, 1, sum_fun,
                                   meas = "total_change_newtrips",
                                   val = "annual_cost",
                                   high = 10000,
                                   med = 5000,
                                   low = 1000,
                                   prefix = "Daily Trips")


  
  
}