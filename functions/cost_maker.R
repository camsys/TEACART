

#You can uncomment these if you want to test the script 
# observeEvent(input$state_input, {
#   browser()})


  # #fake inputs
  # tab_no = 12
  # proj_life = 30
  # cols <- c('road_class','area_type')
  # output_table <- cost_output_RoadwayExp()
  # ini_cost_table <- rvs$Costs[rvs$Costs$table_no_ui == tab_no,]

sum_fun <- function(x,meas,val,high,med,low,prefix){
 #browser()
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

cost_function <- function(ini_cost_table, #this is the rvs cost table prefiltered by table number
                          output_table, #this it the processing output
                          col_sel, #these are the columns of the rvs cost table that are necessary to get the individual rows e.g. road_class, area_type, etc
                          proj_life, #not sure if we should be pulling this in through a table in Raw Data?
                          scalar_list = NULL, #this is only neccesary for transit table
                          style
                          ){
  
  #browser()
  #real input
  cols <- c(col_sel, 'cost_type') #add cost type to the columns that will be used for groupin
  
  if("facility_type" %in% names(output_table)){
    output_table <- output_table %>% rename("cap_proj_type" = "facility_type")
    type_name = "facility_type"
  }
  
  #IF Else is for indicating transit or other tables
  #This function basically calcualtes the cost parameters page by getting the annualized cost with is usually capital cost/project life + variable cost
  if('annual_cost' %in% names(ini_cost_table)){
    
    cost_table <- ini_cost_table
    
    } else if('proj_life' %in% names(ini_cost_table)){
      
      cols <- c(cols, 'proj_life')
      
      cost_table = ini_cost_table %>% 
        group_by_at(cols) %>%
        summarise(value = sum(value)) %>%
        pivot_wider(names_from = cost_type, values_from = value) %>%
        mutate(annual_cost = cap/proj_life + var) %>%
        select(-c(proj_life,cap,var))
      
    
  }else if('var1' %in% unique(ini_cost_table$cost_type)){ 
    
    cost_table = ini_cost_table %>% 
      group_by_at(cols) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(names_from = cost_type, values_from = value) %>% left_join(scalar_list) %>%
      mutate(var = var1*scalar_1 + var2*scalar_1) %>% #create variable cost based on scalars
      select(-c(var1,var2,scalar_1)) %>%
      mutate(annual_cost = cap/proj_life + var) %>%
      select(-c(cap,var))
    
  } else if('var' %in% unique(ini_cost_table$cost_type)){
    
    cost_table = ini_cost_table %>% 
      group_by_at(cols) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(names_from = cost_type, values_from = value) %>%
      mutate(annual_cost = cap/proj_life + var)
  } else {
    cost_table = ini_cost_table %>% 
      group_by_at(cols) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(names_from = cost_type, values_from = value) %>%
      mutate(annual_cost = cap/proj_life)
  }
  
  #This function is applied to each row to estimate the summary value based on supplied high, medium, low values
  #The high medium low values are the same for each project type


  #This is where the non-summary output comes from
  if(length(col_sel) == 0){
    temp_table <- cbind(output_table, cost_table) %>%
      mutate(gGHG_per_1m = ifelse(total_change_gGHG == 0, NA, -1*total_change_gGHG/annual_cost),
             VMT_per_1m = ifelse(total_change_VMT  == 0, NA, -1*total_change_VMT/(annual_cost/1000000)),
             nox_per_1m = ifelse(total_change_gnox  == 0, NA, -1*total_change_gnox/(annual_cost)),
             pm25_per_1m = ifelse(total_change_gpm25  == 0, NA, -1*total_change_gpm25/(annual_cost)),
             newtrips_per_1m = ifelse(total_change_newtrips == 0, NA, -1*total_change_newtrips/(annual_cost/1000000)))
  } else {
  temp_table <- left_join(output_table, cost_table) %>%
    mutate(gGHG_per_1m = ifelse(total_change_gGHG == 0, NA, -1*total_change_gGHG/annual_cost),
           VMT_per_1m = ifelse(total_change_VMT  == 0, NA, -1*total_change_VMT/(annual_cost/1000000)),
           nox_per_1m = ifelse(total_change_gnox  == 0, NA, -1*total_change_gnox/(annual_cost)),
           pm25_per_1m = ifelse(total_change_gpm25  == 0, NA, -1*total_change_gpm25/(annual_cost)),
           newtrips_per_1m = ifelse(total_change_newtrips == 0, NA, -1*total_change_newtrips/(annual_cost/1000000)))
  }
  
  #This is where the summary function is applied for each different column type
  temp_table$MTGHG_sum <- apply(temp_table, 1, sum_fun,
                                meas = "total_change_gGHG",
                                val = "annual_cost",
                                high = 2000,
                                med = 1000,
                                low = 500,
                                prefix = "CO2")
  temp_table$VMT_sum <- apply(temp_table, 1, sum_fun,
                              meas = "total_change_VMT",
                              val = "annual_cost",
                              high = 10000000*1000000, #this is accounting for the millions of dollar investment denominator 
                              med = 5000000*1000000,
                              low = 1000000*1000000,
                              prefix = "VMT")
  temp_table$MTnox_sum <- apply(temp_table, 1, sum_fun,
                              meas = "total_change_gnox",
                              val = "annual_cost",
                              high = 10,
                              med = 5,
                              low = 1,
                              prefix = "NOx")
  temp_table$MTpm25_sum <- apply(temp_table, 1, sum_fun,
                               meas = "total_change_gpm25",
                               val = "annual_cost",
                               high = .1,
                               med = 0.05,
                               low = 0.01,
                               prefix = "PM25")
  temp_table$newtrips_sum <- apply(temp_table, 1, sum_fun,
                                   meas = "total_change_newtrips",
                                   val = "annual_cost",
                                   high = 10000*1000000, #this is accounting for the millions of dollar investment denominator 
                                   med = 5000*1000000,
                                   low = 1000*1000000,
                                   prefix = "Daily Trips")
  
  if(style == "summary"){
    
    temp_table <- temp_table %>% 
      #rename(type_name = cap_proj_type) %>%
      select(c(col_sel, 
               'MTGHG_sum',
               'VMT_sum',
               'MTnox_sum',
               'MTpm25_sum',
               'newtrips_sum')) %>%
      #rename(type_name = "cap_proj_type") %>%
      rename('Summary MT GHG' = 'MTGHG_sum',
             'Summary VMT' = 'VMT_sum',
             'Summary MT NOx' = 'MTnox_sum',
             'Summary MT PM2.5' = 'MTpm25_sum',
             'Summary Daily Active Trips' = 'newtrips_sum')
    
  } else if(style == "detail"){
    
    temp_table <- temp_table  %>% 
      #rename(type_name = cap_proj_type) %>%
      select(c(col_sel,
             'gGHG_per_1m',
             'VMT_per_1m',
             'nox_per_1m',
             'pm25_per_1m',
             'newtrips_per_1m')) %>%
      #rename(type_name = "cap_proj_type") %>%
      rename("MT GHG" = 'gGHG_per_1m',
             "VMT" = 'VMT_per_1m',
             "MT NOx" = 'nox_per_1m',
             "MT PM2.5" = 'pm25_per_1m',
             "Daily Active Trips" = 'newtrips_per_1m')
    
  }

return(temp_table)
  
}
