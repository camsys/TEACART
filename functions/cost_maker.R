

#You can uncomment these if you want to test the script 
# observeEvent(input$state_input, {
#   browser()})


  # #fake inputs
  # tab_no = 12
  # proj_life = 30
  # cols <- c('road_class','area_type')
  # output_table <- cost_output_RoadwayExp()
  # ini_cost_table <- rvs$Costs[rvs$Costs$table_no_ui == tab_no,]


cost_function <- function(ini_cost_table, #this is the rvs cost table prefiltered by table number
                          output_table, #this it the processing output
                          cols, #these are the columns of the rvs cost table that are necessary to get the individual rows e.g. road_class, area_type, etc
                          proj_life, #not sure if we should be pulling this in through a table in Raw Data?
                          var1_scalar = NULL, #this is only neccesary for transit table
                          var2_scalar = NULL#this is only neccesary for transit table
                          ){
  #real input
  cols <- c(cols, 'cost_type') #add cost type to the columns that will be used for groupin
  
  #IF Else is for indicating transit or other tables
  #This function basically calcualtes the cost parameters page by getting the annualized cost with is usually capital cost/project life + variable cost
  if('var1' %in% unique(ini_cost_table$cost_type)){ 
    
    
    cost_table = ini_cost_table %>% 
      group_by_at(cols) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(names_from = cost_type, values_from = value) %>%
      mutate(var = var1*var1_scalar + var2*var2_scalar) %>% #create variable cost based on scalars
      select(-c(var1,var2)) %>%
      mutate(annual_cost = cap/proj_life + var)
    
  } else {
    
    cost_table = ini_cost_table %>% 
      group_by_at(cols) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(names_from = cost_type, values_from = value) %>%
      mutate(annual_cost = cap/proj_life + var)
      
  }
  
  #This function is applied to each row to estimate the summary value based on supplied high, medium, low values
  #The high medium low values are the same for each project type
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

  #This is where the non-summary output comes from
  temp_table <- left_join(output_table, cost_table) %>%
    mutate(MTCO2_per_1m = ifelse(total_change_MTCO2 == 0, NA, -1*total_change_MTCO2/annual_cost),
           VMT_per_1m = ifelse(total_change_VMT  == 0, NA, -1*total_change_VMT/(annual_cost/1000000)),
           nox_per_1m = ifelse(total_change_mtnox  == 0, NA, -1*total_change_mtnox/(annual_cost)),
           pm25_per_1m = ifelse(total_change_pm25  == 0, NA, -1*total_change_pm25/(annual_cost)),
           newtrips_per_1m = ifelse(total_change_newtrips == 0, NA, -1*total_change_newtrips/(annual_cost/1000000)))
  
  #This is where the summary function is applied for each different column type
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
                              high = 10000000*1000000, #this is accounting for the millions of dollar investment denominator 
                              med = 5000000*1000000,
                              low = 1000000*1000000,
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
                                   high = 10000*1000000, #this is accounting for the millions of dollar investment denominator 
                                   med = 5000*1000000,
                                   low = 1000*1000000,
                                   prefix = "Daily Trips")


  
  
}