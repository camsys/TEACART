
reshaping_projects <- function(user_data,
                               rvs,
                               is_year_table,
                               tbl_no,
                               col1,
                               col2,
                               col3,
                               col4,
                               horizon_year_1,
                               horizon_year_2,
                               horizon_year_3){
  
  # print(paste0("RUNNING: Reshaping Function for table: ", tbl_no))
  # if(tbl_no == 2){browser()}
  ## reshape the table
  
  if (is_year_table){
  
  if(length(unique(user_data$col)) == 4){
    browser()
    var1 = user_data$value[user_data$col == 0]
    var2 = user_data$value[user_data$col == 1]
    var3 = user_data$value[user_data$col == 2]
    var4 = user_data$value[user_data$col == 3]
    
    modified_data <- data.frame(var1 = var1,
                                var2 = var2,
                                var3 = var3,
                                var4 = var4) %>%
      pivot_longer(tail(names(.), 3), names_to = c("year"))%>%
      mutate(year = case_when(year == 'var2' ~ "horizon_year_1",
                              year == 'var3' ~ "horizon_year_2",
                              year == 'var4' ~ "horizon_year_3")) %>%
      mutate(value = as.numeric(value)) %>%
      left_join(references, by = c("var1" = "description")) %>%
      mutate(var1 = field) %>%
      select(-field) 
    
    ##browser()
    
    
    y_names = c('var1', 'year')
    x_names = c(col1, 'year')
    
    updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']]%>%
      left_join(modified_data, by = setNames(y_names,x_names)) # setNames(y,x)
    ##browser()
    return(updated_data)
    
  } else if(length(unique(user_data$col)) == 5){
    var1 = user_data$value[user_data$col == 0]
    var2 = user_data$value[user_data$col == 1]
    var3 = user_data$value[user_data$col == 2]
    var4 = user_data$value[user_data$col == 3]
    var5 = user_data$value[user_data$col == 4]
  
  modified_data <- data.frame(var1 = var1,
                              var2 = var2,
                              var3 = var3,
                              var4 = var4,
                              var5 = var5) %>%
    pivot_longer(tail(names(.), 3), names_to = c("year"))%>%
    mutate(year = case_when(year == 'var3' ~ "horizon_year_1",
                            year == 'var4' ~ "horizon_year_2",
                            year == 'var5' ~ "horizon_year_3")) %>%
    mutate(value = as.numeric(value)) %>%
    left_join(references, by = c("var2" = "description")) %>%
    mutate(var2 = field) %>%
    select(-field) 
  
  ##browser()
  
  
  y_names = c('var1','var2','year')
  x_names = c(col1,col2,'year')
  
  updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']]%>%
    left_join(modified_data, by = setNames(y_names,x_names)) # setNames(y,x)
  ##browser()
  return(updated_data)
  
  } else if(length(unique(user_data$col)) == 7){
    
    browser()
    var1 = user_data$value[user_data$col == 0]
    var2 = user_data$value[user_data$col == 1]
    var3 = user_data$value[user_data$col == 2]
    var4 = user_data$value[user_data$col == 3]
    var5 = user_data$value[user_data$col == 4]
    var6 = user_data$value[user_data$col == 5]
    var7 = user_data$value[user_data$col == 6]
    
    modified_data <- data.frame(var1 = var1,
                                var2 = var2,
                                var3 = var3,
                                var4 = var4,
                                var5 = var5,
                                var6 = var6,
                                var7 = var7) %>%
      pivot_longer(tail(names(.), 3), names_to = c("year"))%>%
      mutate(year = case_when(year == 'var5' ~ "horizon_year_1",
                              year == 'var6' ~ "horizon_year_2",
                              year == 'var7' ~ "horizon_year_3")) %>%
      mutate(value = as.numeric(value)) %>%
      left_join(references, by = c("var4" = "description")) %>%
      mutate(var4 = field) %>%
      select(-field)
    
    
    y_names = c('var1','var2','var3','var4','year')#very janky how to deal with units column being renamed and then needing to be unrenamed
    x_names = c(col1,col2,col3,col4,'year')
    updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']]%>%
      left_join(modified_data, by = setNames(y_names,x_names)) # setNames(y,x) 

    return(updated_data)
  }else if(length(unique(user_data$col)) == 6){
    browser()
    var1 = user_data$value[user_data$col == 0]
    var2 = user_data$value[user_data$col == 1]
    var3 = user_data$value[user_data$col == 2]
    var4 = user_data$value[user_data$col == 3]
    var5 = user_data$value[user_data$col == 4]
    var6 = user_data$value[user_data$col == 5]
    
    modified_data <- data.frame(var1 = var1,
                                var2 = var2,
                                var3 = var3,
                                var4 = var4,
                                var5 = var5,
                                var6 = var6) %>%
      pivot_longer(tail(names(.), 3), names_to = c("year"))%>%
      mutate(year = case_when(year == 'var4' ~ "horizon_year_1",
                              year == 'var5' ~ "horizon_year_2",
                              year == 'var6' ~ "horizon_year_3")) %>%
      mutate(value = as.numeric(value)) %>%
      left_join(references, by = c("var3" = "description")) %>%
      mutate(var3 = field) %>%
      select(-field) 
    
    browser()
    
    
    y_names = c('var1','var2','var3','year')
    x_names = c(col1,col2,col3,'year')
    
    updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']] %>%
      left_join(modified_data, by = setNames(y_names,x_names)) # setNames(y,x) 
    ##browser()
    
    return(updated_data)
    }
    } else{
      if(length(unique(user_data$col)) == 3){
    browser()
    var1 = user_data$value[user_data$col == 0]
    var2 = user_data$value[user_data$col == 1]
    var3 = user_data$value[user_data$col == 2]

    modified_data <- data.frame(var1 = var1,
                                var2 = var2,
                                var3 = var3) %>% 
      rename(value = var3) %>%
      mutate(value = as.numeric(value))%>%
      select(-var2)
  
    browser()
    
    
    y_names = c('var1')
    x_names = c(col1)
    
    updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != c('value')]]%>%
      left_join(modified_data, by = setNames(y_names,x_names)) # setNames(y,x)
    browser()
    
    return(updated_data)
    
      } else if(length(unique(user_data$col)) == 6){ # only for transit 
        browser()
        var1 = user_data$value[user_data$col == 0]
        var2 = user_data$value[user_data$col == 1]
        var3 = user_data$value[user_data$col == 2]
        
        modified_data <- data.frame(var1 = var1,
                                    var2 = var2,
                                    var3 = var3) %>% 
          rename(value = var3) %>%
          mutate(value = as.numeric(value))%>%
          select(-var2)
        
        browser()
        
        
        y_names = c('var1')
        x_names = c(col1)
        
        updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != c('value')]]%>%
          left_join(modified_data, by = setNames(y_names,x_names)) # setNames(y,x)
        browser()
        
        return(updated_data)
        
      }
    }
  
}

