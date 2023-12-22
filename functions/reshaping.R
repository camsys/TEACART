
reshaping <- function(user_data,
                      rvs,
                      tbl_no,
                      col1,
                      col2,
                      col3,
                      horizon_year_1,
                      horizon_year_2,
                      horizon_year_3){
  
  ## reshape the table
  if (length(unique(user_data$col)) == 5){
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
    mutate(year = case_when(year == 'var3' ~ horizon_year_1,
                            year == 'var4' ~ horizon_year_2,
                            year == 'var5' ~ horizon_year_3),
           value = as.numeric(value)) 
  
  ##browser()
  
  
  y_names = c('var1','var2','year')
  x_names = c(col1,col2,'year')
  
  updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']]%>%
    left_join(modified_data, by = setNames(y_names,x_names)) # setNames(y,x)
  ##browser()
  return(updated_data)
  }else{
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
      mutate(year = case_when(year == 'var4' ~ horizon_year_1,
                              year == 'var5' ~ horizon_year_2,
                              year == 'var6' ~ horizon_year_3),
             value = as.numeric(value)) 
    
    #browser()
    
    
    y_names = c('var1','var2','var3','year')
    x_names = c(col1,col2,col3,'year')

        updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']]%>%
      left_join(modified_data, by = setNames(y_names,x_names)) # setNames(y,x)
    ##browser()
    
    return(updated_data)
  }
  
}
