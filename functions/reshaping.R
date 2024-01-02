reshaping_projects2 <- function(user_data,
                               rvs,
                               tbl_no,
                               col1,
                               col2 = NA,
                               col3 = NA,
                               horizon_year_1,
                               horizon_year_2,
                               horizon_year_3){
  
  no_row =  nrow(user_data)/length(unique(user_data$col))
  modified_data <- data.frame(matrix(nrow = no_row))
  for (i in 1:length(unique(user_data$col))) {
    col_name <- paste0("var", i)
    modified_data[[col_name]] = user_data$value[user_data$col == i-1]
  }
  
  modified_data <- modified_data %>% 
    select_if(~any(!is.na(.))) %>%
    pivot_longer(tail(names(.), 3), names_to = c("year"))%>%
    mutate(year = case_when(year ==  colnames(modified_data)[length(colnames(modified_data)) - 2]~ "horizon_year_1",
                            year == colnames(modified_data)[length(colnames(modified_data)) - 1] ~ "horizon_year_2",
                            year == colnames(modified_data)[length(colnames(modified_data))] ~ "horizon_year_3")) %>%
    mutate(value = as.numeric(value))

  if (!is.na(col3) & col3 == "unit"){
    modified_data <- modified_data  %>%
      left_join(references, by = c("var3" = "description")) %>%
      mutate(var3 = field) %>%
      select(-field) }

  if(length(unique(user_data$col)) == 4){ # when there is only one str field
    y_names = c('year')
    x_names = c('year')}
  else if (length(unique(user_data$col)) == 5) { # specifically for tbl9, unit is needed for join. 
    y_names = c('var1','year')
    x_names = c(col1,'year')
  }else if(!is.na(col3)){ # when three columns are needed for joining
    y_names = c('var1','var2','var3','year')
    x_names = c(col1,col2,col3,'year')
  } else{ # all other table can be proper joined by 2 common fields.
    y_names = c('var1','var2','year')
    x_names = c(col1,col2,'year')
  }

  updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']] %>%
    left_join(modified_data, by = setNames(y_names,x_names)) %>% # setNames(y,x) 
  select(-contains("var"))

  return(updated_data)
}


### assumption function
reshaping_assmp <- function(user_data,
                             rvs,
                             tbl_no){
  
  no_row =  nrow(user_data)/length(unique(user_data$col))
  modified_data <- data.frame(matrix(nrow = no_row))

    for (i in 1:length(unique(user_data$col))) {
    col_name <- paste0("var", i)
    modified_data[[col_name]] = user_data$value[user_data$col == i-1]
  }

  names(modified_data)[length(names(modified_data))]<-"value"     ## change the last column name to value

  modified_data <- modified_data[, colSums(!is.na(modified_data)) > 0] #  ## drop all na column
  
  browser()

  col_list <- c(colnames(modified_data)[-ncol(modified_data)])
  
  # another loop to join value from references.
  for (var in col_list){
    y_col = c('description')
    x_col = c(var)
    modified_data <- modified_data %>%
      left_join(references,setNames(y_col,x_col)) %>%
      mutate(field = ifelse(is.na(field),get(var),field)) %>%
      select(-var) %>%
      rename(!!var := field)  }
  
  modified_data[modified_data == ""] <- NA

  browser()
  
  y_names = col_list
  x_names = c(colnames( rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']][colSums(!is.na( rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']])) > 0])) 
  x_names = x_names[-c(1,2,3)] # remove the first 'category','table_no_ui','table'
  
  updated_data <- rvs[rvs$table_no_ui == tbl_no,colnames(rvs)[colnames(rvs) != 'value']] %>%
    left_join(modified_data, by = setNames(y_names,x_names)) %>% # setNames(y,x) 
    select(-contains("var")) %>%
    mutate(value = as.numeric(value))
  
  browser()
  
   return(updated_data)
}


## reshape cost still working on. 
reshaping_cost <- function(user_data,
                   rvs,
                   tbl_no){
  # reshape the data
  no_row =  nrow(user_data)/length(unique(user_data$col))
  modified_data <- data.frame(matrix(nrow = no_row))
  
  for (i in 1:length(unique(user_data$col))) {
    col_name <- paste0("var", i)
    modified_data[[col_name]] = user_data$value[user_data$col == i-1]
  }
  
  browser()
  
  cols_to_pivot <- names(modified_data)[3:ncol(modified_data)]
  
  modified_data <- modified_data %>% 
    select_if(~any(!is.na(.))) %>%
    pivot_longer(cols = cols_to_pivot, names_to = c("unit"), values_to = "value")%>%
    mutate(value = as.numeric(value))
  
  browser()
}
