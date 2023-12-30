
reshaping_projects_2_years <- function(user_data,
                                       rvs,
                                       tbl_no,
                                       horizon_year_1,
                                       horizon_year_2,
                                       horizon_year_3){
  
  print(paste0("RUNNING: revised reshaping function for table: ", tbl_no))

browser()
  
  # Determine the number of columns to pivot (excluding the first column)
  num_cols_to_pivot <- ncol(user_data) - 1
#  horizon_years <- c(input$horizon_year_1, input$horizon_year_2, input$horizon_year_3)
  horizon_years <- c(horizon_year_1, horizon_year_2, horizon_year_3)
  
  # Pivot and mutate
  modified_data <- user_data %>%
    pivot_longer(cols = tail(names(user_data), 3), names_to = "year") %>%
    mutate(year = horizon_years[match(year, tail(names(user_data), 3))]) %>%
    mutate(value = as.numeric(value)) %>%
    left_join(references, by = c("units" = "description"))


    x_names <- c(list(...), 'year')
    y_names <- names(modified_data)[names(modified_data) != "value"]
    
    
    updated_data <- rvs[rvs$table_no_ui == tbl_no, colnames(rvs)[colnames(rvs) != 'value']] %>%
      left_join(modified_data, by = setNames(y_names, x_names))
    
    
    return(updated_data)
}


