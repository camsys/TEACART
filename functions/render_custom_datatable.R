# new function
render_custom_datatable <- function(#input_reactives,
                                    data_reactive,
                                    table_number,
                                    is_year_table = TRUE,
                                    is_cost_table = FALSE,
                                    is_advanced_table = FALSE,
                                    non_editable_cols,
                                    page_length,
                                    comma_rows,
                                    percent_rows,
                                    currency_rows,
                                    decimal_rows,
                                    pivot_col = c()) {
  
  print(paste0('RUNNING: Render custom datatable for table: ', table_number))
  
  select_fun <- function(x) !all(is.na(x)|x == '')
  
  
    conditionally_transform <- function(df) {
    if (is_year_table == TRUE & is_advanced_table == FALSE) {
      df %>%
        pivot_wider(names_from = year, values_from = value) %>%
        select(-c(table_no_ui, table, category)) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_1), horizon_year_1) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_2), horizon_year_2) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_3), horizon_year_3) 
    } else if (is_cost_table == TRUE & nrow(data_reactive[data_reactive$table_no_ui == table_number,]) != 1 & table_number != 13){
      df %>% 
        select(-c(table_no_ui, table_no_ui_revised, cost_type, table, category)) %>%
        pivot_wider(names_from = unit, values_from = value)
    } else if (is_advanced_table == TRUE & is_year_table == TRUE){
      df %>%
        select(pivot_col) %>%
        pivot_wider(names_from = year, values_from = value)
    }
      else {
      df %>%
        select(-c(table_no_ui, table, category))
      }
    }

    #lapply(input_reactives, req)
    req(data_reactive)
    
    
    if(is_cost_table == TRUE & nrow(data_reactive[data_reactive$table_no_ui == table_number,]) != 1 & table_number != 13){ 
  #  for Costs tab, no need to join the references
    reshaped_table <- data_reactive  %>%
      filter(table_no_ui == table_number) %>% 
      conditionally_transform() %>% 
      ungroup() %>%
      select(where(select_fun)) %>% #NOTE: This will delete columns with NAs so if you send it empty data watch out
      #mutate(unit = map_chr(unit, ~ references_vector[.x] %||% .x)) %>%
      rename(any_of(references_vector))
    } else if (is_advanced_table == TRUE & is_year_table == TRUE){
      reshaped_table <- data_reactive  %>%
        filter(table_no_ui == table_number) %>% 
        conditionally_transform() %>% 
        ungroup() %>%
        #select(where(select_fun)) %>% #NOTE: This will delete columns with NAs so if you send it empty data watch out
        rename(any_of(references_vector))
    }else {
      reshaped_table <- data_reactive  %>%
        filter(table_no_ui == table_number) %>% 
        conditionally_transform() %>% 
        ungroup() %>%
        select(where(select_fun)) %>% #NOTE: This will delete columns with NAs so if you send it empty data watch out
        left_join(references, by = c("unit" = "field")) %>%
        mutate(unit = description) %>%
        select(-description) %>%
        #mutate(unit = map_chr(unit, ~ references_vector[.x] %||% .x)) %>%
        rename(any_of(references_vector))
    } 
    
    # browser()
    
    

    returnDT<-datatable(
      reshaped_table,
      rownames = FALSE,
      editable = list(target = 'all', disable = list(columns = non_editable_cols)),
      selection = "none",
      options = list(
        pageLength = page_length,
        # scrollX = TRUE,
        # scrollY = TRUE,
        columnDefs = list(
          list(
            targets = '_all',
            render = DT::JS(
              sprintf(
                "function(data, type, row, meta) {
                  if (type === 'display') {
                    var commaRows = [%s];
                    var percentRows = [%s];
                    var currencyRows = [%s];
                    var decimalRows = [%s];
                
                    var formatter = null;
                    if (commaRows.includes(meta.row)) {
                      formatter = function(d) { return Number(d).toLocaleString('en-US'); };
                    }
                    if (percentRows.includes(meta.row)) {
                      formatter = function(d) { return (Number(d) * 100).toFixed(2) + '%%'; };
                    }
                    if (currencyRows.includes(meta.row)) {
                      formatter = function(d) { return '$' + Number(d).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); };
                    }
                    if (decimalRows.includes(meta.row)) {
                      formatter = function(d) { return Number(d).toFixed(1); };
                    }
                    
                //console.log('the data: ' + data)
                //console.log('the type: '+ type)
                //console.log('the row: ' + row)
                //console.log('the meta: ' + meta)
                //console.log('the formatter' + formatter)
                
                
                    return formatter && !isNaN(data) && data !== null && data !== '' ? formatter(data) : data;
                  }
                  return data;
                }",
                paste(comma_rows, collapse = ", "), 
                paste(percent_rows, collapse = ", "),
                paste(currency_rows, collapse = ", "),
                paste(decimal_rows, collapse = ", ")
              )
            )
          )
        )
      )
    )  
    return(returnDT)
}
