# SLFLAG: Would we break something if we add table type to the arguments and set it that way?
# Suggest we flag for a future improvement

# new function
render_custom_datatable <- function(#input_reactives,
                                    data_reactive,
                                    table_number,
                                    is_budget_table = FALSE,
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
 #if(is_budget_table & table_number == 1){browser()}

  req(input$base_year)
  req(input$horizon_year_1)
  req(input$horizon_year_2)
  req(input$horizon_year_3)
  print(paste0('RUNNING: Render custom datatable for table: ', table_number))

  select_fun <- function(x) !all(is.na(x)|x == '')
  
    conditionally_transform <- function(df) {
      # note that this function fails if you feed it fewer than five columns
    if (is_year_table == TRUE & is_advanced_table == FALSE) {
      if(!is.null(rvs$Baseline$horizon_year_1)){
        # browser()
      df %>%
        pivot_wider(names_from = year, values_from = value) %>%
        select(-c(table_no_ui, table, category)) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_1), horizon_year_1) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_2), horizon_year_2) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_3), horizon_year_3)} 
      else{
        # browser()
        
            df %>%
              pivot_wider(names_from = year, values_from = value) %>%
              select(-c(table_no_ui, table, category))
           }
            
    } else if (is_budget_table == TRUE & is_advanced_table == FALSE & is_cost_table == FALSE & is_year_table == FALSE) {
      df %>% 
        select(-c(table_no_ui, table_no_ui_revised,table), -any_of(pivot_col))
    } else if (is_cost_table == TRUE & nrow(data_reactive[data_reactive$table_no_ui == table_number,]) != 1 & table_number != 18){ #slchanged
      df %>% 
        select(-c(table_no_ui, table_no_ui_revised, 
                  cost_type, table, category)) %>%
        pivot_wider(names_from = unit, values_from = value)
    } else if (is_advanced_table == TRUE & is_year_table == TRUE){
      df %>%
        select(pivot_col) %>%
        pivot_wider(names_from = year, values_from = value)
    }
      else {
      df %>%
        select(-c(table_no_ui, table_no_ui_revised, 
                  cost_type, table, category))
      }
    }

    #lapply(input_reactives, req)
    req(data_reactive)
    
    if(is_cost_table == TRUE & nrow(data_reactive[data_reactive$table_no_ui == table_number,]) != 1 & table_number != 18){ #slchanged
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
    } else {
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
      # browser()
    } 
    
    if(is_budget_table){
      #if(table_number %in% c(5)){browser()}
      if(table_number %in% c(5, 7,8,9,11,13,15)){
        # browser()
        select_list <- c("description", "category")
        } else {select_list <- c("description","category","unit")}
      reshaped_table <- data_reactive  %>%
        filter(table_no_ui == table_number) %>% 
        conditionally_transform() %>% 
        ungroup() %>% 
        select(where(select_fun)) %>% #NOTE: This will delete columns with NAs so if you send it empty data watch out
        left_join(references, by = c("unit" = "field")) %>%
        mutate(unit = description) %>%
        select(-(any_of(select_list))) %>% #NOTE: this is where we remove the category and unit field easy to add back
        #mutate(unit = map_chr(unit, ~ references_vector[.x] %||% .x)) %>%
        #mutate(value = value/100)|>
        rename(any_of(references_vector))
      
      returnDT<-datatable(
        reshaped_table,
        rownames = FALSE, # looks like a big edit to change this - will need to tweak the reshaping function and set units for the first column
        editable = list(target = 'all', disable = list(columns = non_editable_cols)),
        selection = "none",
        options = list(
          pageLength = page_length,
          paging = FALSE,
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
                    var percentRows = [%s];\
                    var currencyRows = [%s];
                    var decimalRows = [%s];
                
                    var formatter = null;
                    if (commaRows.includes(meta.row)) {
                      formatter = function(d) { return Number(d).toLocaleString('en-US'); };
                    }
                    if (percentRows.includes(meta.row)) {
                      formatter = function(d) { return (Number(d) * 100).toFixed(2) + '%%'; };
                    }
                                        if (percentRows.includes(meta.row)) {
                      formatter = function(d) { return (Number(d)).toFixed(2) + '%%'; };
                    }
                    if (currencyRows.includes(meta.row)) {
                      formatter = function(d) { return '$' + Number(d).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); };
                    }
                    if (decimalRows.includes(meta.row)) {
                      formatter = function(d) { return Number(d).toLocaleString('en-US', {maximumFractionDigits: 2}); };
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
      )  |> formatCurrency("Value",digits = 2, currency = "%", before = F)
      
    } else {  

    returnDT<-datatable(
      reshaped_table,
      rownames = FALSE, # looks like a big edit to change this - will need to tweak the reshaping function and set units for the first column
      editable = list(target = 'all', disable = list(columns = non_editable_cols)),
      selection = "none",
      options = list(
        pageLength = page_length,
        paging = FALSE,
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
                      formatter = function(d) { return Number(d).toLocaleString('en-US', {maximumFractionDigits: 2}); };
                    }
                    
                    if (!formatter && !isNaN(data) && data !== null && data !== '') {
                      formatter = function(d) { return Number(d).toLocaleString('en-US'); };
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
    }
    #print('fin')
    return(returnDT)
}
