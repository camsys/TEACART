# new function
render_custom_datatable_SLVER <- function(#input_reactives,
                                    data_reactive,
                                    table_number,
                                    is_year_table = TRUE,
                                    non_editable_cols,
                                    page_length,
                                    comma_rows,
                                    percent_rows,
                                    currency_rows,
                                    decimal_rows) {
  
  print(paste0('RUNNING: Render Custom Datatable SLVER for table: ', table_number))
  
  select_fun <- function(x) !all(is.na(x)|x == '')
  
  
    conditionally_transform <- function(df) {
    if (is_year_table == TRUE) {
      df %>%
        pivot_wider(names_from = year, values_from = value) %>%
        select(-c(table_no_ui, table, category)) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_1), horizon_year_1) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_2), horizon_year_2) %>%
          rename_with(~as.character(rvs$Baseline$horizon_year_3), horizon_year_3) 
    } else {
      df %>%
        select(-c(table_no_ui, table, category))
      }
    }

    

    #lapply(input_reactives, req)
    req(data_reactive)

    reshaped_table <- data_reactive  %>%
      filter(table_no_ui == table_number) %>% 
      conditionally_transform() %>% 
      ungroup() %>%
      select(where(select_fun)) %>% #NOTE: This will delete columns with NAs so if you send it empty data watch out
      left_join(references, by = c("unit" = "field")) %>%
      mutate(unit = description) %>%
      select(-description) %>%
      #mutate(unit = map_chr(unit, ~ references_vector[.x] %||% .x)) %>%
      rename(any_of(references_vector)) #SETH Q: This is nice but IDK how it works is it a different rename function from the dplyr?
    
    #print(reshaped_table)
      #SETH - adding these to rename the horizon year columns probably a cleaner way? rename_with lets me work with the variables in dplyr (i think)

    
    returnDT<-datatable(
      reshaped_table,
      rownames = FALSE,
      editable = list(target = 'all', disable = list(columns = non_editable_cols)),
      selection = "none",
      options = list(
        pageLength = page_length,
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
                    } else if (percentRows.includes(meta.row)) {
                      formatter = function(d) { return (Number(d) * 100).toFixed(2) + '%%'; };
                    } else if (currencyRows.includes(meta.row)) {
                      formatter = function(d) { return '$' + Number(d).toLocaleString('en-US'); };
                    } else if (decimalRows.includes(meta.row)) {
                      formatter = function(d) { return Number(d).toFixed(1); };
                    }
                    
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
