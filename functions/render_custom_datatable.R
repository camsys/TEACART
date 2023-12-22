# new function
render_custom_datatable <- function(input_reactives,
                                    data_reactive,
                                    table_number,
                                    is_year_table,
                                    non_editable_cols,
                                    page_length,
                                    comma_rows,
                                    percent_rows,
                                    currency_rows,
                                    decimal_rows) {
  
  lapply(input_reactives, req)
  
    conditionally_pivot <- function(df) {
    if (is_year_table == TRUE) { 
      df |> 
        pivot_wider(names_from = year, values_from = value)
    } else {
      df
    }
    }
    
    # conditionally_select <- function(df) {
    #   if (is_year_table == TRUE) { 
    #     df |> 
    #       select(-c(table_no_ui, table, unit, category))
    #   } else {
    #     select(-c(year, table_no_ui, table, unit, category))
    #   }
    # }
    
  output <- renderDT({
    
    reshaped_table <- data_reactive |> 
      filter(table_no_ui == table_number) |>
      conditionally_pivot() |>
      ungroup() |> 
      select(-c(table_no_ui, table, unit, category)) |> 
      select_if(~ !all(is.na(.) | . == '')) |> 
      rename(any_of(references_vector))
    
    datatable(
      reshaped_table,
      rownames = FALSE,
      editable = list(target = 'all', disable = list(columns = non_editable_cols)),
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
  }, server = FALSE)
  
  return(output)
}
