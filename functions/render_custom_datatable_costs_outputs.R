# SLFLAG: Are we using this? I think we should delete it
render_custom_datatable_costs_outputs <- function(data_reactive,
                                                  table_number,
                                                  page_length) {

  print(paste0('RUNNING: Rendering cost putput table ', table_number))
  
  req(input$cost_view)

  # function to drop empty columns - note that this deletes columns with NAs
  # so if it gets empty data watch out
  select_fun <- function(x) !all(is.na(x)|x == '')

    
  # function to choose data based on table provided, remove empty columns, conditionally 
  # mutate from detail to summary values (high and so on) depending on whether 'summary' is ticked in the UI
  if(input$cost_view == "detail") {
    reshaped_table <- data_reactive  %>%
      filter(table_no_ui == table_number) %>%
      select(where(select_fun)) 
  } else {
    reshaped_table <- data_reactive %>%
      filter(table_no_ui) == table_number %>%
      select(where(select_fun)) %>%
      # placeholder logic for summary transformation done in the cost effectiveness tab
      mutate("Summary value" = case_when(
        value > 500 ~ 'High',
        TRUE ~ 'Low'
      ))
  }

  
# datatable set up
  display_table <- datatable(reshaped_table,
                             extensions = c('RowGroup','Buttons'),
                             options = list(autoWidth = TRUE,
                                            width = '100%',
                                            pageLength = page_length,
                                            dom = 'tB',
                                            buttons = c('copy', 'csv', 'excel', 'pdf')),
                             rownames = FALSE) |>
    formatNumber(c(1,2,5), decimalMark = '.', thousandsSeparator = ',') |>
    formatRound(c(3,4), digits = 2)

return(display_table)
}
