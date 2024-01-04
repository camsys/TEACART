# # new function
# render_custom_datatable_costs_outputs <- function(data_reactive,
#                                                   is_summary = FALSE,
#                                                   table_number,
#                                                   page_length) {
# 
#   print(paste0('RUNNING: Rendering cost putput table ', table_number))
# 
#   # function to drop empty columns
#   select_fun <- function(x) !all(is.na(x)|x == '')
# 
#   # choosing data based on the table number provided and removing empty columns
#   reshaped_table <- data_reactive  %>%
#     filter(table_no_ui == table_number) %>%
#     select(where(select_fun)) #NOTE: This will delete columns with NAs so if you send it empty data watch out
# 
# 
#     # assembling the datatable for rendering
#   datatable(reshaped_table,
#             extensions = c('RowGroup','Buttons'),
#             options = list(autoWidth = TRUE,
#                            width = '100%',
#                            pageLength = page_length,
#                            dom = 'tB',
#                            buttons = c('copy', 'csv', 'excel', 'pdf')),
#             rownames = FALSE) |>
#     formatNumber(c(1,2,5), decimalMark = '.', thousandsSeparator = ',') |>
#     formatRound(c(3,4), digits = 2)
# 
# 
# return(returnDT)
# }
