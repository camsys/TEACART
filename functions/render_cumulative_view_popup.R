### Renders table inside cumulative view popup
render_cumulative_view_popup <- function(my_rv, my_table_no, my_cols){
  #browser()
  my_rv[["Projects"]] %>% 
    make_project_table_cumulative(table_no = my_table_no, cols = my_cols) %>%
    get_horizon_years(my_rv = my_rv) %>%
    pivot_wider(names_from = year, values_from = value) %>% 
    select(-any_of(my_cols)) %>%
    datatable(rownames = F, options = list(dom = "t", pageLength = 55))
}
