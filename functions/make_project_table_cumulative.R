#this functions takes the reactive projects tables, table number, and a list of columns, returns them with a cumulative value field
# projects_df <- rvs$Projects[rvs$Projects$unit == "new_retimed_signal",]
# table_no = 9
# cols = c('area_type','road_class')
# years_list = c(rvs$Baseline$horizon_year_1,
#               rvs$Baseline$horizon_year_2,
#               rvs$Baseline$horizon_year_3)
make_project_table_cumulative <- function(projects_df, table_no, cols = NULL){
  
  projects_df %>%
    filter(table_no_ui == table_no) %>%
    select(cols, "value", "year") %>% 
    pivot_wider(values_from = value, names_from = year) %>% 
    mutate(horizon_year_2 = horizon_year_1 + horizon_year_2) %>%
    mutate(horizon_year_3 = horizon_year_2 + horizon_year_3) %>%  
    pivot_longer(cols = starts_with("horizon_year_"), names_to = "year",values_to = "value")
    # mutate(
    #   year = case_when(year == "horizon_year_1" ~ years_list[1],
    #                    year == "horizon_year_2" ~ years_list[2],
    #                    year == "horizon_year_3" ~ years_list[3])) 
  
}
