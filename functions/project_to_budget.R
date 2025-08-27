project_to_budget <- function(rvs, input) {
  check_tbl <- scenario_sum()
  
  if (all(check_tbl[, 3:ncol(check_tbl)] == 0, na.rm = TRUE) &&
      input$fill_projects_bttn == 0 && 
      all(rvs$Projects$value == 0)) {
    
    req(input$budget_start_year)
    req(input$budget_years_covered)
    req(input$budget_total)
    
    temp_budget <- rvs$Budget |>
      mutate(value = value/100) |>
      mutate(table_no_ui_revised = as.character(table_no_ui_revised))
    
    temp_costs <- rvs$Costs |>
      mutate(table_no_ui_revised = as.character(table_no_ui_revised)) |>
      filter(table_no_ui_revised != "-1") |>
      group_by(table_no_ui_revised) |>
      summarise(cost_parameter = sum(value), .groups = "drop")
    
    temp_join <- left_join(temp_budget, temp_costs) |>
      mutate(cost_parameter = case_when(
        !is.na(land_use) ~ 1,
        !is.na(land_use) & land_use == "Land Use Incentives" ~ 1000000,
        TRUE ~ cost_parameter
      ))
    
    start <- input$budget_start_year
    end <- start + input$budget_years_covered - 1
    total <- input$budget_total * 1000000
    total_years <- input$budget_years_covered
    
    year1 <- input$horizon_year_1
    year2 <- ifelse(input$horizon_year_2 <= 2040, input$horizon_year_2, 2040)
    year3 <- ifelse(input$horizon_year_3 <= 2040, input$horizon_year_3, 2040)
    
    foo <- temp_join |>
      dplyr::mutate(
        horizon_year_1_cnt = case_when(
          category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                          "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                          "Park-and-Ride") ~ ifelse(year1 - start < input$budget_years_covered, 
                                                    ifelse(year1 - start > 0, year1 - start, 0),  
                                                    input$budget_years_covered),
          category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                          "Public Transportation: Rail", "Bus Priority Treatment",
                          "Travel Demand Management", "EV Charging Infrastructure",
                          "Transit Service Cuts", "Micromobility",
                          "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year1) <= input$budget_years_covered, 
                                                          sum(c(start:end) <= year1),  
                                                          input$budget_years_covered),
          TRUE ~ ifelse(year1 - (start+2) < input$budget_years_covered, 
                        ifelse(year1 - (start+2) > 0, year1 - (start+2), 0),  
                        input$budget_years_covered)
        ),
        horizon_year_2_cnt = case_when(
          category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                          "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                          "Park-and-Ride") ~ ifelse(year2 - start < input$budget_years_covered, 
                                                    ifelse(year2 - start > 0, year2 - start, 0),  
                                                    input$budget_years_covered),
          category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                          "Public Transportation: Rail", "Bus Priority Treatment",
                          "Travel Demand Management", "EV Charging Infrastructure",
                          "Transit Service Cuts", "Micromobility",
                          "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year2) <= input$budget_years_covered, 
                                                          sum(c(start:end) <= year2),  
                                                          input$budget_years_covered),
          TRUE ~ ifelse(year2 - (start+2) < input$budget_years_covered, 
                        ifelse(year2 - (start+2) > 0, year2 - (start+2), 0),  
                        input$budget_years_covered)
        ),
        horizon_year_3_cnt = case_when(
          category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                          "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                          "Park-and-Ride") ~ ifelse(year3 - start < input$budget_years_covered, 
                                                    ifelse(year3 - start > 0, year3 - start, 0),  
                                                    input$budget_years_covered),
          category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                          "Public Transportation: Rail", "Bus Priority Treatment",
                          "Travel Demand Management", "EV Charging Infrastructure",
                          "Transit Service Cuts", "Micromobility",
                          "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year3) <= input$budget_years_covered, 
                                                          sum(c(start:end) <= year3),  
                                                          input$budget_years_covered),
          TRUE ~ ifelse(year3 - (start+2) < input$budget_years_covered, 
                        ifelse(year3 - (start+2) > 0, year3 - (start+2), 0),  
                        input$budget_years_covered)
        )
      ) |>
      dplyr::mutate(
        horizon_year_1_budget = total * value * (horizon_year_1_cnt / total_years),
        horizon_year_2_budget = total * value * (horizon_year_2_cnt / total_years),
        horizon_year_3_budget = total * value * (horizon_year_3_cnt / total_years),
        horizon_year_1 = horizon_year_1_budget / cost_parameter,
        horizon_year_2 = horizon_year_2_budget / cost_parameter,
        horizon_year_3 = horizon_year_3_budget / cost_parameter
      ) |>
      select(-c(table_no_ui, value, table_no_ui_revised)) |>
      pivot_longer(cols = c(horizon_year_1, horizon_year_2, horizon_year_3), 
                   names_to = "year", values_to = "value") |>
      select(any_of(names(rvs$Projects)))
    
    temp_projects <- rvs$Projects |> select(-value)
    
    foobar <- left_join(temp_projects, foo)
    foobar[foobar == "NA"] <- NA
    foobar[is.na(foobar$value), "value"] <- 0
    
    foobar_discum <- foobar %>%
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(
        horizon_year_1 = horizon_year_1, 
        horizon_year_3 = horizon_year_3 - horizon_year_2,
        horizon_year_2 = horizon_year_2 - horizon_year_1
      ) %>%
      pivot_longer(cols = starts_with("horizon_year_"),
                   names_to = "year", 
                   values_to = 'value')
    
    rvs$Projects <- foobar_discum
  }
  
  return(rvs$Projects)
}