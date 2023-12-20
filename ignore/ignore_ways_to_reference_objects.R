### LOAD -----------------
library(tidyverse)
library(readxl)

### READ OBJECTS
source("read_from_user_inputs.R")

### Not actually a reactive value but just an example
not_rv <- read_user_inputs_excel("data/2.User_Inputs.xlsx")

### GOAL:
# this script demonstrates different ways to pull the table, vector, or value we want from the objects in the back end
# this is ideally in one single line, it can be really difficult to read code that have single objects spanning multiple lines

### HELPER FUNCTION -----------------------

table_shape <- function(df){
  df %>% 
  select_if(~any(!is.na(.))) %>%
  pivot_wider(names_from = "unit", values_from = "value")  
}

vector_shape <- function(df){
  shaped_df <- df %>%
    select_if(~any(!is.na(.))) %>%
    select(-category, -table_no, -table, -unit) %>% # removes columns that don't really identify the value, but context is key!
    # pivot_wider(names_from = "unit", values_from = "value") %>%
    unite("key", -value, sep = "", remove = T)
  
  set_names(x = shaped_df$value, nm = shaped_df$key)
}

convert_to_nested_list <- function(df){ 
  # Create an empty list
  my_list <- list()
  
  # Get number of primary keys, adding a 2 for the transit column
  num_primary_keys <- if(nrow(df) == 1){get_num_primary_keys(df) + 1}else{get_num_primary_keys(df) + 2}
  
  # Get the names of the primary key columns
  primary_keys <- names(df)[1:num_primary_keys]
  
  # Recursive function to create nested lists
  create_nested_list <- function(df, keys){
    # Base case: if there's only one key left, create a named vector
    if(length(keys) == 1){
      named_vector <- setNames(as.list(df[1, -(1:(length(primary_keys)-1))]), names(df)[-(1:(length(primary_keys)-1))])
      return(named_vector)
    }
    # Recursive case: create a list for each unique value of the first key
    else{
      nested_list <- list()
      for(i in unique(df[[keys[1]]])){
        nested_list[[as.character(i)]] <- create_nested_list(df[df[[keys[1]]] == i, ], keys[-1])
      }
      return(nested_list)
    }
  }
  
  # Populate the list
  my_list <- create_nested_list(df, primary_keys)
  
  return(my_list)
}

get_num_primary_keys <- function(df){
  # Get the column names
  cols <- names(df)
  
  # Check each combination of columns
  for(i in seq_along(cols)){
    combinations <- combn(cols, i, simplify = FALSE)
    for(comb in combinations){
      # If the number of unique rows for this combination of columns is equal to the number of rows in the data frame,
      # then this combination of columns can act as a primary key
      if(nrow(df[ , comb]) == nrow(unique(df[ , comb]))){
        return(length(comb))
      }
    }
  }
  
  # If no combination of columns can act as a primary key, return NULL
  return(NULL)
}

### OPTION 1: table_shape and vector_shape functions -------------------------

# if we want to return a table
filter(not_rv$Assumptions, category == "Bicycle and Pedestrian", unit == "prior_mode_share") %>% table_shape()

# if we want to grab a value, we could do something like this, but this returns two values!!!! So we have to be careful
not_rv$Assumptions$value[not_rv$Assumptions$unit == "annualization_factor"]

# perhaps it is better to designate the category every time
not_rv$Assumptions$value[not_rv$Assumptions$category == "Bicycle and Pedestrian" & not_rv$Assumptions$unit == "annualization_factor"]

# this is a dplyr way to do it, and it is a little shorter
filter(not_rv$Assumptions, category == "Bicycle and Pedestrian", unit == "annualization_factor") %>% pull(value)

# sometimes, we might a want a named vector out of a table, can be easier to work with
filter(not_rv$Assumptions, category == "Bicycle and Pedestrian", unit == "prior_mode_share") %>% vector_shape()


### OPTION 2: Nested List ----------------------------
# the idea with this one is that we would split each category into their own table, as to simplify the referencing
nested_list_bike_ped <- 
  filter(not_rv$Assumptions, category == "Bicycle and Pedestrian", unit == "prior_mode_share") %>%
  select(-table_no, -table) %>%
  table_shape() %>%
  convert_to_nested_list()

# I find this the easiest to pull specific values out of
nested_list_bike_ped$`Bicycle and Pedestrian`$Urban
