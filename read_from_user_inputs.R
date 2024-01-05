### LOAD --------------------------------------
library(tidyverse)


### USER INPUTS ----------------------------
### Can eventually be set to empty tables/empty columns
### the reading functions here will be the same as one the user uploads a csv and reads in project inputs
### some objects are tables, some are lists
### lists are easier to pull values from, tables are easier to visualize and push to front end

# used to read in input reactives - check with Qi to see if we still need this,
# looks like it's only being used in the upload function
read_user_inputs_excel <- function(filename){

  Baseline <- read_excel(path = filename, sheet = "Baseline") %>% as.vector()
  Costs <- read_excel(path = filename, sheet = "Costs")
  Assumptions <- read_excel(path = filename, sheet = "Assumptions")
  Advanced <- read_excel(path = filename, sheet = "Advanced")
  Projects <- read_excel(path = filename, sheet = "Projects")
  
  return(list("Baseline" = Baseline,
              "Costs" = Costs,
              "Assumptions" = Assumptions, 
              "Advanced" = Advanced,
              "Projects" = Projects))
}

# used to read in reactive values (rvs)
read_user_inputs_version2 <- function(filename){
  
  Baseline <- read_excel(path = filename, sheet = "Baseline") %>% as.vector()
  Costs <- read_excel(path = filename, sheet = "Costs")
  Assumptions <- read_excel(path = filename, sheet = "Assumptions")
  Advanced <- read_excel(path = filename, sheet = "Advanced")
  Projects <- read_excel(path = filename, sheet = "Projects")
  
  reactive_data <- reactiveValues(
    "Baseline" = Baseline,
    "Costs" = Costs,
    "Assumptions" = Assumptions, 
    "Advanced" = Advanced,
    "Projects" = Projects
    )
  
  return(reactive_data)
}

# used to initially create the output tables
read_output_tables <- function(filename){
  
  cost_output <- read_excel(path = filename, sheet = "costs")
  baseline_output <- read_excel(path = filename, sheet = "baseline")
  
  reactive_data <- reactiveValues(
    "cost_output" = cost_output,
    "baseline_output" = baseline_output
  )
  
  return(reactive_data)
}