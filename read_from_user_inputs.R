### LOAD --------------------------------------
library(tidyverse)


### USER INPUTS ----------------------------
### Can eventually be set to empty tables/empty columns
### the reading functions here will be the same as one the user uploads a csv and reads in project inputs
### some objects are tables, some are lists
### lists are easier to pull values from, tables are easier to visualize and push to front end

read_user_inputs_excel <- function(filename){
  ### KEY INPUTS -----------------------------------------------------------------
  Key_Inputs <- read_excel("2.User_Inputs.xlsx", sheet = "Key_Inputs") %>% as.vector()
  
  ### COST PARAMETERS ------------------------------------------------------------
  Cost_Parameters <- read_excel("2.User_Inputs.xlsx", sheet = "Cost_Parameters")
  
  ### ASSUMPTIONS ----------------------------------------------------------------
  Assumptions <- read_excel("2.User_Inputs.xlsx", sheet = "Assumptions")
  
  ### BASELINE PARAMETERS --------------------------------------------------------
  Baseline_Parameters <- read_excel("2.User_Inputs.xlsx", sheet = "Baseline_Parameters")
  
  ### CAPITAL_PROJECT_INPUTS -----------------------------------------------------
  Capital_Project_Inputs <- read_excel("2.User_Inputs.xlsx", sheet = "Capital_Project_Inputs") %>%
    filter(state == my_state)
}