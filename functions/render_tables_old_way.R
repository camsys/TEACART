# functions related to rendering the tables for the UI --------------------

# reading in static tables from the assumptions.xlsx etc files
# note that this is a true global object - moving to server would make this a 
# session-level object
# to delete once all tables have been updated
read_static_tables <- function(file_name, sheet_names) {
  for (i in seq_along(sheet_names)) {
    assign(sheet_names[i],
           read_xlsx(file_name,
                     sheet = i,
                     skip = 1,
                     col_names = TRUE),
           envir = .GlobalEnv)
  }
}

# the way tables are created using the old assumptions.xlsx files

create_table = function(data, editable = 'row', server = TRUE, ...) {
  renderDT(data,
           selection = 'none',
           server = server,
           editable = editable,
           rownames = FALSE,
           ...)
}



# create a graph ----------------------------------------------------------


create_graph <- function(data, indicator){
}
