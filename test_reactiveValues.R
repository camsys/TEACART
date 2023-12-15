#### just serves as an example of how to read in parameters and rewrite them using reactiveValues
library(shiny)
library(DT)

### read in raw_data
source("globals.R")

ui <- fluidPage(
  DTOutput("raw_data_tbl"),
  DTOutput("rv_whole_tbl"),
  DTOutput("rv_partial_tbl"),
  DTOutput("rv_reactive_tbl")
)

server <- function(input, output, session) {
  
  # Initialize reactiveValues
  rv <- reactiveValues()
  rv$Baseline_Parameters <- read_excel("2.User_Inputs.xlsx", sheet = "Baseline_Parameters")
    
  # browser()
  
  ## EXAMPLE 1 - SHOW RAW DATA ----------
  output$raw_data_tbl <- renderDT({
    # browser()
    DT::datatable(data = AEO_VMT_Base,
                  escape = FALSE,
                  options = list(pageLength = 10,
                                 autoWidth = FALSE,
                                 buttons = c('csv', 'excel')
                  ),
                  extensions = 'Buttons',
                  filter = 'bottom',
                  rownames = FALSE
    )
  })
  
  ## EXAMPLE 2 - REACTIVE VALUE - showing the whole table ----------
  
  output$rv_whole_tbl <- renderDT({
    DT::datatable(data = rv$Baseline_Parameters,
                  escape = FALSE,
                  options = list(pageLength = 10,
                                 autoWidth = FALSE,
                                 buttons = c('csv', 'excel')
                  ),
                  extensions = 'Buttons',
                  filter = 'bottom',
                  rownames = FALSE,
                  editable = T
    )
  })
    
  # Update the table when it is edited
  proxy_rv_whole_tbl = dataTableProxy('rv_whole_tbl')
  
  observeEvent(input$rv_whole_tbl_cell_edit, {
    browser()
    info = input$rv_whole_tbl_cell_edit
    str(info)
    i = info$row
    j = info$col + 1 ### we add one because HTML indexes from 0 whereas R indexes from 1
    v = info$value
    rv$Baseline_Parameters[[i, j]] <<- DT::coerceValue(v, rv$Baseline_Parameters[[i, j]])
    replaceData(proxy_rv_whole_tbl, rv$Baseline_Parameters, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  
  ## EXAMPLE 3 - Manipulate within a rendering statement --------------
  
  # output$rv_partial_tbl <- renderDT({
  #   DT::datatable(data = rv$Baseline_Parameters %>% filter(category == "Fuel Apportionment"),
  #                 escape = FALSE,
  #                 options = list(pageLength = 10,
  #                                autoWidth = FALSE,
  #                                buttons = c('csv', 'excel')
  #                 ),
  #                 extensions = 'Buttons',
  #                 filter = 'bottom',
  #                 rownames = FALSE
  #   )
  # })
  
  ## EXAMPLE 4 - Manipulate within reactive function and render that ---------
  
  # output$rv_reactive_tbl <- renderDT({
  #   DT::datatable(data = rv$Baseline_Parameters,
  #                 escape = FALSE,
  #                 options = list(pageLength = 10,
  #                                autoWidth = FALSE,
  #                                buttons = c('csv', 'excel')
  #                 ),
  #                 extensions = 'Buttons',
  #                 filter = 'bottom',
  #                 rownames = FALSE
  #   )
  # })
  
  ### READ from user_input_click
  # observeEvent(input$user_clicks_submit_excel, {
  #   rv$Cost_Parameters <- read_excel(input$user_clicks_submit_excel, sheet = "Cost_Parameters")
  # })
  
}

shinyApp(ui = ui, server = server)