library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(data.table)
library(thematic)
library(shinyBS)   # tooltips, maybe - shinyBS uses bootstrap 3 and bslib uses 5
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(plotly)
library(shiny)
library(shinyjs)
library(tinytex)
library(tools)
library(shinyalert)
library(knitr)
library(ggplot2)
if (!require(kableExtra)) {
  install.packages("kableExtra")
  library(kableExtra)
}
if (!require(tinytex)) {
  install_tinytex()
}
library(showtext)
showtext_auto()
library(scales)

# load source files -------------------------------------------------------

all_files <- c(list.files("functions", full.names = TRUE))

data_list <- list()


# reading in xlsx and R files only - note
for (file in all_files) {
  if (grepl("\\.R$", file)) {
    source(file)
  }
}

source("globals.R")
source("read_from_user_inputs.R")

# ui ----------------------------------------------------------------------

ui <- function(request) {
  
  tagList(
    
    # Leave this function for adding external resources
    # Application UI logic
    #title = "",
    # styles ------------------------------------------------------------------
    # #a0cf66 is a georgetown color but intense - color below is a milder variation
    tags$head(
      tags$style(HTML("
            .no-click {
            pointer-events: none;
            background-color: #f9f9f9;
            }
              td.first-column {
    background-color: #d2d8e7 !important;
  }
            .accordion-button.collapsed {
                background-color: #e3ebd5;
            }
            ol.spaced-images li {
            margin-bottom: 2em;
            }
            .btn-custom {
                background-color: #e3ebd5 !important;
                height: 54px;
                width: 100%;
            } 
            .well.card-flex {
            display: flex;
            flex-direction: row; 
            }
            .invisible-well.card-flex {
            display: flex;
            flex-direction: row; 
            }
             well.invisible-well {
	          background-color: transparent !important;
	          border: none !important;
            box-shadow: none !important;
             }
                  .half-card {
            width: 50%;
            margin-right: 20px;
            display: inline-block;
            }
            .nav.navbar-nav .form-group.shiny-input-container {margin-bottom: 0; height: 50px;}
            .nav.navbar-nav .form-group.shiny-input-container > label {display: inline;}
            
        ")),
      tags$link(rel = "stylesheet", 
                href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"),
      tags$link(rel = "stylesheet", 
                href = "https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap"),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400&family=Oswald:wght@300;400;500&display=swap"),
      tags$link(rel = "stylesheet", 
                href = "TEACART.css?v=1.0.1"),
      tags$script(src="https://code.jquery.com/ui/1.13.1/jquery-ui.min.js"),
      tags$script(src = "TEACART.js")
      
    ),
    
    page_navbar(
      position = "fixed-top",
      #title = "TEA-CART",
      id = "APP_PAGE",
      theme = bs_theme(
        version = 5,
        fg = "#0d204d", # this has the same CMYK values as #173A89 but 70% saturation on black as opposed to 46%
        primary = "#173A89", #2A8627
        base_font = font_google("Inter"),
        font_scale = NULL,
        preset = "pulse",
        bg = "#fff"
      ), 
      
      # footer -----------------------------------------------------------------------
      footer =  fluidRow( class="footer",
                          #SLBOOKMARK
                          useShinyjs(),
                          column(width = 12,
                                 tags$a(tags$img(src = "GCC_Logo_Contrast.svg", class="footer-logo gcc-logo"),
                                        href = "https://www.georgetownclimate.org/", target = "_blank"),
                                 #tags$p("Adapted from TEA-CART Excel Model Version 1.14"),
                                 tags$p("Last updated November 3, 2025"),
                                 tags$p("Developed by Cambridge Systematics, Inc."),
                                 tags$p("under contract to Georgetown Climate Center"),
                                 tags$p("©2025 Georgetown Climate Center (All Rights Reserved)")
                          ),
                          #column(width = 2,),
                          # column(width = 2,
                          #        shiny::actionButton("inputs_btn",
                          #                            class = "btn btn-primary",
                          #                            label = "Inputs"
                          #        ),
                          #        tags$ul(
                          #          tags$li(
                          #            shiny::actionButton("inbaseline",class = "btn btn-secondary",label = "Baseline")),
                          #          tags$li(
                          #            shiny::actionButton("inprojects",class = "btn btn-secondary",label = "Projects")),
                          #          tags$li(
                          #            shiny::actionButton("incosts",class = "btn btn-secondary",label = "Costs")),
                          #          tags$li(
                          #            shiny::actionButton("inassumptions",class = "btn btn-secondary",label = "Assumptions")),
                          #          tags$li(
                          #            shiny::actionButton("inscenarios",class = "btn btn-secondary",label = "Scenarios")),
                          #          tags$li(
                          #            shiny::actionButton("inadvanced",class = "btn btn-secondary",label = "Advanced"))
                          #        )
                          # ),
                          # column(width = 2,
                          #        shiny::actionButton("outputs_btn",
                          #                            class = "btn btn-primary",
                          #                            label = "Outputs"
                          #        ),
                          #        tags$ul(
                          #          tags$li(
                          #            shiny::actionButton("outbaseline",class = "btn btn-secondary",label = "Baseline GHG Forecast")),
                          #          tags$li(
                          #            shiny::actionButton("outscenario",class = "btn btn-secondary",label = "Scenario Summary")),
                          #          tags$li(
                          #            shiny::actionButton("outsummary",class = "btn btn-secondary",label = "Strategy Summary")),
                          #          tags$li(
                          #            shiny::actionButton("outcosteff",class = "btn btn-secondary",label = "Cost-Effectiveness"))
                          #        )
                          # ),
                          # column(width = 1,
                          #        tags$div(class = "make-this-colunar",
                          #                 shiny::actionButton("about_btn",
                          #                                     class = "btn btn-primary",
                          #                                     label = "About"
                          #                 ),
                          #                 shiny::actionButton("how_to_btn",
                          #                                     class = "btn btn-primary",
                          #                                     label = "How-to"
                          #                 ),
                          #                 shiny::actionButton("sources_btn",
                          #                                     class = "btn btn-primary",
                          #                                     label = "Sources"
                          #                 )
                          #        )
                          # )
      ), 
      # sidebar -----------------------------------------------------------------
      
      
      sidebar = sidebar(width = 450, 
                        class = "stick-sidebar",
                        h2('Quick Guide'),
                        HTML("<p>This sidebar has a high-level step-by-step guide for entering and saving data in TEA-CART. </p>
                        "),
                        h4('New User'),
                        HTML("<ol>
                            <li>Navigate to the <b>Inputs</b> tab.</li>
                            <li>Input your values in the <b>Baseline, Projects / Budget, Costs,</b> and <b>Assumptions</b> tabs.</li>
                            <li>When you are done entering data, press <b>CTRL + Enter</b> on your keyboard to initiate the calculations.</li>
                            <li>Select the desired combination of strategies in the <b>Scenarios</b> tab.</li>
                            <li>Navigate to the <b>Outputs</b> tab to view your results.</li>
                            </ol>"),
                        h4('Returning User'),
                        HTML("<p><b>Upload User Inputs</b></p>"),
                        HTML("Note: Set tool to Budget mode if entering a budget scenario."),
                        fileInput("user_inputs_upload",
                                  label = NULL,
                                  accept = c(".xlsx")),
                        
                        bsTooltip(id = "user_inputs_upload",
                                  "Upload the file generated by the TEA-CART tool with user inputs.",
                                  options = list(container = "body"),
                                  placement = "right"),
                        h4('Save My Work'),
                        downloadButton("user_inputs_download", "Download User Inputs"),
                        # HTML("<p>When you are done entering data, remember to press <b>CTRL + Enter</b> on your keyboard to initiate the calculations.</p>"),
                        
                        # nav_spacer(),
                        # nav_spacer(),
                        # nav_spacer(),
                        downloadButton("result_data","Download Output Tables"),
                        # downloadButton("pdf_report","Download Summary Report"),
                        HTML("<p>More information can be found in the <b>How-to</b> tab.</p>
                             <p>
                             For more detailed guidance, please refer to the <a href='https://camsys.shinyapps.io/TEA-CART/_w_5d75c74185704a68841983641c922643/_w_926631baafd6430185a4724987bec338/TEACART%20User%20Guide%20and%20Methodology%20v.1.10.3.pdf'>User Guide and Methodology Documentation</a>.")),
      
      
      # welcome page ------------------------------------------------------------
      nav_panel(title = "TEA-CART",
                p(),
                h2("Welcome"),
                HTML("<p>Welcome to <b>Transportation Evaluation and Carbon 
                Reduction Tool (TEA-CART)</b>, a web-based application that 
                supports planning-level analysis to estimate how different 
                transportation investments will affect future real-world 
                outcomes for people and the environment.<br>
                <p>
<i>Would you like to…<br>
<p>
…calculate how a proposed set  of transportation investments might  reduce 
-- or increase --  greenhouse gas emissions from cars and trucks?<br>
<p>
…estimate how adding 10 miles of new bike lanes will affect driver behavior 
and the number of vehicle-miles traveled?<br>
<p>
…compare the air emissions outcomes of one potential portfolio of 
investments with another?  For example, which would do more to reduce 
emissions from transportation in your community: spending $1 million on electric
buses and charging infrastructure, or investing those same funds in expanding 
light rail and transit oriented development?</i> <br>
<p>
TEA-CART can help answer these and other questions in ways that are 
accessible for a variety of users. Anyone can use this tool, but we 
anticipate that it will be most useful for state, regional, and local 
agency officials who aim to achieve better climate and public health 
outcomes through transportation planning and investment decision making.<br>
<p>
Click on the <b>About</b> tab to learn more about the tool’s inputs, outputs, 
and potential applications.<br>
<p>
<b>This application has been tested for use in the Firefox and Chrome 
browsers in a Windows (PC) environment. This is 
recommended for optimal experience.</b><br>
<p>
Follow <a href='https://www.georgetownclimate.org/blog/tea-cart-landing.html'>
                  this link</a> to access illustrative investment scenarios
and a related discussion to help TEA-CART users get oriented. This includes:</p>
<ul>
  <li>A business-as-usual budget scenario, and</li>
  <li>A multi-modal budget scenario</li>
</ul>
<p>
"),
                actionLink("sources_btn", "View Sources"),
                HTML("<br>"),
                actionLink("guide_btn", "View User Guide and Methodology Documentation"),
                p(),
                HTML("<i>To report a problem in the tool, please send an email to 
           <u>climate@georgetown.edu</u> with “TEA-CART Help” in the subject line.</i>
           <br><br>
           ")
                
      ),
      
      # about page --------------------------------------------------------------
      
      nav_panel(title = "About",
                #    h2("Transportation Evaluation and Carbon Reduction Tool (TEA-CART)"),
                p(),
                h2("About"),
                HTML(
                  "<p>Georgetown Climate Center’s (GCC) Transportation Evaluation 
                  and Carbon Reduction Tool (TEA-CART) offers planning-level 
                  analysis to estimate the GHG performance of transportation 
                  capital program investments. The functionality of the tool 
                  has been informed by transportation agency officials. It is 
                  designed to serve as a resource for state Departments of 
                  Transportation (DOT) and Metropolitan Planning Organization 
                  (MPO) practitioners conducting long-term planning, project 
                  prioritization, and performance management. </p>
                  <p>The primary purpose of TEA-CART is to help practitioners 
                  account for the environmental performance of proposed 
                  projects, so they can set meaningful targets for greenhouse 
                  gas or vehicle-miles traveled reduction and develop capital 
                  plans that prioritize transportation projects that will 
                  help achieve those goals. This tool also estimates how 
                  transportation projects could affect NOx and PM2.5 emissions.
                  TEA-CART was developed by Cambridge Systematics under contract 
                  with the Georgetown Climate Center, which facilitated extensive 
                  input from state and federal officials.</p>
                  <p><b>Tool Functionality:</b> TEA-CART is easy to use, accepting 
                  simple inputs to evaluate the GHG performance of surface 
                  transportation capital projects during the planning or 
                  programming stage. TEA-CART is also highly customizable. 
                  While the tool is pre-populated with default assumptions 
                  for both the U.S. overall, as well as each individual 
                  state. Most assumptions can be substituted with user-provided 
                  data, when available, so it can also be used by local or 
                  regional governments (e.g., Metropolitan Planning 
                  Organizations). </p>
                  <p>It is important to reiterate that TEA-CART is designed to 
                  inform planning-stage decision making, before highly specific, 
                  project-level information is available. For project-level 
                  analysis <a href='https://crp.trb.org/nchrpwebresource1/10-0transportation-systems-planning/'>
                  other tools</a> are available, like the 
                  <a href='https://www.fhwa.dot.gov/environment/air_quality/cmaq/toolkit/'> CMAQ Toolkit</a>.
                  <p><b>Inputs:</b> Inputs to the tool typically include those 
                  available during the state or MPO transportation capital 
                  program planning process. For example:</p>
                  <ul><li>New lane-miles of infrastructure,</li>
                  <li>Number of new electric vehicle chargers,</li>
                  <li>Number of diesel-powered buses replaced by electric vehicles,</li>
                  <li>Dollars invested in electric bike incentives, or</li>
                  <li>Dollars invested in roadway resurfacing.</li>
                  </ul>"
                ),
                # div(class = "about-datatable",
                #     style = "width: 100%; overflow-x: auto;",
                #     DTOutput("UI_tables")
                # ),
                HTML("
                  <p>The tool is designed to be updated in real-time, so that 
                  results can be seen as soon as new project information is 
                  provided by the user. </p>
                  <p><b>Outputs:</b> The tool generates valuable, customizable outputs, 
                  including:</p>
                  <ul><li>A baseline inventory and forecast of GHG emissions,</li>
                  <li>Estimated GHG emissions and related impacts of a capital 
                  program -- or a hypothetical set of capital projects -- 
                  across user-selected horizon years, and</li>
                  <li>Information on the cost-effectiveness of various project 
                  types.</li>
                  </ul>
                  "
                ),
                HTML("<i>To report a problem in the tool, please send an email to 
                    <u>climate@georgetown.edu</u> with “TEA-CART Help” in the subject 
                    line.</i>
                    <br><br>")
                
      ),
      
      
      # how-to page -------------------------------------------------------------
      nav_panel(title = "How-to",
                p(),
                h2("Guidance on using the tool"),
                HTML('
  <p>As you enter data, please keep the following in mind:</p>
  <ul>
    <li>This application does not require a login and is free to use.</li>
    <li>In the left side panel, there is a button to <b>Download User Inputs</b>. <b><i>It is essential that you download data prior to long periods of inactivity</i></b>. If you need to leave the application before completing the analysis, you can later upload the saved file to continue your work.</li>
    <li>Data are entered in tables. To begin entering (or editing) data, double click anywhere in the table using your mouse. When you are done entering data, press <b>CTRL + ENTER</b> on your keyboard (this initiates the calculation). Data can only be entered for one table at a time.</li>
    <li>When you are ready to see the results of your analysis, proceed to the <b>Scenarios</b> tab and select scenarios for analysis. Either choose groups of projects (to support a comparison) or click the button to select all projects for each scenario.</li>
  </ul>
  <p>Click <a href="TEA-CART How-to Guide_Final.pdf">here</a> to download a PDF copy of this guide.<br>
  <p>
  To learn more about how to use the tool and how it works, please see the <a href="TEACART User Guide and Methodology v.1.10.3.pdf">User Guide and Methodology Documentation</a>.<br>
  <p>
  <i>To report a problem in the tool, please send an email to <u>climate@georgetown.edu</u> 
  with “TEA-CART Help” in the subject line.</i>
  <p>

  <h2>Steps to use the tool</h2>
  <ol class="spaced-images">
    <li>Use the navigation panel at the top to select <b>Inputs</b>.</li>
    <br>
    <img src="how_to_step_1.svg" style="width:90%; max-width:intrinsic; height:auto;">
    <br><br><br>

    <li>Within <b>Inputs</b>, select <b>Baseline</b> to choose your state and enter the years used for the planning forecast. You may also change some parameters for the forecast, the scope of emissions to include (for example, whether to include certain upstream emissions), and other assumptions.
    Advanced users may choose to edit the downloaded data file directly, as an alternative method for generating user inputs to upload.</li>
    <br>
    <img src="how_to_step_2.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>
    
    <li>At the bottom of the <b>Baseline</b> tab, if you would like to provide project-level inputs, click on <b>Capital Projects</b> under the <b>Select Mode</b> dropdown, and proceed to <i>step 4</i>. 
    Alternatively, if you would like to provide budget-level inputs, click on <b>Budget</b> under the dropdown, and skip to <i>step 6</i> below.</li>
    <br>
    <img src="how_to_step_3.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>

    <li>Within <b>Projects</b>, enter information about each project. To begin entering (or editing) data, double click in the table using your mouse.</li>
    <br>
    <img src="how_to_step_4.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>

    <li>When you are done entering data, press <b>CTRL + ENTER</b> on your keyboard (note, your cursor must be in a cell, inside the table, when you do this). Data can only be entered for one table at a time.</li>
    <br>
    <img src="how_to_step_5.svg" style="width:90%; max-width:intrinsic; height:auto; display:block;">
    <br><br>

    <li>In the <b>Budget</b> tab, data can be entered in the same way as in the <b>Projects</b> tab. If you already entered data in the <b>Projects</b> tab, go to <i>step 8</i> below. If, instead, you would like 
    to provide budget-level inputs, double click with your mouse in the Budget tables to begin entering values. Once you are done, press <b>CTRL + ENTER</b> on your keyboard (note, again, your cursor must be in a cell, 
    inside the table, when you do this). As a reminder, data can only be entered one table at a time.</li>
    <br>
    <img src="how_to_step_6.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>

    <li>After you are done entering <b>Budget</b> values, you must click on <b>Fill Projects Tab with Budget Inputs</b> (at the top of the <b>Budget</b> tab), to initiate the calculation.</li>
    <br>
    <img src="how_to_step_7.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>

    <li>Regularly download the data that you have entered. You can download it by clicking the <b>Download User Inputs</b> button on the sidebar.</li>
    <br>
    <img src="how_to_step_8.svg" style="width:20%; max-width:intrinsic; height:auto; display:block;">
    <br><br>

    <li>If desired, within <b>Costs</b>, enter custom unit costs for the project type. Note that default values have already been provided.</li>
    <br>
    <img src="how_to_step_9.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>

    <li>Further <b>Assumptions</b> for the analysis are for advanced users. Data can be changed similar to the previous tabs. For more information about changing assumptions, 
    please refer to the User Guide and Methodology Documentation.</li>
    <br>
    <img src="how_to_step_10.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>

    <li>In the <b>Scenarios</b> tab, click the boxes to choose which groups of projects to include in the scenario analysis.</li>
    <br>
    <img src="how_to_step_11.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>

    <li>It is possible to use a custom forecast on the <b>Advanced</b> tab for the number of EVs on the road (for example, for states that have a goal to add one million EVs to the road by a certain deadline), future VMT, and other advanced parameters. 
    Refer to the User Guide and Methodology Documentation for more information.</li>
    <br>

    <li>To see the results of the data that have been entered, refer to the <b>Outputs</b> tab.</li>
    <br>
    <img src="how_to_step_13.svg" style="width:90%; max-width:100%; height:auto; display:block;">
    <br><br>

    <li>Click <b>Download Output Tables</b> in the sidebar to get an Excel file with all of this information.</li>
    <br>
    <img src="how_to_step_14.svg" style="width:20%; max-width:100%; height:auto; display:block;">
  </ol>
  <br><br>
')
      ),
      
      # inputs page -------------------------------------------------------------
      
      nav_panel(title = "Inputs",
                navset_card_pill(
                  id = "INPUTS_TABS",
                  
                  # baseline inputs ---------------------------------------------------------                  
                  nav_panel(title = "Baseline",
                            fluidRow(HTML("<p>Please enter <b>key inputs</b> below to define the timing and scope of your TEA-CART baseline and analysis, 
                                          including: <b>State</b>, <b>Base Year</b>, <b>Horizon Years</b>, <b>Geographic Scope</b>, and <b>Emissions Scope</b>.</p>
                                          <p>Be sure to indicate the form of inputs for your analysis 
                                          by selecting either <b><i>Capital Projects</i></b> or <b><i>Budget</i></b>, under <b>Select Mode</b>.</p><br>"
                            )),
                            # fluidRow(
                            #   DT::dataTableOutput("test_data")
                            # ),
                            #p("Select the state, years, and scope of baseline GHG forecast."),
                           fluidRow(
                              class = "baseline-content",
                              column(12,
                                     
                                     tabPanel(title = "Key Inputs"),
                                     tags$div(class = "well invisible-well",
                                              selectInput("state_input",
                                                          HTML("<span>State:</span><br><p>The state for which you are conducting analysis.</p>"),
                                                          selected = "Maryland",
                                                          c(state.name,"United States")),
                                              numericInput("base_year",
                                                           HTML(paste('<span>Base Year:</span> ',
                                                                      p("The first year of analysis and the reference point for assessing baseline trends."),
                                                                      sep = "")
                                                           ),
                                                           value = 2021,
                                                           min = 2021,
                                                           max = 2050,
                                                           step = 1),
                                              p(" "),
                                              numericInput("horizon_year_1",
                                                           HTML(paste('<span>Horizon Year 1:</span> ',
                                                                      p("The first (future) year for which projects may be entered and the first year for which TEA-CART will generate outputs."),
                                                                      sep = "")
                                                           ),
                                                           value = 2025,
                                                           min = 2021,
                                                           max = 2050,
                                                           step = 1),
                                              p(" "),
                                              numericInput("horizon_year_2",
                                                           HTML(paste('<span>Horizon Year 2:</span> ',
                                                                      p("The second (future) year for which projects may be entered and the second year for which TEA-CART will generate outputs."),
                                                                      sep = "")
                                                           ),
                                                           value = 2030,
                                                           min = 2021,
                                                           max = 2050,
                                                           step = 1),
                                              p(""),
                                              numericInput("horizon_year_3",
                                                           HTML(paste('<span>Horizon Year 3:</span> ',
                                                                      p("The third (future) year for which projects may be entered and the third year for which TEA-CART will generate outputs."),
                                                                      sep = "")
                                                           ),
                                                           value = 2050,
                                                           min = 2021,
                                                           max = 2050,
                                                           step = 1)),
                                     br(),
                                     tags$div(class = "well card-flex",
                                              tags$div(class = "half-card",
                                                       selectInput("transportation_scope",
                                                                   HTML("<span>Transportation System Scope:</span> <br> <p>There are two options:<br>
                                        All Roadways: Emissions from on-road travel on all state roadways.<br>
                                        NHS Only: Emissions from on-road travel on National Highway System (NHS) roadways only.<br>
                                        Note that TEA-CART only includes emissions from on-road travel."),
                                                                   c("All Roadways","NHS Only"),
                                                                   "All Roadways")
                                              ),
                                              tags$div(class = "half-card",
                                                       selectInput("vmt_forecast_input",
                                                                   HTML("<span>VMT Forecast:</span> <br> <p>The vehicle miles traveled (VMT) forecast used for baseline projections. A custom forecast can be entered in the Advanced tab under Inputs."),
                                                                   c("Default","Custom"),
                                                                   "Default")),           
                                     ),
                                     tags$div(class = "well invisible-well card-flex",
                                              tags$div(class = "half-card",
                                                       selectInput("scope_emissions",
                                                                   HTML("<span>Emissions Scope: Include Electricity:</span> <br> <p>The scope of transportation emissions reported. By default, all direct emissions (emissions occurring at the vehicle tailpipe) are reported.<br>
                                                                        Select 'Yes' for Include Electricity to report emissions associated with the electricity used to power electric vehicles."),
                                                                   choices = c("Yes" = 1, "No" = 0),
                                                                   selected = "Yes")),
                                              tags$div(class = "half-card",
                                                       selectInput("ev_baseline_input",
                                                                   HTML("<span>Vehicle Electrification Baseline:</span> <br> <p>The vehicle electrification forecast used for baseline projections. A custom forecast can be entered in the Advanced tab under Inputs."),
                                                                   c("AEO Baseline",
                                                                     "ACC",
                                                                     "ACC II",
                                                                     "ACC II + ACT",
                                                                     "Custom"),
                                                                   "AEO Baseline")),                                 
                                     ),
                                     tags$div(class = "well card-flex",
                                              tags$div(class = "half-card",
                                                       selectInput("scope_fuels",
                                                                   HTML("<span>Emissions Scope: Include Upstream Fuels:</span> <br> <p>Upstream Fuels refer to emissions associated with the production, extraction, and transportation of liquid and gaseous fuels including gasoline, diesel and CNG."),
                                                                   choices = c("Yes" = 1, "No" = 0),
                                                                   selected = "No")
                                              ),
                                              tags$div(class = "half-card",
                                                       selectInput("grid_emissions_input",
                                                                   HTML("<span>Electricity Grid Emissions Net-Zero Year:</span> <br> <p>The target year for achieving net-zero electricity grid emissions.<br>"),
                                                                   choices = c("No Change", 2021:2050),
                                                                   selected= 2050)
                                              ),
                                     ),
                                     tags$div(class = "well card-flex",
                                              tags$div(class = "half-card",
                                                       selectInput("land_use_factor",
                                                                   HTML("<span>Apply Land Use Multiplier to Transit Investment:</span> <br> <p>This multiplier represents total VMT reduction, including reductions related to more efficient land use patterns supported by transit, relative to the direct VMT reduction from increased transit ridership. See the User Guide and Methodology Documentation for further discussion."),
                                                                   choices = c("Yes" = 1, "No" = 0),
                                                                   selected = 0)
                                              ),
                                              tags$div(class = "half-card",
                                                       selectInput("include_rail",
                                                                   HTML("<span>Include Rail Emission:</span> <br> <p>Include rail in emissions evaluation.<br>"),
                                                                   choices = c("Yes" = 1, "No" = 0),
                                                                   selected = 0)),
                                     ),
                                     tags$div(class = "well card-flex",
                                              tags$div(class = "half-card",
                                                       selectInput("mode_choice",
                                                                   HTML("<span>Select Mode: </span> <br> <p>Choose between Capital Projects or Budget, to indicate whether you plan to input data under the Projects tab (using project-level information) or the Budget tab (i.e., dollar values).<br>"),
                                                                   choices = c("Capital Projects",
                                                                               "Budget"),
                                                                   selected = "Capital Projects")
                                              )
                                     )
                              )),
                  ),
                  
                  
                  # projects tab ui ---------------------------------------------------------
                  nav_panel(title = "Projects",
                            fluidRow(column(9,
                                            HTML("<p>Please provide <b>project-level inputs</b> for 
                                    one or more of the project categories shown 
                                    below. Make sure that <b><i>Capital Projects</i></b>
                                    is selected under <b><i>Select Mode</i></b>, in the <b>Baseline</b> tab.
                                    <p><b>Alternatively, go to the Budget tab</b> if you would like to provide <i>budget-level</i> inputs instead.
                                    <p>Keeping in mind the following two rules when entering project-level inputs:<br>
                                    <p>
                                    i) All projects are assumed to be “constructed” or “in operation” by the corresponding horizon year. For example, if the user inputs 2 miles of new bicycle lanes under the first horizon year (e.g., 2025) in the “New” category of additions / replacements, it is assumed that those bike lanes will be fully constructed by 2025.<br>
                                    <p>
                                    ii) Project inputs from one year are automatically coded to “carry over” into future years (i.e., miles of new bike lane constructed in 2010 are “carried over” into future years and continue to operate past their construction year).<br>
                                    <p>
                                    To see cumulative project totals (based on user inputs in this tab) go to the <b>Cumulative Projects Totals</b> tab under <b>Outputs</b>.
                                    <p>
                                   
                                   "
                                            )),
                                     column(3,
                                            actionButton("fill_budget_bttn", "Fill Budget Tab with Project Inputs", 
                                                         class = "btn-custom",
                                                         style = "width:100%; height:90%;"
                                            ))
                                     
                            ),
                            # fluidRow(class = "budget-buttons",
                            #          #actionButton("fill_projects_bttn", "Fill Project Tab with Budget Inputs", class = "btn-custom"),
                            #           actionButton("fill_budget_bttn", "Fill Budget Tab with Project Inputs", class = "btn-custom")
                            #          ),
                            # bike ped
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         'Projects 1 | Bicycle & Pedestrian Lane Miles of New Infrastructure ',
                                         HTML("This category represents implementation of any <b>two-way miles of new 
           bicycle or pedestrian facility.</b> The default assumption for these 
           project types is that any new bicycle or pedestrian facility would 
           be two-way. For one-way facilities, please enter half the 
           total miles for the facility."),
                                       ),
                                       id = "acc1",
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_bikeped_projs_tbl", "Reset Projects 1", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("bikeped_projs_tbl")
                              
                            ),
                            
                            
                            
                            # transit fixed route
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 2 | Transit: Increased Fixed Route Service",
                                         HTML("This category represents additions of <b>new fixed route service <a href = 'https://www.transit.dot.gov/ntd/national-transit-database-ntd-glossary'>vehicles operated in maximum service (VOMS)</a>.</b> 
               Fixed route service vehicles include vehicles operated along a prescribed route according to a fixed schedule."
                                         ),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_fixed_projs_tbl", "Reset Projects 2", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_fixed_projs_tbl")
                            ),
                            
                            
                            # transit DR
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 3 | Transit: Increased Demand Response Service",
                                         HTML("This category represents addition of any <b>new demand response service vehicles operated in maximum service (VOMS).</b> 
              Demand response service vehicles include non-fixed route services that are initiated by customers and require advanced scheduling, 
                        such as vehicles provided by public entities, nonprofits, and private providers."
                                         ),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_dr_projs_tbl", "Reset Projects 3", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_dr_projs_tbl")
                            ),
                            
                            # fleet electrification
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 4 | Transit: Fleet Electrification",
                                         HTML("This category represents the <b>replacement of any fossil-fueled vehicles with an electric vehicle</b>, 
                        with the assumption that any new vehicle is again replaced by the new technology type at the end of its life cycle. "),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_el_projs_tbl", "Reset Projects 4", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_el_projs_tbl")
                            ),
                            
                            # bus priority
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 5 | Public Transportation: Bus Priority Treatment",
                                         HTML("This category represents addition of miles of <b>new bus priority treatment</b>. Bus priority treatment refers 
                        to the improvement of transit speed and reliability between stops by changing the designation of street space. 
                        Some examples include a bus-only lane, which assigns exclusive street space to buses, and a bus approach lane, 
                        which assigns exclusive street spaces to buses as they approach an intersection."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_bus_projs_tbl", "Reset Projects 5", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_bus_projs_tbl")
                            ),
                            
                            # public transportation - rail
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 6 | Public Transportation: Rail",
                                         HTML("This category represents addition of any 
                                              <b>new rail vehicles operating in maximum 
                                              service (VOMS)</b>, including service on 
                                              light rail or streetcar lines, heavy 
                                              rail, and commuter rail.",),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_public_rail_projs_tbl", "Reset Projects 6", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("public_rail_projs_tbl")
                            ),
                            
                            
                            # TDM
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 7 | Travel Demand Management (TDM)",
                                         HTML("This category represents the <b>number of employees covered through the TDM Program Outreach</b>. 
                   TDM programs are designed to shift travel demand and change traveler behavior, with the goal of 
                   reducing single-occupancy vehicle travel and encouraging the use of public transit, walking, biking, teleworking, and ridesharing. ",),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_tdm_projs_tbl", "Reset Projects 7", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("tdm_projs_tbl")
                            ),
                            
                            # Micromobility
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 8 | Micromobility",
                                         HTML("This category represents the <b>number of e-bikes funded</b> through the implementation of <b> e-bike subsidies</b>. 
                   An e-bike subsidy reimburses part of the cost of an e-bike. All dollar values should be in <b>current year dollars.</b>"),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_micro_projs_tbl", "Reset Projects 8", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("micro_projs_tbl")
                            ),
                            
                            # traffic ops
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 9 | Traffic Operations - Intersection Improvements ",
                                         HTML("This category represents any <b>improvements made to traffic operations at intersections</b>, such as new or retimed signals or new traffic-flow roundabouts."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_traffic_ops_projs_tbl", "Reset Projects 9", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("traffic_ops_projs_tbl")
                            ),
                            
                            # MHDEV
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 10 | Medium- and Heavy-Duty Vehicle (MHDV) Replacement",
                                         HTML("This category represents <b>replacement of any fossil fuel medium- or heavy-duty vehicles with electric vehicles</b>, 
                   with the assumption that any new vehicle is again replaced by the new technology type at the end of its life cycle."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_mhdev_projs_tbl", "Reset Projects 10", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("mhdev_projs_tbl")
                            ),
                            
                            
                            # Park & Ride
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 11 | Park-and-Ride",
                                         HTML("This category represents any <b>new addition or expansion of Park-and-Ride spaces</b>. A Park-and-Ride space 
                   allows private transport users to park their vehicles at a large parking space and continue their commute via public transport."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_pnr_projs_tbl", "Reset Projects 11", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("pnr_projs_tbl")
                            ),
                            
                            # EVSI
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 12 | Charging Infrastructure and EV Incentives ",
                                         HTML("This category represents any <b>new addition or expansion of EV charging ports and EV purchase incentives</b>. 
                   EV charging ports supply electric power for recharging electric vehicles. EV incentives offset the cost of EVs for purchasers. All dollar values should be in <b>current year dollars.</b> Dedicated chargers (e.g., DCFC: Dedicated truck/bus) are <i>not</i> for public use."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_evsi_projs_tbl", "Reset Projects 12", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("evsi_projs_tbl")
                            ),
                            
                            # freight intermodal facilities
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 13 | Freight Intermodal Facilities",
                                         HTML("This category represents the assumed 
                                         annual growth rate 
                                              of freight rail, the energy intensity as 
                                              measured in British Thermal Units (BTU) 
                                              per ton-mile, and the change in annual 
                                              VMT or ton-miles per unit of investment. All dollar values should be in <b>millions</b> of <b>current year dollars.</b>"),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_freight_projs_tbl", "Reset Projects 13", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("freight_projs_tbl")
                            ),
                            
                            
                            # Roadway expansion
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 14 | Roadway Expansion",
                                         HTML("This category represents addition of any <b>new lane-miles of roadways,</b> 
                                              based  on the facility type of the roadway 
                                              and the area type of the facility."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_expansion_projs_tbl", "Reset Projects 14", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("expansion_projs_tbl")
                            ),
                            
                            #Roadway Surfacing
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 15 | Roadway Resurfacing",
                                         HTML("This category indicates lane miles resurfaced to <b>reduce surface roughness and decrease rolling resistance</b> on roadways."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_road_resurf_projs_tbl", "Reset Projects 15", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("road_resurf_projs_tbl")
                            ),
                            
                            #Land Use
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 16 | Land Use",
                                         HTML("This category represents spending and 
                                         rezoned acres in support of <b>walkable, 
                                              transit-oriented development (TOD) areas</b>, using land use strategies such as placing destinations closer together and in environments more conducive to transit and non-motorized travel. All dollar values should be in <b>current year dollars.</b>"),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_land_use_projs_tbl", "Reset Projects 16", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("land_use_projs_tbl")
                            ),
                            
                            #Transit Cuts
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 17 | Transit Service Cuts",
                                         HTML("This category represents <b>cuts to transit service</b> from current service levels, resulting from funding cuts.</b>"),
                                         # bslib::card_footer(popover(trigger = tags$span("See Cumulative", style = "color: blue; text-decoration: underline;"), 
                                         #                            title = "Cumulative View",
                                         #                            placement = "bottom",
                                         #                            options = list(container = "body"),
                                         #                            DTOutput(outputId = "cumul_transit_cuts_projs_tbl")
                                         # ))
                                         # FLAG this is the only place the cumulative calculation still exists
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_cuts_projs_tbl", "Reset Projects 17", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_cuts_projs_tbl")
                            ),
                            
                            
                            
                            
                  ),
                  
                  
                  
                  # budget tab ui -----------------------------------------------------------
                  
                  nav_panel(title = "Budget",
                            fluidRow(
                              column(9,
                                     HTML("<p> Please provide <b>budget-level inputs</b> for one 
                        or more of the project categories shown below. Make sure that <b><i>Budget</i></b>
                        is selected under <b><i>Select Mode</i></b>, in the <b>Baseline</b> tab.
                        <p><b>Alternatively, go to the Projects tab</b> if you would like to provide <i>project-level</i> inputs instead.
                        <p>Outputs are not generated automatically. After filling out the budget <b> click on the <i>Fill Project Tab 
                        with Budget Inputs</i> button in order to generate outputs</b>.
                        <p>Keep in mind the following three rules when entering 
                        budget-level inputs:<br>
                        <p>
                        i) All spending is assumed to be equally distributed 
                        over the years of spending by the corresponding 
                        horizon year.<br>
                        ii) Spending estimates are assumed to be averages 
                        based on historical data.<br>
                        iii) For some strategies there is a multi-year lag between the first year of investment and the first year in 
                        which the tool will estimate changes in emissions or other outputs.<br>
                        <br>
                        
                       ")
                                     
                              ),
                              column(3,
                                     actionButton("fill_projects_bttn", "Fill Projects Tab with Budget Inputs",
                                                  class = "btn-custom",
                                                  style = "width:100%; height:90%;")
                              )
                            ),
                            # fluidRow(class = "budget-buttons",
                            #          actionButton("fill_projects_bttn", "Fill Project Tab with Budget Inputs", class = "btn-custom")#,
                            #          #actionButton("fill_budget_bttn", "Fill Budget Tab with Project Inputs", class = "btn-custom")
                            #          ),
                            fluidRow( class = "budget-inputs",
                                      numericInput("budget_start_year",
                                                   HTML(paste('Funding Start Year: ',
                                                              as.character(tags$i(class = "fa fa-info-circle", 
                                                                                  title = "The first year of spending.")),
                                                              sep = " ")
                                                   ),
                                                   value = 2026,
                                                   min = 2020,
                                                   max = 2050,
                                                   step = 1
                                      ),
                                      numericInput("budget_years_covered",
                                                   HTML(paste('Total Years Covered: ',
                                                              as.character(tags$i(class = "fa fa-info-circle", 
                                                                                  title = "The total years over which spending occurs.")),
                                                              sep = " ")
                                                   ),
                                                   value = 5,
                                                   min = 1,
                                                   max = 20,
                                                   step = 1
                                      ),
                                      
                                      numericInput("budget_total",
                                                   HTML(paste0('Allocated Budget: ',
                                                               as.character(tags$i(class = "fa fa-info-circle",
                                                                                   title = "This number should be equal to 100.")),
                                                               sep = " ")
                                                   ),
                                                   value = 100
                                      ),
                                      ## FUNDING SUMMARY
                                      
                            ),
                            fluidRow(
                              DT::dataTableOutput("funding_summary_tbl"),
                              p(),
                              p(),
                              
                            ),
                            
                            
                            
                            ## BUDGET TABLES
                            
                            # bike ped - 1
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 1 | Bicycle &
           Pedestrian Lane Miles of New Infrastructure ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on bike/ped projects.")),
                                                    sep = "")),
                                         HTML("This category represents spending on any <b>two-way miles of new 
           bicycle or pedestrian facility</b>, expressed as a percentage (%) of the total budget (shown at the top of this tab). The default assumption for these 
           project types is that any new bicycle or pedestrian facility would 
           be two-way. For one-way facilities, please enter half the 
           budget amount."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_bikeped_budget_tbl", "Reset Budget 1", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("bikeped_budget_tbl")
                              
                            ),
                            p(),
                            # transit - 2
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 2 | Transit: Increased Fixed Route Service ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on transit fixed route service.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>new 
                                         fixed route service <a href = 'https://www.transit.dot.gov/ntd/national-transit-database-ntd-glossary'>vehicles operated in maximum service (VOMS)</a></b>, 
                                         expressed as a percentage (%) of the total budget (shown at the top of this tab). 
               Fixed route service vehicles include vehicles operated along a prescribed route according to a fixed schedule."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_fr_budget_tbl", "Reset Budget 2", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_fr_budget_tbl")
                              
                            ),
                            p(),
                            # transit demand response - 3
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 3 | Transit: Increased Demand Response Service ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on transit demand response service.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>new demand 
                                         response service vehicles operated in maximum service (VOMS)</b>,
                                         expressed as a percentage (%) of the total budget (shown at the top of this tab).
                                         Demand response service vehicles include non-fixed route 
                                         services that are initiated by customers and require 
                                         advanced scheduling, such as vehicles provided by public
                                              entities, nonprofits, and private providers."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_dr_budget_tbl", "Reset Budget 3", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_dr_budget_tbl")
                              
                            ),
                            p(),
                            # transit elec - 4
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 4 | Transit: Fleet Electrification ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on transit fleet electrification.")),
                                                    sep = "")),
                                         HTML("This category represents spending toward the <b>replacement 
                                         of any fossil-fueled 
                                              vehicles with an electric vehicle</b>,
                                              expressed as a percentage (%) of the total budget
                                              (shown at the top of this tab). Any new vehicles are 
                                              assumed to be replaced again by the 
                                              new technology type at the end of its life cycle."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_elec_budget_tbl", "Reset Budget 4", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_elec_budget_tbl")
                              
                            ),
                            p(),
                            # transit bus priority - 5
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 5 | Public Transportation: Bus Priority Treatment ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on bus priority treatment.")),
                                                    sep = "")),
                                         HTML("This category represents spending on miles 
                                              of <b>new bus priority treatment</b>, 
                                              expressed as a percentage (%) of the total 
                                              budget (shown at the top of this tab). Bus priority 
                                              treatment refers to the improvement of 
                                              transit speed and reliability between 
                                              stops by changing the designation of street 
                                              space. Some examples include a bus-only 
                                              lane, which assigns exclusive street 
                                              space to buses, and a bus approach lane, 
                                              which assigns exclusive street spaces to 
                                              buses as they approach an intersection."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_bus_priority_budget_tbl", "Reset Budget 5", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_bus_priority_budget_tbl")
                              
                            ),
                            p(),
                            # rail - 6
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 6 | Public Transportation: Rail ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on rail projects.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>new rail vehicles operating in maximum service (VOMS)</b>, expressed as a percentage (%) of the total budget (shown at the top of this tab)."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_rail_budget_tbl", "Reset Budget 6", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("rail_budget_tbl")
                              
                            ),
                            p(),
                            # tdm - 7
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 7 | Travel Demand Management (TDM) ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on travel demand management.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>employment 
                                              covered through the TDM Program Outreach</b>, expressed as a percentage (%) of the total budget (shown at the top of this tab). TDM programs 
                                              are designed to shift travel demand and change traveler behavior, 
                                              with the goal of reducing single-occupancy vehicle travel and 
                                              encouraging the use of public transit, walking, biking, 
                                              teleworking, and ridesharing."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_tdm_budget_tbl", "Reset Budget 7", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("tdm_budget_tbl")
                              
                            ),
                            p(),
                            # micromobility - 8
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 8 | Micromobility ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on micromobility.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>e-bike subsidies</b>, 
                                         expressed as a percentage (%) of the total budget (shown at the 
                                         top of this tab). 
                                              An e-bike subsidy reimburses part of the cost of an e-bike."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_micromobility_budget_tbl", "Reset Budget 8", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("micromobility_budget_tbl")
                              
                            ),
                            p(),
                            # traffic ops - 9
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 9 | Traffic Operations - Intersection Improvements ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on traffic operations.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>improvements 
                                              made to traffic operations at intersections</b>, 
                                              expressed as a percentage (%) of the total budget 
                                              (shown at the top of this tab).
                                              Examples of such traffic operations include new or retimed signals or new traffic-flow roundabouts."), # Examples of such traffic operations include new or retimed signals or new traffic-flow roundabouts.”
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_traffic_ops_budget_tbl", "Reset Budget 9", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("traffic_ops_budget_tbl")
                              
                            ),
                            p(),
                            # mdhd - 10
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 10 | Medium- and Heavy-Duty Vehicle (MHDV) Replacement ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on medium- and heavy-duty vehicle replacement.")),
                                                    sep = "")),
                                         HTML("This category represents spending toward <b>replacement 
                                              of any fossil fuel medium- or heavy-duty 
                                              vehicles with electric vehicles</b>, expressed as a 
                                              percentage (%) of the total budget (shown at the 
                                              top of this tab). Any new vehicles are assumed to 
                                              be replaced again by the new technology type at the end of its life cycle."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_mdhd_budget_tbl", "Reset Budget 10", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("mdhd_budget_tbl")
                              
                            ),
                            p(),
                            # pnr - 11
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 11 | Park-and-Ride ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on Park-and-Ride projects.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>new 
                                              addition or expansion of Park-and-Ride spaces</b>, 
                                              expressed as a percentage (%) of the total budget 
                                              (shown at the top of this tab). A Park-and-Ride 
                                              space allows private transport users to park 
                                              their vehicles at a large parking space and continue 
                                              their commute via public transport."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_pnr_budget_tbl", "Reset Budget 11", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("pnr_budget_tbl")
                              
                            ),
                            p(),
                            # ev - 12
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 12 | Charging Infrastructure and EV Incentives ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on new or expanded EV charging ports.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>new 
                                              or expanded EV charging ports and EV purchase incentives</b>, expressed 
                                              as a percentage (%) of the total budget (shown 
                                              at the top of this tab). EV charging ports 
                                              supply electric power for recharging 
                                              electric vehicles. EV incentives 
                                              offset the cost of EVs for purchasers.  Dedicated chargers (e.g., DCFC: Dedicated truck/bus) are <i>not</i> for public use."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_ev_budget_tbl", "Reset Budget 12", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("ev_budget_tbl")
                              
                            ),
                            p(),
                            # freight - 13
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 13 | Freight Intermodal Facilities ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on freight intermodal facilities.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>intermodal 
                                              freight investments</b>, expressed as a percentage (%) 
                                              of the total budget (shown at the top of this tab)."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_freight_budget_tbl", "Reset Budget 13", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("freight_budget_tbl")
                              
                            ),
                            p(),
                            # roadway expansion - 14
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 14 | Roadway Expansion ',
                                                    as.character(tags$i(class = "fa fa-info-circle", 
                                                                        title = "Budget spending on roadway expansion.")),
                                                    sep = "")),
                                         HTML("This category represents spending on <b>new lane-miles of roadways,</b> expressed as a percentage (%) of the total budget (shown at the top of this tab)."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_expansion_budget_tbl", "Reset Budget 14", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("expansion_budget_tbl")
                              
                            ),
                            p(),
                            # land use - 15
                            # fluidRow(
                            #   column(10,
                            #          accordion(
                            #            accordion_panel(
                            #              HTML(paste('Budget 15 | Land Use ',
                            #                         as.character(tags$i(class = "fa fa-info-circle", 
                            #                                             title = "Budget spending on land use.")),
                            #                         sep = "")),
                            #              HTML("This category represents additional spending 
                            #                   toward more <b>compact and transit-oriented, walkable 
                            #                   development</b>, expressed as a percentage (%) of the total budget (shown at the top of this tab). Dollars in this category represent 
                            #                   incentives for development in walkable, transit-oriented 
                            #                   development (TOD) areas."),
                            #            ),
                            #            open = TRUE
                            #          ),
                            #   ),
                            #   column(2,
                            #          actionButton("reset_land_use_budget_tbl", "Reset Budget 15", class = "btn-custom"),
                            #   ),
                            # ),
                            # fluidRow(
                            #   DT::dataTableOutput("land_use_budget_tbl")
                            #   
                            # ),
                            # p(),
                            
                            #                           roadway resurfacing - 15
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 15 | Roadway Resurfacing ',
                                                    as.character(tags$i(class = "fa fa-info-circle",
                                                                        title = "Budget spending on roadway resurfacing.")),
                                                    sep = "")),
                                         HTML("This category includes spending to <b>reduce surface 
                                         roughness and decrease rolling resistance on 
                                              roadways,</b> expressed as a percentage (%) of 
                                              the total budget (shown at the top of this tab)."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_resurfacing_budget_tbl", "Reset Budget 15", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("resurfacing_budget_tbl")
                              
                            ),
                            
                            p(),
                            
                            #                           land use - 16
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         HTML(paste('Budget 16 | Land Use ',
                                                    as.character(tags$i(class = "fa fa-info-circle",
                                                                        title = "Budget spending on Land Use")),
                                                    sep = "")),
                                         HTML("This category includes spending to <b>land use incentives,</b> expressed as a percentage (%) of the total budget (shown at the top of this tab)."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_land_use_budget_tbl",
                                                  "Reset Budget 16", class = "btn-custom"),
                              ),
                            ),
                            fluidRow(DT::dataTableOutput("land_use_budget_tbl")),
                            p(),
                            
                  ),
                  
                  
                  # costs tab ui ------------------------------------------------------------
                  nav_panel(title = "Costs",
                            fluidRow(HTML("<p>This tab provides information on 
                              the <b>cost inputs</b> for the project categories 
                              shown below. 
                              <br><br>
                              All costs are already populated with default values. 
                              <b>Users do not need to edit this tab in order for the tool to work.</b>
                              <br><br>
                              <b>To edit cost information</b>, please click on the different fields 
                              to overwrite the default values with any custom 
                              values provided by the user.<br>
                                          <br>
                                          Note that there is no <i>Costs 4</i> or <i>Costs 17</i>. 
                                          Costs related to transit electrification 
                                          can be found in <i>Costs 2</i> (with other transit vehicle costs). 
                                          Costs related to transit service cuts  
                                          cannot be updated by the user.<br>
                                          <br>
                                          All dollar values are in <b>2024 dollars.</b><br>
                                          <br>")),
                            
                            # bike ped costs
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 1 | Bicycle & Pedestrian Costs",
                                         HTML("This category represents the <b>overall cost per mile</b> of bicycle or pedestrian facilities being implemented."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_bikeped_costs_tbl", "Reset Costs 1", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("bikeped_costs_tbl")
                            ),
                            
                            
                            # transit fixed route costs
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 2 | Transit: Fixed Route Service Costs",
                                         HTML("This category represents the <b>capital cost per vehicle, operation and maintenance (O&M) cost per vehicle revenue miles (VRM)</b>, and <b>fuel cost per VRM</b> for addition of any new fixed route service vehicles."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_fixed_costs_tbl", "Reset Costs 2", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_fixed_costs_tbl")
                            ),
                            
                            
                            # transit DR costs
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 3 | Transit: Demand Response (DR) Service Costs",
                                         HTML("This category represents the <b>capital cost per vehicle, operation and maintenance (O&M) cost per vehicle revenue miles (VRM)</b>, and <b>fuel cost per VRM</b> for addition of any new demand response service vehicles."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_dr_costs_tbl", "Reset Costs 3", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_dr_costs_tbl")
                            ),
                            
                            
                            # bus priority costs
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 5 | Public Transportation: Bus Priority Treatment Costs",
                                         HTML("This category represents the <b>cost per mile of red paint</b> for addition of miles of new bus priority treatment."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_pub_trans_priority_costs_tbl", "Reset Costs 5", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("pub_trans_priority_costs_tbl")
                            ),
                            
                            
                            # pub trans rail costs
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 6 | Public Transportation: Rail Costs",
                                         HTML("This category represents the <b>capital cost per vehicle, operation and maintenance (O&M) cost per vehicle revenue miles (VRM)</b>, and <b>fuel cost per VRM</b> for addition of any new rail vehicles operating in annual maximum service (VOMS)."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_pub_trans_rail_costs_tbl", "Reset Costs 6", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("pub_trans_rail_costs_tbl")
                            ),
                            
                            
                            # tdm costs
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 7 | Travel Demand Management (TDM) Costs",
                                         HTML("This category represents the <b>cost per employee</b> of the TDM Program Outreach."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_tdm_costs_tbl", "Reset Costs 7", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("tdm_costs_tbl")
                            ),
                            
                            
                            # micromobility costs
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 8 | Micromobility Costs",
                                         HTML("This category represents the <b>subsidy provided per e-bike.</b>"),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_micro_costs_tbl", "Reset Costs 8", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("micro_costs_tbl")
                            ),
                            
                            
                            
                            # traffic ops costs
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 9 | Traffic Operations - Intersection Improvement Costs",
                                         HTML("This category represents the <b>cost per improvement</b> for any improvements made to traffic operations at intersections."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_traffic_ops_costs_tbl", "Reset Costs 9", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("traffic_ops_costs_tbl")
                            ),
                            
                            
                            # mhdv replacement costs
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 10 | Medium- and Heavy-Duty Vehicle (MHDV) Replacement Costs",
                                         HTML("This category represents the <b>capital cost per vehicle</b> for all medium- and heavy-duty vehicles replaced with new electric vehicles."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_mhdev_costs_tbl", "Reset Costs 10", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("mhdev_costs_tbl")
                            ),
                            
                            
                            # p&r costs
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 11 | Park-and-Ride Costs",
                                         HTML("This category represents the <b>cost per space</b> for any new addition or expansion of Park-and-Ride spaces."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_pnr_costs_tbl", "Reset Costs 11", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("pnr_costs_tbl")
                            ),
                            
                            
                            # evsi costs
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 12 | Charging Infrastructure Costs ",
                                         HTML("This category represents the <b>hardware cost per port</b> and <b>installation cost per port</b> for any new addition or expansion of EV charging ports."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_evsi_costs_tbl", "Reset Costs 12", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("evsi_costs_tbl")
                            ),
                            
                            # intermodal freight investment costs
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 13 | Intermodal Freight Investment Costs",
                                         HTML("This category represents the <b>cost of any intermodal investment.</b>"),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_intermodal_costs_tbl", "Reset Costs 13", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("intermodal_costs_tbl")
                            ),
                            
                            # roadway expansion costs
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 14 | Roadway Expansion Costs",
                                         HTML("This category represents the <b>capital cost per lane-mile</b> and <b>annual maintenance cost per lane-mile</b> for addition of any new lane-miles of roadways."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_roadway_expand_costs_tbl", "Reset Costs 14", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("roadway_expand_costs_tbl")
                            ),
                            
                            #roadway resurfacing 
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 15 | Roadway Resurfacing",
                                         HTML("This category represents the <b>cost per lane-mile</b> of improving roadway surfaces, based on 20xx data."), #AHFLAG
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_roadwayresurf_costs_tbl", "Reset Costs 15", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("roadwayresurf_costs_tbl")
                            ),
                            
                            #land use
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 16 | Land Use",
                                         HTML("This category represents the land use incentive portion of the land use projects. Rezoning projects have no established cost and are not included as part of the cost effectiveness outputs. This category specifically represents the <b>cost per shifted household</b>."), #AHFLAG
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_landuse_costs_tbl", "Reset Costs 16", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("landuse_costs_tbl")
                            ),
                            
                            # fuel price costs
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Costs 18 | Fuel Price",
                                         HTML("This category represents the <b>cost per unit of fuel</b>, based on 2022 data."), #AHFLAG
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_fuel_costs_tbl", "Reset Costs 16", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("fuel_costs_tbl")
                            ),
                            
                            
                            
                  ),
                  
                  # assumptions tab ui ------------------------------------------------------
                  nav_panel(title = "Assumptions",
                            fluidRow(HTML("<p>This tab provides information on 
                                          the <b>input assumptions</b> for the 
                                          categories shown below. These assumptions 
                                          affect the GHG impact and effectiveness of 
                                          each strategy category. To better understand which 
                                          assumptions relate to which strategies, refer to 
                                          the User Guide & Methodology Documentation.
                                          <br><br>
                                          All assumptions are already populated with default values. 
                                          <b>Users do not need to edit this tab in order for the tool to work.</b>
                                          <br><br>
                                          <b>To edit assumptions</b>, please click on the 
                                          different fields to overwrite the default 
                                          values with any custom values provided by the user.<br>
                                          <br>")
                                     
                            ),
                            
                            # bike ped parameters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 1 | Bicycle & Pedestrian Parameters",
                                         HTML("<p>
                                  This category includes the following data types:<br>
                                  <p>
                                  (i) <b>Prior drive mode share of new walkers / bikers</b>, which represents the fraction of new walkers / bikers who would have previously driven a car. This is calculated as a percentage of the new walkers / bikers or “persons per square mile” (ppsm) within the different area types defined.<br>
                                  <p>
                                  (ii) <b>Average trip length</b>, which represents the average Walk and / or Bike trip length in miles.<br>
                                  <p>
                                  (iii) <b>Annualization</b>, which represents the annual number of days multiplier for the mode-shift to walking / biking.")
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_bikeped_assmps_tbl", "Reset Assumptions 1", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("bikeped_assmps_tbl")
                            ),
                            
                            
                            # transit parameters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 2 | Transit Parameters",
                                         HTML("
                                  <p>This category includes the following data types for transit vehicles:<br>
                                  <p>
                                  (i) <b>On-Road Vehicle Fuel Economy</b>, which represents the miles a transit vehicle is able to travel based on its fuel source. This is calculated in miles per gallon of gasoline equivalent (mpgge).<br>
                                  <p>
                                  (ii) <b>Average trip length</b>, which represents the average length of the personal vehicle trip displaced by transit, in miles.<br>
                                  <p>
                                  (iii) <b>Average pax-mi per vehicle-mile (load factor)</b>, which represents the average number of passengers on board a transit vehicle at any given time. This is calculated as the ratio of passenger miles traveled to transit vehicle miles traveled, also known as the load factor.<br>
                                  <p>
                                  (iv) <b>Prior drive mode share of new riders</b>, which represents the fraction of new transit riders who would have previously driven a car. This is calculated as a the number of new riders who would have driven, divided by the total number of new riders.<br>
                                  <p>
                                  (v) <b>Vehicle Revenue Mile per Vehicle</b>, which represents the annual miles that a transit vehicle is scheduled to travel or actually travels while in revenue service.<br>
                                  <p>
                                  (vi) <b>Bus Priority Factors</b>, which include bus priority % travel time change, bus ridership elasticity with respect to travel time, number of routes affected, number of daily buses per route, percentage of route-hours affected, and weekday annualization."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_transit_assmps_tbl", "Reset Assumptions 2", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_assmps_tbl")
                            ),
                            
                            
                            # tdm parameters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 3 | Travel Demand Management (TDM) Parameters",
                                         HTML("
                                  <p>
                                  This category represents the <b>travel demand management data</b> from the Commuter Reduction Program, which includes the average reduction in one-person-vehicle driving, average work trip length during automobile use, and the corresponding annualization factor."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_tdm_assmps_tbl", "Reset Assumptions 3", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("tdm_assmps_tbl")
                            ),
                            
                            
                            # micromobility parameters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 4 | Micromobility Parameters",
                                         HTML("
                                  <p>
                                  <b>Micromobility Data</b>: This category represents the strategy parameters corresponding to E-bike subsidies, including e-bike cost, subsidy coverage of cost, bike trips per week, average trip length, and share of e-bikers previously driving automobiles."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_micro_assmps_tbl", "Reset Assumptions 4", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("micro_assmps_tbl")
                            ),
                            
                            
                            # traffic ops paramaters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 5 | Traffic Operations & Roadway Expansion Parameters",
                                         HTML("
                                  <p>
                                  This category includes the following strategy parameters related to <b>traffic operations and roadway expansion</b>:<br>
                                  <p>
                                  (i) <b>Annual Average Daily Traffic (AADT)</b>, which represents the total volume of vehicle traffic on a road or highway averaged over 365 days.<br>
                                  <p>
                                  (ii) <b>Percent Truck Traffic (%)</b>, which represents the percentage of trucks in each facility type.<br>
                                  <p>
                                  (iii) <b>VMT per lane-mile</b>, which represents the vehicle miles traveled per lane of roadway in each facility type.<br>
                                  <p>
                                  (iv) <b>Travel Speed</b>, which represents the average travel speed of vehicles in miles per hour in each facility type.<br>
                                  <p>
                                  (v) <b>Induced Travel Elasticities</b>, which are the percent change in VMT with respect to percent change in lane-miles or travel time for each facility type."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_traffic_ops_assmps_tbl", "Reset Assumptions 5", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("traffic_ops_assmps_tbl")
                            ),
                            
                            
                            # MHDV parameters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 6 | Medium- & Heavy-Duty Vehicle (MHDV) Replacement Parameters",
                                         HTML("
                                  <p>
                                  This category represents the <b>miles driven per replaced medium- and heavy-duty vehicle per year</b>.
                                  "),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_mhdv_assmps_tbl", "Reset Assumptions 6", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("mhdv_assmps_tbl")
                            ),
                            
                            
                            # P&R parameters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 7 | Park-and-Ride Parameters",
                                         HTML("
                                  <p>
                                  This category represents the <b>utilization of Park-and-Ride spaces.</b>"),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_pnr_assmps_tbl", "Reset Assumptions 7", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("pnr_assmps_tbl")
                            ),
                            
                            
                            # evsi parameters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 8 | Charging Infrastructure and EV Incentives Parameters",
                                         HTML("This category represents the <b>elasticity of the number of vehicle sales with respect to the number of charging ports</b>."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_evsi_assmps_tbl", "Reset Assumptions 8", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("evsi_assmps_tbl")
                            ),
                            
                            # land use parameters
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Assumptions 9 | Land Use (Smart Growth) Incentives Parameters",
                                         HTML("This category represents <b>new spending per household shifted to a 'smart growth' area.</b>"),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_landuse_assmps_tbl", "Reset Assumptions 9", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              DT::dataTableOutput("landuse_assmps_tbl")
                            )
                            
                            
                            
                            
                  ),
                  
                  # scenarios tab ui --------------------------------------------------------
                  nav_panel(title = "Scenarios",
                            fluidRow(HTML("<p>This tab enables the user to select different 
                              combinations of strategies – based on <i>Project</i> or <i>Budget</i> 
                              inputs – to include in the results displayed on the Outputs tabs. 
                              <br><br>
                              The user can select up to two “scenarios” 
                              with different combinations of project or budget inputs.<br>
                              <br>")),
                            fluidRow(
                              p(""),
                              h3("Scenario Selections"),
                              DT::DTOutput("scenario_tbl")
                            ),
                            fluidRow(
                              column(8, offset = 2, align = "right", actionButton("select_all_scenario1", "Select All for Scenario 1")),
                              column(2, align = "right", actionButton("select_all_scenario2", "Select All for Scenario 2"))
                            ),
                  ),
                  
                  
                  
                  # advanced tab ui ---------------------------------------------------------
                  nav_panel(title = "Advanced",
                            HTML("<p>This tab provides users the ability to enter 
                                 custom VMT forecasts and custom EV adoption 
                                 forecasts, as well as other advanced 
                                 parameters, including fuel mixes for specified 
                                 vehicle categories and construction and 
                                 maintenance emissions.</p><br>"),
                            
                            # custom forecast advanced
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Advanced 1 | Custom Forecast: Electric Vehicles (EVs)",
                                         HTML("This represents the percentage of on-road vehicles (stock) that are EVs.")
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_ev_forecast_sheet_tbl", "Reset Advanced 1", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("ev_forecast_sheet_tbl")
                            ),
                            
                            
                            # VMT Custom forecast
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Advanced 2 | Custom Forecast: Vehicle Miles Traveled (VMT)",
                                         HTML("This represents the vehicle miles traveled (VMT) forecast used for baseline projections."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_vmt_forecast_sheet_tbl", "Reset Advanced 2", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("vmt_forecast_sheet_tbl")
                            ),
                            
                            
                            # Onroad Public Transit - Fuel Technology Fraction
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Advanced 3 | Onroad Public Transit - Fuel Technology Fraction",
                                         HTML("This represents the type and fraction of assumed fuel technologies used by on-road public transit."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_onroad_fuel_tech_frac_sheet_tbl", "Reset Advanced 3", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              h3(""),
                              DT::dataTableOutput("onroad_fuel_tech_frac_sheet_tbl")
                            ),
                            
                            # passenger rail
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Advanced 4 | Passenger Rail",
                                         HTML("This represents the type of assumed fuel technologies used by passenger rail systems."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_pass_rail_sheet_tbl", "Reset Advanced 4", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("pass_rail_sheet_tbl")
                            ),
                            
                            # Freight Rail
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Advanced 5 | Freight Rail",
                                         HTML("This represents the assumed annual 
                                              growth rate of freight rail, the energy 
                                              intensity as measured in British 
                                              thermal units (BTU) per ton-mile, and 
                                              the change in annual VMT or ton-miles 
                                              per unit of investment."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_freight_rail_sheet_tbl", "Reset Advanced 5", class = "btn-custom")
                              ),
                              
                            ),
                            fluidRow(
                              # h3("Freight Rail"),
                              DT::dataTableOutput("freight_rail_sheet_tbl")
                            ),
                            
                            # Construction and Maintenance
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Advanced 6 | Construction and Maintenance",
                                         HTML("This represents the estimated emissions from construction and maintenance activities."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_construction_sheet_tbl", "Reset Advanced 6", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              # h3("Construction and Maintenance"),
                              DT::dataTableOutput("construction_sheet_tbl")
                            ),
                            
                            # fuel apportionments
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Advanced 7 | Fuel Apportionments",
                                         HTML("This represents the percentage of plug-in hybrid electric vehicle (PHEV) miles driven on electricity."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                              column(2,
                                     actionButton("reset_fuel_apportionment_sheet_tbl", "Reset Advanced 7", class = "btn-custom")
                              ),
                            ),
                            fluidRow(
                              # h3("Fuel Apportionments"),
                              DT::dataTableOutput("fuel_apportionment_sheet_tbl")
                            )
                            
                            
                            
                            
                            
                            
                  )
                ) # end navset card pill
      ),
      
      # outputs tab ui ----------------------------------------------------------
      # baseline outputs ui -----------------------------------------------------
      
      
      nav_panel(title = "Outputs",
                navset_card_pill(
                  id = "OUTPUTS_TABS",
                  placement = "above",
                  nav_panel(title = "Baseline GHG Forecast",
                            
                            HTML("This tab provides a projection 
                                 of baseline greenhouse gas emissions 
                                 (metric tons of carbon dioxide equivalent or MT CO2e) 
                                 for the time horizons selected in the <b>Baseline</b> tab.<br>
                                 <br>"),
                            p(),
                            h3("Baseline Transportation GHG Forecast"),
                            fluidRow(width = 12,
                                     column(width = 6,
                                            plotlyOutput("baseline_ghg_line", width = "auto", height = "auto")
                                     ),
                                     column(width = 6, #move year to top put these two in a card, title pie chart mention year, title drop down "Select year"
                                            plotlyOutput("baseline_ghg_pie", width = "auto", height = "auto")
                                     )
                            ),
                            fluidRow(width = 12,
                                     column(width = 10),
                                     column(width = 2, 
                                            selectInput("pie_graph_year","", choices = c("2021","2025","2030","2050")))
                            ),
                            fluidRow(
                              DT::dataTableOutput("baseline_outputs")
                            )
                  ),
                  
                  
                  # scenario summary ui ----------------------------------------
                  
                  
                  nav_panel(title = "Scenario Summary",
                            HTML("<p>This tab reports the scenario-level outputs for greenhouse gas 
                                 emissions (metric tons of carbon dioxide 
                                 equivalent or MT CO2e); vehicle miles traveled 
                                 (VMT); local pollution from oxides of nitrogen 
                                 (NOx) and fine particulate matter (PM2.5); 
                                 and daily active trips.
                                 <br><br>
                                 <p>For the Scenario Summary, please select 
                                 an indicator from the dropdown below to compare your selected scenarios 
                                 with the baseline GHG forecast.</p>
                                 <br>"),
                            fluidRow(
                              p(""),
                              title = "Select Indicator",
                              selectizeInput(inputId = "scenario_indicator",
                                             label = "Indicator",
                                             selected = 'em_mt_co2_change',
                                             choices = c( 
                                               'MT CO2e' = 'Emissions (MT CO2e)',
                                               'VMT' = 'VMT (millions)',
                                               'MT NOx' = 'NOx Reduction (MT)',
                                               'MT PM2.5' = 'PM2.5 Reduction (MT)',
                                               'Daily Active Trips' = 'New Daily Active Trips')
                              )),
                            fluidRow(
                              column(6,
                                     plotlyOutput("scenario_line_graph", width = "auto", height = "auto")),
                              column(6, 
                                     plotlyOutput("scenario_bar_graph", width = "auto", height = "auto"))),
                            fluidRow(
                              column(12,
                                     DT::DTOutput('emission_change_tbl'))),
                  ),
                  
                  
                  # strategy summary ui ----------------------------------------
                  
                  
                  nav_panel(title = "Strategy Summary",
                            HTML("<p>This tab reports the annual strategy-level outputs for greenhouse gas 
                                 emissions (metric tons of carbon dioxide 
                                 equivalent or MT CO2e); vehicle miles traveled 
                                 (VMT); local pollution from oxides of nitrogen 
                                 (NOx) and fine particulate matter (PM2.5); 
                                 and daily active trips.</p>
                                 <br>
                                 <p>For the Strategy Summary, please select the 
                                 desired scenario and indicator to view the 
                                 changes at the strategy level.</p>
                                 <br>"),
                            fluidRow(
                              p(""),
                              title = "Select Indicator",
                              selectizeInput(inputId = "strategy_indicator",
                                             label = "Indicator",
                                             selected = NULL,
                                             choices = c( 
                                               'MT CO2e' = 'total_change_MTCO2',
                                               'VMT (miles)' = 'total_change_VMT',
                                               'MT NOx' = 'total_change_mtnox',
                                               'MT PM2.5' = 'total_change_pm25',
                                               'Daily Active Trips' = 'total_change_newtrips')
                              ),
                              selectizeInput(inputId = "strategy_scen_select",
                                             label = "Scenario",
                                             selected = NULL,
                                             choices = c( 
                                               'Scenario 1' = 'scen_1',
                                               'Scenario 2' = 'scen_2')
                              )),
                            fluidRow(
                              column(6,
                                     DT::DTOutput('strategy_summary_tbl')),
                              column(6,
                                     plotlyOutput("strategy_summary_graph", width = "auto", height = "auto"))),
                  ),
                  
                  
                  # cost-effectiveness ui ---------------------------------------------------
                  
                  nav_panel(title = "Cost-Effectiveness",
                            HTML("This tab allows users to review the cost-effectiveness of each strategy as 
                                 measured by the change in annual output of the indicator (e.g. MT CO2e) per 
                                 $1 million of investment. All cost-effectiveness outputs are calculated based 
                                 on values entered in the Baseline, Costs and Assumptions Inputs tabs (user-provided 
                                 inputs in the project or budget tabs do not affect cost-effectiveness).<br>"),
                            fluidRow(
                              radioButtons(inputId = "cost_view",
                                           "Level of detail:",
                                           c("Detailed results" = "detail", "Summary results" = "summary"),
                                           selected = "summary")),
                            fluidRow(
                              p("All results are reported in terms of annual reduction per $M investment."),
                              fluidRow( class = 'cost-table search',
                                        h4("Cost-Effectiveness 1 | Bicycle & Pedestrian"),
                                        DT::dataTableOutput("bikeped_costs_outputs_tbl"),
                                        p(""),
                                        
                              )
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 2 | Transit: Increased Fixed Route Service"),
                                      DT::dataTableOutput("transit_fixed_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 3 | Transit: Increased Demand Response Service"),
                                      DT::dataTableOutput("transit_dr_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 4 | Public Transportation: Bus Priority Treatment"),
                                      DT::dataTableOutput("pub_trans_priority_costs_outputs_tbl")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 5 | Transit: Fleet Electrification"),
                                      DT::dataTableOutput("transit_zeb_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 6 | Public Transportation: Rail"),
                                      DT::dataTableOutput("pub_trans_rail_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 7 | Travel Demand Management"),
                                      DT::dataTableOutput("tdm_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 8 | Micromobility"),
                                      DT::dataTableOutput("micro_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 9 | Traffic Operations: Intersections"),
                                      DT::dataTableOutput("traffic_ops_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 10 | Medium- and Heavy-Duty Vehicle Replacement (Electrification)"),
                                      DT::dataTableOutput("mhdev_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 11 | Park & Ride"),
                                      DT::dataTableOutput("pnr_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 12 | EV Charging Infrastructure"),
                                      DT::dataTableOutput("evsi_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      h4("Cost-Effectiveness 13 | Freight Intermodal Facilities"),
                                      p(""),
                                      DT::dataTableOutput("intermodal_costs_outputs_tbl")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      p(""),
                                      h4("Cost-Effectiveness 14 | Roadway Expansion"),
                                      DT::dataTableOutput("roadway_expand_costs_outputs_tbl"),
                                      p("")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      h4("Cost-Effectiveness 15 | Roadway Resurfacing"),
                                      p(""),
                                      DT::dataTableOutput("roadwayresurf_cuts_costs_outputs_tbl")
                            ),
                            fluidRow( class = 'cost-table',
                                      p(""),
                                      h4("Cost-Effectiveness 16 | Land Use"),
                                      p(""),
                                      DT::dataTableOutput("landuse_costs_outputs_tbl")
                            ),
                            
                  ),
                  
                  # cumulative projects ui -------------------------------------
                  nav_panel(title = "Cumulative Project Totals",
                            fluidRow(
                              HTML("<p>These tables represent the cumulative project totals based on user inputs in the <b>Projects</b> tab.
            These totals are provided solely for the purpose of displaying these numbers as a point of reference.<br>
            <p>
            
                                   "
                              ),),
                            
                            # bike ped
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         'Cumulative Projects 1 | Bicycle & Pedestrian Lane Miles of New Infrastructure ',
                                         HTML("This category represents implementation of any <b>two-way miles of new 
           bicycle or pedestrian facility."),
                                       ),
                                       id = "acc1",
                                       open = TRUE
                                     ),
                              ),
                              # column(2,
                              #        actionButton("reset_bikeped_projscumu_tbl", "Reset Projects 1", class = "btn-custom"),
                              # ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("bikeped_projscumu_tbl")
                                     ))
                            ),
                            
                            
                            
                            # transit fixed route
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 2 | Transit: Increased Fixed Route Service",
                                         HTML("This category represents additions of <b>new fixed route service <a href = 'https://www.transit.dot.gov/ntd/national-transit-database-ntd-glossary'>vehicles operated in maximum service (VOMS)</a>.</b> 
               Fixed route service vehicles include vehicles operated along a prescribed route according to a fixed schedule."
                                         ),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("transit_fixed_projscumu_tbl")
                                     ))
                            ),
                            
                            
                            # transit DR
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 3 | Transit: Increased Demand Response Service",
                                         HTML("This category represents addition of any <b>new demand response service vehicles operated in maximum service (VOMS).</b> 
              Demand response service vehicles include non-fixed route services that are initiated by customers and require advanced scheduling, 
                        such as vehicles provided by public entities, nonprofits, and private providers."
                                         ),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              DT::dataTableOutput("transit_dr_projscumu_tbl")
                            ),
                            
                            # fleet electrification
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 4 | Transit: Fleet Electrification",
                                         HTML("This category represents the <b>replacement of any fossil-fueled vehicles with an electric vehicle</b>, 
                        with the assumption that any new vehicle is again replaced by the new technology type at the end of its life cycle. "),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("transit_el_projscumu_tbl")
                                     ))
                            ),
                            
                            # bus priority
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 5 | Public Transportation: Bus Priority Treatment",
                                         HTML("This category represents addition of miles of <b>new bus priority treatment</b>. Bus priority treatment refers 
                        to the improvement of transit speed and reliability between stops by changing the designation of street space. 
                        Some examples include a bus-only lane, which assigns exclusive street space to buses, and a bus approach lane, 
                        which assigns exclusive street spaces to buses as they approach an intersection."),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("transit_bus_projscumu_tbl")
                                     ))
                            ),
                            
                            # public transportation - rail
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 6 | Public Transportation: Rail",
                                         HTML("This category represents addition of any 
                                              <b>new rail vehicles operating in maximum 
                                              service (VOMS)</b>, including service on 
                                              light rail or streetcar lines, heavy 
                                              rail, and commuter rail."#,
                                         ),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("public_rail_projscumu_tbl")
                                     ))
                            ),
                            
                            # TDM
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 7 | Travel Demand Management (TDM)",
                                         HTML("This category represents the <b>number of employees covered through the TDM Program Outreach</b>. 
                   TDM programs are designed to shift travel demand and change traveler behavior, with the goal of 
                   reducing single-occupancy vehicle travel and encouraging the use of public transit, walking, biking, teleworking, and ridesharing. ",),
                                         
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("tdm_projscumu_tbl")
                                     ))
                            ),
                            
                            # Micromobility
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 8 | Micromobility",
                                         HTML("This category represents the <b>number of e-bikes funded</b> through the implementation of <b> e-bike subsidies</b>. 
                   An e-bike subsidy reimburses part of the cost of an e-bike."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("micro_projscumu_tbl")
                                     ))
                            ),
                            
                            # traffic ops
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Projects 9 | Traffic Operations - Intersection Improvements ",
                                         HTML("This category represents any <b>improvements made to traffic operations at intersections</b>, such as new or retimed signals or new traffic-flow roundabouts."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("traffic_ops_projscumu_tbl")
                                     ))
                            ),
                            
                            # MHDEV
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 10 | Medium- and Heavy-Duty Vehicle (MHDV) Replacement",
                                         HTML("This category represents <b>replacement of any fossil fuel medium- or heavy-duty vehicles with electric vehicles</b>, 
                   with the assumption that any new vehicle is again replaced by the new technology type at the end of its life cycle."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("mhdev_projscumu_tbl")
                                     ))
                            ),
                            
                            # Park & Ride
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 11 | Park-and-Ride",
                                         HTML("This category represents any <b>new addition or expansion of Park-and-Ride spaces</b>. A Park-and-Ride space 
                   allows private transport users to park their vehicles at a large parking space and continue their commute via public transport."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("pnr_projscumu_tbl")
                                     ))
                            ),
                            
                            
                            # EVSI
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 12 | Charging Infrastructure and EV Incentives ",
                                         HTML("This category represents any <b>new addition or expansion of EV charging ports</b>. 
                   EV charging ports supply electric power for recharging electric vehicles. EV incentives offset the cost of EVs for purchasers."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("evsi_projscumu_tbl")
                                     ))
                            ),
                            
                            # freight intermodal facilities
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 13 | Freight Intermodal Facilities",
                                         HTML("This category represents the assumed 
                                         annual growth rate 
                                              of freight rail, the energy intensity as 
                                              measured in British Thermal Units (BTU) 
                                              per ton-mile, and the change in annual 
                                              VMT or ton-miles per unit of investment."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("freight_projscumu_tbl")
                                     ))
                            ),
                            
                            # Roadway expansion
                            
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 14 | Roadway Expansion",
                                         HTML("This category represents addition of any <b>new lane-miles of roadways,</b> 
                                              based  on the facility type of the roadway 
                                              and the area type of the facility."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("expansion_projscumu_tbl")
                                     ))
                            ),
                            
                            #Roadway Surfacing
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 15 | Roadway Resurfacing",
                                         HTML("This category indicates lane miles resurfaced to <b>reduce surface roughness and decrease rolling resistance</b> on roadways."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("road_resurf_projscumu_tbl")
                                     ))
                            ),
                            
                            
                            #Land Use  - this used to be cumulative projects 18
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 16 | Land Use",
                                         HTML("This category represents spending and 
                                         rezoned acres in support of <b>walkable, 
                                              transit-oriented development (TOD) areas</b>, using land use strategies such as placing destinations closer together and in environments more conducive to transit and non-motorized travel."),
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("land_use_projscumu_tbl")
                                     ))
                            ),
                            
                            
                            
                            #Transit Cuts
                            fluidRow(
                              column(10,
                                     accordion(
                                       accordion_panel(
                                         "Cumulative Projects 17 | Transit Service Cuts",
                                         HTML("This category represents <b>cuts to transit service</b> from current service levels, resulting from funding cuts.</b>"),
                                         # bslib::card_footer(popover(trigger = tags$span("See Cumulative", style = "color: blue; text-decoration: underline;"), 
                                         #                            title = "Cumulative View",
                                         #                            placement = "bottom",
                                         #                            options = list(container = "body"),
                                         #                            DTOutput(outputId = "cumul_transit_cuts_projscumu_tbl")
                                         # ))
                                         # FLAG this is the only place the cumulative calculation still exists
                                       ),
                                       open = TRUE
                                     ),
                              ),
                            ),
                            fluidRow(
                              column(12,
                                     div(style = "font-weight: normal;",
                                         DT::dataTableOutput("transit_cuts_projscumu_tbl")
                                     ))
                            ),
                            
                            
                            # fluidRow(
                            #   DT::dataTableOutput("land_use_projscumu_tbl")
                            # ),
                            
                            
                            
                  ),
                )),
      
      
      # sources tab ui ----------------------------------------------------------
      
      
      
      nav_panel(p(),
                title = "Sources",
                h2("Sources"),
                p("The following resources were used in developing the TEA-CART tool."),
                
                DT::dataTableOutput("source_table"),
                tags$script(HTML("var header = $('.navbar > .container-fluid');
      header.prepend('<div class=\"gcc-logo header-logo\" style=\"float:left\"><a target=\"_blank\" href=\"https://www.georgetownclimate.org/\"><img src=\"GCC_Logo_Contrast.svg\" alt=\"alt\" style=\"float:left;max-width:250px;width:100%;height:auto\"> </a></div>');
                       console.log(header)") # moved this script tag so that it doesn't create a new tab. 
                ),
      ),
      
      # header logo ------------------------------------------------------------
      nav_spacer(),
      # tags$script(HTML("var header = $('.navbar > .container-fluid');
      # header.append('<div style=\"float:right\"><a target=\"_blank\" href=\"https://www.georgetownclimate.org/\"><img src=\"GCC_Logo_Contrast.svg\" alt=\"alt\" style=\"float:right;max-width:250px;width:100%;height:auto\"> </a></div>');
      #                  console.log(header)")
      #             ),
      # nav_menu(
      #   title = tags$a(tags$img(src = "GCC_Logo_Transparent_Stacked.png", height = "30px"),
      #                  href = "https://www.georgetownclimate.org/", target = "_blank"))
      
      
    )
    
  )
  
}

# enabling thematic
thematic::thematic_shiny(font = "auto")

# setting default ggplot theme
theme_set(theme_bw(base_size = 16))


# Start Server Function ---------------------------------------------------

server <- function(input, output, session) {
  #Source Local Scripts --------------------------------------------------------
  source("functions/render_custom_datatable.R", local = T)
  source("functions/reshaping.R", local = T)
  source("functions/make_project_table_cumulative.R")
  
  #set reactiveValues ----------------------------------------------------------
  rv <- reactiveValues()
  rvs <- read_user_inputs_version2("data/2.User_Inputs.xlsx")
  
  rvs_out <- read_output_tables("data/3.Model_Outputs.xlsx")
  
  #update and record 
  #key_inputs updater ----------------------------------------------------------
  key_inputs_listen <- reactive({
    print('ki_li trigger')
    list(input$state_input,
         input$base_year,
         input$horizon_year_1,
         input$horizon_year_2,
         input$horizon_year_3,
         input$transportation_scope,
         input$scope_emissions,
         input$scope_fuels,
         input$vmt_forecast_input,
         input$vmt_nhs,
         input$ev_baseline_input,
         input$grid_emissions_input,
         input$land_use_factor,
         input$budget_start_year,
         input$budget_years_covered,
         input$budget_total,
         input$include_rail,
         input$mode_choice)
  })
  
  
  
  
  #page error checker ----------------------------------------------------------
  
  #these define which tab/page the users was on
  which_input_tab <- reactiveValues(curr_input_tab = "",
                                    prev_input_tab = "")
  
  observeEvent(input$INPUTS_TABS, {
    #req(input$INPUTS_TABS)
    print('tab change')
    which_input_tab$prev_input_tab <- which_input_tab$curr_input_tab
    which_input_tab$curr_input_tab <- input$INPUTS_TABS
    
  })
  
  
  
  which_page <- reactiveValues(curr_page = "ini",
                               prev_page = "")
  
  observeEvent(input$APP_PAGE, {
    #req(input$APP_PAGE)
    if(which_page$prev_page == "ini"){toggle_sidebar(id = "dwn_sidebar")}
    which_page$prev_page <- which_page$curr_page
    which_page$curr_page <- input$APP_PAGE
    
  })
  
  #here's where the notification is sent to the user
  observeEvent(input$INPUTS_TABS,{
    
    if(which_input_tab$prev_input_tab == "Baseline"){
      warning = c()
      
      if(is.na(input$grid_emissions_input)|
         (input$grid_emissions_input<2021)|
         input$grid_emissions_input>2050){
        warning = c(warning, "Due to an inncorrect input, a default value for Electricity Grid Emissions Net-Zero Year has been assumed.")
        updateSelectInput(inputId = "grid_emissions_input",value=2050)
        rvs$Baseline$elec_grid_emissions_net_zero = input$grid_emissions_input
      }
      
      if(is.na(input$base_year)){
        print('this happened')
        warning = c(warning, "Due to an inncorect input, a default value for Base Year has been assumed.")
        updateNumericInput(inputId = "base_year",value=2021)
        rvs$Baseline$base_year = input$base_year
        
      } else if(!is.na(input$horizon_year_1)&
                !is.na(input$horizon_year_2)&
                !is.na(input$horizon_year_3)&(
                  input$base_year >= input$horizon_year_1|
                  input$base_year >= input$horizon_year_2|
                  input$base_year >= input$horizon_year_3)){
        warning = c(warning, "Base Year is higher than ")
      }
      
      if(is.na(input$horizon_year_1)){
        warning = c(warning, "Due to an inncorect input, a default value for Horizon Year 1 has been assumed.")
        updateNumericInput(inputId = "horizon_year_1",value=2025)
        rvs$Baseline$horizon_year_1 = input$horizon_year_1
        
      } else if(!is.na(input$horizon_year_2)&
                !is.na(input$horizon_year_3)&
                (input$horizon_year_1 >= input$horizon_year_2|
                 input$horizon_year_1 >= input$horizon_year_3)){
        warning = c(warning,"Horizon Year 1 is higher than future horizon year values.")
      }
      
      if(is.na(input$horizon_year_2)){
        warning = c(warning, "Due to an inncorect input, a default value for Horizon Year 2 has been assumed.")
        updateNumericInput(inputId = "horizon_year_2",value=2030)
        rvs$Baseline$horizon_year_2 = input$horizon_year_2
        
      } else  if(!is.na(input$horizon_year_3)&(input$horizon_year_2 >= input$horizon_year_3)){
        warning = c(warning,"Horizon Year 2 is higher than Horizon Year 3")
      }
      
      if(is.na(input$horizon_year_3)){
        warning = c(warning, "Due to an inncorect input, a default value for Horizon Year 3 has been assumed.")
        updateNumericInput(inputId = "horizon_year_3",value=2050)
        rvs$Baseline$horizon_year_3 = input$horizon_year_3
        
      }
      
      if(length(warning) != 0){
        showNotification(HTML(paste0(warning, sep = '<br/>')), type = "warning")
      }
      
    }
    
  })
  
  observeEvent(input$APP_PAGE, {
    
    if(which_page$curr_page == "Outputs"){
      if(is.null(rvs$Scenarios)){
        
        warning = HTML("You have not selected any Scenarios. <br/> 
                    Output tabs will show no results without a scenario <br/>
                    selection. Navigate to Inputs > Scenarios to make your selection")
        
        showNotification(HTML(warning, type = "error"))
        
      } else if(sum(rvs$Scenarios$Scenario1)+sum(rvs$Scenarios$Scenario2) == 0){
        
        warning = HTML("You have not selected any Scenarios.
                    Output tabs will show no results without a scenario selection. <br/>
                    Navigate to Inputs > Scenarios to make your selection")
        
        showNotification(HTML(warning), type = "error")
      }
    }
    
    
  })
  
  observeEvent(key_inputs_listen(),{
    print("RUNNING: Update rvs$Baseline key inputs")
    rvs$Baseline <- data.frame(state = input$state_input,
                               base_year = input$base_year,
                               horizon_year_1 = input$horizon_year_1,
                               horizon_year_2 = input$horizon_year_2,
                               horizon_year_3 = input$horizon_year_3,
                               trans_system_scope = input$transportation_scope,
                               include_electricity = input$scope_emissions,
                               include_upstream_fuels = input$scope_fuels,
                               # vmt_nhs = input$vmt_nhs,
                               vmt_forecast = input$vmt_forecast_input,
                               veh_elec_baseline = input$ev_baseline_input,
                               elec_grid_emissions_net_zero = input$grid_emissions_input,
                               land_use_factor = input$land_use_factor,
                               include_rail = input$include_rail,
                               budget_start_year = input$budget_start_year,
                               budget_years_covered = input$budget_years_covered,
                               budget_total = input$budget_total,
                               mode_choice = input$mode_choice
    )
    
    updateSelectInput(inputId = "pie_graph_year",
                      label = "",
                      choices = c(input$base_year,
                                  input$horizon_year_1,
                                  input$horizon_year_2,
                                  input$horizon_year_3))
  })
  
  ## Add warnings message for Projects mode and budget mode
  observeEvent(input$INPUTS_TABS,{
    if(input$INPUTS_TABS == 'Projects' & input$mode_choice == 'Budget'){
      shinyalert(
        title = "Warning",
        text = 
          paste0(
            "TEA-CART is currently set to the ", input$mode_choice, " mode. No user edits are allowed on the Projects tab. ",
            "To make edits in this tab, please change the mode selection in the Baseline tab to 'Capital Projects.'"),
        type = "info"
      )
    } else if (input$INPUTS_TABS == 'Budget' & input$mode_choice == 'Capital Projects'){
      shinyalert(title = "Warning",
                 text  = paste0("TEA-CART is currently set to the ", input$mode_choice, " mode. No user edits are allowed on the Budget tab. ",
                                "To make edits in this tab, please change the mode selection in the Baseline tab to 'Budget.'"))
    }
  },ignoreInit = T)
  
  
  nav_select(id = "INPUTS_TABS",selected = "Projects")
  
  # Initiate or Upload User Inputs -----------------------------------------
  
  
  observeEvent(input$user_inputs_upload, {
    if(isTruthy(input$user_inputs_upload)){
      
      
      user_inputs <- read_user_inputs_excel(input$user_inputs_upload$datapath)
      #everything below is used in error checking
      user_inputs_raw <- read_user_inputs_excel("data/2.User_Inputs.xlsx")
      
      check1 <- ifelse(is.character(user_inputs$Baseline$state),""," state input,")
      check2 <- ifelse(is.numeric(user_inputs$Baseline$base_year),""," base year input,")
      check3 <- ifelse(is.numeric(user_inputs$Baseline$horizon_year_1),""," horizon year 1 input,")
      check4 <- ifelse(is.numeric(user_inputs$Baseline$horizon_year_2),""," horizon year 2 input,")
      check5 <- ifelse(is.numeric(user_inputs$Baseline$horizon_year_3),""," horizon year 3 input,")
      check6 <- ifelse(is.character(user_inputs$Baseline$trans_system_scope),""," transportation system scope input,")
      check7 <- ifelse(is.character(user_inputs$Baseline$include_electricity),""," include electricity input,")
      check8 <- ifelse(is.character(user_inputs$Baseline$include_upstream_fuels),""," include upstream fuels input,")
      check9 <- ifelse(is.character(user_inputs$Baseline$vmt_forecast),""," VMT forecast input,")
      check10 <- ifelse(is.character(user_inputs$Baseline$veh_elec_baseline),""," electricity baseline input,")
      check11 <- ifelse(is.character(user_inputs$Baseline$elec_grid_emissions_net_zero)|is.numeric(user_inputs$Baseline$elec_grid_emissions_net_zero),""," net zero year input,")
      check12 <- ifelse(is.character(user_inputs$Baseline$land_use_factor),""," land use factor input,")
      check13 <- ifelse(is.character(user_inputs$Baseline$include_rail),""," rail emissions input,")
      check14 <- ifelse(is.numeric(user_inputs$Baseline$budget_start_year),""," budget start year input,")
      check15 <- ifelse(is.numeric(user_inputs$Baseline$budget_years_covered),""," budget years covered input,")
      check16 <- ifelse(is.numeric(user_inputs$Baseline$budget_total),""," budget total input,")
      check_17 <- ifelse(is.character(user_inputs$Baseline$mode_choice),""," mode choice input,")
      input_checks <- paste0(check1, check2, check3, check4, check5, check6, check7, check8, check9,
                             check10, check11, check12, check13, check14, check15, check16, check16,check_17)
      
      if(input_checks == ""){warning <- ""} else {
        input_check <- input_checks |> str_sub(start = 0, end = nchar(input_checks) - 1)
        warning <- paste0("There was a problem with a user input:",input_check, "<br> Please check that the values for this input(s) are one of the allowable values and has the correct type e.g. 1 is saved as a number and not text<br>.")
      }
      
      #first we check that the non-editable parts are the same
      check18 <- all_equal(user_inputs$Costs[,names(user_inputs$Costs) != 'value'],user_inputs_raw$Costs[,names(user_inputs_raw$Costs) != 'value'],ignore_row_order = T)
      if(isTRUE(check18)){
        check18 <- ""
        #if they are the same we check if the value is the correct type
        if(!is.numeric(user_inputs$Costs$value)){check18<-"Cost input values are non-numeric<br>"} 
      } else {
        check18 <- paste0("Issue with Cost table: ",check18,"<br>")
      }
      
      #first we check that the non-editable parts are the same
      check19 <- all_equal(user_inputs$Budget[,names(user_inputs$Budget) != 'value'],user_inputs_raw$Budget[,names(user_inputs_raw$Budget) != 'value'],ignore_row_order = T)
      if(isTRUE(check19)){
        check19 <- ""
        #if they are the same we check if the value is the correct type
        if(!is.numeric(user_inputs$Budget$value)){check19<-"Budget input values are non-numeric<br>"} 
      } else {
        check19 <- paste0("Issue with Budget table: ",check19,"<br>")
      }
      check20 <- all_equal(user_inputs$Funding[,1:3],user_inputs_raw$Funding[,1:3],ignore_row_order = T)
      if(isTRUE(check20)){
        check20 <- ""
        #if they are the same we check if the value is the correct type
        if(!is.numeric(user_inputs$Funding[,4][[1]])|!is.numeric(user_inputs$Funding[,5][[1]])){check20<-"Funding input values are non-numeric<br>"} 
      } else {
        check20 <- paste0("Issue with Funding table: ",check20,"<br>")
      }
      
      check21 <- all_equal(user_inputs$Assumptions[,names(user_inputs$Assumptions) != 'value'],user_inputs_raw$Assumptions[,names(user_inputs_raw$Assumptions) != 'value'],ignore_row_order = T)
      if(isTRUE(check21)){
        check21 <- ""
        #if they are the same we check if the value is the correct type
        if(!is.numeric(user_inputs$Assumptions$value)){check21<-"Assumptions table input values are non-numeric<br>"} 
      } else {
        check21 <- paste0("Issue with Assumptions table: ",check21,"<br>")
      }
      
      check22 <- all_equal(user_inputs$Advanced[,names(user_inputs$Advanced) != 'value'],user_inputs_raw$Advanced[,names(user_inputs_raw$Advanced) != 'value'],ignore_row_order = T)
      if(isTRUE(check22)){
        check22 <- ""
        #if they are the same we check if the value is the correct type
        #if(!is.numeric(user_inputs$Advanced$value)){check22<-"Advanced table input values are non-numeric<br>"} 
      } else {
        check22 <- paste0("Issue with Advanced table: ",check22,"<br>")
      }
      
      check23 <- all_equal(user_inputs$Projects[,names(user_inputs$Projects) != 'value'],user_inputs_raw$Projects[,names(user_inputs_raw$Projects) != 'value'],ignore_row_order = T)
      if(isTRUE(check23)){
        check23 <- ""
        #if they are the same we check if the value is the correct type
        if(!is.numeric(user_inputs$Projects$value)){check23<-"Projects table input values are non-numeric<br>"} 
      } else {
        check23 <- paste0("Issue with Projects table: ",check23,"<br>")
      }
      check24 <- all_equal(user_inputs$Scenarios[,!(names(user_inputs$Scenarios) %in% c("Scenario1","Scenario2"))],user_inputs_raw$Scenarios[,!(names(user_inputs_raw$Scenarios) %in% c("Scenario1","Scenario2"))],ignore_row_order = T)
      if(isTRUE(check24)){
        check24 <- ""
        #if they are the same we check if the value is the correct type
        if(!is.logical(user_inputs$Scenarios$Scenario1)|!is.logical(user_inputs$Scenarios$Scenario2)){check24<-"Scenarios table input values are non-boolean (TRUE or FALSE)"} 
      } else {
        check24 <- paste0("Issue with Scenarios table: ",check24)
      }
      
      warning <- paste0(warning,check18,check19,check20,check21,check22,check23, check24)
      if(warning != "") {
        showNotification(HTML(warning), type = 'error')
        user_inputs <- read_user_inputs_excel("data/2.User_Inputs.xlsx")
      }
      
      
    } else{      #print('huh')
      user_inputs <- read_user_inputs_excel("data/2.User_Inputs.xlsx")
    }
    
    
    
    
    for(name in names(user_inputs)) {
      rvs[[name]] <- user_inputs[[name]]
    }
    
    # Assign each table in user_inputs to rv
    
    ## update baseline page options & now budget options
    updateSelectInput(session, "state_input", selected = rvs$Baseline$state)
    updateSelectInput(session, "base_year", selected = rvs$Baseline$base_year)
    updateSelectInput(session, "horizon_year_1", selected = rvs$Baseline$horizon_year_1)
    updateSelectInput(session, "horizon_year_2", selected = rvs$Baseline$horizon_year_2)
    updateSelectInput(session, "horizon_year_3", selected = rvs$Baseline$horizon_year_3)
    updateSelectInput(session, "transportation_scope", selected = rvs$Baseline$trans_system_scope)
    updateSelectInput(session, "scope_emissions", selected = ifelse(rvs$Baseline$include_electricity, "1", "0"))
    updateSelectInput(session, "scope_fuels", selected = ifelse(rvs$Baseline$include_upstream_fuels, "1", "0"))
    updateSelectInput(session, "vmt_forecast_input", selected = rvs$Baseline$vmt_forecast)
    updateSelectInput(session, "ev_baseline_input", selected = rvs$Baseline$veh_elec_baseline)
    updateSelectInput(session, "grid_emissions_input", selected = rvs$Baseline$elec_grid_emissions_net_zero)
    updateSelectInput(session, "land_use_factor", selected = ifelse(rvs$Baseline$land_use_factor == 'Yes', "1", "0"))
    updateSelectInput(session, "include_rail", selected = ifelse(rvs$Baseline$include_rail == 'Yes', "1", "0"))
    updateSelectInput(session, "budget_start_year", selected = rvs$Baseline$budget_start_year)
    updateSelectInput(session, "budget_years_covered", selected = rvs$Baseline$budget_years_covered)
    updateSelectInput(session, "budget_total", selected = rvs$Baseline$budget_total)
    updateSelectInput(session, "mode_choice", selected = rvs$Baseline$mode_choice)
    

    
  }, ignoreNULL = F, ignoreInit = T)
  
  # Download user inputs -------------------------------------------------------
  
  output$user_inputs_download <- downloadHandler(
    filename = function() {
      paste0("2.User_Inputs_", format(Sys.time(), "%m-%d_%H-%M"), ".xlsx")
    },
    content = function(file) {
      
      base_input <- data.frame(rvs$Baseline) %>%
        mutate(include_electricity = ifelse(include_electricity == 1,'TRUE','FALSE'),
               include_upstream_fuels = ifelse(include_upstream_fuels  == 1, 'TRUE','FALSE'),
               land_use_factor = ifelse(land_use_factor == 1, 'Yes','No'),
               include_rail = ifelse(include_rail == 1, 'Yes','No'))

      ### to avoid any uploading issue - why these are character?? 
      rvs$Budget$table_no_ui <- as.numeric(rvs$Budget$table_no_ui)
      rvs$Costs$table_no_ui <- as.numeric(rvs$Costs$table_no_ui)
      
      references <- read_xlsx("data/2.User_Inputs.xlsx", sheet = "References") #read in a copy, will be included in the download user inputs
      return(openxlsx::write.xlsx(x = list("Costs" = rvs$Costs,
                                           "Assumptions" = rvs$Assumptions,
                                           "Baseline" = base_input,
                                           "Projects" = rvs$Projects,
                                           "Budget" = rvs$Budget,
                                           "Funding_Summary" = rvs$Funding,
                                           "Advanced" = rvs$Advanced,
                                           "References" = references,
                                           "Scenarios" = rvs$Scenarios), 
                                  file = file))
    }
  )
  
  ##
  # Download result data -------------------------------------------------------
  ## Qi working here
  output$result_data <- downloadHandler(
    filename = function() {
      paste0("2.Estimated_Results_", format(Sys.time(), "%m-%d_%H-%M"), ".xlsx")
    },
    content = function(file) {
      req(baseline_ghg_forecast())
      
      dt <- baseline_ghg_forecast()
      
      
      dt_onroad <- dt %>% ungroup() %>%# select(-veh_supertype) %>%
        filter(veh_supertype %in% c("Light-Duty Vehicles","Medium-/Heavy-Duty Vehicles")) %>%
        summarise(across(where(is.numeric),sum)) %>%
        mutate(veh_supertype = "Total (Onroad Vehicles)")
      dt_all <- dt %>% ungroup() %>%# select(-veh_supertype) %>%
        #filter(veh_supertype %in% c("Light-Duty Vehicles","Medium-/Heavy-Duty Vehicles")) %>%
        summarise(across(where(is.numeric),sum))
      growth <- dt_all[[1,1]]
      dt_growth <- dt_all %>% 
        mutate(across(where(is.numeric), ~(.x - growth)/growth, .names = "{.col}")) %>%
        mutate(veh_supertype = "Total (All Transportation)")
      dt_all <- dt_all %>% 
        mutate(veh_supertype = "Total (All Transportation)")
      ghg_data <- rbind(dt, dt_onroad, dt_all, dt_growth) %>%
        rename("Emissions" = "veh_supertype")
      
      #get and modify the scen data: 
      scen_data <- scenario_summary_results() %>%
        filter(grepl("Reduction", table_title)|table_title == "New Daily Active Trips") %>%
        filter(!grepl("%",table_title)) %>%
        mutate(table_title = case_when(table_title == "Emissions Reduction (MT from Baseline)" ~ 'CO2',
                                       table_title == "VMT Reduction (millions from Baseline)" ~ 'VMT',
                                       table_title == "NOx Reduction (MT)" ~ 'NOx',
                                       table_title == "PM2.5 Reduction (MT)" ~ 'PM2.5',
                                       table_title == "New Daily Active Trips" ~ 'New Daily Active Trips')) %>%
        rename(indicator = table_title) %>%
        pivot_longer(cols = as.character(c(rvs$Baseline$base_year,
                                           rvs$Baseline$horizon_year_1,
                                           rvs$Baseline$horizon_year_2,
                                           rvs$Baseline$horizon_year_3)), 
                     names_to = "Year",
                     values_to = "mt_reduction") %>%
        mutate(mt_reduction = ifelse(mt_reduction == "-","0",mt_reduction)) %>%
        mutate(mt_reduction = as.numeric(mt_reduction)) %>%
        mutate_if(is.numeric, ~round(., 1))
      
      cost_data <- all_costs_detail() 
      
      strategy_data <- scenario_sum()
      
      ## add a tab showing the unit description
      
      reference_tab <- data.frame(
        Header = c("MT GHG", "VMT", "MT NOx", "MT PM2.5","Daily Active Trips"),
        Description = c(
          "Annual GHG reductions (metric tons) per $1 million invested",
          "Annual Vehicle Miles Traveled (VMT) reduction per $1 million invested",
          "Annual NOx reductions (metric tons) per $1 million invested",
          "Annual PM2.5 reductions (metric tons) per $1 million invested",
          "Annual Daily Active Trips reductions (metric tons) per $1 million invested"
        )
      )
      
      tab_list <- list(
        "Reference" = reference_tab,
        "GHG Result" = ghg_data,
        "Scenario Result" = scen_data,
        "Strategy Result" = strategy_data)
      
      cost_tabs <- cost_data
      names(cost_tabs) <- paste0("Cost - ", names(cost_data))
      
      tab_list <- c(tab_list, cost_tabs)
      
      tab_list <- lapply(tab_list, function(df) {
        if ("table" %in% names(df)) {
          df <- df[ , setdiff(names(df), "table"), drop = FALSE]
        }
        df
      })
      
      return(openxlsx::write.xlsx(x = tab_list, 
                                  file = file))
    }
  )
  ## end of download result data
  
  # UI tables ---------------------------------------------------------------
  
  UI_tables <- read_xlsx("data/2.User_Inputs.xlsx", sheet = "UI_Tables")
  
  output$UI_tables <- renderDT({
    datatable(
      UI_tables,
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        columnDefs = list(
          list(
            targets = 2:4,
            className = "dt-center"
          )
        )
      )
    )
  })
  
  
  # server sources ---------------------------------------------------
  
  sources_data <- read_xlsx("data/sources.xlsx", sheet = 1, col_names = TRUE)
  
  output$source_table <- renderDT({
    DT::datatable(sources_data,
                  escape = FALSE,
                  options = list(pageLength = 10,
                                 autoWidth = FALSE,
                                 buttons = c('csv', 'excel')
                  ),
                  extensions = 'Buttons',
                  filter = 'bottom',
                  selection = 'none',
                  rownames = FALSE)
  })
  
  
  
  # Budget Inputs: Render ---------------------------------------------------
  
  
  observe({
    req(rvs$Baseline$budget_start_year)
    req(rvs$Baseline$budget_years_covered)
    
    start_year <- rvs$Baseline$budget_start_year
    end_year <- rvs$Baseline$budget_start_year + rvs$Baseline$budget_years_covered
    
    updateNumericInput(
      inputId = "budget_total",
      label = HTML(paste0("Total Budget ($M) for ", start_year, " - ", end_year,":"),
                   sep = " ")
    )
    
    
  })
  
  # Project Tables: Render ------------------------------------------------------
  
  output$bikeped_projs_tbl <- renderDT({
    req(rvs)
    temp_send <- rvs$Projects[rvs$Projects$table_no_ui == 1,]
    
    if (input$mode_choice == 'Capital Projects'){
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 1,
        non_editable_cols = c(0, 1, 2),
        page_length = 21,
        comma_rows = 0:21,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))}
    else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 1,
        non_editable_cols = c(0:5),
        page_length = 21,
        comma_rows = 0:21,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$transit_fixed_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    
    if (input$mode_choice == 'Capital Projects'){
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 2,
        non_editable_cols = c(0, 1, 2, 3),
        page_length = 10,
        comma_rows = 0:6,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 2,
        non_editable_cols = c(0:6),
        page_length = 10,
        comma_rows = 0:6,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
    
  })
  
  output$transit_dr_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    
    if (input$mode_choice == 'Capital Projects'){
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 3,
        non_editable_cols = c(0, 1, 2, 3),
        page_length = 10,
        comma_rows = 0:6,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 3,
        non_editable_cols = c(0:6),
        page_length = 10,
        comma_rows = 0:6,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$transit_el_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    
    if (input$mode_choice == 'Capital Projects'){
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 4,
        non_editable_cols = c(0, 1, 2, 3),
        page_length = 10,
        comma_rows = 0:8,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 4,
        non_editable_cols = c(0:6),
        page_length = 10,
        comma_rows = 0:8,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$transit_bus_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){ 
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 5,
        non_editable_cols = c(0),
        page_length = 10,
        comma_rows = integer(1),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 5,
        non_editable_cols = c(0:3),
        page_length = 10,
        comma_rows = integer(1),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$public_rail_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){ 
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 6,
        non_editable_cols = c(0,1,2,3),
        page_length = 8,
        comma_rows = 0:3,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 6,
        non_editable_cols = c(0:6),
        page_length = 8,
        comma_rows = 0:3,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$tdm_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){ 
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 7,
        non_editable_cols = c(0),
        page_length = 10,
        comma_rows = 0,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 7,
        non_editable_cols = c(0:3),
        page_length = 10,
        comma_rows = 0,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$micro_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){ 
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 8,
        non_editable_cols = c(0),
        page_length = 10,
        comma_rows = 0,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 8,
        non_editable_cols = c(0:3),
        page_length = 10,
        comma_rows = 0,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$traffic_ops_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){     
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 9,
        non_editable_cols = c(0,1,2),
        page_length = 10,
        comma_rows = 0:3,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 9,
        non_editable_cols = c(0:5),
        page_length = 10,
        comma_rows = 0:3,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$mhdev_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){     
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 10,
        non_editable_cols = c(0,1,2),
        page_length = 10,
        comma_rows = 0:4,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 10,
        non_editable_cols = c(0:5),
        page_length = 10,
        comma_rows = 0:4,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$pnr_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){    
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 11,
        non_editable_cols = c(0),
        page_length = 1,
        comma_rows = 0,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 11,
        non_editable_cols = c(0:3),
        page_length = 1,
        comma_rows = 0,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  
  output$evsi_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){    
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 12,
        non_editable_cols = c(0,1),
        page_length = 10,
        comma_rows = 0:4,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 12,
        non_editable_cols = c(0:4),
        page_length = 10,
        comma_rows = 0:4,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  
  output$freight_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){     
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 13,
        non_editable_cols = c(0),
        page_length = 10,
        comma_rows = 0:4,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 13,
        non_editable_cols = c(0:3),
        page_length = 10,
        comma_rows = 0:4,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0)) 
    }
    
  })
  
  
  output$expansion_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){     
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 14,
        non_editable_cols = c(0,1,2),
        page_length = 10,
        comma_rows = 0:4,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 14,
        non_editable_cols = c(0:5),
        page_length = 10,
        comma_rows = 0:4,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  
  output$transit_cuts_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    
    if (input$mode_choice == 'Capital Projects'){  
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 17,#slchanged
        non_editable_cols = c(0, 1, 2),
        page_length = 10,
        comma_rows = 0:7,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 17,#slchanged
        non_editable_cols = c(0:5),
        page_length = 10,
        comma_rows = 0:7,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$land_use_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){  
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 16,
        non_editable_cols = c(0, 1),
        page_length = 10,
        comma_rows = 0:15,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 16,
        non_editable_cols = c(0:4),
        page_length = 10,
        comma_rows = 0:15,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  
  output$road_resurf_projs_tbl <- renderDT({
    
    req(rvs$Projects)
    temp_send <- rvs$Projects
    if (input$mode_choice == 'Capital Projects'){    
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 15,#slchanged
        non_editable_cols = c(0),
        page_length = 10,
        comma_rows = 0:2,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0)) 
    } else {
      render_custom_datatable(
        data_reactive = temp_send,
        table_number = 15,#slchanged
        non_editable_cols = c(0:3),
        page_length = 10,
        comma_rows = 0:2,
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0)) 
    }
    
    
  })
  
  # Project Tables: Observe and update edits to projects ------------------------
  # observe edits to the bikeped_projs
  observeEvent(input$bikeped_projs_tbl_cell_edit, {
    req(rvs$Projects)
    
    rvs$Projects[rvs$Projects$table_no_ui == 1,] <- reshaping_projects2(input$bikeped_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 1,
                                                                        col1 = 'area_type',
                                                                        col2 = 'facility_type',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
  })
  
  # observe edits to the transit_fixed_projs
  observeEvent(input$transit_fixed_projs_tbl_cell_edit, {
    req(rvs$Projects)
    
    rvs$Projects[rvs$Projects$table_no_ui == 2,] <- reshaping_projects2(input$transit_fixed_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 2,
                                                                        col1 = 'area_type',
                                                                        col2 = 'fuel_type',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
  })
  
  # observe edits to the transit_dr_projs_tbl_cell_edit
  observeEvent(input$transit_dr_projs_tbl_cell_edit, {
    req(rvs$Projects)
    
    rvs$Projects[rvs$Projects$table_no_ui == 3,] <- reshaping_projects2(input$transit_dr_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 3,
                                                                        col1 = 'area_type',
                                                                        col2 = 'fuel_type',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
  })
  
  # observe edits to the transit_el_projs_tbl
  observeEvent(input$transit_el_projs_tbl_cell_edit, {
    req(rvs$Projects)
    
    rvs$Projects[rvs$Projects$table_no_ui == 4,] <- reshaping_projects2(input$transit_el_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 4,
                                                                        col1 = 'area_type',
                                                                        col2 = 'fuel_type',
                                                                        col3 = 'transit_mode',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
  })
  
  # observe edits to the transit_bus_projs_tbl  
  observeEvent(input$transit_bus_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 5,] <- reshaping_projects2(input$transit_bus_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 5,
                                                                        col1 = 'unit',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the public_rail_projs table
  observeEvent(input$public_rail_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 6,] <- reshaping_projects2(input$public_rail_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 6,
                                                                        col1 = 'area_type',
                                                                        col2 = 'fuel_type',
                                                                        col3 = 'transit_mode',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the tdm_projs_tbl table
  observeEvent(input$tdm_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 7,] <- reshaping_projects2(input$tdm_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 7,
                                                                        col1 = 'unit',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the micro_projs_tbl table
  observeEvent(input$micro_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 8,] <- reshaping_projects2(input$micro_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 8,
                                                                        col1 = 'unit',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the traffic_ops_projs_tbl table
  observeEvent(input$traffic_ops_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 9,] <- reshaping_projects2(input$traffic_ops_projs_tbl_cell_edit,
                                                                        rvs$Projects,
                                                                        tbl_no = 9,
                                                                        col1 = 'area_type',
                                                                        col2 = 'road_class',
                                                                        col3 = 'unit',
                                                                        horizon_year_1 = input$horizon_year_1,
                                                                        horizon_year_2 = input$horizon_year_2,
                                                                        horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the mhdev_projs_tbl table
  observeEvent(input$mhdev_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 10,] <- reshaping_projects2(input$mhdev_projs_tbl_cell_edit,
                                                                         rvs$Projects,
                                                                         tbl_no = 10,
                                                                         col1 = 'veh_type',
                                                                         col2 = 'fuel_type',
                                                                         horizon_year_1 = input$horizon_year_1,
                                                                         horizon_year_2 = input$horizon_year_2,
                                                                         horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the mhdev_projs_tbl table
  observeEvent(input$pnr_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 11,] <- reshaping_projects2(input$pnr_projs_tbl_cell_edit,
                                                                         rvs$Projects,
                                                                         tbl_no = 11,
                                                                         col1 = 'unit',
                                                                         horizon_year_1 = input$horizon_year_1,
                                                                         horizon_year_2 = input$horizon_year_2,
                                                                         horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the evsi_projs_tbl table
  observeEvent(input$evsi_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 12,] <- reshaping_projects2(input$evsi_projs_tbl_cell_edit,
                                                                         rvs$Projects,
                                                                         tbl_no = 12,
                                                                         col1 = 'charge_port_detail',
                                                                         horizon_year_1 = input$horizon_year_1,
                                                                         horizon_year_2 = input$horizon_year_2,
                                                                         horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    # browser()
  })
  
  # observe edits to the freight_projs_tbl table
  observeEvent(input$freight_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 13,] <- reshaping_projects2(input$freight_projs_tbl_cell_edit,
                                                                         rvs$Projects,
                                                                         tbl_no = 13,
                                                                         col1 = 'unit',
                                                                         horizon_year_1 = input$horizon_year_1,
                                                                         horizon_year_2 = input$horizon_year_2,
                                                                         horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the expansion_projs_tbl table
  observeEvent(input$expansion_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 14,] <- reshaping_projects2(input$expansion_projs_tbl_cell_edit,
                                                                         rvs$Projects,
                                                                         tbl_no = 14,
                                                                         col1 = 'area_type',
                                                                         col2 = 'road_class',
                                                                         horizon_year_1 = input$horizon_year_1,
                                                                         horizon_year_2 = input$horizon_year_2,
                                                                         horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  observeEvent(input$transit_cuts_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 17,] <- reshaping_projects2(input$transit_cuts_projs_tbl_cell_edit, #slchanged
                                                                         rvs$Projects,
                                                                         tbl_no = 17,
                                                                         col1 ='area_type',
                                                                         col2 = 'transit_mode',
                                                                         col3 = 'unit',
                                                                         horizon_year_1 = input$horizon_year_1,
                                                                         horizon_year_2 = input$horizon_year_2,
                                                                         horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  observeEvent(input$land_use_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 16,] <- reshaping_projects2(input$land_use_projs_tbl_cell_edit,
                                                                         rvs$Projects,
                                                                         tbl_no = 16,
                                                                         col1 = 'land_use',
                                                                         horizon_year_1 = input$horizon_year_1,
                                                                         horizon_year_2 = input$horizon_year_2,
                                                                         horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  observeEvent(input$road_resurf_projs_tbl_cell_edit, {
    
    rvs$Projects[rvs$Projects$table_no_ui == 15,] <- reshaping_projects2(input$road_resurf_projs_tbl_cell_edit, #slchanged
                                                                         rvs$Projects,
                                                                         tbl_no = 15,
                                                                         horizon_year_1 = input$horizon_year_1,
                                                                         horizon_year_2 = input$horizon_year_2,
                                                                         horizon_year_3 = input$horizon_year_3)
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # Project Tables: Reset buttons on projects ---------------------------------------------------
  
  observeEvent(input$reset_bikeped_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 1,] <- initial_projects[initial_projects$table_no_ui == 1, ]
  })  
  
  observeEvent(input$reset_transit_fixed_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 2,] <- initial_projects[initial_projects$table_no_ui == 2, ]
  })  
  
  observeEvent(input$reset_transit_dr_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 3,] <- initial_projects[initial_projects$table_no_ui == 3, ]
  })  
  
  observeEvent(input$reset_transit_el_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 4,] <- initial_projects[initial_projects$table_no_ui == 4, ]
  })  
  
  observeEvent(input$reset_transit_bus_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 5,] <- initial_projects[initial_projects$table_no_ui == 5, ]
  })  
  
  observeEvent(input$reset_public_rail_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 6,] <- initial_projects[initial_projects$table_no_ui == 6, ]
  })  
  
  observeEvent(input$reset_tdm_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 7,] <- initial_projects[initial_projects$table_no_ui == 7, ]
  })  
  
  observeEvent(input$reset_micro_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 8,] <- initial_projects[initial_projects$table_no_ui == 8, ]
  })  
  
  observeEvent(input$reset_traffic_ops_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 9,] <- initial_projects[initial_projects$table_no_ui == 9, ]
  })  
  
  observeEvent(input$reset_mhdev_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 10,] <- initial_projects[initial_projects$table_no_ui == 10, ]
  })  
  
  observeEvent(input$reset_pnr_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 11,] <- initial_projects[initial_projects$table_no_ui == 11, ]
  })  
  
  observeEvent(input$reset_evsi_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 12,] <- initial_projects[initial_projects$table_no_ui == 12, ]
  })  
  
  observeEvent(input$reset_freight_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 13,] <- initial_projects[initial_projects$table_no_ui == 13, ]
  })  
  
  observeEvent(input$reset_expansion_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 14,] <- initial_projects[initial_projects$table_no_ui == 14, ]
  })  
  
  observeEvent(input$reset_transit_cuts_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 17,] <- initial_projects[initial_projects$table_no_ui == 17, ] #slchanged
  })  
  
  observeEvent(input$reset_road_resurf_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 16,] <- initial_projects[initial_projects$table_no_ui == 16, ]
  })
  
  observeEvent(input$reset_land_use_projs_tbl, {
    rvs$Projects[rvs$Projects$table_no_ui == 15,] <- initial_projects[initial_projects$table_no_ui == 15, ] #slchanged
  })  
  
  
  
  # Project Mode - reset all if user select Budget Mode ---------------------
  ## new added by Qi, this is aiming to ensure user made edits under both mode by switch between modes.
  observeEvent(input$mode_choice,{
    if(input$mode_choice == 'Budget'){
      rvs$Projects[rvs$Projects$table_no_ui == 1,] <- initial_projects[initial_projects$table_no_ui == 1, ]
      rvs$Projects[rvs$Projects$table_no_ui == 2,] <- initial_projects[initial_projects$table_no_ui == 2, ]
      rvs$Projects[rvs$Projects$table_no_ui == 3,] <- initial_projects[initial_projects$table_no_ui == 3, ]
      rvs$Projects[rvs$Projects$table_no_ui == 4,] <- initial_projects[initial_projects$table_no_ui == 4, ]
      rvs$Projects[rvs$Projects$table_no_ui == 5,] <- initial_projects[initial_projects$table_no_ui == 5, ]
      rvs$Projects[rvs$Projects$table_no_ui == 6,] <- initial_projects[initial_projects$table_no_ui == 6, ]
      rvs$Projects[rvs$Projects$table_no_ui == 7,] <- initial_projects[initial_projects$table_no_ui == 7, ]
      rvs$Projects[rvs$Projects$table_no_ui == 8,] <- initial_projects[initial_projects$table_no_ui == 8, ]
      rvs$Projects[rvs$Projects$table_no_ui == 9,] <- initial_projects[initial_projects$table_no_ui == 9, ]
      rvs$Projects[rvs$Projects$table_no_ui == 10,] <- initial_projects[initial_projects$table_no_ui == 10, ]
      rvs$Projects[rvs$Projects$table_no_ui == 11,] <- initial_projects[initial_projects$table_no_ui == 11, ]
      rvs$Projects[rvs$Projects$table_no_ui == 12,] <- initial_projects[initial_projects$table_no_ui == 12, ]
      rvs$Projects[rvs$Projects$table_no_ui == 13,] <- initial_projects[initial_projects$table_no_ui == 13, ]
      rvs$Projects[rvs$Projects$table_no_ui == 14,] <- initial_projects[initial_projects$table_no_ui == 14, ]
      rvs$Projects[rvs$Projects$table_no_ui == 17,] <- initial_projects[initial_projects$table_no_ui == 17, ]
      rvs$Projects[rvs$Projects$table_no_ui == 16,] <- initial_projects[initial_projects$table_no_ui == 16, ]
      rvs$Projects[rvs$Projects$table_no_ui == 15,] <- initial_projects[initial_projects$table_no_ui == 15, ]
    } else if (input$mode_choice =="Capital Projects"){
      rvs$Budget[rvs$Budget$table_no_ui == 1,] <- initial_budget[initial_budget$table_no_ui == 1, ]
      rvs$Budget[rvs$Budget$table_no_ui == 2,] <- initial_budget[initial_budget$table_no_ui == 2, ]
      rvs$Budget[rvs$Budget$table_no_ui == 3,] <- initial_budget[initial_budget$table_no_ui == 3, ]
      rvs$Budget[rvs$Budget$table_no_ui == 4,] <- initial_budget[initial_budget$table_no_ui == 4, ]
      rvs$Budget[rvs$Budget$table_no_ui == 5,] <- initial_budget[initial_budget$table_no_ui == 5, ]
      rvs$Budget[rvs$Budget$table_no_ui == 6,] <- initial_budget[initial_budget$table_no_ui == 6, ]
      rvs$Budget[rvs$Budget$table_no_ui == 7,] <- initial_budget[initial_budget$table_no_ui == 7, ]
      rvs$Budget[rvs$Budget$table_no_ui == 8,] <- initial_budget[initial_budget$table_no_ui == 8, ]
      rvs$Budget[rvs$Budget$table_no_ui == 9,] <- initial_budget[initial_budget$table_no_ui == 9, ]
      rvs$Budget[rvs$Budget$table_no_ui == 10,] <- initial_budget[initial_budget$table_no_ui == 10, ]
      rvs$Budget[rvs$Budget$table_no_ui == 11,] <- initial_budget[initial_budget$table_no_ui == 11, ]
      rvs$Budget[rvs$Budget$table_no_ui == 12,] <- initial_budget[initial_budget$table_no_ui == 12, ]
      rvs$Budget[rvs$Budget$table_no_ui == 13,] <- initial_budget[initial_budget$table_no_ui == 13, ]
      rvs$Budget[rvs$Budget$table_no_ui == 14,] <- initial_budget[initial_budget$table_no_ui == 14, ]
      rvs$Budget[rvs$Budget$table_no_ui == 16,] <- initial_budget[initial_budget$table_no_ui == 16, ]
      rvs$Budget[rvs$Budget$table_no_ui == 17,] <- initial_budget[initial_budget$table_no_ui == 17, ]
      rvs$Budget[rvs$Budget$table_no_ui == 15,] <- initial_budget[initial_budget$table_no_ui == 15, ]
      
      
    }
  }, ignoreInit = TRUE)
  
  
  
  # Project Cumulative Tables: Render ------------------------------------------
  observe({
    temp <- rvs$Projects
    temp <- temp |> pivot_wider(names_from = year, values_from = value)
    temp$horizon_year_2 <- temp$horizon_year_2 + temp$horizon_year_1
    temp$horizon_year_3 <- temp$horizon_year_3 + temp$horizon_year_2
    temp <- temp |> pivot_longer(cols = c("horizon_year_1","horizon_year_2","horizon_year_3"), names_to = "year",values_to = "value")
    rvs$Projectscumu <- temp
  })
  
  output$bikeped_projscumu_tbl <- renderDT({
    req(rvs)
    temp_send <- rvs$Projectscumu[rvs$Projectscumu$table_no_ui == 1,]
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 1,
      non_editable_cols = c(0:5),
      page_length = 21,
      comma_rows = 0:21,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
  })
  
  output$transit_fixed_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 2,
      non_editable_cols = c(0:6),
      page_length = 10,
      comma_rows = 0:6,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$transit_dr_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 3,
      non_editable_cols =c(0:6),
      page_length = 10,
      comma_rows = 0:6,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$transit_el_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 4,
      non_editable_cols = c(0:6),
      page_length = 10,
      comma_rows = 0:8,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$transit_bus_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 5,
      non_editable_cols = c(0:3),
      page_length = 10,
      comma_rows = integer(1),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$public_rail_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 6,
      non_editable_cols = c(0:6),
      page_length = 8,
      comma_rows = 0:3,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$tdm_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 7,
      non_editable_cols = c(0:3),
      page_length = 10,
      comma_rows = 0,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$micro_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 8,
      non_editable_cols = c(0:3),
      page_length = 10,
      comma_rows = 0,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$traffic_ops_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 9,
      non_editable_cols = c(0,1,2,3,4,5),
      page_length = 10,
      comma_rows = 0:3,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$mhdev_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 10,
      non_editable_cols = c(0:5),
      page_length = 10,
      comma_rows = 0:4,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$pnr_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 11,
      non_editable_cols = c(0:3),
      page_length = 1,
      comma_rows = 0,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  
  output$evsi_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 12,
      non_editable_cols = c(0,1,2,3,4,5),
      page_length = 10,
      comma_rows = 0:4,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  
  output$freight_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 13,
      non_editable_cols = c(0:3),
      page_length = 10,
      comma_rows = 0:4,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  
  output$expansion_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 14,
      non_editable_cols = c(0:5),
      page_length = 10,
      comma_rows = 0:4,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  
  output$transit_cuts_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 17, #slchanged
      non_editable_cols = c(0:5),
      page_length = 10,
      comma_rows = 0:7,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  output$land_use_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    # temp_send$value[temp_send$table_no_ui == 16]<- temp_send$value[temp_send$table_no_ui  ==16]/1000000 
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 16,
      non_editable_cols =c(0:4),
      page_length = 10,
      comma_rows = 0:15,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  
  output$road_resurf_projscumu_tbl <- renderDT({
    
    req(rvs$Projectscumu)
    temp_send <- rvs$Projectscumu
    
    render_custom_datatable(
      data_reactive = temp_send,
      table_number = 15, #slchanged
      non_editable_cols = c(0:3),
      page_length = 10,
      comma_rows = 0:2,
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
    
  })
  
  # BUDGET: Render ---------------------------------------------------
  
  
  output$bikeped_budget_tbl <- renderDT({
    req(rvs$Budget)
    
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 1,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))}
    else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 1,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  
  output$transit_fr_budget_tbl <- renderDT({
    req(rvs$Budget)
    
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 2,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0),
        pivot_col = c("transit_mode"))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 2,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0),
        pivot_col = c("transit_mode"))
    }
  })
  
  
  output$transit_dr_budget_tbl <- renderDT({
    req(rvs$Budget)
    
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 3,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0),
        pivot_col = c("transit_mode"))
    }else{
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 3,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0),
        pivot_col = c("transit_mode"))
    }
  })
  
  output$transit_elec_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 4,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:4),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 4,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:3),#c(0:4),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$transit_bus_priority_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 5,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 5,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$rail_budget_tbl <- renderDT({
    req(rvs$Budget)
    
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 6,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0),
        pivot_col = c("area_type"))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 6,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0),
        pivot_col = c("area_type"))
    }
  })
  
  output$tdm_budget_tbl <- renderDT({
    req(rvs$Budget)
    
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 7,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 7,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$micromobility_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 8,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 8,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
    
  })
  
  output$traffic_ops_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 9,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:2),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0)#,
        #pivot_col = c("road_class")
      ) 
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 9,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:3),#c(0:2),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0)#,
        #pivot_col = c("road_class")
      )
    }
  })
  
  output$mdhd_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 10,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 10,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$pnr_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 11,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 11,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$ev_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 12,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0),#c(0:2),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 12,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:2),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$freight_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 13,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 13,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$expansion_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 14,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 14,
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$land_use_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      # browser()
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 16, #slchanged
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0),#c(0:2),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 16, #slchanged
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:2),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$transit_cuts_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 17, #slchanged
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 17, #slchanged
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:2),#c(0:3),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  output$resurfacing_budget_tbl <- renderDT({
    req(rvs$Budget)
    if (input$mode_choice == 'Budget'){
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 15, #slchanged
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    } else {
      render_custom_datatable(
        data_reactive = rvs$Budget,
        table_number = 15, #slchanged
        is_year_table = FALSE,
        is_cost_table = FALSE,
        is_advanced_table = FALSE,
        is_budget_table = TRUE,
        non_editable_cols = c(0:1),#c(0:1),
        page_length = 21,
        comma_rows = integer(0),
        percent_rows = integer(0),
        currency_rows = integer(0),
        decimal_rows = integer(0))
    }
  })
  
  # BUDGET: Editable --------------------------------------------------------
  # observe edits to the bikeped_projs
  
  observeEvent(input$bikeped_budget_tbl_cell_edit, {
    req(rvs$Budget)
    
    rvs$Budget[rvs$Budget$table_no_ui == 1,] <- reshaping_budget(input$bikeped_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 1,
                                                                 col1 = 'area_type',
                                                                 col2 = 'facility_type')
  })
  
  # observe edits to the transit_fixed_projs
  observeEvent(input$transit_fr_budget_tbl_cell_edit, {
    req(rvs$Budget)
    
    rvs$Budget[rvs$Budget$table_no_ui == 2,] <- reshaping_budget(input$transit_fr_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 2,
                                                                 col1 = 'area_type',
                                                                 col2 = 'fuel_type',
                                                                 col3 = 'transit_mode')
  })
  
  # observe edits to the transit_dr_projscumu_tbl_cell_edit
  observeEvent(input$transit_dr_budget_tbl_cell_edit, {
    req(rvs$Budget)
    
    rvs$Budget[rvs$Budget$table_no_ui == 3,] <- reshaping_budget(input$transit_dr_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 3,
                                                                 col1 = 'area_type',
                                                                 col2 = 'fuel_type',
                                                                 col3 = 'transit_mode')
  })
  
  # observe edits to the transit_el_projs_tbl
  observeEvent(input$transit_elec_budget_tbl_cell_edit, {
    req(rvs$Budget)
    
    rvs$Budget[rvs$Budget$table_no_ui == 4,] <- reshaping_budget(input$transit_elec_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 4,
                                                                 col1 = 'area_type',
                                                                 col2 = 'fuel_type',
                                                                 col3 = 'transit_mode')
  })
  
  # observe edits to the transit_bus_projs_tbl  
  observeEvent(input$transit_bus_priority_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 5,] <- reshaping_budget(input$transit_bus_priority_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 5,
                                                                 col1 = 'unit')
  })
  
  # observe edits to the public_rail_projs table
  observeEvent(input$rail_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 6,] <- reshaping_budget(input$rail_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 6,
                                                                 col1 = 'fuel_type',
                                                                 col2 = 'transit_mode')
  })
  
  # observe edits to the tdm_projs_tbl table
  observeEvent(input$tdm_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 7,] <- reshaping_budget(input$tdm_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 7,
                                                                 col1 = 'unit')
    
  })
  
  # observe edits to the micro_projs_tbl table
  observeEvent(input$micromobility_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 8,] <- reshaping_budget(input$micromobility_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 8,
                                                                 col1 = 'unit')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the traffic_ops_projs_tbl table
  observeEvent(input$traffic_ops_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 9,] <- reshaping_budget(input$traffic_ops_budget_tbl_cell_edit,
                                                                 rvs$Budget,
                                                                 tbl_no = 9,
                                                                 col1 = 'area_type',
                                                                 col2 = 'road_class',
                                                                 col3 = 'unit'
    )
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the mhdev_projs_tbl table
  observeEvent(input$mdhd_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 10,] <- reshaping_budget(input$mdhd_budget_tbl_cell_edit,
                                                                  rvs$Budget,
                                                                  tbl_no = 10,
                                                                  col1 = 'veh_type',
                                                                  col2 = 'fuel_type')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the mhdev_projs_tbl table
  observeEvent(input$pnr_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 11,] <- reshaping_budget(input$pnr_budget_tbl_cell_edit,
                                                                  rvs$Budget,
                                                                  tbl_no = 11,
                                                                  col1 = 'unit')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the evsi_budget_tbl table
  observeEvent(input$ev_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 12,] <- reshaping_budget(input$ev_budget_tbl_cell_edit,
                                                                  rvs$Budget,
                                                                  tbl_no = 12,
                                                                  col1 = 'charge_port_detail')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the freight_budget_tbl table
  observeEvent(input$freight_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 13,] <- reshaping_budget(input$freight_budget_tbl_cell_edit,
                                                                  rvs$Budget,
                                                                  tbl_no = 13,
                                                                  col1 = 'unit')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  # observe edits to the expansion_budget_tbl table
  observeEvent(input$expansion_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 14,] <- reshaping_budget(input$expansion_budget_tbl_cell_edit,
                                                                  rvs$Budget,
                                                                  tbl_no = 14,
                                                                  col1 = 'area_type',
                                                                  col2 = 'road_class')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  observeEvent(input$transit_cuts_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 17,] <- reshaping_budget(input$transit_cuts_budget_tbl_cell_edit, #slchanged
                                                                  rvs$Budget,
                                                                  tbl_no = 17,
                                                                  col1 = 'area_type',
                                                                  col2 ='transit_mode')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  observeEvent(input$land_use_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 16,] <- reshaping_budget(input$land_use_budget_tbl_cell_edit, #slchanged
                                                                  rvs$Budget,
                                                                  tbl_no = 16,
                                                                  col1 = 'land_use',
                                                                  col2 = 'unit')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  observeEvent(input$resurfacing_budget_tbl_cell_edit, {
    
    rvs$Budget[rvs$Budget$table_no_ui == 15,] <- reshaping_budget(input$resurfacing_budget_tbl_cell_edit, #slchanged
                                                                  rvs$Budget,
                                                                  tbl_no = 15,
                                                                  col1 = 'unit')
    
    
    #updated_table[user_data$row,"value"] <- as.numeric(user_data$value)
    
  })
  
  
  #BUDGET - PROJECT IO --------------------------------------------------
  observeEvent(input$fill_projects_bttn,{
    
    if(!isTruthy(input$budget_start_year)|!isTruthy(input$budget_years_covered)|!isTruthy(input$budget_total)){
      warning <- "Input(s) in the budget tab have been set to a value that is not usable by the tool:"
      warning_add_1 <- ifelse(!isTruthy(input$budget_start_year), " Budget start year,","")
      warning_add_2 <- ifelse(!isTruthy(input$budget_years_covered), " Budget years covered,","")
      warning_add_3 <- ifelse(!isTruthy(input$budget_total), " Budget total,","")
      
      warning <- paste0(warning, warning_add_1, warning_add_2, warning_add_3)  
      warning <- warning |> trimws() |>  str_sub(start = 1, end = nchar(warning) -1)
      showNotification(HTML(warning), type = "error")
    }
    
    req(input$budget_start_year)
    req(input$budget_years_covered)
    req(input$budget_total)
    
    temp_budget <- rvs$Budget |> 
      mutate(value = value/100)
    temp_budget$table_no_ui_revised = as.character(temp_budget$table_no_ui_revised)
    
    temp_costs <- rvs$Costs 
    temp_costs$table_no_ui_revised = as.character(temp_costs$table_no_ui_revised)
    temp_costs <- temp_costs |> 
      filter(table_no_ui_revised != "-1") |> 
      group_by(table_no_ui_revised) |> 
      summarise(cost_parameter = sum(value)) |> ungroup()
    
    temp_join <- left_join(temp_budget,temp_costs) |> 
      mutate(cost_parameter = case_when(!is.na(land_use) ~ 1, 
                                        !is.na(land_use)&land_use == "Land Use Incentives"~1000000,
                                        TRUE ~ cost_parameter)) #|> mutate(value = 1)
    #start <- input$horizon_year_1
    #total <- input$budget_total
    # start <- input$budget_start_year
    # end <- start + input$budget_years_covered 
    # total <- input$budget_total*1000000
    # total_years <- input$budget_years_covered #+ 1 ## QS: is this used somewhere else?
    # 
    # # horizon_year_1_cnt <- sum(c(start:end) < input$horizon_year_1)
    # # horizon_year_2_cnt <- sum(c(start:end) < input$horizon_year_2) - horizon_year_1_cnt
    # # horizon_year_3_cnt <- sum(c(start:end) < input$horizon_year_3) - horizon_year_2_cnt - horizon_year_1_cnt
    # 
    # #heck <- temp_join |> mutate(check = (total*value)/cost_parameter)
    # foo <- temp_join |> 
    #   dplyr::mutate(horizon_year_1_budget = total*value*(horizon_year_1_cnt/total_years),
    #          horizon_year_2_budget = total*value*(horizon_year_2_cnt/total_years),
    #          horizon_year_3_budget = total*value*(horizon_year_3_cnt/total_years),
    #          horizon_year_1 = horizon_year_1_budget/cost_parameter,
    #          horizon_year_2 = horizon_year_2_budget/cost_parameter,
    #          horizon_year_3 = horizon_year_3_budget/cost_parameter) |> select(-c(table_no_ui,value,table_no_ui_revised)) |> 
    #   pivot_longer(cols = c(horizon_year_1, horizon_year_2, horizon_year_3), names_to = "year", values_to = "value") |> 
    #   #mutate(value = value_new) |> 
    #   select(any_of(names(rvs$Projects))) 
    # temp_projects <- rvs$Projects |> select(-value)
    # 
    # # browser()
    # 
    # # foo[is.na(foo)] <- "NA"  # Qi: comment out? 
    # # temp_projects[is.na(temp_projects)] <- "NA"  # Qi: comment out? 
    # foobar <- left_join(temp_projects, foo) 
    # foobar[foobar == "NA"] <- NA
    # foobar[is.na(foobar$value),"value"]<-0
    # #foobar <- foobar |> mutate(value = value_new) |> select(-value_new)
    # #browser()
    # rvs$Projects <- foobar
    
    start <- input$budget_start_year
    end <- start + input$budget_years_covered -1
    total <- input$budget_total*1000000
    total_years <- input$budget_years_covered #+ 1 # qi commented out +1
    
    year1 <- input$horizon_year_1
    year2 <- ifelse (input$horizon_year_2 <= 2040, input$horizon_year_2, 2040)
    year3 <- ifelse (input$horizon_year_3 <= 2040, input$horizon_year_3, 2040)
    
    foo <- temp_join |>
      dplyr::mutate(horizon_year_1_cnt = 
                      case_when(category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                                                "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                                                "Park-and-Ride") ~ ifelse(year1 - start < input$budget_years_covered, ifelse(year1 - start >0, year1 - start, 0),  input$budget_years_covered),
                                category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                                                "Public Transportation: Rail", "Bus Priority Treatment",
                                                "Travel Demand Management", "EV Charging Infrastructure",
                                                "Transit Service Cuts", "Micromobility",
                                                "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year1)  <= input$budget_years_covered, sum(c(start:end) <= year1),  input$budget_years_covered),
                                TRUE ~ ifelse(year1 - (start+2) < input$budget_years_covered, ifelse(year1 - (start+2) >0, year1 - (start+2), 0),  input$budget_years_covered)
                      ),
                    horizon_year_2_cnt = 
                      case_when(category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                                                "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                                                "Park-and-Ride") ~ ifelse(year2 - start < input$budget_years_covered, ifelse(year2 - start >0, year2 - start, 0),  input$budget_years_covered),
                                category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                                                "Public Transportation: Rail", "Bus Priority Treatment",
                                                "Travel Demand Management", "EV Charging Infrastructure",
                                                "Transit Service Cuts", "Micromobility",
                                                "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year2)  <= input$budget_years_covered, sum(c(start:end) <= year2),  input$budget_years_covered),
                                TRUE ~ ifelse(year2 - (start+2) < input$budget_years_covered, ifelse(year2 - (start+2) >0, year2 - (start+2), 0),  input$budget_years_covered)
                      ),
                    horizon_year_3_cnt = 
                      case_when(category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                                                "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                                                "Park-and-Ride") ~ ifelse(year3 - start < input$budget_years_covered, ifelse(year3 - start >0, year3 - start, 0),  input$budget_years_covered),
                                category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                                                "Public Transportation: Rail", "Bus Priority Treatment",
                                                "Travel Demand Management", "EV Charging Infrastructure",
                                                "Transit Service Cuts", "Micromobility",
                                                "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year3)  <= input$budget_years_covered, sum(c(start:end) <= year3),  input$budget_years_covered),
                                TRUE ~ ifelse(year3 - (start+2) < input$budget_years_covered, ifelse(year3 - (start+2) >0, year3 - (start+2), 0),  input$budget_years_covered)
                      )
      ) |>
      dplyr::mutate(horizon_year_1_budget = total*value*(horizon_year_1_cnt/total_years),
                    horizon_year_2_budget = total*value*(horizon_year_2_cnt/total_years),
                    horizon_year_3_budget = total*value*(horizon_year_3_cnt/total_years),
                    horizon_year_1 = horizon_year_1_budget/cost_parameter,
                    horizon_year_2 = horizon_year_2_budget/cost_parameter,
                    horizon_year_3 = horizon_year_3_budget/cost_parameter) |> select(-c(table_no_ui,value,table_no_ui_revised)) |>
      pivot_longer(cols = c(horizon_year_1, horizon_year_2, horizon_year_3), names_to = "year", values_to = "value") |>
      #mutate(value = value_new) |>
      select(any_of(names(rvs$Projects)))
    temp_projects <- rvs$Projects |> select(-value)
    
    # browser()
    
    # foo[is.na(foo)] <- "NA"  # Qi: comment out?
    # temp_projects[is.na(temp_projects)] <- "NA"  # Qi: comment out?
    foobar <- left_join(temp_projects, foo)
    foobar[foobar == "NA"] <- NA
    foobar[is.na(foobar$value),"value"]<-0
    #foobar <- foobar |> mutate(value = value_new) |> select(-value_new)

    ## the landuse $ is different, it need special treat as it is $Million. 
    foobar$value[foobar$table_no_ui == 16]<- foobar$value[foobar$table_no_ui  ==16]/1000000 
    ### at the step above, the projects value are cumulative values, however, under the project mode, the cumulative step is done afterward, so we need to de-cumulate the value. 
    foobar_discum <- foobar %>% 
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(horizon_year_1 = horizon_year_1, 
             horizon_year_3 = horizon_year_3- horizon_year_2,
             horizon_year_2 = horizon_year_2 - horizon_year_1) %>%
      pivot_longer(cols = starts_with("horizon_year_"),
                   names_to = "year", 
                   values_to = 'value')
    
    
    rvs$Projects <- foobar_discum
  })
  
  observeEvent(input$fill_budget_bttn,{
    #browser()
    if(!isTruthy(input$budget_total)){
      warning <- "Input(s) in the budget tab have been set to a value that is not usable by the tool: Budget total is inccorect"
      
      #warning <- paste0(warning, warning_add_1, warning_add_2, warning_add_3)  |> str_sub(end = -1)
      showNotification(HTML(warning), type = "error")
    }
    req(input$budget_start_year)
    req(input$budget_years_covered)
    req(input$budget_total)
    temp_budget <- rvs$Budget |> 
      mutate(value = value/100)
    temp_budget$table_no_ui_revised = as.character(temp_budget$table_no_ui_revised)
    
    temp_costs <- rvs$Costs 
    temp_costs$table_no_ui_revised = as.character(temp_costs$table_no_ui_revised)
    temp_costs <- temp_costs |> 
      filter(table_no_ui_revised != "-1") |> 
      group_by(table_no_ui_revised) |> 
      summarise(cost_parameter = sum(value))
    
    temp_join <- left_join(temp_budget,temp_costs) |> 
      mutate(cost_parameter = case_when(!is.na(land_use) ~ 1, 
                                        !is.na(land_use)&land_use == "Land Use Incentives"~1000000,
                                        TRUE ~ cost_parameter)) |> 
      select(-value)
    #start <- input$horizon_year_1
    #total <- input$budget_total
    #start <- input$budget_start_year
    #end <- start + input$budget_years_covered
    total <- input$budget_total*1000000
    #total_years <- sum(c(start:end))
    #horizon_year_1_cnt <- sum(c(start:end) < input$horizon_year_1)
    #horizon_year_2_cnt <- sum(c(start:end) < input$horizon_year_2) - horizon_year_1_cnt
    #horizon_year_3_cnt <- sum(c(start:end) < input$horizon_year_3) - horizon_year_2_cnt - horizon_year_1_cnt
    temp_projects <- rvs$Projects
    temp_join[is.na(temp_join)] <- "NA"
    temp_projects[is.na(temp_projects)] <- "NA"
    
    temp_projects <- temp_projects |> group_by_all() |> ungroup(c(year,value)) |> summarise(value = sum(value)) |> select(-table_no_ui)
    foo <- left_join(temp_join, temp_projects) |> 
      mutate(value = 100*(value*cost_parameter/total)) |>#View()
      select(-cost_parameter)
    foo[foo == "NA"] <- NA
    foo[is.na(foo$value),"value"]<-0
    
    
    #foobar <- foobar |> mutate(value = value_new) |> select(-value_new)
    #browser()
    
    
    rvs$Budget <- foo
  })
  # FUNDING: Render ---------------------------------------------------------
  #warning_count_funding <- 0 
  observe({
    req(input$budget_total)
    #browser()
    tempf<-rvs$Funding |> select(-perc_allocated)
    total <- rvs$Budget$value |> sum(na.rm = T)
    tempb<-rvs$Budget |> 
      mutate(category = case_when(
        category == "Bicycle and Pedestrian" ~ "Bicycle and Pedestrian Lane Miles of New Infrastructure",
        category == "Transit: Increased Fixed Route Service" ~ "Transit: Increased Fixed Route Service",
        category == "Transit: Increased Demand Response Service" ~ "Transit: Increased Demand Response Service",
        category == "Transit: Fleet Electrification" ~ "Transit: Fleet Electrification",
        category == "Bus Priority Treatment" ~ "Bus Priority Treatment",
        category == "Public Transportation: Rail" ~ "Public Transportation: Rail",
        category == "Travel Demand Management"~"Travel Demand Management",
        category == "Micromobility"~"Micromobility",
        category == "Traffic Operations"~"Traffic Operations",
        category == "Medium- and Heavy-Duty Vehicle Replacement"~ "Medium- and Heavy-Duty Vehicle Replacement",
        category == "Park-and-Ride"~"Park-and-Ride",
        category == "EV Charging Infrastructure" ~ "Charging Infrastructure and EV Incentives",
        category == "Freight Intermodal Facilities" ~ "Freight Intermodal Facilities",
        category == "Roadway expansion" ~ "Roadway Expansion",
        category == "Roadway Resurfacing"~"Roadway Resurfacing",
        category == "Land Use" ~ "Land Use", 
        # category == "Transit Service Cuts" ~ "Transit Service Cuts",
        TRUE ~ "zzzERROR")
      ) |> 
      group_by(category) |> 
      summarise(perc_allocated = sum(value, na.rm = T)) |>
      add_row(category = "Total","perc_allocated" = total)
    
    
    
    temp <- left_join(tempf, tempb, by = c("funding_summary" = "category"))
    #browser()
    #temp[,"perc_allocated"] <- temp[,"perc_allocated"]/100
    temp[,length(temp) - 1] <- temp[,"perc_allocated"]/100*input$budget_total
    #browser()
    if(sum(temp$perc_allocated[temp$funding_name == "total_funding"],na.rm = T) > 100){
      #warning_count_funding <<- warning_count_funding + 1
      warning = HTML("You have programmed more than 100% of your total budget.")
      showNotification(HTML(warning), type = "warning")
    }# else {warning_count_funding <<- 0}
    rvs$Funding <- temp
  })
  
  output$funding_summary_tbl <- renderDataTable({
    #browser()
    formatted_funding <- rvs$Funding %>%
      select(-funding_name) %>%
      rename(any_of(references_vector))
    
    datatable(
      formatted_funding,
      class = "compact",
      options = list(
        columnDefs = list(
          list(orderable = FALSE, targets = "_all"),
          list(width = '60px', targets = 0) # making first column narrow
        ),
        paging = FALSE,
        dom = "t"
      ),
      rownames = FALSE,
      callback = JS("
table.on('draw', function(){
  // Disable clicking in first four columns
  table.columns([0,1,2,3]).nodes().flatten().to$().addClass('no-click');
  
  //Adding a max width for the first column because columndefs is not making the change
  table.columns(0).nodes().flatten().to$().css('width', '60px');

  // Add a custom class to first column for styling
  table.columns([0]).nodes().flatten().to$().addClass('first-column');

  // Bold the last row
  var api = table;
  var rows = api.rows({ page: 'current' }).nodes();
  var lastRowIndex = api.rows().data().length - 1;

  rows.each(function(row, i){
    if (i === lastRowIndex) {
      $(row).find('td').css('font-weight', 'bold');
    }
  });
});
    "),
      editable = list(
        target = 'all',
        disable = list(columns = 0:4)
      ),
      selection = "none"
    ) %>%
      formatCurrency("% Allocated", digits = 1, currency = "%", before = FALSE) %>%
      formatCurrency("Million $", digits = 1) %>%
      formatStyle(
        "Million $",
        background = styleColorBar(formatted_funding[["Million $"]], "#66cc66"),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle(
        "% Allocated",
        background = styleColorBar(formatted_funding[["% Allocated"]], "#b266b2"),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
  
  # server assumptions ------------------------------------------------------
  
  # assumptions_names <- c("bikeped_assmps",
  #                        "transit_assmps",
  #                        "tdm_assmps",
  #                        "micro_assmps",
  #                        "traffic_ops_assmps",
  #                        "mhdv_assmps",
  #                        "pnr_assmps",
  #                        "evsi_assmps",
  #                        "landuse_assmpts")
  
  # read_static_tables("data/assumptions.xlsx", assumptions_names)
  
  
  ## create assumption tables -----------------------------------------------------------
  
  
  output$bikeped_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 1,
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),  
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = 2:7)
  })
  
  
  
  output$transit_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 2,
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),
      page_length = 50,
      comma_rows = c(18, 47),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = c(0:17,19:46))
  })
  
  output$tdm_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 7, #slchanged
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = c(0:2))
  })
  
  output$micro_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 8, #slchanged
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = c(0:4))
  })
  
  output$traffic_ops_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 9, #slchanged
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),
      page_length = 16,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = c(0:15))
  })
  
  output$mhdv_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 10, #slchanged
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = c(0:15))
  })
  
  output$pnr_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 11, #slchanged
      is_year_table = FALSE,
      non_editable_cols = c(0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = c(0:15))
  })
  
  output$evsi_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 12, #slchanged
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = c(0:15))
  })
  
  output$landuse_assmps_tbl <- renderDT({
    req(rvs$Assumptions)
    
    render_custom_datatable(
      data_reactive = rvs$Assumptions,
      table_number = 16, #slchanged
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = c(0:15))
  })
  
  ## make tables editable ----------------------------------------------------
  
  assumptions_names <- c("bikeped_assmps",
                         "transit_assmps",
                         "tdm_assmps",
                         "micro_assmps",
                         "traffic_ops_assmps",
                         "mhdv_assmps",
                         "pnr_assmps",
                         "evsi_assmps",
                         "landuse_assmps")
  
  # observe edits to bikeped_assmps
  observeEvent(input$bikeped_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 1,] <- reshaping_assmp(input$bikeped_assmps_tbl_cell_edit,
                                                                          rvs$Assumptions,
                                                                          tbl_no = 1)
  })
  
  #observe edits to transit_assmps
  observeEvent(input$transit_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 2,] <- reshaping_assmp(input$transit_assmps_tbl_cell_edit,
                                                                          rvs$Assumptions,
                                                                          tbl_no = 2
    )
  })
  
  #observe edits to tdm_assmps
  observeEvent(input$tdm_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 7,] <- reshaping_assmp(input$tdm_assmps_tbl_cell_edit, #slchanged
                                                                          rvs$Assumptions,
                                                                          tbl_no = 7)
  })
  
  #observe edits to micro_assmps
  observeEvent(input$micro_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 8,] <- reshaping_assmp(input$micro_assmps_tbl_cell_edit, #slchanged
                                                                          rvs$Assumptions,
                                                                          tbl_no = 8)
  })
  
  #observe edits to traffic_ops_assmps
  observeEvent(input$traffic_ops_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 9,] <- reshaping_assmp(input$traffic_ops_assmps_tbl_cell_edit, #slchanged
                                                                          rvs$Assumptions,
                                                                          tbl_no = 9)
  })
  
  #observe edits to mhdv_assmps
  observeEvent(input$mhdv_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 10,] <- reshaping_assmp(input$mhdv_assmps_tbl_cell_edit, #slchanged
                                                                           rvs$Assumptions,
                                                                           tbl_no = 10)
  })
  
  #observe edits to pnr_assmps
  observeEvent(input$pnr_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 11,] <- reshaping_assmp(input$pnr_assmps_tbl_cell_edit, #slchanged
                                                                           rvs$Assumptions,
                                                                           tbl_no = 11)
  })
  
  #observe edits to evsi_assmps
  observeEvent(input$evsi_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 12,] <- reshaping_assmp(input$evsi_assmps_tbl_cell_edit, #slchanged
                                                                           rvs$Assumptions,
                                                                           tbl_no = 12)
  })
  
  #observe edits to landuse_assmps
  observeEvent(input$landuse_assmps_tbl_cell_edit, {
    req(rvs$Assumptions)
    
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 16,] <- reshaping_assmp(input$landuse_assmps_tbl_cell_edit,#slchanged
                                                                           rvs$Assumptions,
                                                                           tbl_no = 16)
  })
  
  
  
  # observe reset button on budget ------------------------------------------
  
  # adrienne here
  
  observeEvent(input$reset_bikeped_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 1,] <- initial_budget[initial_budget$table_no_ui == 1, ]
  })
  
  observeEvent(input$reset_transit_fr_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 2,] <- initial_budget[initial_budget$table_no_ui == 2, ]
  })
  
  observeEvent(input$reset_transit_dr_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 3,] <- initial_budget[initial_budget$table_no_ui == 3, ]
  })
  
  observeEvent(input$reset_transit_elec_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 4,] <- initial_budget[initial_budget$table_no_ui == 4, ]
  })
  
  observeEvent(input$reset_transit_bus_priority_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 5,] <- initial_budget[initial_budget$table_no_ui == 5, ]
  })
  
  observeEvent(input$reset_rail_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 6,] <- initial_budget[initial_budget$table_no_ui == 6, ]
  })
  
  observeEvent(input$reset_tdm_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 7,] <- initial_budget[initial_budget$table_no_ui == 7, ]
  })
  
  observeEvent(input$reset_micromobility_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 8,] <- initial_budget[initial_budget$table_no_ui == 8, ]
  })
  
  observeEvent(input$reset_traffic_ops_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 9,] <- initial_budget[initial_budget$table_no_ui == 9, ]
  })
  
  observeEvent(input$reset_mdhd_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 10,] <- initial_budget[initial_budget$table_no_ui == 10, ]
  })
  
  observeEvent(input$reset_pnr_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 11,] <- initial_budget[initial_budget$table_no_ui == 11, ]
  })
  
  observeEvent(input$reset_ev_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 12,] <- initial_budget[initial_budget$table_no_ui == 12, ]
  })
  
  observeEvent(input$reset_freight_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 13,] <- initial_budget[initial_budget$table_no_ui == 13, ]
  })
  
  
  observeEvent(input$reset_expansion_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 14,] <- initial_budget[initial_budget$table_no_ui == 14, ]
  })
  
  observeEvent(input$reset_land_use_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 16,] <- initial_budget[initial_budget$table_no_ui == 16, ] #slchanged
  })
  
  observeEvent(input$reset_transit_cuts_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 17,] <- initial_budget[initial_budget$table_no_ui == 17, ] #slchanged
  })
  
  observeEvent(input$reset_resurfacing_budget_tbl, {
    rvs$Budget[rvs$Budget$table_no_ui == 15,] <- initial_budget[initial_budget$table_no_ui == 15, ] #slchanged
  })
  
  
  
  # observe reset buttons on assumptions ---------------------------------------------------
  
  
  observeEvent(input$reset_bikeped_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 1,] <- initial_assumptions[initial_assumptions$table_no_ui == 1, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_transit_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 2,] <- initial_assumptions[initial_assumptions$table_no_ui == 2, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_tdm_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 7,] <- initial_assumptions[initial_assumptions$table_no_ui == 7, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_micro_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 8,] <- initial_assumptions[initial_assumptions$table_no_ui == 8, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_traffic_ops_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 9,] <- initial_assumptions[initial_assumptions$table_no_ui == 9, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_mhdv_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 10,] <- initial_assumptions[initial_assumptions$table_no_ui == 10, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_pnr_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 11,] <- initial_assumptions[initial_assumptions$table_no_ui == 11, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_evsi_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 12,] <- initial_assumptions[initial_assumptions$table_no_ui == 12, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_landuse_assmps_tbl, {
    rvs$Assumptions[rvs$Assumptions$table_no_ui == 16,] <- initial_assumptions[initial_assumptions$table_no_ui == 16, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  
  # COST: inputs server ------------------------------------------------------------
  ## COST: create input tables -----------------------------------------------------------
  
  output$bikeped_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 1,
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:1),
      page_length = 21,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:21),
      decimal_rows = integer(0))
  })
  
  
  output$transit_fixed_costs_tbl <- renderDT({
    req(rvs$Costs)
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 2,
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:2),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$transit_dr_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 3,
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:2),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$pub_trans_priority_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 5, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$pub_trans_rail_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 6, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:1),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$tdm_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 7, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$micro_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 8, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$traffic_ops_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 9, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:2),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$mhdev_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 10, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:1),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  
  output$pnr_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 11, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$evsi_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 12, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$roadway_expand_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 14, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:1),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$fuel_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 18, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$roadwayresurf_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 15,
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$intermodal_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 13, #slchange
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  output$landuse_costs_tbl <- renderDT({
    req(rvs$Costs)
    
    render_custom_datatable(
      data_reactive = rvs$Costs,
      table_number = 16,
      is_year_table = FALSE,
      is_cost_table = TRUE,
      non_editable_cols = c(0:0),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = c(0:10),
      decimal_rows = integer(0))
  })
  
  ## COST: make editable -----------------------------------------------------------
  
  
  #reshaping
  #observe change to bikeped_costs_tbl
  observeEvent(input$bikeped_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 1,] <- reshaping_cost(input$bikeped_costs_tbl_cell_edit,
                                                             rvs$Costs,
                                                             num_col = 2,
                                                             tbl_no = 1,
                                                             unit1 = 'per_mile_cost',
                                                             unit2 = 'per_mile_maintain_cost',
                                                             col_list = c('area_type',
                                                                          'cap_proj_type',
                                                                          'unit'))
  })
  
  
  
  ## observe change to transit_fixed_costs
  observeEvent(input$transit_fixed_costs_tbl_cell_edit, {
    req(rvs$Costs)
    print('the reshape for table 2 is running')
    rvs$Costs[rvs$Costs$table_no_ui == 2,] <- reshaping_cost(input$transit_fixed_costs_tbl_cell_edit,
                                                             rvs$Costs,
                                                             num_col = 3, # how many numeric columns 
                                                             tbl_no = 2,
                                                             unit1 = 'per_veh_cap_cost',
                                                             unit2 = 'per_VRM_fuel_cost',
                                                             unit3 = 'per_VRM_onm_cost',
                                                             col_list = c('area_type',
                                                                          'fuel_type',
                                                                          'unit'))
  })
  
  ## observe change to transit_dr_costs
  observeEvent(input$transit_dr_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 3,] <- reshaping_cost(input$transit_dr_costs_tbl_cell_edit,
                                                             rvs$Costs,
                                                             tbl_no = 3,
                                                             num_col = 3, # how many numeric columns,
                                                             unit1 = 'per_veh_cap_cost',
                                                             unit2 = 'per_VRM_fuel_cost',
                                                             unit3 = 'per_VRM_onm_cost',
                                                             col_list = c('area_type',
                                                                          'fuel_type',
                                                                          'transit_mode',
                                                                          'unit'))
  })
  
  ## observe change to pub_trans_priority_costs
  observeEvent(input$pub_trans_priority_costs_tbl_cell_edit, {
    req(rvs$Costs)  
    
    rvs$Costs[rvs$Costs$table_no_ui == 5,] <- reshaping_cost(input$pub_trans_priority_costs_tbl_cell_edit, #slchanged
                                                             rvs$Costs,
                                                             tbl_no = 5,
                                                             num_col = 1,
                                                             col_list = c('unit')
    )
  })
  
  
  ## observe change to pub_trans_rail_costs
  observeEvent(input$pub_trans_rail_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 6,] <- reshaping_cost(input$pub_trans_rail_costs_tbl_cell_edit, #slchanged
                                                             rvs$Costs,
                                                             tbl_no = 6,
                                                             num_col = 3, # how many numeric columns,
                                                             unit1 = 'per_veh_cap_cost',
                                                             unit2 = 'per_VRM_fuel_cost',
                                                             unit3 = 'per_VRM_onm_cost',
                                                             col_list = c('fuel_type',
                                                                          'transit_mode',
                                                                          'unit')
    )
  })
  
  
  ## observe change to pub_trans_rail_costs
  observeEvent(input$tdm_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 7,] <- reshaping_cost(input$tdm_costs_tbl_cell_edit, #slchanged
                                                             rvs$Costs,
                                                             tbl_no = 7,
                                                             num_col = 1,
                                                             col_list = c('unit'))
  })
  
  ## observe change to micro_costs
  observeEvent(input$micro_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 8,] <- reshaping_cost(input$micro_costs_tbl_cell_edit, #slchanged
                                                             rvs$Costs,
                                                             tbl_no = 8,
                                                             num_col = 1,
                                                             col_list = c('unit'))
  })
  
  ## observe change to traffic_ops_costs
  observeEvent(input$traffic_ops_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 9,] <- reshaping_cost(input$traffic_ops_costs_tbl_cell_edit, #slchanged
                                                             rvs$Costs,
                                                             tbl_no = 9,
                                                             num_col = 2, # how many numeric columns,
                                                             unit1 = 'annual_maintenance_cost',
                                                             unit2 = 'cost_per_improvement',
                                                             col_list = c('road_class',
                                                                          'area_type',
                                                                          'cap_proj_type',
                                                                          'unit'))
  })
  
  
  ## observe change to mhdev_costs
  observeEvent(input$mhdev_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 10,] <- reshaping_cost(input$mhdev_costs_tbl_cell_edit, #slchanged
                                                              rvs$Costs,
                                                              tbl_no = 10,
                                                              num_col = 3, # how many numeric columns,
                                                              unit1 = 'per_VRM_fuel_cost',
                                                              unit2 = 'per_mile_onm_cost',
                                                              unit3 = 'per_veh_cap_cost',
                                                              col_list = c('fuel_type',
                                                                           'veh_type',
                                                                           'unit'))
  })
  
  
  ## observe change to pnr_costs
  observeEvent(input$pnr_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 11,] <- reshaping_cost(input$pnr_costs_tbl_cell_edit, #slchanged
                                                              rvs$Costs,
                                                              tbl_no = 11,
                                                              num_col = 1,
                                                              col_list = c('unit'))
  })
  
  ## observe change to evsi_costs
  observeEvent(input$evsi_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 12,] <- reshaping_cost(input$evsi_costs_tbl_cell_edit, #slchanged
                                                              rvs$Costs,
                                                              tbl_no = 12,
                                                              num_col = 2, # how many numeric columns,
                                                              unit1 = 'per_unit_hardware_cost',
                                                              unit2 = 'per_unit_installation_cost',
                                                              col_list = c('DCFC_level',
                                                                           'unit'))
  })
  
  ## observe change to roadway_expand_costs
  observeEvent(input$roadway_expand_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 13,] <- reshaping_cost(input$roadway_expand_costs_tbl_cell_edit, #slchanged
                                                              rvs$Costs,
                                                              tbl_no = 13,
                                                              num_col = 2, # how many numeric columns,
                                                              unit1 = 'per_ln_mile_cap_cost',
                                                              unit2 = 'per_ln_mile_cost',
                                                              col_list = c('road_class',
                                                                           'area_type',
                                                                           'unit'))
  })
  
  ## observe change to fuel_costs
  observeEvent(input$fuel_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 18,] <- reshaping_cost(input$fuel_costs_tbl_cell_edit, #slchanged
                                                              rvs$Costs,
                                                              tbl_no = 18,
                                                              num_col = 1, # how many numeric columns,
                                                              col_list = c('fuel_type'))
  })
  
  
  ## observe change to intermodal_costs
  observeEvent(input$intermodal_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 13,] <- reshaping_cost(input$intermodal_costs_tbl_cell_edit, #slchanged
                                                              rvs$Costs,
                                                              tbl_no = 13,
                                                              num_col = 1,
                                                              col_list = c('unit'))
  }) # end of reshaping
  
  ## observe change to resurf
  observeEvent(input$roadwayresurf_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 15,] <- reshaping_cost(input$roadwayresurf_costs_tbl_cell_edit,
                                                              rvs$Costs,
                                                              tbl_no = 15,
                                                              num_col = 1,
                                                              col_list = c('unit'))
  }) # end of reshaping
  
  ## observe change to land use
  observeEvent(input$landuse_costs_tbl_cell_edit, {
    req(rvs$Costs)
    
    rvs$Costs[rvs$Costs$table_no_ui == 16,] <- reshaping_cost(input$landuse_costs_tbl_cell_edit,
                                                              rvs$Costs,
                                                              tbl_no = 16,
                                                              num_col = 1,
                                                              col_list = c('unit'))
  }) # end of reshaping
  
  ## COST: observe reset buttons for costs -----------------------------------------
  
  
  observeEvent(input$reset_bikeped_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 1,] <- initial_costs[initial_costs$table_no_ui == 1, ]
    #browser()
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_transit_fixed_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 2,] <- initial_costs[initial_costs$table_no_ui == 2, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_transit_dr_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 3,] <- initial_costs[initial_costs$table_no_ui == 3, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_pub_trans_priority_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 5,] <- initial_costs[initial_costs$table_no_ui == 5, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_tdm_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 6,] <- initial_costs[initial_costs$table_no_ui == 6, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_pub_trans_rail_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 7,] <- initial_costs[initial_costs$table_no_ui == 7, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_micro_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 8,] <- initial_costs[initial_costs$table_no_ui == 8, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_traffic_ops_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 9,] <- initial_costs[initial_costs$table_no_ui == 9, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_mhdev_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 10,] <- initial_costs[initial_costs$table_no_ui == 10, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_pnr_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 11,] <- initial_costs[initial_costs$table_no_ui == 11, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
    
  })  
  
  observeEvent(input$reset_evsi_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 12,] <- initial_costs[initial_costs$table_no_ui == 12, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_roadway_expand_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 14,] <- initial_costs[initial_costs$table_no_ui == 14, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_fuel_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 18,] <- initial_costs[initial_costs$table_no_ui == 18, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_intermodal_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 13,] <- initial_costs[initial_costs$table_no_ui == 13, ] #slchanged
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })
  
  observeEvent(input$reset_roadwayresurf_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 15,] <- initial_costs[initial_costs$table_no_ui == 15, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })    
  observeEvent(input$reset_landuse_costs_tbl, {
    rvs$Costs[rvs$Costs$table_no_ui == 16,] <- initial_costs[initial_costs$table_no_ui == 16, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  
  # SCENARIOS: inputs -------------------------------------------------
  
  
  strategy_names <- c("Bicycle and Pedestrian",
                      "Transit Service Expansion",
                      "Micromobility",
                      "Travel Demand Management",
                      "Park-and-Ride",
                      "Transit Electrification",
                      "Medium- and Heavy-duty Vehicle Replacement",
                      "Charging Infrastructure and EV Incentives",
                      "Intermodal Freight Investment",
                      "Traffic Operations",
                      "Roadway Expansion",
                      "Roadway Resurfacing",
                      "Land Use", 
                      "Transit Service Cuts"
  )
  
  rowName <- function(scenario) {
    as.character(
      checkboxInput(paste0("row", gsub(" ", "", scenario)), label = scenario)
    )
  }
  rowNames <- vapply(strategy_names, rowName, character(1))
  
  # Create a reactive data frame
  reactive_scenario <- reactiveVal()
  
  observe({
    req(rvs$Scenarios)
    reactive_scenario(rvs$Scenarios)
  })
  
  selected_scenario <- reactiveValues(rows = NULL)
  
  # Render the checkbox table
  output$scenario_tbl <- renderDT({
    #browser()
    datatable(
      reactive_scenario(),
      escape = FALSE, 
      #editable = list(target = c(2,3)),
      options = list(
        pageLength =15,
        paging = FALSE, 
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(3, 4)),
          list(render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    return '<input type=\"checkbox\" class=\"checkbox\" ' + (data ? 'checked' : '') + '/>';",
            "  }",
            "  return data;",
            "}"
          ), targets = c(3, 4))
        ),
        dom = 'Bfrtip'
      ),
      selection = 'multiple'
    )
  })
  
  # Update the reactive data when boxes are selected
  observeEvent(input$scenario_tbl_cell_clicked, {
    info <- input$scenario_tbl_cell_clicked
    if (!is.null(info$col) && info$col %in% c(3, 4)) {
      col_name <- colnames(reactive_scenario())[info$col]
      reactive_scenario_updated <- reactive_scenario()
      reactive_scenario_updated[info$row, col_name] <- !reactive_scenario_updated[info$row, col_name]
      reactive_scenario(reactive_scenario_updated)
    }
    rvs$Scenarios <- reactive_scenario()
    # browser()
  }) 
  
  shinyjs::enable(selector = ".checkbox")
  
  observeEvent(input$select_all_scenario1, {
    #browser()
    reactive_scenario_updated <- reactive_scenario()
    ifelse(sum(reactive_scenario_updated$Scenario1) == 12,reactive_scenario_updated$Scenario1<-FALSE,reactive_scenario_updated$Scenario1 <- TRUE)
    reactive_scenario(reactive_scenario_updated)
    rvs$Scenarios <- reactive_scenario()
  })
  
  # Observer to select all checkboxes under Scenario 2
  observeEvent(input$select_all_scenario2, {
    reactive_scenario_updated <- reactive_scenario()
    ifelse(sum(reactive_scenario_updated$Scenario2) == 12,reactive_scenario_updated$Scenario2<-FALSE,reactive_scenario_updated$Scenario2 <- TRUE)
    reactive_scenario(reactive_scenario_updated)
    rvs$Scenarios <- reactive_scenario()
  })
  
  # ADVANCED: inputs ---------------------------------------------------------
  
  # advanced_names <- c("ev_forecast_sheet",
  #                     "vmt_forecast_sheet",
  #                     "onroad_fuel_tech_frac_sheet",
  #                     "pass_rail_sheet",
  #                     "freight_rail_sheet",
  #                     "construction_sheet",
  #                     "fuel_apportionment_sheet")
  # 
  # read_static_tables("data/advanced.xlsx", advanced_names)
  # 
  
  ## ADVANCED: create tables -----------------------------------------------------------
  
  output$ev_forecast_sheet_tbl <- renderDT({
    render_custom_datatable(
      data_reactive = rvs$Advanced,
      table_number = 1,
      is_year_table = TRUE,
      is_advanced_table = TRUE,
      non_editable_cols = c(0), 
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0),
      pivot_col = c('year','veh_type','value'))
  })
  
  output$vmt_forecast_sheet_tbl <- renderDT({
    render_custom_datatable(
      data_reactive = rvs$Advanced,
      table_number = 2,
      is_year_table = TRUE,
      is_advanced_table = TRUE,
      non_editable_cols = c(0),  
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0),
      pivot_col = c('year','veh_type','value'))
  })
  
  output$onroad_fuel_tech_frac_sheet_tbl <- renderDT({
    
    render_custom_datatable(
      data_reactive = rvs$Advanced,
      table_number = 3,
      is_year_table = FALSE,
      is_advanced_table = TRUE,
      non_editable_cols = c(0, 1,2),
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
  })
  
  # SLFLAG - did you set this up? It does not look editable. Leaving this for now!
  
  output$pass_rail_sheet_tbl <- renderDT({
    #browser()
    
    
    # callback_pass_rail <- JS(
    #   "var tbl = $(table.table().node());",
    #   "var id = tbl.closest('.datatables').attr('id');",
    #   "function onUpdate() {",
    #   "  var cellinfo = [{",
    #   "    value: updatedCell.data()",
    #   "  }];",
    #   "  Shiny.setInputValue(id + '_cell_edit:DT.cellInfo', cellinfo);",
    #   "}",
    #   "table.MakeCellsEditable({",
    #   "  onUpdate: onUpdate,",
    #   "  inputCss: 'my-input-class',",
    #   "  columns: [2], ",
    #   "  rows: null, ",
    #   "  confirmationButton: {",
    #   "    confirmCss: 'my-confirm-class',",
    #   "    cancelCss: 'my-cancel-class'",
    #   "  },",
    #   "  inputTypes: [",
    #   "    {",
    #   "      column: 2,",
    #   "      type: 'list',",
    #   "      options: [",
    #   "        {value: 'Diesel', display: 'Diesel'},",
    #   "        {value: 'Electric',      display: 'Electric'},",
    #   "      ]",
    #   "    }",
    #   "  ]",
    #   "});")
    # 
    # path <- "./www" # folder containing the files dataTables.cellEdit.js
    # 
    # # and dataTables.cellEdit.css
    # dep <- htmltools::htmlDependency(
    #   "CellEdit", "1.0.19", path, 
    #   script = "dataTables.cellEdit.js", stylesheet = "dataTables.cellEdit.css")
    # 
    # dtable <- 
    #   rvs$Advanced %>%
    #   filter(table_no_ui == 4) %>% 
    #   select(mode_service, unit, value) %>% 
    #   rename(any_of(references_vector)) %>% 
    #   datatable(callback = callback_pass_rail, rownames = F)
    # 
    # ### OLD
    # # render_custom_datatable(
    # # data_reactive = rvs$Advanced,
    # # table_number = 4,
    # # is_year_table = FALSE,
    # # non_editable_cols = c(0, 1),
    # # page_length = 10,
    # # comma_rows = integer(0),
    # # percent_rows = integer(0),
    # # currency_rows = integer(0),
    # # decimal_rows = integer(0))
    # 
    # ### WORKS WITH A SIMPLE EXAMPLE BELOW
    # # dat_pass_rail <- data.frame(
    # #   Action = c("Keep data", "Keep data", "Keep data"),
    # #   X = c(1, 2, 3),
    # #   Y = c("a", "b", "c")
    # # )
    # # 
    # # ## the datatable
    # # dtable <- datatable(
    # #   dat_pass_rail, callback = callback_pass_rail, rownames = FALSE, 
    # #   options = list(
    # #     columnDefs = list(
    # #       list(targets = "_all", className = "dt-center")
    # #     )
    # #   )
    # # )
    # 
    # dtable$dependencies <- c(dtable$dependencies, list(dep))
    # return(dtable)
    render_custom_datatable(
      data_reactive = rvs$Advanced,
      table_number = 4,
      is_year_table = FALSE,
      non_editable_cols = c(0:1),  
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
  },server = FALSE)
  
  output$freight_rail_sheet_tbl <- renderDT({
    
    render_custom_datatable(
      data_reactive = rvs$Advanced,
      table_number = 5,
      is_year_table = FALSE,
      non_editable_cols = c(0),  
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
  })
  
  output$construction_sheet_tbl <- renderDT({
    
    render_custom_datatable(
      data_reactive = rvs$Advanced,
      table_number = 6,
      is_year_table = FALSE,
      non_editable_cols = c(0),  
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
  })
  
  output$fuel_apportionment_sheet_tbl <- renderDT({
    
    render_custom_datatable(
      data_reactive = rvs$Advanced,
      table_number = 7,
      is_year_table = FALSE,
      non_editable_cols = c(0, 1),  
      page_length = 10,
      comma_rows = integer(0),
      percent_rows = integer(0),
      currency_rows = integer(0),
      decimal_rows = integer(0))
  })
  ## ADVANCED: make editable -----------------------------------------------------------
  
  
  #I think these two top events should be removed
  # observeEvent(input$ev_forecast_edit, {
  #   ev_forecast <<- editData(ev_forecast, input$ev_forecast_edit, 'ev_forecast_tbl')
  # })
  # 
  # 
  # observeEvent(input$vmt_forecast_edit, {
  #   vmt_forecast <<- editData(vmt_forecast, input$vmt_forecast_edit, 'vmt_forecast_tbl')
  # })
  
  
  # reshaping ev_forecast_sheet_tbl  #checkpoint
  
  observeEvent(input$ev_forecast_sheet_tbl_cell_edit, {
    req(rvs$Advanced)
    
    rvs$Advanced[rvs$Advanced$table_no_ui == 1,] <- reshaping_advanced(input$ev_forecast_sheet_tbl_cell_edit,
                                                                       rvs$Advanced,
                                                                       tbl_no = 1,
                                                                       col_list = c('veh_type','year'))
  })
  
  # reshaping vmt_forecast_sheet_tbl
  observeEvent(input$vmt_forecast_sheet_tbl_cell_edit, {
    req(rvs$Advanced)
    
    rvs$Advanced[rvs$Advanced$table_no_ui == 2,] <- reshaping_advanced(input$vmt_forecast_sheet_tbl_cell_edit,
                                                                       rvs$Advanced,
                                                                       tbl_no = 2,
                                                                       col_list = c('veh_type','year'))
  })
  
  #reshaping onroad_fuel_tech_frac_sheet_tbl
  observeEvent(input$onroad_fuel_tech_frac_sheet_tbl_cell_edit, {
    req(rvs$Advanced)
    
    rvs$Advanced[rvs$Advanced$table_no_ui == 3,] <- reshaping_advanced(input$onroad_fuel_tech_frac_sheet_tbl_cell_edit,
                                                                       rvs$Advanced,
                                                                       tbl_no = 3,
                                                                       col_list = c('transit_mode','fuel_type'))
  })
  
  #reshaping pass_rail_sheet_tbl
  observeEvent(input$pass_rail_sheet_tbl_cell_edit, {
    req(rvs$Advanced)
    #browser()
    temp <- reshaping_advanced(input$pass_rail_sheet_tbl_cell_edit,
                               rvs$Advanced,
                               tbl_no = 4,
                               col_list = c('mode_service','unit'))
    old <- rvs$Advanced[rvs$Advanced$table_no_ui == 4,]
    if(sum(temp$value %in% c("Diesel","Electric")) != 4){
      vals <- temp$value[!(temp$value %in% c("Diesel","Electric"))]
      warning = paste0("Please input either Diesel or Electric (case sensative) for Advanced Table 4, you imputed the value(s): ", vals)
      
      
      rvs$Advanced[rvs$Advanced$table_no_ui == 4,] <- old
      showNotification(HTML(warning), type = "error")
      print('next')
    } else {rvs$Advanced[rvs$Advanced$table_no_ui == 4,] <- temp}
  })
  
  #reshaping freight_rail_sheet_tbl
  observeEvent(input$freight_rail_sheet_tbl_cell_edit, {
    req(rvs$Advanced)
    
    rvs$Advanced[rvs$Advanced$table_no_ui == 5,] <- reshaping_advanced(input$freight_rail_sheet_tbl_cell_edit,
                                                                       rvs$Advanced,
                                                                       tbl_no = 5,
                                                                       col_list = c('unit'))
  })
  
  #reshaping construction_sheet_tbl
  observeEvent(input$construction_sheet_tbl_cell_edit, {
    req(rvs$Advanced)
    
    rvs$Advanced[rvs$Advanced$table_no_ui == 6,] <- reshaping_advanced(input$construction_sheet_tbl_cell_edit,
                                                                       rvs$Advanced,
                                                                       tbl_no = 6,
                                                                       col_list = c('unit'))
  })
  
  #reshaping pass_rail_sheet_tbl
  observeEvent(input$fuel_apportionment_sheet_tbl_cell_edit, {
    req(rvs$Advanced)
    
    rvs$Advanced[rvs$Advanced$table_no_ui == 7,] <- reshaping_advanced(input$fuel_apportionment_sheet_tbl_cell_edit,
                                                                       rvs$Advanced,
                                                                       tbl_no = 7,
                                                                       col_list = c('veh_type'))
  })
  ## ADVANCED: reset buttons ---------------------------------------------------
  
  
  observeEvent(input$reset_ev_forecast_sheet_tbl, {
    rvs$Advanced[rvs$Advanced$table_no_ui == 1,] <- initial_advanced[initial_advanced$table_no_ui == 1, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_vmt_forecast_sheet_tbl, {
    rvs$Advanced[rvs$Advanced$table_no_ui == 2,] <- initial_advanced[initial_advanced$table_no_ui == 2, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$onroad_fuel_tech_frac_sheet, {
    rvs$Advanced[rvs$Advanced$table_no_ui == 3,] <- initial_advanced[initial_advanced$table_no_ui == 3, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_pass_rail_sheet_tbl, {
    rvs$Advanced[rvs$Advanced$table_no_ui == 4,] <- initial_advanced[initial_advanced$table_no_ui == 4, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_freight_rail_sheet_tbl, {
    rvs$Advanced[rvs$Advanced$table_no_ui == 5,] <- initial_advanced[initial_advanced$table_no_ui == 5, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_construction_sheet_tbl, {
    rvs$Advanced[rvs$Advanced$table_no_ui == 6,] <- initial_advanced[initial_advanced$table_no_ui == 6, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  observeEvent(input$reset_fuel_apportionment_sheet_tbl, {
    rvs$Advanced[rvs$Advanced$table_no_ui == 7,] <- initial_advanced[initial_advanced$table_no_ui == 7, ]
    updateSelectInput(inputId = 'state_input',selected = rvs$Baseline$state[[1]])
  })  
  
  # Outputs Tab --------------------------------------------------------
  
  
  # BASELINE GHG FORECAST -------------------------------------------------
  
  output$baseline_outputs <- renderDT({
    #browser()
    req(baseline_ghg_forecast())
    
    dt <- baseline_ghg_forecast()
    
    dt_onroad <- dt %>% ungroup() %>% # select(-veh_supertype) %>% View()
      filter(veh_supertype %in% c("Light-Duty Vehicles","Medium-/Heavy-Duty Vehicles")) %>%
      summarise(across(where(is.numeric),sum))  %>%
      mutate(veh_supertype = "Total (OnRoad)")
    
    dt_all <- dt %>% ungroup() %>%# select(-veh_supertype) %>%
      #filter(veh_supertype %in% c("Light-Duty Vehicles","Medium-/Heavy-Duty Vehicles")) %>%
      summarise(across(where(is.numeric),sum))
    
    growth <- dt_all[[1,1]]
    dt_pers <- dt_all %>% 
      mutate(across(where(is.numeric), ~(.x - growth)/growth, .names = "{.col}")) %>%
      mutate(veh_supertype = "Total (All Transportation - % Change)")
    
    dt_all <- dt_all %>% 
      mutate(veh_supertype = "Total (All Transportation)")
    
    dt_fin <- rbind(dt, dt_onroad, dt_all,dt_pers) %>%
      rename("Emissions" = "veh_supertype")
    comma_rows = c(0:7)
    percent_rows = 8
    currency_rows = NULL
    decimal_rows= NULL
    x<-datatable(
      dt_fin,
      rownames = FALSE,
      selection = "none",
      
      options = list(
        
        pageLength = 10, 
        paging = FALSE,
        columnDefs = list(
          list(
            targets = '_all',
            render = DT::JS(
              sprintf(
                "function(data, type, row, meta) {
                  if (type === 'display') {
                    var commaRows = [%s];
                    var percentRows = [%s];
                    var currencyRows = [%s];
                    var decimalRows = [%s];
                
                    var formatter = null;
                    if (commaRows.includes(meta.row)) {
                      formatter = function(d) { return Number(d).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); };
                    }
                    if (percentRows.includes(meta.row)) {
                      formatter = function(d) { return (Number(d) * 100).toFixed(2) + '%%'; };
                    }
                    if (currencyRows.includes(meta.row)) {
                      formatter = function(d) { return '$' + Number(d).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); };
                    }
                    if (decimalRows.includes(meta.row)) {
                      formatter = function(d) { return Number(d).toFixed(1); };
                    }
                    
                //console.log('the data: ' + data)
                //console.log('the type: '+ type)
                //console.log('the row: ' + row)
                //console.log('the meta: ' + meta)
                //console.log('the formatter' + formatter)
                
                
                    return formatter && !isNaN(data) && data !== null && data !== '' ? formatter(data) : data;
                  }
                  return data;
                }",
                paste(comma_rows, collapse = ", "), 
                paste(percent_rows, collapse = ", "),
                paste(currency_rows, collapse = ", "),
                paste(decimal_rows, collapse = ", ")
              )
            )
          )
        )
      )) %>%
      formatStyle(0,
                  target = "row",
                  #rows = c(7),
                  fontWeight = styleRow(c(7,8,9),"bold")
                  #'font-size' = "11px"
      )
    return(x)
    
  })
  
  output$baseline_ghg_line <- renderPlotly({
    df_lines <- baseline_ghg_forecast_all_years() %>% 
      filter(year >= 2021) %>%
      group_by(year) %>% summarise(Emissions = sum(Emissions))
    
    df_in <- baseline_ghg_forecast() %>% 
      ungroup() %>% summarise(across(where(is.numeric), sum)) %>%
      pivot_longer(everything(), values_to = "Emissions", names_to = "year")
    df_points <- df_in %>% mutate(year = as.numeric(year))
    
    lplot <- plot_ly(df_lines, x = ~year, y = ~Emissions, type = 'scatter', mode = 'lines',
                     hoverinfo = 'skip') %>%
      add_trace(df_points, x = ~df_points$year, y = ~df_points$Emissions, type = 'scatter', mode = 'markers',
                connectgaps = FALSE,
                text = c("Base Year","Horizon Year 1","Horizon Year 2", "Horizon Year 3"),
                
                hovertemplate = paste0('%{text}: %{x}<br>', 
                                       'Emissions: %{y:.2s} <br> <extra></extra>'),
                name = "") %>%
      layout(showlegend = FALSE,
             xaxis = list(title = 'Year'),
             yaxis = list(title = "Emisions", separatethousands= TRUE)) %>%
      config(displayModeBar = FALSE) 
    
    return(lplot)
  })
  
  output$baseline_ghg_pie <- renderPlotly({
    #browser()
    yr <- input$pie_graph_year
    df_in <- baseline_ghg_forecast() 
    df_in <- df_in %>% ungroup() %>% select(veh_supertype, yr) %>%
      rename("Emissions" = yr)
    
    # Define colors for the pie slices
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    
    comm_plot <- df_in %>% plot_ly(source = "sourceName") %>% 
      add_pie(labels = ~veh_supertype, 
              values = ~Emissions, 
              automargin = TRUE, 
              key = ~veh_supertype, hole = 0.6, sort = TRUE, 
              direction = "clockwise",
              hovertemplate = ~paste("%{label} <br>", paste0(round(Emissions, digits = 0)," CO2e"), "</br> %{percent} <extra></extra>"), 
              marker = list(colors = ~veh_supertype, line = list(color = "#595959", width = 1)), 
              #textfont = list(family = "Arial", size = 10),
              textposition = "none",
              hoverlabel = list(bgcolor = "white", font = list(color = "black", weight = "bold"), bordercolor = colors, borderwidth = 3)) %>% # Change hover tooltip color and add border
      
      layout(
        showlegend = TRUE, autosize = T) %>% 
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian")#,
             # toImageButtonOptions= list(filename = saveName,
             #                            width = saveWidth,
             #                            height =  saveHeight)
      )
  })
  
  # COST Outputs ----------------------------------------------------
  
  # costs_outputs_names <- c("bikeped_costs_outputs",
  #                          "transit_fixed_costs_outputs",
  #                          "transit_dr_costs_outputs",
  #                          "pub_trans_priority_costs_outputs",
  #                          "transit_zeb_costs_outputs",
  #                          "pub_trans_rail_costs_outputs",
  #                          "tdm_costs_outputs",
  #                          "micro_costs_outputs",
  #                          "traffic_ops_costs_outputs",
  #                          "mhdev_costs_outputs",
  #                          "pnr_costs_outputs",
  #                          "evsi_costs_outputs",
  #                          "roadway_expand_costs_outputs",
  #                          "intermodal_costs_outputs"
  #                          )
  # 
  # read_static_tables("data/costs_outputs.xlsx", costs_outputs_names)
  
  ## COST Outputs: create tables -----------------------------------------------------------
  
  output$bikeped_costs_outputs_tbl <- renderDT({
    print("RENDERING: Cost Output Table BikePed Costs Outputs")
    # browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==1,],
      output_table = cost_output_bikeped(),
      col_sel = c('area_type','cap_proj_type'),
      proj_life = 30,
      #val1_scalar = ,
      #val2_scalar = ,
      style = input$cost_view
    ) %>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>%
        DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  #  observe({browser()})
  
  output$transit_fixed_costs_outputs_tbl <- renderDT({
    print("RENDERING: Transit Fixed Bus Costs Outputs")
    # browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==2,],
      output_table = cost_output_transitservice() %>% filter(table == "Transit: Increased Fixed Route Service (VOMS)"),
      col_sel = c('area_type','fuel_type','transit_mode'),
      proj_life = 12,
      scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('area_type','transit_mode','value')] %>% 
        rename("scalar_1" = "value"),
      style = input$cost_view
    ) %>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE))  
    
    if(input$cost_view == "detail"){
      x <- x %>%
        DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
    
  })
  
  output$transit_dr_costs_outputs_tbl <- renderDT({
    print("RENDERING: Transit Fixed DR Costs Outputs")
    # browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==3,],
      output_table = cost_output_transitservice() %>% filter(table == "Transit: Increased Demand Response Service (VOMS)"),
      col_sel = c('area_type','fuel_type','transit_mode'),
      proj_life = 12,
      scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('area_type','transit_mode','value')] %>% rename("scalar_1" = "value"),
      style = input$cost_view) %>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
    
  })
  
  output$pub_trans_priority_costs_outputs_tbl <- renderDT({   
    print("RENDERING: Transit Priority Costs Outputs")
    # browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==5,], #slchanged
      output_table = cost_output_transitservice() %>% 
        filter(table == "Public Transportation: Bus Priority Treatment") %>%
        mutate(total_change_newtrips = -total_change_newtrips),  # the cost effectiveness for this project is slightly different 
      col_sel = c(),
      proj_life = 5, 
      style = input$cost_view) %>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>%DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  public_elec_replacement_cost_table <- reactive({
    print("RENDERING: PT Electric Veh Replacment Costs Outputs")
    scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('area_type','transit_mode','value')] %>% rename("scalar_1" = "value")
    
    base <- rvs$Costs[rvs$Costs$table_no_ui %in% c(2,3),] %>% # & rvs$Costs$fuel_type != "Electric",]  %>%
      group_by(area_type, transit_mode,fuel_type, cost_type) %>% 
      summarise(value = sum(value)) %>% 
      pivot_wider(names_from = cost_type, values_from = value) %>% left_join(scalar_list) %>%
      mutate(var = var1*scalar_1 + var2*scalar_1) %>% #create variable cost based on scalars
      select(-c(var1,var2)) %>%
      mutate(annual_cost = cap/12 + var)
    
    elec <- base %>%ungroup() %>% filter(fuel_type == "Electric") %>% select(-c(fuel_type, cap, var, scalar_1)) %>% rename(elec_cost = annual_cost)
    
    fin <- left_join(base, elec) %>% filter(fuel_type != "Electric")%>% mutate(annual_cost = elec_cost - annual_cost) %>%
      select(area_type, transit_mode, fuel_type, annual_cost)
    return(fin)
  })
  
  output$transit_zeb_costs_outputs_tbl <- renderDT({   
    print("RENDERING: Transit Electric Bus Costs Outputs")
    #browser()
    temp <- cost_function(
      ini_cost_table =  public_elec_replacement_cost_table() |> arrange(area_type, transit_mode, fuel_type), #%>% filter(table %in% c("Transit: Increased Demand Response Service (VOMS)","Transit: Increased Fixed Route Service (VOMS)")),
      output_table = cost_output_transitselect() |> arrange(area_type, transit_mode, fuel_type),
      col_sel = c('area_type','fuel_type','transit_mode'),
      proj_life = 12,
      #scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('area_type','transit_mode','value')],
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$pub_trans_rail_costs_outputs_tbl <- renderDT({   
    print("RENDERING: Public Transit Rail Costs Outputs")
    # browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==6,], #slchanged
      output_table = cost_output_transitservice() %>% filter(table == "Public Transportation: Rail (VOMS)"),
      col_sel = c('fuel_type','transit_mode'),
      proj_life = 30,
      #BEN: val 1 is only referencing light rail revenue miles 
      scalar_list = rvs$Assumptions[rvs$Assumptions$table_no_ui==2 & rvs$Assumptions$unit =='rev_mi_per_veh',c('transit_mode','value')]%>% rename("scalar_1" = "value"),      
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$tdm_costs_outputs_tbl <- renderDT({   
    print("RENDERING: TDM Costs Outputs")
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui== 7,], #slchanged
      output_table = cost_output_TDM(),
      col_sel = c(),
      proj_life = 1,
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x <- datatable(temp,
                   rownames = FALSE,
                   selection = "none",
                   options = list(
                     pageLength = 50,
                     searching = FALSE,
                     paging = FALSE,
                     info = FALSE))
    
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$micro_costs_outputs_tbl <- renderDT({   
    print("RENDERING: Micro Costs Outputs")
    # browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui== 8,], #slchanged
      output_table = cost_output_micro(),
      col_sel = c(),
      proj_life = 6,
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$traffic_ops_costs_outputs_tbl <- renderDT({   
    print("RENDERING: OPS Costs Outputs")
    #browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==9,]%>% #slchanged
        left_join(data.frame(cap_proj_type = c("New roundabouts","New or retimed signal"),
                             proj_life = c(30,5))),
      output_table = output_cost_OPS(),
      col_sel = c('road_class','area_type','cap_proj_type'),
      proj_life = NA,
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) #%>% # to show NA in table
    #mutate(year = as.character(year)) %>% rename(Year = year)
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>%DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$mhdev_costs_outputs_tbl <- renderDT({   
    
    print("RENDERING: MHDEV Costs Outputs")
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==10,] %>% rename('veh_subtype' = 'fuel_type'),#slchanged
      output_table = cost_effectiveness_MDHD(),
      col_sel = c('veh_type','veh_subtype'),
      proj_life = 12,
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$pnr_costs_outputs_tbl <- renderDT({   
    print("RENDERING: PNR Costs Outputs")
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==11,],#slchanged
      output_table = cost_output_pnr(),
      col_sel = c(),
      proj_life = 30,
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<- datatable(temp,
                  rownames = FALSE,
                  selection = "none",
                  options = list(
                    pageLength = 50,
                    searching = FALSE,
                    paging = FALSE,
                    info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$evsi_costs_outputs_tbl <- renderDT({  
    print("RENDERING: EVSI Costs Outputs")
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==12,],#slchanged
      output_table = cost_effectiveness_EVSE(),
      col_sel = c('charge_port_detail'), #Change to port detail?
      proj_life = 10,
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>%DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$roadway_expand_costs_outputs_tbl <- renderDT({  
    print("RENDERING: Roadway Exp Costs Outputs")
    #browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==14,], #slchanged
      output_table = cost_output_RoadwayExp(),
      col_sel = c('road_class','area_type'),
      proj_life = 30,#needs to project lifes actually :(
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) # to show NA in table
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  output$roadwayresurf_cuts_costs_outputs_tbl <- renderDT({
    print("RENDERING: Roadway Resurfacing Costs Outputs")
    # browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==15,],
      output_table = cost_output_roadway_resurf(),
      col_sel = c(),
      proj_life = 1,
      style = input$cost_view)
    temp <- temp%>%
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) #%>% # to show NA in table
    #mutate(year = as.character(year)) %>% rename(Year = year)
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE))
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  #Fuel Price Table
  
  output$intermodal_costs_outputs_tbl <- renderDT({
    print("RENDERING: Freight Intermodal cuts Costs Outputs")
    #browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==13,], #slchanged
      output_table = cost_effectiveness_freight(),
      col_sel = c(),
      proj_life = 30,
      style = input$cost_view)%>% 
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) #%>% # to show NA in table
    #mutate(year = as.character(year)) %>% rename(Year = year)
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE)) 
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  
  output$landuse_costs_outputs_tbl <- renderDT({
    print("RENDERING: Land Use Costs Outputs")
    # browser()
    temp <- cost_function(
      ini_cost_table = rvs$Costs[rvs$Costs$table_no_ui==16,],
      output_table = cost_output_land_use(),
      col_sel = c(),
      proj_life = 10,
      style = input$cost_view)%>%
      rename(any_of(references_vector)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) #%>% # to show NA in table
    #mutate(year = as.character(year)) %>% rename(Year = year)
    
    x<-datatable(temp,
                 rownames = FALSE,
                 selection = "none",
                 options = list(
                   pageLength = 50,
                   searching = FALSE,
                   paging = FALSE,
                   info = FALSE))
    if(input$cost_view == "detail"){
      x <- x %>% DT::formatRound(which(sapply(temp, is.numeric)), digits = 3)}
    return(x %>% formatStyle(names(temp), color = styleEqual("-", "red")))
  })
  
  # server scenarios outputs ------------------------------------------------
  ## Qi adding a condition when user click on the Outputs, and all the scenario are 0s.- considering force the fill project with budget. 
  
  observeEvent(input$OUTPUTS_TABS, {

            if (input$OUTPUTS_TABS %in% c("Strategy Summary",
                                      "Scenario Summary",
                                      "Cumulative Project Totals")) {
    
    check_tbl <- scenario_sum()
    # browser()
    if (all(check_tbl[, 3:ncol(check_tbl)] == 0, na.rm = TRUE) &&
        input$fill_projects_bttn == 0 && 
        all(rvs$Projects$value == 0)) {
      # browser()
      # Simulate a click on the budget button
      req(input$budget_start_year)
      req(input$budget_years_covered)
      req(input$budget_total)
      
      temp_budget <- rvs$Budget |>
        mutate(value = value/100)
      temp_budget$table_no_ui_revised = as.character(temp_budget$table_no_ui_revised)
      
      temp_costs <- rvs$Costs
      temp_costs$table_no_ui_revised = as.character(temp_costs$table_no_ui_revised)
      temp_costs <- temp_costs |>
        filter(table_no_ui_revised != "-1") |>
        group_by(table_no_ui_revised) |>
        summarise(cost_parameter = sum(value)) |> ungroup()
      
      temp_join <- left_join(temp_budget,temp_costs) |>
        mutate(cost_parameter = case_when(!is.na(land_use) ~ 1,
                                          !is.na(land_use)&land_use == "Land Use Incentives"~1000000,
                                          TRUE ~ cost_parameter)) #|> mutate(value = 1)
      #start <- input$horizon_year_1
      #total <- input$budget_total
      start <- input$budget_start_year
      end <- start + input$budget_years_covered -1
      total <- input$budget_total*1000000
      total_years <- input$budget_years_covered #+ 1 # qi commented out +1
      
      year1 <- input$horizon_year_1
      year2 <- ifelse (input$horizon_year_2 <= 2040, input$horizon_year_2, 2040)
      year3 <- ifelse (input$horizon_year_3 <= 2040, input$horizon_year_3, 2040)
      
      foo <- temp_join |>
        dplyr::mutate(horizon_year_1_cnt = 
                        case_when(category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                                                  "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                                                  "Park-and-Ride") ~ ifelse(year1 - start < input$budget_years_covered, ifelse(year1 - start >0, year1 - start, 0),  input$budget_years_covered),
                                  category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                                                  "Public Transportation: Rail", "Bus Priority Treatment",
                                                  "Travel Demand Management", "EV Charging Infrastructure",
                                                  "Transit Service Cuts", "Micromobility",
                                                  "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year1)  <= input$budget_years_covered, sum(c(start:end) <= year1),  input$budget_years_covered),
                                  TRUE ~ ifelse(year1 - (start+2) < input$budget_years_covered, ifelse(year1 - (start+2) >0, year1 - (start+2), 0),  input$budget_years_covered)
                        ),
                      horizon_year_2_cnt = 
                        case_when(category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                                                  "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                                                  "Park-and-Ride") ~ ifelse(year2 - start < input$budget_years_covered, ifelse(year2 - start >0, year2 - start, 0),  input$budget_years_covered),
                                  category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                                                  "Public Transportation: Rail", "Bus Priority Treatment",
                                                  "Travel Demand Management", "EV Charging Infrastructure",
                                                  "Transit Service Cuts", "Micromobility",
                                                  "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year2)  <= input$budget_years_covered, sum(c(start:end) <= year2),  input$budget_years_covered),
                                  TRUE ~ ifelse(year2 - (start+2) < input$budget_years_covered, ifelse(year2 - (start+2) >0, year2 - (start+2), 0),  input$budget_years_covered)
                        ),
                      horizon_year_3_cnt = 
                        case_when(category %in% c("Bicycle and Pedestrian","Transit: Fleet Electrification",
                                                  "Traffic Operations", "Medium- and Heavy-Duty Vehicle Replacement",
                                                  "Park-and-Ride") ~ ifelse(year3 - start < input$budget_years_covered, ifelse(year3 - start >0, year3 - start, 0),  input$budget_years_covered),
                                  category %in% c("Transit: Increased Fixed Route Service","Transit: Increased Demand Response Service",
                                                  "Public Transportation: Rail", "Bus Priority Treatment",
                                                  "Travel Demand Management", "EV Charging Infrastructure",
                                                  "Transit Service Cuts", "Micromobility",
                                                  "Roadway Resurfacing") ~ ifelse(sum(c(start:end) <= year3)  <= input$budget_years_covered, sum(c(start:end) <= year3),  input$budget_years_covered),
                                  TRUE ~ ifelse(year3 - (start+2) < input$budget_years_covered, ifelse(year3 - (start+2) >0, year3 - (start+2), 0),  input$budget_years_covered)
                        )
        ) |>
        dplyr::mutate(horizon_year_1_budget = total*value*(horizon_year_1_cnt/total_years),
                      horizon_year_2_budget = total*value*(horizon_year_2_cnt/total_years),
                      horizon_year_3_budget = total*value*(horizon_year_3_cnt/total_years),
                      horizon_year_1 = horizon_year_1_budget/cost_parameter,
                      horizon_year_2 = horizon_year_2_budget/cost_parameter,
                      horizon_year_3 = horizon_year_3_budget/cost_parameter) |> select(-c(table_no_ui,value,table_no_ui_revised)) |>
        pivot_longer(cols = c(horizon_year_1, horizon_year_2, horizon_year_3), names_to = "year", values_to = "value") |>
        #mutate(value = value_new) |>
        select(any_of(names(rvs$Projects)))
      temp_projects <- rvs$Projects |> select(-value)
      
      # foo[is.na(foo)] <- "NA"  # Qi: comment out?
      # temp_projects[is.na(temp_projects)] <- "NA"  # Qi: comment out?
      foobar <- left_join(temp_projects, foo)
      foobar[foobar == "NA"] <- NA
      foobar[is.na(foobar$value),"value"]<-0
      
      ## the landuse $ is different, it need special treat as it is $Million. 
      foobar$value[foobar$table_no_ui == 16]<- foobar$value[foobar$table_no_ui  ==16]/1000000 
      
      ### at the step above, the projects value are cumulative values, however, under the project mode, the cumulative step is done afterward, so we need to de-cumulate the value. 
      foobar_discum <- foobar %>% 
        pivot_wider(names_from = year, values_from = value) %>%
        mutate(horizon_year_1 = horizon_year_1, 
               horizon_year_3 = horizon_year_3- horizon_year_2,
               horizon_year_2 = horizon_year_2 - horizon_year_1) %>%
        pivot_longer(cols = starts_with("horizon_year_"),
                     names_to = "year", 
                     values_to = 'value')
      
      
      rvs$Projects <- foobar_discum
      
    }
     }
  }#, ignoreInit = TRUE
  )
  
  output$emission_change_tbl <- renderDataTable({
    results <- scenario_summary_results()
    # browser()
    comma_rows = c(0:4,7:11,14:19)
    percent_rows = c(5,6,12,13)
    currency_rows = NULL
    decimal_rows= NULL
    fin_table<-datatable(results,
                         rownames = FALSE,
                         selection = "none",
                         extension = 'RowGroup',
                         
                         options = list(rowGroup = list(dataSrc = c(5)),
                                        columnDefs = list(list(targets = c(5), visible = FALSE),
                                                          list(
                                                            targets = '_all',
                                                            render = DT::JS(
                                                              sprintf(
                                                                "function(data, type, row, meta) {
                                                     if (type === 'display') {
                                                     var commaRows = [%s];
                                                     var percentRows = [%s];
                                                     var currencyRows = [%s];
                                                     var decimalRows = [%s];
                
                                                     var formatter = null;

                                                     if (commaRows.includes(meta.row)) {
                                                     formatter = function(d) { return Number(d).toLocaleString('en-US'); };
                                                     }
                                                     if (percentRows.includes(meta.row)) {
                                                     formatter = function(d) { return (Number(d) * 100).toFixed(2) + '%%'; };
                                                     }
                                                     if (currencyRows.includes(meta.row)) {
                                                     formatter = function(d) { return '$' + Number(d).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); };
                                                     }
                                                     if (decimalRows.includes(meta.row)) {
                                                     formatter = function(d) { return Number(d).toFixed(1); };
                                                     }
                    
                                                     //console.log('the data: ' + data)
                                                     //console.log('the type: '+ type)
                                                     //console.log('the row: ' + row)
                                                     //console.log('the meta: ' + meta)
                                                     //console.log('the formatter' + formatter)
                                                     return formatter && !isNaN(data) && data !== null && data !== '' ? formatter(data) : data;
                                                     }
                                                     return data;
                                                     }",
                                                                paste(comma_rows, collapse = ", "), 
                                                                paste(percent_rows, collapse = ", "),
                                                                paste(currency_rows, collapse = ", "),
                                                                paste(decimal_rows, collapse = ", ")
                                                              )
                                                            )
                                                          )
                                        ),
                                        page_length = 20,
                                        searching = FALSE,
                                        paging = FALSE,
                                        info = FALSE))
    
    return(fin_table)
  })
  
  output$scenario_line_graph <- renderPlotly({
    table_filt <- input$scenario_indicator
    results <- scenario_summary_results() %>%
      filter(table_title == table_filt) %>%
      #filter(Scenario != "Baseline") %>% 
      pivot_longer(cols = as.character(c(rvs$Baseline$base_year,
                                         rvs$Baseline$horizon_year_1,
                                         rvs$Baseline$horizon_year_2,
                                         rvs$Baseline$horizon_year_3)), 
                   names_to = "year", values_to = "value") %>%
      filter(year != rvs$Baseline$base_year)  %>%
      mutate(value = as.numeric(value)) %>%
      mutate(visible = ifelse(Scenario == "Baseline","legendonly","visible"))
    #max <- max(results$value)
    #min <- min(results$value)
    
    lplot<-results %>%
      plotly::plot_ly(x = ~ year,
                      y = ~ value,
                      color = ~Scenario,
                      type = 'scatter',
                      mode = 'lines',
                      visible = ~visible) %>%
      layout(yaxis = list(title = table_filt, separatethousands= TRUE),
             xaxis = list(title = "Year"),
             barmode = "group") %>%
      config(displayModeBar = FALSE) 
    
    
    return(lplot)
  })
  
  output$scenario_bar_graph <- renderPlotly({
    table_filt <- input$scenario_indicator
    results <- scenario_summary_results() %>%
      filter(table_title == table_filt) %>%
      filter(Scenario != "Baseline") %>% 
      pivot_longer(cols = as.character(c(rvs$Baseline$base_year,
                                         rvs$Baseline$horizon_year_1,
                                         rvs$Baseline$horizon_year_2,
                                         rvs$Baseline$horizon_year_3)), 
                   names_to = "year", values_to = "value") %>%
      filter(year != rvs$Baseline$base_year) %>%
      mutate(value = as.numeric(value))
    
    
    results %>%
      plotly::plot_ly(x = ~ year,
                      y = ~ value,
                      color = ~Scenario,
                      type = 'bar') %>%
      layout(yaxis = list(title = table_filt, separatethousands= TRUE),
             xaxis = list(title = "Year"),
             barmode = "group") %>%
      config(displayModeBar = FALSE) %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
    
  })
  
  
  # read dummy data
  # scenario_result <- readxl::read_excel("data/scenario_simplified.xlsx")
  # 
  # observeEvent(input$scenario_indicator,{
  #   dat_temp <- scenario_result |> 
  #     filter( indicator== input$scenario_indicator, !is.na(mt_reduction)) |> 
  #     select(-'value',-'pct_reduction') |> 
  #     mutate(year = as.character(year)) |> 
  #     pivot_wider(names_from = scenario,
  #                 values_from = mt_reduction)
  #   
  # })
  
  
  # 
  # output$downloadscenario_result <- downloadHandler(
  #   filename = function(){
  #     paste("scenario_results",
  #           Sys.Date(),
  #           ".csv",
  #           sep="")},
  #   content = function(file) {
  #     tbl_out= scenario_result |> 
  #       rename()
  #     write.csv(tbl_out, file,row.names = F)
  #   })
  
  # server Strategy Summary outputs ------------------------------------------------
  
  # observeEvent(c(input$strategy_scen_select,
  #                input$strategy_indicator),{
  #   req(reactive_scenario())
  
  
  output$strategy_summary_tbl <- DT::renderDataTable({
    # browser()
    scen_filter <- reactive_scenario()
    req( scenario_sum())
    
    if (input$strategy_scen_select == 'scen_1' ){
      scen_select <-   scen_filter %>% select(-'Scenario2') %>%
        rename(scen = Scenario1)
    } else if (input$strategy_scen_select == 'scen_2'){
      scen_select <-   scen_filter %>% select(-'Scenario1') %>% 
        rename(scen = Scenario2)
    }
    
    strategy_temp <- scenario_sum() %>% left_join(scen_select,by= c('Strategy' = 'Grouped Projects')) %>% filter(scen == TRUE) %>%
      select('year', Strategy,input$strategy_indicator)
    
    total_row <- strategy_temp %>%
      pivot_wider(names_from = year, values_from = input$strategy_indicator) %>%
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      summarise(Strategy = "Total", across(where(is.numeric), sum)) 
    
    
    data_temp <- bind_rows(
      strategy_temp %>%
        pivot_wider(names_from = year, values_from = input$strategy_indicator) %>%
        mutate(across(where(is.numeric), ~round(., 2))),
      total_row
    ) %>% mutate(across(where(is.numeric), ~ prettyNum(., big.mark = ",")))
    
    rank_list <- c("Bicycle and Pedestrian",
                   "Transit Service Expansion",
                   "Transit Electrification",
                   "Travel Demand Management",
                   "Micromobility",
                   "Traffic Operations",
                   "Medium- and Heavy-duty Vehicle Replacement",
                   "Park-and-Ride",
                   "Charging Infrastructure and EV Incentives",
                   "Intermodal Freight Investment",
                   "Roadway Expansion",
                   "Roadway Resurfacing",
                   "Land Use",
                   "Transit Service Cuts",
                   "Total")
    
    # browser()
    data_temp <- data_temp[order(match(data_temp$Strategy, rank_list)), ]
    
    data_temp %>%
      DT::datatable(
        escape = FALSE,
        rownames = FALSE,
        options = list(dom = 't', pageLength = 15,
                       initComplete = JS(
                         "function(settings, json) {",
                         "  var table = settings.oInstance.api();",
                         "  var lastRow = table.rows().count() - 1;",
                         "  table.rows().every(function(index, element) {",
                         "    if (index === lastRow) {",
                         "      $(this.node()).css('font-weight', 'bold');",
                         "    } else {",
                         "      $(this.node()).css('font-weight', 'normal');",
                         "    }",
                         "  });",
                         "}")))
  })
  
  
  
  output$strategy_summary_graph <- renderPlotly({
    scen_filter <-  reactive_scenario()
    req( scenario_sum())
    if (input$strategy_scen_select == 'scen_1' ){
      scen_select <-   scen_filter %>% select(-'Scenario2') %>%
        rename(scen = Scenario1)
    } else if (input$strategy_scen_select == 'scen_2'){
      scen_select <-   scen_filter %>% select(-'Scenario1') %>% 
        rename(scen = Scenario2)
    }
    colors <- data.frame(Strategy = c("Bicycle and Pedestrian", "Charging Infrastructure and EV Incentives", 
                                      "Intermodal Freight Investment", "Land Use", 
                                      "Medium- and Heavy-duty Vehicle Replacement", "Micromobility", 
                                      "Park-and-Ride", "Roadway Expansion",                    
                                      "Roadway Resurfacing", "Traffic Operations",
                                      "Transit Electrification", "Transit Service Cuts",             
                                      "Transit Service Expansion", "Travel Demand Management" ), 
                         colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", 
                                    "#FFFF33", "#A65628", "#F781BF", "#984EA3", 
                                    "#66C2A5", "#8DA0CB", "#F2BFE0", 
                                    "#A6D854", "#8C564B", "#BEAED4"))
    
    strategy_temp <- scenario_sum() %>% left_join(scen_select,by= c('Strategy' = 'Grouped Projects')) %>% filter(scen == TRUE) %>%
      select('year', Strategy,input$strategy_indicator) %>%
      arrange(year, Strategy)
    strategy_temp <- left_join(strategy_temp, colors)
    p <- plot_ly()
    for (i in unique(strategy_temp$Strategy)) {
      rows <- strategy_temp$Strategy == i
      p<-add_trace(p,data = strategy_temp[rows,], 
                   x = ~factor(year), 
                   y = ~get(input$strategy_indicator),
                   #color = strategy_temp$Strategy,
                   name = strategy_temp$Strategy[rows],
                   type = "bar",
                   hoverinfo = 'text',
                   textposition = 'none',
                   marker = list(color = strategy_temp$colors[rows][[1]]),
                   hovertemplate = paste0(strategy_temp$Strategy[rows],'<br>',#'%{x}:<br>', 
                                          'Emissions: %{y:.4s}<extra></extra>')
      ) |>
        layout(
          xaxis =list(title = ""), #list(title = "Year"),
          yaxis = list(title = "Total Change"),
          margin = list(b = 80),
          barmode = "relative",
          legend = list(
            orientation = 'h',
            x = 0.5,
            xanchor = 'center',
            y = -0.3,
            yanchor = 'top')) |> 
        config(displaylogo = FALSE, 
               modeBarButtonsToRemove = c("toImage","zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian", "hoverClosestGeo", "hoverClosest3d", "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "hoverClosestCartesian"))
    }
    
    return(p)
    
  })
  
  
  
  #Processing working ----
  
  source("processing_scripts/processing_Base_Projections.R", local = TRUE)
  source("processing_scripts/processing_BikePed.R", local = TRUE) #Qi done
  source("processing_scripts/processing_TransitService.R", local = TRUE) #Qi done 
  source("processing_scripts/processing_Micro.R", local = TRUE) #Qi done
  source("processing_scripts/processing_OPS.R", local = TRUE)
  source("processing_scripts/processing_MDHD.R", local = TRUE) 
  source("processing_scripts/processing_TransitElec.R", local = TRUE)  
  source("processing_scripts/processing_TransitService.R", local = TRUE) 
  source("processing_scripts/processing_TDM.R", local = TRUE) 
  source("processing_scripts/processing_ParkRide.R", local = TRUE) 
  source("processing_scripts/processing_freight.R", local = T) 
  source("processing_scripts/processing_EVSE.R", local = T) 
  source("processing_scripts/processing_RoadwayExp.R", local = TRUE) 
  
  source("processing_scripts/processing_TransitService_Cuts.R",local = T) #in progress
  source("processing_scripts/processing_LandUse.R",local = T) #in progress
  source("processing_scripts/processing_roadway_resurfacing.R",local = T) #in progress
  
  source("functions/cost_maker.R", local = TRUE)
  source("processing_scripts/processing_Allassump.R", local = TRUE) #Finished
  
  ## download PDF report
  
  ## capture if user ever update the budget inputs
  
  # used_budget <- reactiveVal(FALSE)
  
  # observeEvent(input$INPUTS_TABS , {
  #   if (input$INPUTS_TABS  == "Budget") {
  #     user_visited_budget(TRUE)
  #   }
  # })
  
  # used_budget <- reactive({
  #   input$budget_start_year != 2025 ||
  #     input$budget_years_covered != 5 ||
  #     input$budget_total != 100
  # })
  
  
  # output$pdf_report <- downloadHandler(
  #   
  #   filename = function(){
  #     paste("Summary Report",
  #           Sys.Date(),
  #           ".pdf",
  #           sep="")},
  #   content = function(file) {
  #     
  #     req(baseline_ghg_forecast())
  #     
  #     dt <- baseline_ghg_forecast()
  #     
  #     if (used_budget()) {
  #       
  #       include_budget <- 1
  #     } else {
  #       include_budget <- 0
  #     }
  # 
  #     dt_onroad <- dt %>% ungroup() %>%# select(-veh_supertype) %>%
  #       filter(veh_supertype %in% c("Light-Duty Vehicles","Medium-/Heavy-Duty Vehicles")) %>%
  #       summarise(across(where(is.numeric),sum)) %>%
  #       mutate(veh_supertype = "Total (Onroad Vehicles)")
  #     dt_all <- dt %>% ungroup() %>%# select(-veh_supertype) %>%
  #       #filter(veh_supertype %in% c("Light-Duty Vehicles","Medium-/Heavy-Duty Vehicles")) %>%
  #       summarise(across(where(is.numeric),sum))
  #     growth <- dt_all[[1,1]]
  #     dt_growth <- dt_all %>% 
  #       mutate(across(where(is.numeric), ~(.x - growth)/growth, .names = "{.col}")) %>%
  #       mutate(veh_supertype = "Total (All Transportation)")
  #     dt_all <- dt_all %>% 
  #       mutate(veh_supertype = "Total (All Transportation)")
  #     ghg_data <- rbind(dt, dt_onroad, dt_all, dt_growth) %>%
  #       rename("Emissions" = "veh_supertype")
  #     
  #     #get and modify the scen data: 
  #     scen_data <- scenario_summary_results() %>%
  #       filter(grepl("Reduction", table_title)|table_title == "New Daily Active Trips") %>%
  #       filter(!grepl("%",table_title)) %>%
  #       mutate(table_title = case_when(table_title == "Emissions Reduction (MT from Baseline)" ~ 'CO2',
  #                                      table_title == "VMT Reduction (millions from Baseline)" ~ 'VMT',
  #                                      table_title == "NOx Reduction (MT)" ~ 'NOx',
  #                                      table_title == "PM2.5 Reduction (MT)" ~ 'PM2.5',
  #                                      table_title == "New Daily Active Trips" ~ 'New Daily Active Trips')) %>%
  #       rename(indicator = table_title) %>%
  #       pivot_longer(cols = as.character(c(rvs$Baseline$base_year,
  #                                          rvs$Baseline$horizon_year_1,
  #                                          rvs$Baseline$horizon_year_2,
  #                                          rvs$Baseline$horizon_year_3)), 
  #                    names_to = "Year",
  #                    values_to = "mt_reduction") %>%
  #       mutate(mt_reduction = ifelse(mt_reduction == "-","0",mt_reduction)) %>%
  #       mutate(mt_reduction = as.numeric(mt_reduction)) %>%
  #       mutate_if(is.numeric, ~round(., 1))
  #     
  #     
  #     cost_data <- all_costs() %>%
  #       lapply(., replace_underscores) %>%
  #       lapply(., replace_na_with_string) %>%
  #       lapply(., remove_year_column) %>%
  #       make_column_names_proper(.)
  #     
  #     Sys.sleep(1)
  # 
  #     # check # of columns
  #     # for (name in names(cost_data)) {
  #     #    cat(name, "has", ncol(cost_data[[name]]), "columns\n")}
  # 
  #     # Render the R Markdown file to PDF
  #     shiny::withProgress(
  #       message = paste0("Downloading", input$dataset, " Data"),
  #       value = 0,
  #       {
  #         shiny::incProgress(1/10)
  #         Sys.sleep(1)
  #         shiny::incProgress(5/10)
  #         unloadNamespace("kableExtra")
  #         rmarkdown::render(input = paste0(getwd(),"/Report_Template.qmd"),
  #                           output_file = file,
  #                           params = list(
  #                             ghg_data = ghg_data,
  #                             scen_data = scen_data,
  #                             cost_data = cost_data,
  #                             state = input$state_input,
  #                             bsae_year = input$base_year,
  #                             horizon_year_1 = input$horizon_year_1,
  #                             horizon_year_2 = input$horizon_year_2,
  #                             horizon_year_3 = input$horizon_year_3,
  #                             trans_scope = input$transportation_scope,
  #                             em_scope = input$scope_emissions,
  #                             fuel_scope = input$scope_fuels,
  #                             vmt = input$vmt_forecast_input,
  #                             vmt_nhs = input$vmt_nhs,
  #                             ev = input$ev_baseline_input,
  #                             grid_em = input$grid_emissions_input,
  #                             lu = input$land_use_factor,
  #                             funding_tbl = rvs$Funding,
  #                             funding_yr = input$funding_start_year,
  #                             funding_dur = input$funding_years,
  #                             funding_bgt = rvs$Funding,
  #                             rail = input$include_rail,
  #                             include_bgt = include_budget
  #                           ),
  #                           output_format = "pdf_document",
  #                           output_options = list(
  #                             keep_tex = TRUE,
  #                             verbose = TRUE#,
  #                             #latex_engine = 'xelatex'
  #                           )
  #         )
  #       }
  #     )
  #     
  #   }
  # )
  
  
  #footer buttons-------------------------
  observeEvent(c(input$inputs_btn,input$inbaseline),{
    nav_select(id = "APP_PAGE",selected = "Inputs")
    nav_select(id = "INPUTS_TABS",selected = "Baseline")
  },ignoreInit = T)
  observeEvent(input$inprojects,{
    nav_select(id = "APP_PAGE",selected = "Inputs")
    nav_select(id = "INPUTS_TABS",selected = "Projects")
  },ignoreInit = T)
  observeEvent(input$incosts,{
    nav_select(id = "APP_PAGE",selected = "Inputs")
    nav_select(id = "INPUTS_TABS",selected = "Costs")
  },ignoreInit = T)
  observeEvent(input$inassumptions,{
    nav_select(id = "APP_PAGE",selected = "Inputs")
    nav_select(id = "INPUTS_TABS",selected = "Assumptions")
  },ignoreInit = T)
  observeEvent(input$inscenarios,{
    nav_select(id = "APP_PAGE",selected = "Inputs")
    nav_select(id = "INPUTS_TABS",selected = "Scenarios")
  },ignoreInit = T)
  observeEvent(input$inadvanced,{
    nav_select(id = "APP_PAGE",selected = "Inputs")
    nav_select(id = "INPUTS_TABS",selected = "Advanced")
  },ignoreInit = T)
  observeEvent(input$outputs_btn,{
    nav_select(id = "APP_PAGE",selected = "Outputs")
    nav_select(id = "OUTPUTS_TABS",selected = "Baseline GHG Forecast")
  },ignoreInit = T)
  observeEvent(input$outbaseline,{
    nav_select(id = "APP_PAGE",selected = "Outputs")
    nav_select(id = "OUTPUTS_TABS",selected = "Baseline GHG Forecast")
  },ignoreInit = T)
  observeEvent(input$outscenario,{
    nav_select(id = "APP_PAGE",selected = "Outputs")
    nav_select(id = "OUTPUTS_TABS",selected = "Scenario Summary")
  },ignoreInit = T)
  observeEvent(input$outsummary,{
    nav_select(id = "APP_PAGE",selected = "Outputs")
    nav_select(id = "OUTPUTS_TABS",selected = "Strategy Summary")
  },ignoreInit = T)
  observeEvent(input$outcosteff,{
    nav_select(id = "APP_PAGE",selected = "Outputs")
    nav_select(id = "OUTPUTS_TABS",selected = "Cost-Effectiveness")
  },ignoreInit = T)
  observeEvent(input$about_btn,{
    nav_select(id = "APP_PAGE",selected = "Welcome")
  },ignoreInit = T)
  observeEvent(input$how_to_btn,{
    nav_select(id = "APP_PAGE",selected = "How-to")
  },ignoreInit = T)
  observeEvent(input$guide_btn,{
    shinyjs::runjs("window.open('TEACART User Guide and Methodology v.1.10.3.pdf', '_blank')")
  },ignoreInit = T)
  observeEvent(input$sources_btn,{
    nav_select(id = "APP_PAGE",selected = "Sources")
  },ignoreInit = T)
}



# Run the application
shinyApp(ui, server)

# ,
# include_upstream_fuels = input$scope_fuels,
# vmt_forecast = input$vmt_forecast_input,
# vmt_nhs = input$vmt_nhs,
# veh_elec_baseline = input$ev_baseline_input,
# elec_grid_emissions_net_zero = input$grid_emissions_input