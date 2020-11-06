
# Load libraries, functions and variables ---------------------------------

#source("global.R")


##### SUS UI SCRIPT ############################################################



# Main UI function --------------------------------------------------------

shinyUI(
  
  dashboardPage(

    skin = "black", 

    dashboardHeader(tags$li(class = "dropdown",
                            tags$style(".main-header {max-height: 55px}"),
                            tags$style(".main-header .logo {height: 50px}")),
                    
                    title = loadingLogo('http:www.drtuff.com', 'logo.png',
                                        'spinning_logo.gif', 50, 50, 50)),
    
    dashboardSidebar(
      width = 250,
      sidebarMenu(id = "tabs",
                  
                  menuItem("Home", tabName = "home", icon = icon("balance-scale")),
                  # conditionalPanel(condition = "input.tabs == 'bivariate'", 
                  #                  plotOutput("map1")),
                  
                  menuItem("Access to green space", icon = icon("envira"), 
                           tabName = "green", badgeLabel = "new", badgeColor = "teal"),
                  # conditionalPanel(condition = "input.tabs == 'green'", 
                  #                  plotOutput("mapGreenSpace")),
                  
                  menuItem("Active living potential", icon = icon("child"), 
                           tabName = "active", badgeLabel = "popular", 
                           badgeColor = "purple"),
                  conditionalPanel(condition = "input.tabs == 'active'",
                                   plotOutput("mapActiveLivingPotential", 
                                              height = 250)),
                  
                  menuItem("Biodiversity", icon = icon("bug"), 
                           tabName = "Biodiversity"),
                  conditionalPanel(condition = "input.tabs == 'Biodiversity'", 
                                   plotOutput("mapBiodiversity")),
                  
                  menuItem("Climate-change risk", 
                           icon = icon("globe-americas"), badgeColor = "yellow"),
                  # conditionalPanel(condition = "input.tabs == 'Climate'", 
                  #                  plotOutput("mapClimateChange")),
                  
                  menuItem("Pedestrian realm", icon = icon("walking"), 
                           tabName = "Pedestrian", badgeLabel = "suggested", 
                           badgeColor = "aqua"),
                  conditionalPanel(condition = "input.tabs == 'Pedestrian'",
                                   plotOutput("map_distancing_capacity", 
                                              height = 250)),
                
                  
                  menuItem("Covid-19", icon = icon("head-side-mask"), 
                           tabName = "Covid", badgeLabel = "health and safety", 
                           badgeColor = "red"),
                  # conditionalPanel(condition = "input.tabs == 'Covid'",
                  #                  plotOutput("mapCovid19")),
                  
                  menuItem("Economic health", icon = icon("dollar-sign"), 
                           tabName = "Economic"),
                  # conditionalPanel(condition = "input.tabs == 'Economic'", 
                  #                  plotOutput("mapEconomic_health")),
                  
                  menuItem("Commuter mode switching", icon = icon("biking"), 
                           tabName = "mode"),
                  conditionalPanel(condition = "input.tabs == 'mode'", 
                                   plotOutput("mapModeShift", height = 250)),
                  
                  menuItem("Agriculture", icon = icon("carrot"), 
                           tabName = "Agriculture"),
                  # conditionalPanel(condition = "input.tabs == 'Agriculture'",
                  #                  plotOutput("mapAgriculture")),
                  
                  menuItem("Energy consumption", icon = icon("fire-alt"), 
                           tabName = "Energy"),
                  # conditionalPanel(condition = "input.tabs == 'Energy'", 
                  #                  plotOutput("mapEnergy")),
                  
                  menuItem("Food availability", icon = icon("cheese"), 
                           tabName = "Food"),
                  # conditionalPanel(condition = "input.tabs == 'Food'", 
                  #                  plotOutput("mapFood")),
                  
                  menuItem("Land use", icon = icon("warehouse"), 
                           tabName = "Land"),
                  # conditionalPanel(condition = "input.tabs == 'Land'", 
                  #                  plotOutput("mapLandUse")),
                  
                
                  
                  menuItem("Public transit", icon = icon("train"), 
                           tabName = "transit", badgeLabel = "on the ballot", 
                           badgeColor = "teal"),
                  # conditionalPanel(condition = "input.tabs == 'transit'", 
                  #                  plotOutput("mapTransitLine"))
                  
                  menuItem("Short-term rentals", icon = icon("airbnb"), 
                           tabName = "rentals"),
                  # conditionalPanel(condition = "input.tabs == 'rentals'",
                  #                  plotOutput("mapShortTermRentals")),
                  
                  menuItem("Water availability", icon = icon("water"), 
                           tabName = "Water")#,
                  # conditionalPanel(condition = "input.tabs == 'Water'", 
                  #                  plotOutput("mapWater")),
                  
                  ), collapsed = FALSE),
    
    ## BODY  
    dashboardBody(
      tags$head(tags$script(HTML(js))),
      tags$head(tags$script(HTML(js2))),
      tags$head(tags$script(HTML(js3))),
      tags$head(tags$style(HTML('
      /* logo */
      .skin-black .main-header .logo {
      background-color: #FFFFFF;
      }
      
      /* logo when hovered */
      .skin-black .main-header .logo:hover {
      background-color: #FFFFFF;
      }
      
      /* navbar (rest of the header) */
      .skin-black .main-header .navbar {
      background-color: #FFFFFF;
      }
      
      /* main sidebar */
      .skin-black .main-sidebar {
      background-color: #FFFFFF;
      }
      
      /* active selected tab in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
      background-color: #0096C9;
      color: #FFFFFF;
      }
      
      /* other links in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu a{
      background-color: #FFFFFF50;
      color: #3C3C3B;
      }
      
      /* other links in the sidebarmenu when hovered */
      .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
      background-color: #0096C910;
      }
      
      /* toggle button when hovered  */
      .skin-black .main-header .navbar .sidebar-toggle:hover{
      background-color: #FFFFFF;
      }
      
      /* body */
      .content-wrapper, .right-side {
      background-color: #FFFFFF;
      }
                                '))),
      
      #setShadow(id = "input_control_left2"),
      
      tabItems(tabItem(tabName = "active",
                            mapdeckOutput(
                              outputId = 'myMap'
                              , height = "1200px"
                            ),
                            tags$head(tags$style(
                              HTML('
             #input_control_overlay {background-color: rgba(0,0,255,0.0);border-width: 0px;}
             #input_control_left {background-color: rgba(0,0,255,0.0);border-width: 0px;}
             #input_control_left2 {background-color: rgba(0,0,255,0.0);border-width: 0px;}
             #input_control_right {background-color: rgba(0,0,255,0.0);border-width: 0px;}')
                            )),
                       
                       absolutePanel(
                         id="input_control_left2",
                         style="z-index:501;",
                         class = "panel panel-default",
                         draggable = FALSE, 
                         top = 60, right = 50,
                         width = 500,
                         
                         conditionalPanel(condition = "output.zoom_ALP == 'OUT'", 
                                          plotOutput("context_plot", height = 200), 
                                          id = "plotContainer2")),
                       jqui_draggable(
                         absolutePanel(
                                id="input_control_overlay",
                                style="z-index:500;",
                                class = "panel panel-default",
                                draggable = TRUE, 
                                top = 60, right = 50,
                                conditionalPanel(
                                  id = "menuContainer",
                                  condition = "output.zoom_ALP == 'IN'",
                                  selectInput("data_for_plot_right", 
                                              label = h3("Try comparing pedestrian capacity with other variables"), 
                                              selected = "MedRent_quant3", choices = var_list_2),
                                  plotOutput("map2", height = 250),
                                                 
                                                 HTML(markdownToHTML(fragment.only=TRUE, text=c(
                                                   "bla bla bla bla bla bla 
         
bal blabla balalallalal
         
bla bla bla bla bla bla")))))
                         , verbatimTextOutput(outputId = "observed_click"))),
               tabItem(tabName = "Pedestrian",
                       mapdeckOutput(
                         outputId = 'PedestrianMap'
                         , height = "1200px"
                       ),
                       absolutePanel(
                         id="input_control_right",
                         style="z-index:501;",
                         class = "panel panel-default",
                         draggable = TRUE, 
                         top = 60, left = "40%",
                         width = 250,
                         conditionalPanel(condition = "output.zoom == 'IN' && input.switch_biv == true",  id = "plotContainer_ped",
                                          helpText(h2(tags$div(align = 'center', (strong("Your second variable:", style = "color:#B2D235"))))), 
                                          plotOutput("second_variable", width = 250, height= 250),
                                          helpText(tags$div(align = 'center', (strong("Drag to move", style = "color:#B2D235"))))
                         ))          
                       
                       ,
                       
                       absolutePanel(
                         id = "controls", class = "panel panel-default",
                         draggable = FALSE, top = "5%",
                         conditionalPanel(condition = "output.zoom == 'IN'", id = "plotContainer_ped_control",
                                          dropdownButton(
                                            label = "",
                                            icon = icon("gear"),
                                            status = "primary",
                                            circle = TRUE,
                                            width = 350,
                                            materialSwitch(inputId = "switch_biv", label = h3(strong("Perform a Bivariate Analysis", style = "color:#B2D235")), status = "primary", value = FALSE),
                                            conditionalPanel(condition = "input.switch_biv == true", id = "plotContainer_ped_control",
                                                             selectInput("data_for_plot_ped", label=h4( tags$em(tags$span(style="color:#3C3C3B", "Select your second variable"))),
                                                                         selected = "agg_proximity_score_quant3", choices = list(
                                                                           "Walkable Access to Key Amenities" = "agg_proximity_score_quant3",
                                                                           "Net Median Income" = "net_median_income_quant3",
                                                                           "Visible Minority Population" = "visible_minority_pop_quant3", 
                                                                           "Immigrant Population" = "immigrants_quant3"))
                                            ),
                                            h4(strong("Montreal Covid-19 Expanded Active Transit Corridors", style = "color:#B2D235")), 
                                            materialSwitch(inputId = "vas_1", label = "Original Plan (May 15, 2020)", status = "info", value = FALSE),
                                            materialSwitch(inputId = "vas_2", label = "Revised Plan (July 25, 2020", status = "info", value = FALSE),
                                            selectInput(
                                              inputId = "variable_ped",
                                              label = h4(strong("Choose more variables and explore further", style = "color:#B2D235")), 
                                              choices = list("Population density per square km" = 1, "Pedestrian social distancing capacity" = 2, "Work commutes by car (%)" = 3, "Trajet MTL 2016 data on pedestrian flows" = 4),
                                              selected = 1
                                            ),
                                            h5(tags$em(tags$span(style="color:#3C3C3B", "Play around with the slider to filter through the map"))), 
                                            h5( chooseSliderSkin(
                                              skin = "Flat",
                                              #c("Shiny", "Flat", "Modern", "Nice", "Simple", "HTML5", "Round", "Square"),
                                              color = "#B2D235"
                                            ),
                                            sliderInput (inputId = "slider_ped",
                                                         label = "",
                                                         0, 12,
                                                         value = c(0, 12),
                                                         step = 1)
                                          ))))), 
               
               
                    tabItem(tabName = "home",
                            fluidPage(
                              imageOutput("homepic", height = 600)
                            )  
                            
                    ),
                    tabItem(tabName = "mode",
                            fluidPage(
                              
                              mapdeckOutput(outputId = "qzmyMap",
                                            height = "1200px"),
                              absolutePanel(
                                id = "controls", class = "panel panel-default",
                                draggable = FALSE, top = 55, left = 300,
                                width = 0, height = 0,
                                dropdownButton(
                                  label = "",
                                  inputId = "drop",
                                  icon = icon("gear"),
                                  status = "primary",
                                  circle = TRUE,
                                  width = 330,
                                  h4(strong("Modal Shift Scenarios")),
                                  radioGroupButtons("radio1",label = "Predefined Scenarios",
                                                    checkIcon = list(
                                                      yes = tags$i(class = "fa fa-check-square", 
                                                                   style = "color: steelblue"),
                                                      no = tags$i(class = "fa fa-square-o", 
                                                                  style = "color: steelblue")),
                                                    choices = list("Scenario 1" = 1,"Scenario 2" = 2, "Reset" = 3),
                                                    selected = 3),
                                  sliderTextInput(
                                    inputId = "slider1",
                                    label = "Cycling Distance (km):", 
                                    choices = seq(from = 1,
                                                  to = 10,
                                                  by = 0.1),
                                    grid = TRUE
                                  ),
                                  
                                  sliderTextInput(
                                    inputId = "slider2",
                                    label = "Elevation Gain (m):", 
                                    choices = seq(from = 10,
                                                  to = 55,
                                                  by = 5),
                                    grid = TRUE
                                  ),
                                  sliderTextInput(
                                    inputId = "slider3",
                                    label = "Time Ratio:", 
                                    choices = seq(from = 1.0,
                                                  to = 3.0,
                                                  by = 0.2),
                                    grid = TRUE
                                  ),
                                  # materialSwitch(inputId = "switch1", 
                                  #                label = "Modelled Cycling Route", 
                                  #                status = "primary", value = FALSE),
                                  hr(),
                                  materialSwitch(inputId = "switch2", 
                                                 label = "Cycling Network", 
                                                 status = "primary", value = TRUE)
                                  
                                )
                              ),
                              absolutePanel(
                                id="panel1",
                                style="z-index:500;",
                                class = "panel panel-default",
                                draggable = FALSE, 
                                top = 60, right = 50,
                                widtth=60,
                                conditionalPanel(
                                  condition = "output.zoom_level == 'ISO'",
                                  h4(strong("Choropleth Map")),
                                  pickerInput(
                                    inputId = "variable",
                                    label = "Select a variable:", 
                                    choices = list("Share of Car Trips" = 2, 
                                                   "Average Commuting Distance" = 3, 
                                                   "Access to Cycling Infrastructure" = 1),
                                    selected = 2
                                  ),
                                  knobInput(
                                    inputId = "knob1",
                                    label = "Car Share by Origin Census Tract:",
                                    step = 0.5,
                                    min = 4,
                                    max = 17,
                                    value = 17,
                                    displayPrevious = TRUE,
                                    lineCap = "round",
                                    fgColor = "#B2D235",
                                    inputColor = "#B2D235"
                                  )
                                ),
                                conditionalPanel(
                                  condition = "output.zoom_level == 'OUT' & input.radio1 <3",
                                  h4(strong("VMT Reduction")),
                                  DT::DTOutput("table")
                                )
                              )
                            )  
                    ))
                  
                )
  ))



