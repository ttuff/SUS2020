##### SUS UI SCRIPT ############################################################

shinyUI(
  
  dashboardPage(

    skin = "black", 

    dashboardHeader(tags$li(class = "dropdown",
                            tags$style(".main-header {max-height: 55px}"),
                            tags$style(".main-header .logo {height: 50px}")),
                    
                    title = loadingLogo('http:www.drtuff.com', 'logo.png',
                                        'spinning_logo.gif', 50, 50, 50)),
    
    ## Left sidebar ------------------------------------------------------------
    
    dashboardSidebar(
      width = 250,
      sidebarMenu(id = "tabs",
                  
                  menuItem("Home", tabName = "home", 
                           icon = icon("balance-scale")),
                  
                  menuItem("Access to green space", icon = icon("envira"), 
                           tabName = "green", badgeLabel = "new", 
                           badgeColor = "teal"),
                  
                  menuItem("Active living potential", icon = icon("child"), 
                           tabName = "active", badgeLabel = "popular", 
                           badgeColor = "purple"),
                  conditionalPanel(condition = "input.tabs == 'active'",
                                   plotOutput("active_map_left", 
                                              height = 250)),
                  
                  menuItem("Agriculture", icon = icon("carrot"), 
                           tabName = "Agriculture"),
                  
                  menuItem("Biodiversity", icon = icon("bug"), 
                           tabName = "Biodiversity"),
                  
                  menuItem("Climate-change risk", 
                           icon = icon("globe-americas"), 
                           badgeColor = "yellow"),
                  
                  menuItem("Commuter mode switching", icon = icon("biking"), 
                           tabName = "mode"),
                  conditionalPanel(condition = "input.tabs == 'mode'", 
                                   plotOutput("commuter_map_left", 
                                              height = 250)),
                  
                  menuItem("Covid-19", icon = icon("head-side-mask"), 
                           tabName = "Covid", badgeLabel = "health and safety", 
                           badgeColor = "red"),
                  
                  menuItem("Pedestrian realm", icon = icon("walking"), 
                           tabName = "Pedestrian", badgeLabel = "suggested", 
                           badgeColor = "aqua"),
                  conditionalPanel(condition = "input.tabs == 'Pedestrian'",
                                   plotOutput("pedestrian_map_left", 
                                              height = 250)),
                  
                  menuItem("Economic health", icon = icon("dollar-sign"), 
                           tabName = "Economic"),
                  
                  menuItem("Energy consumption", icon = icon("fire-alt"), 
                           tabName = "Energy"),
                  
                  menuItem("Food availability", icon = icon("cheese"), 
                           tabName = "Food"),
                  
                  menuItem("Housing", icon = icon("home"), 
                           tabName = "rentals"),
                  
                  menuItem("Land use", icon = icon("warehouse"), 
                           tabName = "Land"),
                  
                  menuItem("Public transit", icon = icon("train"), 
                           tabName = "transit", badgeLabel = "on the ballot", 
                           badgeColor = "teal"),
                  
                  menuItem("Water availability", icon = icon("water"), 
                           tabName = "Water")
                  
      ), collapsed = FALSE),
    
    
    ## Body --------------------------------------------------------------------
    
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
      
      tabItems(
        
        ## Home page -----------------------------------------------------------
        
        tabItem(tabName = "home", 
                fluidPage(imageOutput("homepic", height = 600), 
                          align = "center")),
        
        
        ## Active living potential ---------------------------------------------
        
        tabItem(
          tabName = "active",
          
          mapdeckOutput(outputId = 'myMap', height = "1200px"),
          
          tags$head(tags$style(HTML('
          #title_bar {border-width: 10px; border-color: rgb(255, 255, 255);}
          #input_control_overlay {border-width: 10px; 
          border-color: rgba(255,255,255,1);}
          #input_control_left {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}
          #input_control_left2 {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}
          #active_legend_container {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}'))),
          
          absolutePanel(
            id = "title_bar", class = "panel panel-default", 
            draggable = FALSE, top = 70, left = 270, width = "40%",
            h2("Active living potential: the CanALE index"),
            p(title_text %>% 
                filter(tab == "active", type == "main") %>% 
                pull(text)),
            actionLink("more_info", "Learn more"),
            conditionalPanel(
              condition = "output.more_info_status == 1",
              p(title_text %>% 
                  filter(tab == "active", type == "extra") %>% 
                  pull(text)))),
          
          absolutePanel(
            id = "input_control_overlay", style = "z-index:500;",
            class = "panel panel-default", top = 70, right = 50, width = 300,
            materialSwitch(
              inputId = "active_extrude", 
              label = "View in 3D", 
              status = "primary",
              value = FALSE),
            hr(),
            fluidRow(
              column(width = 8,
                     h4("Compare")),
              column(width = 4, align = "right",
                     actionLink(inputId = "active_hide_compare",
                                label = "Hide"))),
            conditionalPanel(
              condition = "output.active_hide_compare_status == 1",
              selectInput("data_for_plot_right", 
                          label = NULL, 
                          choices = var_list),
              plotOutput("active_map_right", height = 250)),
            hr(),
            fluidRow(
              column(width = 8,
                     h4("Explore")),
              column(width = 4, align = "right",
                     actionLink(inputId = "active_hide_explore",
                                label = "Hide"))),
            conditionalPanel(
              condition = "output.active_hide_explore_status == 1",
              htmlOutput("active_info"),
              conditionalPanel(
                condition = "output.active_poly_select == 1",
                actionLink(inputId = "active_clear_selection", 
                           label = "Clear selection")),
              plotOutput("bivariate_graph", height = 150)
              ),
            hr(),
            fluidRow(
              column(width = 8,
                     h4("Did you know?")),
              column(width = 4, align = "right",
                     actionLink(inputId = "active_hide_dyk",
                                label = "Hide"))),
            conditionalPanel(
              condition = "output.active_hide_dyk_status == 1",
              htmlOutput("did_you_know"))
            ),
          
          absolutePanel(
            id = "active_legend_container", class = "panel panel-default", 
            style = "z-index:500;", bottom = -200, left = 270, fixed = TRUE,
            conditionalPanel(condition = 'input.data_for_plot_right != " "',
                             id = "active_legend", 
                             imageOutput("bivariate_legend")))
          
          ),
        
        
        ## Pedestrian realm ----------------------------------------------------
        
        tabItem(
          tabName = "Pedestrian",
          
          mapdeckOutput(outputId = 'PedestrianMap', height = "1200px"),
          
          tags$head(tags$style(HTML('
          #title_bar_ped {border-width: 10px; border-color: rgb(255, 255, 255);}
          #input_control_right {border-width: 10px; 
          border-color: rgba(255,255,255,1);}
          #ped_legend_container {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}'))),
          
          absolutePanel(
            id = "title_bar_ped", class = "panel panel-default",
            draggable = FALSE, top = 70, left = 270, width = "40%",
            h3(textOutput("title_text_ped")),
            conditionalPanel(
              condition = "output.zoom == 'OUT'",
              id = "plotContainer_ped",
              p(title_text %>%
                  filter(tab == "pedestrian_ct", type == "main") %>%
                  pull(text))),
            conditionalPanel(
              condition = "output.zoom == 'IN'",
              id = "plotContainer_ped",
              p(title_text %>%
                  filter(tab == "pedestrian_da", type == "main") %>%
                  pull(text))),
            actionLink("more_info_ped", "Learn more"),
            conditionalPanel(
              condition = "output.more_info_ped_status == 1 && output.zoom == 'OUT'",
              id = "plotContainer_ped",
              p(title_text %>%
                  filter(tab == "pedestrian_ct", type == "extra") %>%
                  pull(text))),
            conditionalPanel(
              condition = "output.more_info_ped_status == 1 && output.zoom == 'IN'",
              id = "plotContainer_ped",
              p(title_text %>%
                  filter(tab == "pedestrian_da", type == "extra") %>%
                  pull(text)),
              imageOutput("exemplar_ped")),
            conditionalPanel(
              condition = "output.more_info_ped_status == 1 && output.zoom == 'FINAL'",
              id = "plotContainer_ped",
              p(title_text %>%
                  filter(tab == "pedestrian_sidewalk", type == "extra") %>%
                  pull(text)),
              imageOutput("sidewalk_calculation"))),
          
          absolutePanel(
            id = "input_control_right", style="z-index:501;", 
            class = "panel panel-default", top = 70, right = 50, width = 300,
            conditionalPanel(
              condition = "output.zoom == 'IN'",
              id = "plotContainer_ped_control",
            materialSwitch(inputId = "switch_biv", 
                           label = h3(strong("Perform a Bivariate Analysis", 
                                             style = "color:#B2D235")), 
                           status = "primary", value = FALSE),
            conditionalPanel(
              condition = "input.switch_biv == true",
              id = "plotContainer_ped_control",
              selectInput("data_for_plot_ped", 
                        label = h4(tags$em(tags$span(
                          style = "color:#3C3C3B", 
                          "Select your second variable"))), 
                        selected = "agg_proximity_score_quant3", 
                        choices = list(
                          "Walkable Access to Key Amenities" = 
                            "agg_proximity_score_quant3",
                          "Net Median Income" = 
                            "net_median_income_quant3",
                          "Visible Minority Population" = 
                            "visible_minority_pop_quant3", 
                          "Immigrant Population" = 
                            "immigrants_quant3")),
          plotOutput("second_variable", width = 250, height = 250)),
          hr(),
          h4(strong("Montreal Covid-19 Expanded Active Transit Corridors", 
                    style = "color:#B2D235")), 
          materialSwitch(inputId = "vas_1", 
                         label = "Original Plan (May 15, 2020)", 
                         status = "info", value = FALSE),
          materialSwitch(inputId = "vas_2", 
                         label = "Revised Plan (July 25, 2020", 
                         status = "info", value = FALSE),
          selectInput(
            inputId = "variable_ped",
            label = h4(strong("Choose more variables and explore further", 
                              style = "color:#B2D235")), 
            choices = list("Population density per square km" = 1, 
                           "Pedestrian social distancing capacity" = 2, 
                           "Work commutes by car (%)" = 3, 
                           "Trajet MTL 2016 data on pedestrian flows" = 4),
            selected = 1),
          h5(tags$em(tags$span(style = "color:#3C3C3B", 
                               "Play with the slider to filter the map"))), 
          h5(chooseSliderSkin(skin = "Flat",
                              #c("Shiny", "Flat", "Modern", "Nice", "Simple", "HTML5", "Round", "Square"),
                              color = "#B2D235"),
             sliderInput(inputId = "slider_ped", label = "", 0, 12, 
                         value = c(0, 12), step = 1))),
          hr(),
          fluidRow(
            column(width = 8,
                   h4("Explore")),
            column(width = 4, align = "right",
                   actionLink(inputId = "pedestrian_hide_explore",
                              label = "Hide"))),
          conditionalPanel(
            condition = "output.pedestrian_hide_explore_status == 1",
            htmlOutput("pedestrian_info"),
            # conditionalPanel to add later which clears selection if a polygon is clicked
            plotOutput("pedestrian_graph", height = 150)),
          hr(),
          fluidRow(
            column(width = 8,
                   h4("Did you know?")),
            column(width = 4, align = "right",
                   actionLink(inputId = "pedestrian_hide_dyk",
                              label = "Hide"))),
          conditionalPanel(
            condition = "output.pedestrian_hide_dyk_status == 1",
            htmlOutput("did_you_know_ped"))
          ),
          
          absolutePanel(
            id = "ped_legend_container", class = "panel panel-default",
            style = "z-index:500;", bottom = -200, left = 270, fixed = TRUE,
            conditionalPanel(
              condition = 
                "input.switch_biv == true && output.zoom == 'IN' && output.more_info_ped_status == 0",
                             id = "ped_legend",
                             imageOutput("bivariate_legend_ped")),
            conditionalPanel(
              condition = 
                "output.zoom == 'OUT' || output.zoom == 'IN' && input.switch_biv == false && output.more_info_ped_status == 0",
                             id = "ped_legend",
                             imageOutput("univariate_legend_ped")),
            conditionalPanel(condition = "output.zoom == 'FINAL' && output.more_info_ped_status == 0",
                             id = "ped_legend",
                             imageOutput("sidewalk_legend_ped"))
            )
          
          ),
        
            
        ## Commuting mode switch -----------------------------------------------
        
        tabItem(
          tabName = "mode",
          fluidPage(mapdeckOutput(outputId = "qzmyMap", height = "1200px"),
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
                          grid = TRUE),
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
                      widtth = 60,
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
                        
                        )))))))
  )
