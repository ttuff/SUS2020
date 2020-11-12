##### SUS UI SCRIPT ############################################################

dbHeader <- dashboardHeader(tags$li(class = "dropdown",
                                    tags$style(".main-header {max-height: 55px}"),
                                    tags$style(".main-header .logo {height: 50px}")),
                            title = "SUS")

dbHeader$children[[2]]$children <-  loadingLogo('http:www.drtuff.com', 'logo.png',
                                                'spinning_logo.gif', 50, 50, 50)


shinyUI(
  
  dashboardPage(

    skin = "black", 

    dbHeader,
    ### https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
    
    ## Left sidebar ------------------------------------------------------------
    
    dashboardSidebar(
      width = 200,
      sidebarMenu(id = "tabs", 
                  
                  menuItem("SUS Preview", tabName = "home", 
                           icon = icon("balance-scale")),
                  
                  # menuItem("Access to green space", icon = icon("envira"),
                  #          tabName = "green", badgeLabel = "new",
                  #          badgeColor = "teal"),
                  
                  menuItem("Active living potential", icon = icon("child"), 
                           tabName = "active", badgeLabel = "Built environment", 
                           badgeColor = "purple"),
                  conditionalPanel(condition = "input.tabs == 'active'",
                                   plotOutput("active_map_left", 
                                              height = 200)),
                  
                  
                  # 
                  # menuItem("Climate-change risk",
                  #          icon = icon("globe-americas"),
                  #          badgeColor = "yellow"),
                  
                  menuItem("Commuter mode switching", icon = icon("biking"), 
                           tabName = "mode", badgeLabel = "Simulation", badgeColor = "aqua"),
                  conditionalPanel(condition = "input.tabs == 'mode'", 
                                   plotOutput("commuter_map_left", 
                                              height = 200)),
                  
                  # menuItem("Covid-19", icon = icon("head-side-mask"),
                  #          tabName = "Covid", badgeLabel = "health and safety",
                  #          badgeColor = "red"),
                  # 
                  # menuItem("Economic health", icon = icon("dollar-sign"),
                  #          tabName = "Economic"),
                  # 
                  # menuItem("Energy consumption", icon = icon("fire-alt"),
                  #          tabName = "Energy"),
                  # 
                  # menuItem("Food availability", icon = icon("cheese"),
                  #          tabName = "Food"),
                  # 
                  # menuItem("Housing", icon = icon("home"),
                  #          tabName = "rentals"),
                  # 
                  # menuItem("Land use", icon = icon("warehouse"),
                  #          tabName = "Land"),
                  
                  menuItem("Pedestrian realm", icon = icon("walking"), 
                           tabName = "Pedestrian", badgeLabel = "Covid-19", 
                           badgeColor = "red"),
                  conditionalPanel(condition = "input.tabs == 'Pedestrian'",
                                   plotOutput("pedestrian_map_left", 
                                              height = 200))
                  ,
                  menuItem("Biodiversity", icon = icon("bug"),
                           tabName = "Biodiversity", badgeLabel = "Nature-based solutions"),
                  
                  # menuItem("Public transit", icon = icon("train"),
                  #          tabName = "transit", badgeLabel = "on the ballot",
                  #          badgeColor = "teal"),
                  # 
                  br(),

                  menuItem("Why a dashboard?", tabName = "why_dashboard"),
                  menuItem("Meet the team", tabName = "meet_the_team")
                  
      ), collapsed = FALSE),
    
    
    ## Body --------------------------------------------------------------------
    
    dashboardBody(
      tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png")),
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
      height: 60px;
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
      
      tabItems(
        
        ## Home page -----------------------------------------------------------
        
        tabItem(tabName = "home", 
                fluidPage(id = 'home',
                          tags$style('#home {background-color: #FBFBFB;}'),
                  fluidRow(imageOutput("homepic", height = 600), 
                           align = "center"),
                  fluidRow(hr()),
                  fluidRow(br()),
                  fluidRow(imageOutput("mssipic", height = 80), 
                           align = "center"),
                  fluidRow(HTML(paste0(
                    "<h5>An initiative of the <a href = ", "
                    'https://www.mcgill.ca/mssi/'>McGill ",
                    "Sustainability Systems Initiative</a></h5>" 
                  )),
                    align = "center")
                  )),
        
        
        ## Why a dashboard? ----------------------------------------------------
        
        tabItem(tabName = "why_dashboard",
                tags$head(tags$style(HTML('#why_title_bar {border-width: 10px; 
                                          border-color: rgb(255, 255, 255);}'))),
                absolutePanel(
                  id = "why_title_bar", class = "panel panel-default", 
                  draggable = FALSE, top = 70, left = 270, width = "40%",
                  h2("Why a dashboard? The science behind Sus"),
                  imageOutput("glamour_shot", height = 300),
                  p(paste0("Dashboards offer a tool for communicating ", 
                           "sustainability data in a visually based digital ", 
                           "platform. We see a gap in current dashboards ",
                           "going beyond the visualization of pre-existing ",
                           "data at static scales, leaving room for a more ",
                           "future-oriented, scalable, and interactive model.")),
                  p(paste0("Existing data-driven approaches to urban ",
                           "sustainability are characterized by static data, ",
                           "limited user interaction, and the ",
                           "oversimplification of complex urban issues. ", 
                           "They provide little opportunity for user ", 
                           "engagement and exploration of questions ",
                           "connecting different data and issues.")),
                  p(paste0("Some of the limitations of existing dashboards ",
                           "include a bias towards quantifiable, measurable ",
                           "components of sustainability, and a reliance on ",
                           "data with potential bias. Furthermore, they often ",
                           "attempt to play the role of a neutral force to ",
                           "communicate “objective” information on cities.")),
                  p(paste0("Sustainability dashboards should build upon best ",
                           "practices to provide useful tools for individuals ",
                           "and cities alike to examine the many facets of ", 
                           "urban sustainability and question existing ",
                           "assumptions.")),
                  p(paste0("Maintaining transparency with data and ", 
                           "methodologies, ensuring public participation and ",
                           "accurate representation of underprivileged ", 
                           "communities, and using engaging and accessible ", 
                           "tools contribute to the success of a dashboard.")),
                  p(paste0("Sus aims to more accurately represent and better ", 
                           "engage urban residents in order to harness the ", 
                           "momentum surrounding technologically-based ", 
                           "approaches to sustainability for public good.")),
                  br(),
                  p("Further resources:"),
                  HTML(paste0("<ul><li><a href= ''>Robin Basalaev-Binder ",
                              "and David Wachsmuth. 2020. 'Progress in ",
                              "data-driven urban sustainability'. ",
                  "Working paper.</a> <b>(MSSI research)</b></ul>"))
                  
                  )),
        
        
        
        
        ## Active living potential ---------------------------------------------
        
        tabItem(
          tabName = "active",
          
          # Main map
          mapdeckOutput(outputId = 'active_map', height = "1000px"),
          
          # Style tags
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
          
          # Title bar
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
              HTML(title_text %>% 
                     filter(tab == "active", type == "extra") %>% 
                     pull(text)))),
          
          # 3D switch
          absolutePanel(
            id = "input_control_overlay", style = "z-index:500;",
            class = "panel panel-default", top = 70, right = 50, width = 300,
            materialSwitch(
              inputId = "active_extrude", 
              label = "View in 3D", 
              status = "danger",
              value = FALSE),
            hr(),
            
            # Compare panel
            fluidRow(
              column(width = 8, h4("Compare")),
              column(width = 4, align = "right",
                     actionLink(inputId = "active_hide_compare", 
                                label = "Hide"))),
            conditionalPanel(
              condition = "output.active_hide_compare_status == 1",
              selectInput("data_for_plot_right", label = NULL, 
                          choices = var_list),
              plotOutput("active_map_right", height = 250)),
            conditionalPanel(
              condition = "input.active_extrude == 0",
              hr(),
              
              # Explore panel
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
                  condition = "output.active_poly_selected == 1",
                  actionLink(inputId = "active_clear_selection", 
                             label = "Clear selection")),
                plotOutput("bivariate_graph", height = 150)
              ),
              hr(),
              
              # DYK panel
              fluidRow(
                column(width = 8,
                       h4("Did you know?")),
                column(width = 4, align = "right",
                       actionLink(inputId = "active_hide_dyk",
                                  label = "Hide"))),
              conditionalPanel(
                condition = "output.active_hide_dyk_status == 1",
                htmlOutput("did_you_know"))
              )
            ),
          
          # Floating legend
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
          
          mapdeckOutput(outputId = 'PedestrianMap', height = "1000px"),
          
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
              HTML(title_text %>%
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
                        label = h4(tags$span(
                          style = "color:#3C3C3B", 
                          "Select your second variable")), 
                        selected = "agg_proximity_score_quant3", 
                        choices = list(
                          "Walkable Access to Key Amenities" = 
                            "agg_proximity_score",
                          "Net Median Income" = 
                            "net_median_income",
                          "Visible Minority Population" = 
                            "visible_minority_pop", 
                          "Immigrant Population" = 
                            "immigrants")),
              fluidRow(
                column(width = 2, offset = 10, align = "right",
              actionLink(inputId = "pedestrian_hide_second_variable",
                         label = "Hide"))),
              conditionalPanel(
                condition = "output.pedestrian_hide_second_variable_status == 1",
                plotOutput("second_variable", width = 250, height = 250))),
          hr(),
          fluidRow(
            column(width = 8,
                   h4("Montreal Covid-19 Expanded Active Transit Corridors")),
            column(width = 4, align = "right",
                   actionLink(inputId = "vas_hide_explore",
                              label = "Hide"))),
          conditionalPanel(
            ## Adding impossible condition to turn off this box. restore to: "output.vas_hide_explore_status == 1"
            condition = "output.vas_hide_explore_status == 1",
          materialSwitch(inputId = "vas_1", 
                         label = "Original Plan (May 15, 2020)", 
                         status = "info", value = FALSE),
          materialSwitch(inputId = "vas_2", 
                         label = "Revised Plan (July 25, 2020)", 
                         status = "info", value = FALSE)),
          hr(),
          selectInput(
            inputId = "variable_ped",
            label = h4("Choose more variables and explore further"), 
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
            conditionalPanel(
              condition = "output.pedestrian_poly_selected == 1",
              actionLink(inputId = "pedestrian_clear_selection", 
                         label = "Clear selection")),
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
          tags$head(tags$style(HTML('
          #title_bar_commute {border-width: 10px; border-color: rgb(255, 255, 255);}
          #commute_right_bar {border-width: 10px; 
          border-color: rgba(255,255,255,1);}'))),
          
          # Main map
          mapdeckOutput(outputId = "qzmyMap", height = "1000px"),
          
          # Title bar
          absolutePanel(
            id = "title_bar_commute", class = "panel panel-default", 
            draggable = FALSE, top = 70, left = 270, width = "40%",
            h2("Shifting car trips to cycling"),
            p(title_text %>% 
                filter(tab == "commute", type == "main") %>% 
                pull(text)),
            actionLink("commute_more_info", "Learn more"),
            conditionalPanel(
              condition = "output.commute_more_info_status == 1",
              HTML(title_text %>% 
                     filter(tab == "commute", type == "extra") %>% 
                     pull(text)))),
            
          # Explore panel
          absolutePanel(
            id = "commute_right_bar", style = "z-index:500;",
            class = "panel panel-default", top = 70, right = 50, width = 300,
            conditionalPanel(
              condition = "output.zoom_level == 'OUT'",
              h4("Explore"),
              selectInput(
                "commute_variable", label = NULL, 
                choices = list("Share of trips taken by car" = 2, 
                               "Average commuting distance" = 3, 
                               "Access to cycling infrastructure" = 1),
                selected = 2),
              sliderInput(
                inputId = "commute_explore_slider",
                label = "% of trips taken by car, by census tract",
                min = 0,
                max = 100,
                value = c(0, 100)),
              plotOutput("commute_histogram", height = 200)
              ),
            
            # Simulate panel
            conditionalPanel(
              condition = "output.zoom_level == 'IN'",
              
              h4("Simulate"),
              
              radioGroupButtons(
                "radio1",
                label = "Modal shift scenarios",
                choices = list("Baseline" = 3,
                               "Distance" = 1,
                               "Elevation/time" = 2),
                selected = 3),
              
              conditionalPanel(
                condition = "output.zoom_level == 'IN' && input.radio1 <3",
                materialSwitch(inputId = "baseline_switch", 
                               label = "Show baseline", 
                               status = "primary", value = TRUE)
                ),
              
              sliderTextInput(
                inputId = "slider1",
                label = "Cycling distance (km):", 
                choices = seq(from = 1,
                              to = 10,
                              by = 0.1),
                grid = TRUE),
              
              sliderTextInput(
                inputId = "slider2",
                label = "Elevation gain (m):", 
                choices = seq(from = 10,
                              to = 55,
                              by = 5),
                grid = TRUE
              ),
              
              sliderTextInput(
                inputId = "slider3",
                label = "Time ratio:", 
                choices = seq(from = 1.0,
                              to = 3.0,
                              by = 0.2),
                grid = TRUE
              ),
              
              hr(),
              
              conditionalPanel(
                condition = "output.zoom_level == 'IN' && input.radio1 <3",
                h5(strong("VMT Reduction")),
                DT::DTOutput("table")),
              
              hr(),
              materialSwitch(inputId = "switch2", 
                             label = "Cycling network", 
                             status = "primary", value = FALSE)
              )
            )
          )
        )
      )
    )
  )
