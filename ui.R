library(mapdeck)
library(shiny)
library(shinydashboard)
library(jsonify)
library(sf)
library(geojsonsf)
library(tidyverse)
library(raster)
library(mapboxapi)
library(mapdeck)
library(markdown)
library(shinyjqui)
library(plotly)
library(png)
library(shinyWidgets)
library(cowplot)
library(RColorBrewer)
library(classInt)
library(scales)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(extrafont)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(mapdeck)
library(DT)
library(dplyr)

source("bivariate.R")

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

qz <- reactiveValues(zoom_level = 'NO')

js_ped <- "$(document).ready(function(){
  $('#plotContainer_ped').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js_ped_control <- "$(document).ready(function(){
  $('#plotContainer_ped_control').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

rz_pedestrian <- reactiveValues(zoom = 'OUT')


js <- "
$(document).ready(function(){
  $('#plotContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js2 <- "
$(document).ready(function(){
  $('#menuContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
       $(this).css('opacity', 1).animate({opacity: 0}, {duration: 1000});
    })
  });
});
"


js3 <- "
$(document).ready(function(){
  $('#plotContainer2').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
       $(this).css('opacity', 1).animate({opacity: 0}, {duration: 1000});
    })
  });
});
"





rz <- reactiveValues(zoom = 'IN')

loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)")
    ),
    tags$a(href=href,
           div(class = "busy",  
               img(src=loadingsrc,height = height, width = width, alt = alt)),
           div(class = 'notbusy',
               img(src = src, height = height, width = width, alt = alt))
    )
  )
}


shinyUI(
  
  
dashboardPage(skin="black",
                      
                      
                      dashboardHeader(tags$li(class = "dropdown",
                                              tags$style(".main-header {max-height: 55px}"),
                                              tags$style(".main-header .logo {height: 50px}")
                      ),
                      title= loadingLogo('http:www.drtuff.com',
                                         'logo.png',
                                         'spinning_logo.gif', 50, 50, 50)
                      ),
                      dashboardSidebar(width = 250,
                                       
                                       
                                       sidebarMenu(id = "tabs",
                                                   
                                                   menuItem("Home", tabName = "home", icon = icon("balance-scale")),
                                                   #conditionalPanel(condition = "input.tabs == 'bivariate'" ,plotOutput("map1")),
                                                   menuItem("Pedestrian realm", icon = icon("walking"), tabName = "Pedestrian",
                                                            badgeLabel = "suggested", badgeColor = "aqua"),
                                                   conditionalPanel(condition = "input.tabs == 'Pedestrian'" ,plotOutput("map_distancing_capacity", height=250)),
                                                   menuItem("Access to green space", icon = icon("envira"), tabName = "green",
                                                            badgeLabel = "new", badgeColor = "teal"),
                                                   #conditionalPanel(condition = "input.tabs == 'green'" ,plotOutput("mapGreenSpace")),
                                                   menuItem("Active living potential", icon = icon("child"), tabName = "active",
                                                            badgeLabel = "popular", badgeColor = "purple"),
                                                   conditionalPanel(condition = "input.tabs == 'active'" ,plotOutput("mapActiveLivingPotential", height=250)),
                                                   menuItem("Biodiversity", icon = icon("bug"), tabName = "Biodiversity"),
                                                   #conditionalPanel(condition = "input.tabs == 'Biodiversity'" ,plotOutput("mapBiodiversity")),
                                                   menuItem("Commuter mode switching", icon = icon("biking"), tabName = "mode",
                                                            badgeLabel = "in the news", badgeColor = "yellow"),
                                                   conditionalPanel(condition = "input.tabs == 'mode'" ,plotOutput("mapModeShift", height=250)),
                                                   menuItem("Short-term rentals", icon = icon("airbnb"), tabName = "rentals"),
                                                   #conditionalPanel(condition = "input.tabs == 'rentals'" ,plotOutput("mapShortTermRentals")),
                                                   menuItem("Energy consumption", icon = icon("fire-alt"), tabName = "Energy"),
                                                   #conditionalPanel(condition = "input.tabs == 'Energy'" ,plotOutput("mapEnergy")),
                                                   menuItem("Climate change dangers", icon = icon("globe-americas"), tabName = "Climate",
                                                            badgeLabel = "on the ballot", badgeColor = "yellow"),
                                                   #conditionalPanel(condition = "input.tabs == 'Climate'" ,plotOutput("mapClimateChange")),
                                                   menuItem("Economic health", icon = icon("dollar-sign"), tabName = "Economic"),
                                                   #conditionalPanel(condition = "input.tabs == 'Economic'" ,plotOutput("mapEconomic_health")),
                                                   menuItem("Agriculture", icon = icon("carrot"), tabName = "Agriculture"),
                                                   #conditionalPanel(condition = "input.tabs == 'Agriculture'" ,plotOutput("mapAgriculture")),
                                                   menuItem("Food availability", icon = icon("cheese"), tabName = "Food"),
                                                   #conditionalPanel(condition = "input.tabs == 'Food'" ,plotOutput("mapFood")),
                                                   menuItem("Water availability", icon = icon("water"), tabName = "Water"),
                                                   #conditionalPanel(condition = "input.tabs == 'Water'" ,plotOutput("mapWater")),
                                                   menuItem("Land use types", icon = icon("warehouse"), tabName = "Land"),
                                                   #conditionalPanel(condition = "input.tabs == 'Land'" ,plotOutput("mapLandUse")),
                                                   menuItem("Covid 19", icon = icon("head-side-mask"), tabName = "Covid",
                                                            badgeLabel = "health and safety", badgeColor = "red"),
                                                   #conditionalPanel(condition = "input.tabs == 'Covid'" ,plotOutput("mapCovid19")),
                                                   menuItem("New transit line", icon = icon("train"), tabName = "transit",
                                                            badgeLabel = "new", badgeColor = "teal")#,
                                                   #conditionalPanel(condition = "input.tabs == 'transit'" ,plotOutput("mapTransitLine"))
                                       ),
                                       collapsed = FALSE)
                      
                      ## BODY  
                      
                      , dashboardBody(
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
                          tabItem(tabName = "active",
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
                                  )) ,
                                  
                                  absolutePanel(
                                    id="input_control_left2",
                                    style="z-index:501;",
                                    class = "panel panel-default",
                                    draggable = FALSE, 
                                    top = 60, right = 50,
                                    width = 500,
                                    
                                    
                                    conditionalPanel(condition = "output.zoom_ALP == 'OUT'", plotOutput("context_plot", height = 200), id = "plotContainer2")
                                  ),
                                  jqui_draggable(
                                    absolutePanel(
                                      id="input_control_overlay",
                                      style="z-index:500;",
                                      class = "panel panel-default",
                                      draggable = TRUE, 
                                      top = 60, right = 50,
                                      widtth=60,
                                      conditionalPanel(id = "menuContainer",condition = "output.zoom_ALP == 'IN'" ,
                                                       selectInput("data_for_plot_right", label=h3("Select your second variable"), 
                                                                   selected = "MedRent_quant3", choices = var_list),
                                                       plotOutput("map2", height = 250),
                                                       
                                                       HTML(markdownToHTML(fragment.only=TRUE, text=c(
                                                         "bla bla bla bla bla bla 
         
bal blabla balalallalal
         
bla bla bla bla bla bla"
                                                       )))
                                      ))
                                    , verbatimTextOutput(
                                      outputId = "observed_click"
                                    ))
                                  
                          ),
                            tabItem(tabName = "Pedestrian",
                                    mapdeckOutput(
                                      outputId = 'PedestrianMap'
                                      , height = "800px"
                                    ),
                                    jqui_draggable(absolutePanel(
                                      id="input_control_right",
                                      style="z-index:501;",
                                      class = "panel panel-default",
                                      draggable = TRUE, 
                                      top = 60, right = 0,
                                      width = 400,
                                      conditionalPanel(condition = "output.zoom == 'IN'",  id = "plotContainer_ped",
                                                       selectInput("data_for_plot_ped", label=h3("Select your second variable"),
                                                                   selected = "agg_proximity_score_quant3", choices = list(
                                                                     "Walkable Access to Key Amenities" = "agg_proximity_score_quant3",
                                                                     "Net Median Income" = "net_median_income_quant3",
                                                                     "Visible Minority Population" = "visible_minority_pop_quant3", 
                                                                     "Immigrant Population" = "immigrants_quant3")),
                                                       plotOutput("second_variable"),
                                                       HTML(markdownToHTML(fragment.only=TRUE, text=c("Drag to move"))))
                                      )          
                                      
                                    ),
                                    
                                    absolutePanel(
                                      id = "controls", class = "panel panel-default",
                                      draggable = TRUE, top = "5%",
                                      conditionalPanel(condition = "output.zoom == 'IN'", id = "plotContainer_ped_control",
                                      dropdownButton(
                                        label = "",
                                        icon = icon("gear"),
                                        status = "primary",
                                        circle = TRUE,
                                        width = 350,
                                        radioGroupButtons(inputId = "vas_plan",label = "Covid-19 Expanded Active Transportation Network",
                                                          checkIcon = list(
                                                            yes = tags$i(class = "fa fa-check-square", 
                                                                         style = "color: steelblue"),
                                                            no = tags$i(class = "fa fa-square-o", 
                                                                        style = "color: steelblue")),
                                                          choices = list("May 2020 plan" = 1,"July 2020 plan" = 2, "Remove plan view" = 0),
                                                          selected = 0),
                                        materialSwitch(inputId = "switch_biv", label = "Perform Bivariate Analysis", status = "primary", value = FALSE)
                                      )
                                    ))
                                    
                            ),
                          tabItem(tabName = "home",
                                  fluidPage(
                                    imageOutput("homepic", height = 600)
                                  )  
                                  
                          ),
                          tabItem(tabName = "mode",
                                  fluidPage(
                                    
                                    mapdeckOutput(outputId = "qzmyMap",
                                                  height = "800px"),
                                    absolutePanel(
                                      id = "controls", class = "panel panel-default",
                                      draggable = FALSE, top = 55, left = 400,
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
                                        # materialSwitch(inputId = "switch1", label = "Modelled Cycling Route", status = "primary", value = FALSE),
                                        hr(),
                                        materialSwitch(inputId = "switch2", label = "Cycling Network", status = "primary", value = TRUE)
                                        
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
                                          choices = list("Share of Car Trips" = 2, "Average Commuting Distance" = 3, "Access to Cycling Infrastructure" = 1),
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
  
  
  
  
  
  
  
  
  
  
  








