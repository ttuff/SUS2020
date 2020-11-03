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

dropshadow1 <- normalizePath(file.path("/Users/Ty/Dropbox/Dendritic connectivity/www/dropshadow1.png"))
dropshadow2 <- normalizePath(file.path("/Users/Ty/Dropbox/Dendritic connectivity/www/dropshadow2.png"))

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



ui <- dashboardPage(skin="black",
                    
                    
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
        conditionalPanel(condition = "input.tabs == 'Pedestrian'" ,plotOutput("mapPedestrians")),
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
        conditionalPanel(condition = "input.tabs == 'mode'" ,plotOutput("mapModeShift")),
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
     
     
     conditionalPanel(condition = "output.zoom == 'OUT'", plotOutput("context_plot", height = 200), id = "plotContainer2")
   ),
   jqui_draggable(
                absolutePanel(
                id="input_control_right",
                style="z-index:500;",
                class = "panel panel-default",
                draggable = TRUE, 
                top = 60, right = 50,
                widtth=60,
                conditionalPanel(id = "menuContainer",condition = "output.zoom == 'IN'" ,
            	 selectInput("data_for_plot_right", label=h3("Select your second variable"), 
                                                                  selected = "MedRent_quant3", choices = list(
                                                                    "Tenant housing" = "TenantH_quant3",
                                                                    "Sublet rental" = "Subs_quant3",
                                                                    "Over 30 yrs old" = "Plus30_quant3",
                                                                    "Median Rent" = "MedRent_quant3",
                                                                    "Average Rent" = "AvRent_quant3",
                                                                    "Median morgage price" = "MedMort_quant3",
                                                                    "Average morgage price" = "AvMort_quant3",
                                                                    "Median property value" = "MedVal_quant3",
                                                                    "Average property value" = "AvVal_quant3",
                                                                    "Number of owners" = "Owner_quant3",
                                                                    "Owners with morgages" = "Wmortg_quant3",
                                                                    "Over 30yr old that own home" = "Plus30Own_quant3",
                                                                    "CTIR" = "CTIR_quant3",
                                                                    "Less than 30" = "Less30_quant3",
                                                                    "More than 30" = "More30_quant3",
                                                                    "Suitable housing" = "Suitable_quant3",
                                                                    "Non-suitable housing" = "NonSuit_quant3",
                                                                    "Income under 5k" = "Under_5k_proportion_quant3",
                                                                    "Income 5k - 10k" = "IN5k_10k_proportion_quant3",
                                                                    "Income 10k - 15k" = "IN10k_15k_proportion_quant3",
                                                                    "Income 15k - 20k" = "IN15k_proportion_quant3",
                                                                    "Income 20k - 25k" = "N20k_25k_proportion_quant3",
                                                                    "Income 25k - 30k" = "IN25k_30k_proportion_quant3",
                                                                    "Income 30k - 35k" = "IN30k_35k_proportion_quant3",
                                                                    "Income 35k - 40k" = "IN35k_40k_proportion_quant3",
                                                                    "Income 40k - 45k" = "IN40k_45k_proportion_quant3",
                                                                    "Income 45k - 50k" = "IN45k_50k_proportion_quant3",
                                                                    "Income 50k - 60k" = "IN50k_60k_proportion_quant3",
                                                                    "Income 60k - 70k" = "IN60k_70k_proportion_quant3",
                                                                    "Income 70k - 80k" = "IN70k_80k_proportion_quant3",
                                                                    "Income 80k - 90k" = "IN80K_90k_proportion_quant3",
                                                                    "Income 90k - 100k" = "IN90k_proportion_quant3",
                                                                    "Income over 100k" = "INOver100k_proportion_quant3",
                                                                    "Income 100k - 250k" = "IN100k_125_proportion_quant3",
                                                                    "Income 125k - 150k" = "IN125k_150_proportion_quant3",
                                                                    "Income 150k - 200k" = "IN150k_200_proportion_quant3",
                                                                    "Income over 200k" = "Over200k_proportion_quant3",
                                                                    "Proportion of non-immigrants" = "Non_Im_proportion_quant3",
                                                                    "Proportion of immigrants" =  "Imm_proportion_quant3",
                                                                    "Proportion of new immigrants" = "Imm_5year_proportion_quant3",
                                                                    "Drive to work" = "driver_proportion_quant3",
                                                                    "Passenger to work" = "passenger_proportion_quant3",
                                                                    "Public transit to work" = "Pubtrans_proportion_quant3",
                                                                    "Walk to work" = "Walked_proportion_quant3",
                                                                    "Bicycle to work" = "Bicycle_proportion_quant3",
                                                                    "Other transit mode to work" = "Other_proportion_quant3",
                                                                    "15 minutes to work" = "T_15_proportion_quant3",
                                                                    "15-30 minutes to work" = "B15_29_proportion_quant3",
                                                                    "30-45 minutes to work" = "B30_44_proportion_quant3",
                                                                    "0-60 minutes to work" = "O_60_proportion_quant3",
                                                                    "45-60 minutes to work" = "B_45_59_proportion_quant3",
                                                                    "Household income less than 40K" = "under_40K_quant3",
                                                                    "Household income greater than 40K" = "over_40K_quant3",
                                                                    "canALE index" = "ale_tranis_quant3")),
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
          outputId = 'myMap2'
          , height = "800px"
        )
        
        ),
tabItem(tabName = "home",
      fluidPage(
        imageOutput("homepic", height = 600)
      )  
        
)))

)


server <- function(input, output) {

  output$homepic <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path("www/Sus logo transparent.png"))
    
    # Return a list containing the filename and alt text
   return( list(src = filename, contentType = "image/png",  width = 571,
                height = 551))
    
  }, deleteFile = FALSE)
  
  
  
  #print(observe(tops()))
  #observeEvent(input$tabswitch, {
  #  print(input$tabs)
     #newtab <- switch(input$tabs, "bivariate" = "widgets", "widgets" = "bivariate"
    #)
    #updateTabItems(session, "tabs", newtab)
  #})

  observe({
  print(input$input_control_left_position)
  #  print(output$top)
  })
 
    set_token('pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ') ## set your access token

load(file = "/Users/Ty/Dropbox/Dendritic connectivity/Sus class/Bivariate_tester/from_robin/Sus/bivariate shiny app/data/data_for_plot.Rdata")
  
 #class(data_for_plot)
 #library(raster)
#crs(data_for_plot) <- "+proj=longlat +datum=WGS84"
data_for_plot_r <- st_transform(data_for_plot, 4326)
#crs(data_for_plot_r)

data_for_plot_r  <- st_cast(data_for_plot_r, "MULTIPOLYGON")




  ## save colors
  bivariate_color_scale <- tibble(
    "3 - 3" = "#3F2949", # high inequality, high income
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low inequality, high income
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium inequality, medium income
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high inequality, low income
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low inequality, low income
  ) %>%
    gather("group", "fill")
  
  color_scale <- tibble(
    "6" = "#AE3A4E",
    "5" = "#BC7C8F", # medium inequality, medium income
    "4" = "#CABED0",
    "3" = "#4885C1", # high inequality, low income
    "2" = "#89A1C8",
    "1" = "#CABED0" # low inequality, low income
  ) %>%
    gather("group", "fill") 
  
  #color_scale <- cbind(color_scale[,1], color_scale) %>% 
  #as.numeric(color_scale)
  # names(color_scale) <- c("pers","haps", "fill_color")
  # color_scale$pers <- as.numeric(color_scale$pers)
  # color_scale$haps <- as.numeric(color_scale$haps)
  
  colors <- color_scale$fill
  colors <- as.character(colors)
  
  # g <- grid::circleGrob(gp = grid::gpar(fill = "white", col="white"))
  
  # ## maps output
  
  default_background_color <- "transparent"
  default_font_color <- "black"
  default_font_family <- "Helvetica"
  
  theme_map <- function(...) {
    default_background_color <- "transparent"
    default_font_color <- "black"
    default_font_family <- "Helvetica"
    
    theme_minimal() +
      theme(
        text = element_text(family = default_font_family,
                            color = default_font_color),
        # remove all axes
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # add a subtle grid
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # background colors
        plot.background = element_rect(fill = default_background_color,
                                       color = NA),
        panel.background = element_rect(fill = default_background_color,
                                        color = NA),
        legend.background = element_rect(fill = default_background_color,
                                         color = NA),
        legend.position = "none",
        # borders and margins
        plot.margin = unit(c(0, .5, .2, .5), "cm"),
        panel.border = element_blank(),
        panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
        # titles
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 22, hjust = 0,
                                   color = default_font_color),
        plot.title = element_text(size = 15, hjust = 0.5,
                                  color = default_font_color),
        plot.subtitle = element_text(size = 10, hjust = 0.5,
                                     color = default_font_color,
                                     margin = margin(b = -0.1,
                                                     t = -0.1,
                                                     l = 2,
                                                     unit = "cm"),
                                     debug = F),
        # captions
        plot.caption = element_text(size = 7,
                                    hjust = .5,
                                    margin = margin(t = 0.2,
                                                    b = 0,
                                                    unit = "cm"),
                                    color = "#939184"),
        ...
      )
  }
  #set_token('pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ')
  
  ####################################################
  # plot output calls for all 'left' plots
  ####################################################
  
  output$context_plot <- renderPlot({
    
    
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    p <- ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map() 
  
    
    ggdraw() + 
      draw_image(dropshadow1, scale = 1, vjust = -0.003, hjust = -0.003) +
      draw_plot(p)
    
    
  }, bg="transparent")
  
  
  output$mapPedestrians <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  
  output$mapActiveLivingPotential <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    p <- ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map() + 
      theme(legend.position = "none")
  
    
    ggdraw() + 
      draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
      draw_plot(p)
    
    
  }, bg="white")
  
  output$mapModeShift <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(Bicycle_proportion_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapBiodiversity <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  
  
  
  output$mapGreenSpace <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapShortTermRentals <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(TenantH_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapEnergy <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapClimateChange <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapEconomic_health <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(over_40K_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapAgriculture <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapFood <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapWater <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapLandUse <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(MedVal_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapCovid19 <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  output$mapTransitLine <- renderPlot({
    
    data_for_plot_left <- data_for_plot %>%
      dplyr::select(Pubtrans_proportion_quant3)
    
    colnames(data_for_plot_left) <- c("left_variable",  "geometry")
    
    ggplot(data_for_plot_left) +
      geom_sf(
        aes(
          fill = as.factor(left_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(1:3)]))+
      theme_map()
  })
  
  
  
  
  
  ####################################################
  # plot output calls for all 'right' plots
  ####################################################
  
  output$map2 <- renderPlot({
    data_for_plot_right <- data_for_plot %>%
      dplyr::select(input$data_for_plot_right)
    
    colnames(data_for_plot_right) <- c("right_variable",  "geometry")
    
    p <- ggplot(data_for_plot_right) +
      geom_sf(
        aes(
          fill = as.factor(right_variable)
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.01
      ) +
      scale_fill_manual(values=rev(colors[c(4:6)]))+
      theme_map()
  
    
    
    
    ggdraw() + 
      draw_image( dropshadow1, scale = 1.49, vjust = -0.003, hjust = -0.003) +
      draw_plot(p)
    
    
  }, bg="transparent")
  
  data_for_plot_r_bivar <- reactive({
    data_for_plot_bi <- data_for_plot %>%
      dplyr::select(ale_tranis_quant3, input$data_for_plot_right) 
    
    if(length(colnames(data_for_plot_bi)) == 2){data_for_plot_bi <- cbind(data_for_plot_bi[,1], data_for_plot_bi)[,1:3]}
    #print(head(data_for_plot_bi))
    colnames(data_for_plot_bi) <- c("left_variable", "right_variable",  "geometry")
    
    data_for_plot_bivariate <- data_for_plot_bi %>%
      mutate(
        group = paste(
          as.numeric(left_variable), "-",
          as.numeric(right_variable)
        )
      ) %>%
      left_join(bivariate_color_scale, by = "group")
    
    data_for_plot_bivariate <- cbind(data_for_plot_bivariate, as.numeric(data_for_plot_bivariate$left_variable) * as.numeric(data_for_plot_bivariate$right_variable))
    
    
    names(data_for_plot_bivariate)[5] <- 'elevation'
    data_for_plot_bivariate <- data_for_plot_bivariate[,-7]
    #data_for_plot_bivariate <- data_for_plot_bivariate[- which(is.na(data_for_plot_bivariate$elevation)),]
    data_for_plot_bivariate$elevation <- (data_for_plot_bivariate$elevation)^2 *50
    #library(raster)
    #which(is.na(data_for_plot_bivariate$elevation))
    print(max(data_for_plot_bivariate$elevation))
    data_for_plot_r_bivar <- st_transform(data_for_plot_bivariate, 4326)
    #crs(data_for_plot_r)
    
    data_for_plot_r_bivar  <- st_cast(data_for_plot_r_bivar, "MULTIPOLYGON")
    #return(data_for_plot_r_bivar)
  })
  
  isochrones <- mb_isochrone(c(-73.75,45.5),
                             time = c(30),
                             profile = c("walking"),
                             geometry = "polygon",
                             access_token='pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ')
  
  isochrones_path <- mb_isochrone(c(-73.75,45.5),
                             time = c(30),
                             profile = c("driving"),
                             geometry = "linestring",
                             access_token='pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ',
                             keep_color_cols = TRUE)
  
    output$myMap <- renderMapdeck({
    
    	 mapdeck(style = "mapbox://styles/ttuff/ckg422ljr1leo1al42f920pa8", zoom=10.1,location=c(-73.58,45.39), pitch=35) 
       })

    output$myMap2 <- renderMapdeck({
      mapdeck(style = "mapbox://styles/ttuff/ckg422ljr1leo1al42f920pa8", zoom=10,location=c(-73.75,45.4), pitch=35) 
    })
    
    
    
    
      
    
    
      
    
     
    
    
    observeEvent(input$myMap_view_change$zoom, {
    #print(rz$zoom)
      if( input$myMap_view_change$zoom >= 9 & input$myMap_view_change$zoom <= 12){rz$zoom <- 'IN'} else {
        if(  input$myMap_view_change$zoom > 12){rz$zoom <- 'ISO'} else {
          rz$zoom <- 'OUT'}}
    })
    
    output$zoom <- reactive({
      
      
      return(rz$zoom)
      
    })
    outputOptions(output, "zoom", suspendWhenHidden = FALSE)
    
   
    
    observeEvent({input$myMap_polygon_click},{
      js <- input$myMap_polygon_click
      lst <- jsonlite::fromJSON( js )
      print( lst )
      #print(input$myMap_polygon_click)
      
     
        
        
        temporary_here <- data_for_plot_r_bivar() 
        #print(temporary_here$fill)
        temporary_here[which(temporary_here$fill != lst$object$properties$fill_colour),5] <- 0
        temporary_here[which(temporary_here$fill == lst$object$properties$fill_colour),5] <- 4000
        # print("left_variable")
        #print(crs(data_for_plot_bivariate))
        if( rz$zoom == "ISO"){
          
          # mapdeck_update(map_id = "myMap") %>%  
          #  clear_polygon(layer_id = "polylayer")
          
          # print("left_variable")
          #print(crs(data_for_plot_bivariate))
          mapdeck_update(map_id = "myMap")  %>%  
            clear_polygon(layer_id = "polylayer") %>%
            add_polygon(data = isochrones,
                        fill_colour = "time",
                        fill_opacity = 0.5,
                        legend = TRUE,
                        layer_id = "isolayer",
                        update_view = FALSE)  
        } 
        
        if( rz$zoom == "IN"){
          
          # mapdeck_update(map_id = "myMap") %>%  
          #  clear_polygon(layer_id = "polylayer")
          
          # print("left_variable")
          #print(crs(data_for_plot_bivariate))
          mapdeck_update(map_id = "myMap")  %>%
            add_polygon(
              data = temporary_here
              , fill_colour = "fill"
              , fill_opacity = 0
              , elevation = "elevation"
              , update_view = FALSE
              , layer_id = "polylayer"
              , auto_highlight = TRUE
              , highlight_colour = '#FFFFFF90'
              , light_settings =  list(
                lightsPosition = c(-73.75,45, 5000, -74,45.5, 10000)
                , numberOfLights = 2
                , ambientRatio = 1
              ) 
            )  
        }
        if( rz$zoom == "OUT") {
          mapdeck_update(map_id = "myMap")  %>%  
            clear_polygon(layer_id = "polylayer") %>%  
            clear_polygon(layer_id = "isolayer")
        } 
    
    }) 
    
    
    
    
    
    observeEvent({rz$zoom
                 data_for_plot_r_bivar()
                 input$tabs},{
        #print(input$myMap_view_change$zoom)
        # print(rz$zoom )
       # print(head(data_for_plot_r_bivar()))   
                   
    
        if( rz$zoom == "ISO"){
          
         # mapdeck_update(map_id = "myMap") %>%  
          #  clear_polygon(layer_id = "polylayer")
          
          # print("left_variable")
          #print(crs(data_for_plot_bivariate))
          mapdeck_update(map_id = "myMap")  %>%  
            clear_polygon(layer_id = "polylayer") %>%
            add_polygon(data = isochrones,
                        fill_colour = "time",
                        fill_opacity = 0.5,
                        legend = TRUE,
                        layer_id = "isolayer",
                        update_view = FALSE)  
        } 
          
        if( rz$zoom == "IN"){
            
            # mapdeck_update(map_id = "myMap") %>%  
            #  clear_polygon(layer_id = "polylayer")
            
            # print("left_variable")
            #print(crs(data_for_plot_bivariate))
            mapdeck_update(map_id = "myMap")  %>%  
              clear_polygon(layer_id = "isolayer") %>%
              add_polygon(
                data = data_for_plot_r_bivar()
                , fill_colour = "fill"
                , fill_opacity = 0
                , elevation = "elevation"
                , update_view = FALSE
                , layer_id = "polylayer"
                , auto_highlight = TRUE
                , highlight_colour = '#FFFFFF90'
                , legend = FALSE
                , light_settings =  list(
                  lightsPosition = c(0,0, 5000)
                  , numberOfLights = 1
                  , ambientRatio = 1
                ) 
              )  
        }
          if( rz$zoom == "OUT") {
          mapdeck_update(map_id = "myMap")  %>%  
            clear_polygon(layer_id = "polylayer") %>%  
              clear_polygon(layer_id = "isolayer")
        }  
    
})
    
    
   
    
    
   # jqui_draggable("#backpic, #context_plot")
    
}

shinyApp(ui, server)