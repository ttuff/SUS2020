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

js_ped <- "$(document).ready(function(){
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

rz_pedestrian <- reactiveValues(zoom = 'OUT')

rz <- reactiveValues(zoom = 'IN')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  dropshadow1 <- normalizePath(file.path("www/dropshadow1.png"))
  dropshadow2 <- normalizePath(file.path("www/dropshadow2.png"))
  
 # Load data for pedestrian realm 
  
  load(file = "data/census_analysis.Rdata")
  load(file = "data/census_circular.Rdata")
  load(file = "data/data_for_app.Rdata")
  load(file = "data/color_scale.Rdata")
  load(file = "data/bivariate_color_scale.Rdata")
  load(file = "data/sample_points_for_app_WSG.Rdata")
  load(file = "data/census_analysis_WSG.Rdata")
  load(file = "data/data_for_app_WSG.Rdata")
  load(file = "data/centroids.Rdata")
  
  
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

load(file = "www/data_for_plot.Rdata")

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

output$zoom_ALP <- reactive({
  
  
  return(rz$zoom)
  
})
outputOptions(output, "zoom_ALP", suspendWhenHidden = FALSE)



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


### Pedestrian zone - no cars ALLOWED

# Pedestrian social distancing capacity map
output$map_distancing_capacity <- renderPlot({
  
  colors <- color_scale$fill
  colors <- as.character(colors)
  
  p <- ggplot() +
    geom_sf(data = census_circular, fill = "transparent", color = "black", size = 0.05) +
    geom_sf(data = data_for_app,
            aes(
              fill = as.factor(social_distancing_capacity_pop_perc_2m_quant3)
            ),
            # use thin white stroke for municipalities
            color = "white",
            size = 0.03
    ) +
    scale_fill_manual(values=rev(colors[c(1:3)]))+
    theme_void() +
    theme(legend.position = "none")
  
  ggdraw() + 
    draw_image(dropshadow2, scale = 1.85, vjust = 0.01) +
    draw_plot(p)
})

# MapBox studio base map
output$PedestrianMap <- renderMapdeck({
  mapdeck(style = "mapbox://styles/skohn90/ckgjqwg1w00bv1bmorr5oad7q", token = 'pk.eyJ1Ijoic2tvaG45MCIsImEiOiJja2JpNGZjMnUwYm9hMnFwN3Q2bmV5c3prIn0.M-AJKxYD1ETFiBB6swQmJw',
          zoom=8,location=c(-73.75,45.5), pitch=35) 
})

# Choose your second variable
output$second_variable <- renderPlot({
  
  colors <- color_scale$fill
  colors <- as.character(colors)
  
  data_for_plot_ped <- data_for_app %>%
    dplyr::select(input$data_for_plot_ped)
  
  colnames(data_for_plot_ped) <- c("right_variable",  "geometry")
  
  p <- ggplot(data_for_plot_ped) +
    geom_sf(data = census_circular, fill = "transparent", color = "black", size = 0.05) +
    geom_sf(
      aes(
        fill = as.factor(right_variable)
      ),
      # use thin white stroke for municipalities
      color = "white",
      size = 0.03
    ) +
    scale_fill_manual(values=rev(colors[c(4:6)]))+
    theme_void() +
    theme(legend.position = "none")
  
  ggdraw() + 
    draw_image(dropshadow1, scale = 1.8, vjust = 0.01) +
    draw_plot(p)
}, bg="transparent")


## Bivariate chloropleth map

bivariate_chloropleth <- reactive({
  data_for_plot_bi <- data_for_app_WSG %>%
    dplyr::select(social_distancing_capacity_pop_perc_2m_quant3, input$data_for_plot_ped)
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
    left_join(bivariate_color_scale, by = "group") %>% 
    drop_na()
  
  bivariate_chloropleth  <- st_cast(data_for_plot_bivariate, "MULTIPOLYGON")
})


# Set zoom bins
observeEvent(input$PedestrianMap_view_change$zoom, {
  #print(rz_pedestrian$zoom)
  if( input$PedestrianMap_view_change$zoom >= 12 & input$PedestrianMap_view_change$zoom <= 14){rz_pedestrian$zoom <- 'IN'} else {
    rz_pedestrian$zoom <- 'OUT'}
})


# Send reactive zoom variable back to the UI
output$zoom <- reactive({
  return(rz_pedestrian$zoom)
})
outputOptions(output, "zoom", suspendWhenHidden = FALSE)

# Update map if there is a zoom, dataframe or tab change

observeEvent({rz_pedestrian$zoom
  bivariate_chloropleth()
  input$tabs}, {
    print(bivariate_chloropleth())
    if( rz_pedestrian$zoom == "IN"){
      mapdeck_update(map_id = "PedestrianMap")  %>%
        #clear_polygon(layer_id = "dot_density_layer") %>%
        add_polygon(
          data = bivariate_chloropleth()
          , na_colour = "#FFFFFF" 
          ,stroke_colour = "FFFFFF"
          ,stroke_width = 2
          ,fill_colour = "fill"
          , fill_opacity = 1
          , update_view = FALSE
          , layer_id = "chloropleth_layer"
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
    
    if( rz_pedestrian$zoom == "OUT") {
      mapdeck_update(map_id = "PedestrianMap")  %>%  
        clear_polygon(layer_id = "chloropleth_layer")
    }  
  })

})

