##### SUS SERVER SCRIPT ########################################################

# Load libraries, functions and variables ---------------------------------

source("libs.R")


# Load data ---------------------------------------------------------------

# Load bivariate census data
load(file = "data/data_for_plot.Rdata")

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
load(file = "data/original_VAS_plan.Rdata")
load(file = "data/revised_VAS_plan.Rdata")

dropshadow1 <- normalizePath(file.path("www/dropshadow1.png"))
dropshadow2 <- normalizePath(file.path("www/dropshadow2.png"))


# Other prep --------------------------------------------------------------

qz <- reactiveValues(zoom_level = 'NO')

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

# Set access token  
set_token('pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ')


### Main server function #######################################################

shinyServer(function(input, output, session) {
  
  #print(observe(tops()))
  #observeEvent(input$tabswitch, {
  #  print(input$tabs)
  #newtab <- switch(input$tabs, "bivariate" = "widgets", "widgets" = "bivariate"
  #)
  #updateTabItems(session, "tabs", newtab)
  #})
  
  # observe({
  #   print(input$input_control_left_position)
  #   #  print(output$top)
  # })
  
  output$homepic <- renderImage({
   
     # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path("www/Sus logo transparent.png"))
    
    # Return a list containing the filename and alt text
    return(list(src = filename, contentType = "image/png",  width = 571,
                height = 551))
    
    }, deleteFile = FALSE)
  
  

  ####################################################
  # plot output calls for all 'left' plots
  ####################################################
  
  output$context_plot <- renderPlot({
    
    data_for_plot_left <- 
      data_for_plot %>%
      dplyr::select(ale_tranis_quant3) %>% 
      set_names(c("left_variable",  "geometry"))
    
    p <- 
      ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_map() 
    
    ggdraw() + 
      draw_image(dropshadow1, scale = 1, vjust = -0.003, hjust = -0.003) +
      draw_plot(p)
    
    }, bg = "transparent")
  
  output$mapActiveLivingPotential <- renderPlot({
    
    data_for_plot_left <- 
      data_for_plot %>%
      dplyr::select(ale_tranis_quant3) %>% 
      set_names(c("left_variable",  "geometry"))
    
    p <-
      ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_map() + 
      theme(legend.position = "none")
    
    ggdraw() + 
      draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
      draw_plot(p)
    
    }, bg = "white")
  
  output$mapModeShift <- renderPlot({
    
    data_for_plot_left <- 
      data_for_plot %>%
      dplyr::select(Bicycle_proportion_quant3) %>% 
      set_names(c("left_variable",  "geometry"))
    
    ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_map()
    
    })
  
  output$mapBiodiversity <- renderPlot({
    
    data_for_plot_left <- 
      data_for_plot %>%
      dplyr::select(ale_tranis_quant3) %>% 
      set_names(c("left_variable",  "geometry"))
    
    ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_map()
    
    })
  
  output$mapGreenSpace <- renderPlot({
    
    data_for_plot_left <- 
      data_for_plot %>%
      dplyr::select(ale_tranis_quant3) %>% 
      set_names(c("left_variable",  "geometry"))
    
    ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_map()
    
    })
  
  output$mapShortTermRentals <- renderPlot({
    
    data_for_plot_left <- 
      data_for_plot %>%
      dplyr::select(TenantH_quant3) %>% 
      set_names(c("left_variable",  "geometry"))
    
    ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_map()
    
    })
  
  output$mapEnergy <- renderPlot({
    
    data_for_plot_left <- 
      data_for_plot %>%
      dplyr::select(ale_tranis_quant3) %>% 
      set_names(c("left_variable",  "geometry"))
    
    ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
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
    if (rz$zoom == "OUT") {
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
      
      
      if (rz$zoom == "ISO") {
        
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
  
  
  ### Pedestrian zone - no cars ALLOWED ###
  
  # Pedestrian social distancing capacity map
  output$map_distancing_capacity <- renderPlot({
    
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
  
  
  ## Bivariate chloropleth map (reactive value)
  
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
  
  ## Univariate chloropleth map (reactive value)
  univariate_chloropleth <- reactive({
    
    color_scale_2 <- 
      tibble(
        "3" = "#AE3A4E",
        "2" = "#BC7C8F",
        "1" = "#CABED0"
      ) %>%
      gather("group", "fill")
    
    data_for_plot_uni <- data_for_app_WSG %>%
      dplyr::select(social_distancing_capacity_pop_perc_2m_quant3)
    
    colnames(data_for_plot_uni) <- c("left_variable", "geometry")
    
    data_for_plot_uni <- data_for_plot_uni %>%
      mutate(
        group = paste(
          as.numeric(left_variable)
        )
      ) %>%
      left_join(color_scale_2, by = "group")
    
  })
  
  ## Create both VAS plans as reactive values
  
  # May plan
  may_vas_plan <- reactive({
    
    may_vas_plan <- original_plan_disaggregated %>% 
      st_transform(4326)
    
    may_vas_plan  <- st_cast(may_vas_plan, "MULTILINESTRING")
    
  })
  
  
  
  # July plan
  
  july_vas_plan <- reactive({
    
    july_vas_plan <- revised_plan %>% 
      st_transform(4326)
    
    july_vas_plan  <- st_cast( july_vas_plan, "MULTILINESTRING")
  })
  
  
  # Set zoom bins
  observeEvent(input$PedestrianMap_view_change$zoom, {
    #print(rz_pedestrian$zoom)
    if( input$PedestrianMap_view_change$zoom >= 10.5 & input$PedestrianMap_view_change$zoom <= 14){rz_pedestrian$zoom <- 'IN'} else {
      if(  input$PedestrianMap_view_change$zoom > 14){rz_pedestrian$zoom <- 'FINAL'} else {
        rz_pedestrian$zoom <- 'OUT'}}
  })
  
  
  # Send reactive zoom variable back to the UI
  output$zoom <- reactive({
    return(rz_pedestrian$zoom)
  })
  outputOptions(output, "zoom", suspendWhenHidden = FALSE)
  
  # Update map if there is a zoom / dataframe / tab / input change
  
  observeEvent({rz_pedestrian$zoom
    bivariate_chloropleth()
    input$vas_plan
    input$switch_biv
    input$tabs}, {
      #print(bivariate_chloropleth())
      if( rz_pedestrian$zoom == "IN"){
        if (input$vas_plan == 1) {
          if (input$switch_biv == TRUE) {
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "july_plan") %>%
              clear_polygon(layer_id = "univariate_layer") %>% 
              add_polygon(
                data = bivariate_chloropleth()
                , na_colour = "#FFFFFF" 
                ,stroke_colour = "#FFFFFF"
                ,stroke_width = 5
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
              ) %>%  
              add_path(
                data = may_vas_plan()
                , layer_id = "may_plan"
                , stroke_colour = "#FFD700"
                , stroke_width = 40
                , stroke_opacity = 1
                , update_view = FALSE
              )
          }
          if (input$switch_biv == FALSE) {
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "july_plan") %>%
              clear_polygon(layer_id = "chloropleth_layer") %>% 
              add_polygon(
                data = univariate_chloropleth()
                , na_colour = "#FFFFFF" 
                ,stroke_colour = "#FFFFFF"
                ,stroke_width = 5
                ,fill_colour = "fill"
                , fill_opacity = 1
                , update_view = FALSE
                , layer_id = "univariate_layer"
                , auto_highlight = TRUE
                , highlight_colour = '#FFFFFF90'
                , legend = FALSE
                , light_settings =  list(
                  lightsPosition = c(0,0, 5000)
                  , numberOfLights = 1
                  , ambientRatio = 1
                ) 
              ) %>%  
              add_path(
                data = may_vas_plan()
                , layer_id = "may_plan"
                , stroke_colour = "#FFD700"
                , stroke_width = 40
                , stroke_opacity = 1
                , update_view = FALSE
              )
          }
        }
        
        if (input$vas_plan == 2) { 
          if (input$switch_biv == TRUE) {
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "may_plan") %>%
              clear_polygon(layer_id = "univariate_layer") %>% 
              add_polygon(
                data = bivariate_chloropleth()
                , na_colour = "#FFFFFF" 
                ,stroke_colour = "#FFFFFF"
                ,stroke_width = 5
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
              ) %>% 
              add_path(
                data = july_vas_plan()
                , layer_id = "july_plan"
                , stroke_colour = "#FFD700"
                , stroke_width = 40
                , stroke_opacity = 1
                , update_view = FALSE
              )
          }
          
          if (input$switch_biv == FALSE) {
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "may_plan") %>%
              clear_polygon(layer_id = "chloropleth_layer") %>%
              add_polygon(
                data = univariate_chloropleth()
                , na_colour = "#FFFFFF" 
                ,stroke_colour = "#FFFFFF"
                ,stroke_width = 5
                ,fill_colour = "fill"
                , fill_opacity = 1
                , update_view = FALSE
                , layer_id = "univariate_layer"
                , auto_highlight = TRUE
                , highlight_colour = '#FFFFFF90'
                , legend = FALSE
                , light_settings =  list(
                  lightsPosition = c(0,0, 5000)
                  , numberOfLights = 1
                  , ambientRatio = 1
                ) 
              ) %>% 
              add_path(
                data = july_vas_plan()
                , layer_id = "july_plan"
                , stroke_colour = "#FFD700"
                , stroke_width = 40
                , stroke_opacity = 1
                , update_view = FALSE
              )
          }  
          
        }
        
        if (input$vas_plan == 0) { 
          
          if (input$switch_biv == TRUE) {
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "may_plan") %>%
              clear_path(layer_id = "july_plan") %>% 
              clear_polygon(layer_id = "univariate_layer") %>% 
              add_polygon(
                data = bivariate_chloropleth()
                , na_colour = "#FFFFFF" 
                ,stroke_colour = "#FFFFFF"
                ,stroke_width = 5
                ,fill_colour = "fill"
                , fill_opacity = 255
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
          
          if (input$switch_biv == FALSE) {
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "may_plan") %>%
              clear_path(layer_id = "july_plan") %>% 
              clear_polygon(layer_id = "chloropleth_layer") %>% 
              add_polygon(
                data = univariate_chloropleth()
                , na_colour = "#FFFFFF" 
                ,stroke_colour = "#FFFFFF"
                ,stroke_width = 5
                ,fill_colour = "fill"
                , fill_opacity = 1
                , update_view = FALSE
                , layer_id = "univariate_layer"
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
        }}
      
      if( rz_pedestrian$zoom == "OUT") {
        mapdeck_update(map_id = "PedestrianMap")  %>%  
          clear_polygon(layer_id = "chloropleth_layer") %>% 
          clear_polygon(layer_id = "univariate_layer") %>% 
          clear_path(layer_id = "may_plan") %>% 
          clear_path(layer_id = "july_plan")
      }  
      
      if( rz_pedestrian$zoom == "FINAL") {
        mapdeck_update(map_id = "PedestrianMap")  %>%  
          clear_polygon(layer_id = "chloropleth_layer") %>% 
          clear_polygon(layer_id = "univariate_layer") %>% 
          clear_path(layer_id = "may_plan") %>% 
          clear_path(layer_id = "july_plan")
      } 
    })
  cycling1 <- loadRData("data/car_1_finals.Rdata")
  cycling2 <- loadRData("data/car_3_finals.Rdata")
  cycling_network <- loadRData("data/reseau_cyclable.Rdata")
  car_share <- loadRData("data/Car_Share.Rdata")
  cycling_access <- loadRData("data/Cycling_Access.Rdata")
  trip_distance <- loadRData("data/Trip_Distance.Rdata")
  scenario1 <- data.frame(c("Criteria: Cycling Distance (km)","Potential Cyclable Trips (per day)", "VMT Savings (per day)"), c(4.4, 60460, 102862))
  scenario2 <- data.frame(c("Criteria: Cycling Distance (km)","Criteria: Elevation Gain (m)", "Criteria: Time Ratio","Potential Cyclable Trips (per day)", "VMT Savings (per day)"), c(4.4,45,2.4, 44205, 72992))
  ###########legend#####
  df_pal1 <- data.frame(
    color = c(1,2,3,4,5),
    color_value = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
    stringsAsFactors = F
  )
  
  cycling_access <- left_join(cycling_access, df_pal1, by = "color")
  
  legend_po1 <- legend_element(
    variables = c("0 - 0.87","0.88 - 1.91","1.92 - 3.08","3.09 - 4.61","4.62 - 16.8"),
    colours = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
    colour_type = "fill",
    variable_type = "category",
    title = "Access to Cycling Infrastructure (km/sq.km)"
  )
  legend1 <- mapdeck_legend(legend_po1)
  
  df_pal2 <- data.frame(
    color = c(1,2,3,4,5),
    color_value = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
    stringsAsFactors = F
  )
  
  car_share <- left_join(car_share, df_pal2, by = "color")
  
  legend_po2 <- legend_element(
    variables = c("4% - 21%","22% - 33%","34% - 47%","48% - 61%","62% - 91%"),
    colours = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
    colour_type = "fill",
    variable_type = "category",
    title = "Share of Car Trips by Origin (%)"
  )
  legend2 <- mapdeck_legend(legend_po2)
  
  df_pal3 <- data.frame(
    color = c(1,2,3,4,5),
    color_value = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
    stringsAsFactors = F
  )
  
  trip_distance <- left_join(trip_distance, df_pal3, by = "color")
  
  legend_po3 <- legend_element(
    variables = c("2.3 - 6.4","6.5 - 7.8","7.9 - 8.9","9.0 - 10.4","10.5 - 22.6"),
    colours = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
    colour_type = "fill",
    variable_type = "category",
    title = "Average Commuting Distance (km)"
  )
  legend3 <- mapdeck_legend(legend_po3)
  ########Output#######
  output$qzmyMap <- renderMapdeck({
    mapdeck(token = "pk.eyJ1Ijoiemhhb3FpYW8wMTIwIiwiYSI6ImNrYXBnbHB3dTFtbDIycWxvZ285cjNmcG0ifQ.fieGPt1pLEgHs1AI8NvjYg",
            style = "mapbox://styles/zhaoqiao0120/ckh1hkzwe02br19nvzt9bvxcg", zoom=10,location=c(-73.611,45.526))
  })
  observeEvent(input$qzmyMap_view_change$zoom, {
    if( input$qzmyMap_view_change$zoom > 10){qz$zoom_level <- 'OUT'} else {
      qz$zoom_level <- 'ISO'}}
  )
  
  observeEvent(input$myMap_view_change$zoom, {
    #print(rz$zoom)
    
  })
  
  
  output$zoom_level <- reactive({
    
    return(qz$zoom_level)
    
  })
  
  outputOptions(output, "zoom_level", suspendWhenHidden = FALSE)
  
  observeEvent(input$radio1, {
    if(input$radio1 == 1){
      updateSliderTextInput(session = session,
                            inputId = "slider1",
                            selected = 4.4)
      updateSliderTextInput(session = session,
                            inputId = "slider2",
                            selected = 55)
      updateSliderTextInput(session = session,
                            inputId = "slider3",
                            selected = 3)
      # showNotification("yayaya",
      #                  type = "message", duration = 3)
    }
    
    else if (input$radio1 == 2){
      updateSliderTextInput(session = session,
                            inputId = "slider1",
                            selected = 4.4)
      updateSliderTextInput(session = session,
                            inputId = "slider2",
                            selected = 45)
      updateSliderTextInput(session = session,
                            inputId = "slider3",
                            selected = 2.4)
      # showNotification("A potentially cyclable trip:\nA car trip where the cycling distance between its origin and destination is shorter than 4.4 kilometers",
      #                  type = "message", duration = 3)
    }
    
  }
  )
  observeEvent(input$switch2, {
    if(input$switch2 == TRUE){
      mapdeck_update(map_id = "qzmyMap")  %>%
        add_path(data = cycling_network,
                 stroke_colour = "#EA3546",
                 stroke_width = 150,
                 layer_id = "network",
                 update_view = FALSE)
    } else {
      mapdeck_update(map_id = "qzmyMap")  %>%
        clear_path(layer_id = "network")
    }
  })
  observeEvent(input$variable,{
    if(input$variable == 1){
      updateKnobInput(session = session,
                      inputId = "knob1",
                      label = "Access to Cycling Infrastructure (km/sq.km):",
                      options = list(
                        step = 0.5,
                        min = 4,
                        max = 17,
                        displayPrevious = TRUE,
                        lineCap = "round",
                        fgColor = "#B2D235",
                        inputColor = "#B2D235"))
      updateKnobInput(session = session,
                      inputId = "knob1",
                      value = 17
      )
    } else if(input$variable == 2){
      updateKnobInput(session = session,
                      inputId = "knob1",
                      label = "Car Share by Origin Census Tract (%):",
                      options = list(
                        step = 1,
                        max = 91,
                        min = 4,
                        lineCap = "round",
                        fgColor = "#1983B0",
                        inputColor = "#1983B0"
                      ))
      updateKnobInput(session = session,
                      inputId = "knob1",
                      value = 91
      )
    } else {
      updateKnobInput(session = session,
                      inputId = "knob1",
                      label = "Average Commuting Distance (km):",
                      options = list(
                        step = 0.5,
                        max = 23.0,
                        min = 3.5,
                        lineCap = "round",
                        fgColor = "#C56F34",
                        inputColor = "#C56F34"
                      )
      )
      updateKnobInput(session = session,
                      inputId = "knob1",
                      value = 23.0
      )
    }
  })
  
  observe({
    
    if( qz$zoom_level == "ISO"){
      updateMaterialSwitch(session = session,
                           inputId = "switch2",
                           value = FALSE)
      if (input$variable == 1) {
        cycling_access_select <- cycling_access[which(cycling_access$cycling_ac <= input$knob1),]
        mapdeck_update(map_id = "qzmyMap")  %>%
          #clear_polygon(layer_id = "choropleth")%>%
          clear_path(layer_id = "cyclable") %>%
          add_polygon(data = cycling_access_select,
                      fill_opacity = 150,
                      fill_colour = "color_value",
                      stroke_colour = "#868683",
                      stroke_width = 100,
                      layer_id = "choropleth",
                      legend = legend1,
                      highlight_colour  =  "#AAFFFFFF",
                      auto_highlight = TRUE,
                      update_view = FALSE)
      } else if (input$variable == 2) {
        car_share_select <- car_share[which(car_share$Car_per <= input$knob1),]
        mapdeck_update(map_id = "qzmyMap")  %>%
          #clear_polygon(layer_id = "choropleth")%>%
          clear_path(layer_id = "cyclable") %>%
          add_polygon(data = car_share_select,
                      fill_opacity = 150,
                      fill_colour = "color_value",#car_share
                      stroke_colour = "#CCD1D1",
                      stroke_width = 100,
                      layer_id = "choropleth",
                      legend = legend2,
                      highlight_colour  =  "#AAFFFFFF",
                      auto_highlight = TRUE,
                      update_view = FALSE)
      } else {
        trip_distance_select <- trip_distance[which(trip_distance$avg_dist <= input$knob1),]
        mapdeck_update(map_id = "qzmyMap")  %>%
          #clear_polygon(layer_id = "choropleth")%>%
          clear_path(layer_id = "cyclable") %>%
          add_polygon(data = trip_distance_select,
                      fill_opacity = 150,
                      fill_colour = "color_value",#car_share
                      stroke_colour = "#CCD1D1",
                      stroke_width = 100,
                      layer_id = "choropleth",
                      legend = legend3,
                      highlight_colour  =  "#AAFFFFFF",
                      auto_highlight = TRUE,
                      update_view = FALSE)
      }
      
      
    }
    if(qz$zoom_level == "OUT") {
      
      # updateMaterialSwitch(session = session,
      #                      inputId = "switch2",
      #                      value = TRUE)
      if(input$radio1 == 1){
        mapdeck_update(map_id = "qzmyMap")  %>%
          clear_polygon(layer_id = "choropleth") %>%
          add_path(data = cycling1,
                   stroke_width  = "total_car",
                   stroke_colour = "#0061FF80",
                   layer_id = "cyclable",
                   update_view = FALSE)
        
        output$table <- renderDT({
          DT::datatable(scenario1,
                        rownames = FALSE, colnames = c("",""), filter = "none",
                        style = "bootstrap",
                        options = list(
                          dom = 'b', ordering = FALSE
                        )
          )
        })
      } else if (input$radio1 == 2){
        mapdeck_update(map_id = "qzmyMap")  %>%
          clear_polygon(layer_id = "choropleth") %>%
          add_path(data = cycling2,
                   stroke_width  = "total_car",
                   stroke_colour = "#722AEE80",
                   layer_id = "cyclable",
                   update_view = FALSE)
        output$table <- renderDT({
          DT::datatable(scenario2,
                        rownames = FALSE, colnames = c("",""), filter = "none",
                        style = "bootstrap",
                        options = list(
                          dom = 'b', ordering = FALSE
                        )
          )
        })
      } else {
        mapdeck_update(map_id = "qzmyMap")  %>%
          clear_polygon(layer_id = "choropleth") %>%
          clear_path(layer_id = "cyclable")
      }
    }
  })
  
})
