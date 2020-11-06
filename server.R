##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  output$homepic <- renderImage({
    filename <- normalizePath(file.path("www/Sus logo transparent.png"))
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
    
    data_for_plot_right <- 
      data_for_plot %>%
      dplyr::select(input$data_for_plot_right) %>% 
      set_names(c("right_variable",  "geometry"))
    
    p <- 
      ggplot(data_for_plot_right) +
      geom_sf(aes(fill = as.factor(right_variable)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(4:6)])) +
      theme_map()
    
    ggdraw() + 
      draw_image(dropshadow1, scale = 1.49, vjust = -0.003, hjust = -0.003) +
      draw_plot(p)
    
    }, bg = "transparent")
  
  
  ### Create the data frame to generate bivariate maps #########################
  
  data_for_plot_r_bivar <- reactive({
    
    data_for_plot_bi <- 
      data_for_plot %>%
      dplyr::select(ale_tranis_quant3, input$data_for_plot_right)
    
    if (length(colnames(data_for_plot_bi)) == 2) {
      data_for_plot_bi <- 
        cbind(data_for_plot_bi[,1], data_for_plot_bi)[,1:3]
    }
    
    colnames(data_for_plot_bi) <- c("left_variable", "right_variable",  "geometry")
    
    data_for_plot_bivariate <- 
      data_for_plot_bi %>%
      mutate(group = paste(as.numeric(left_variable), "-", 
                           as.numeric(right_variable))) %>%
      left_join(bivariate_color_scale, by = "group")
    
    data_for_plot_bivariate <- 
      cbind(data_for_plot_bivariate, 
            as.numeric(data_for_plot_bivariate$left_variable) * 
              as.numeric(data_for_plot_bivariate$right_variable))
    
    names(data_for_plot_bivariate)[5] <- 'elevation'
    
    data_for_plot_bivariate <- data_for_plot_bivariate[,-7]
    
    data_for_plot_bivariate$elevation <- 
      (data_for_plot_bivariate$elevation) ^ 2 * 50

    data_for_plot_r_bivar <- 
      data_for_plot_bivariate %>% 
      st_transform(4326) %>% 
      st_cast("MULTIPOLYGON")
  })
  
  
  
  
  ##############################################
  ## isochrones
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
  
  ### Call initial map
  output$myMap <- renderMapdeck({
    mapdeck(style = "mapbox://styles/ttuff/ckg422ljr1leo1al42f920pa8", zoom=10.1,location=c(-73.58,45.39), pitch=35) 
  })
  
  
  ### Set zoom breaks
  observeEvent(input$myMap_view_change$zoom, {
    if( input$myMap_view_change$zoom >= 9 & input$myMap_view_change$zoom <= 12){rz$zoom <- 'IN'} else {
      if(  input$myMap_view_change$zoom > 12){rz$zoom <- 'ISO'} else {
        rz$zoom <- 'OUT'}}
    })
  
  ### Second zoom reactive 
  output$zoom_ALP <- reactive({
    return(rz$zoom)
  })
  outputOptions(output, "zoom_ALP", suspendWhenHidden = FALSE)
  ## needs different formating
  
  
  ### Click polygon
  observeEvent({input$myMap_polygon_click},{
    js <- input$myMap_polygon_click
    lst <- jsonlite::fromJSON( js )
    print( lst )

    temporary_here <- data_for_plot_r_bivar() 
    temporary_here[which(temporary_here$fill != lst$object$properties$fill_colour),5] <- 0
    temporary_here[which(temporary_here$fill == lst$object$properties$fill_colour),5] <- 4000
   
    if( rz$zoom == "ISO"){
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
  
  
### Observe several triggers
  ### zoom, tag, or dataset change
  
  observeEvent({rz$zoom
    data_for_plot_r_bivar()
    input$tabs},{
   
      if (rz$zoom == "ISO") {
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
      geom_sf(data = census_analysis_quantile,
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
    
    data_for_plot_ped <- census_analysis_quantile %>%
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
    data_for_plot_bi <- census_analysis_quantile_WSG %>%
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
      mutate(prop_driving = round(census_analysis_quantile_WSG$prop_driving, 0),
             pop_density = log(round(census_analysis_quantile_WSG$`pop_density(sqkm)`, 0)),
             trip_scale = census_analysis_quantile_WSG$trip_scale,
             social_distancing = census_analysis_quantile_WSG$social_distancing_capacity_pop_perc_2m) %>% 
      left_join(bivariate_color_scale, by = "group") %>% 
      drop_na(right_variable) 
    
    bivariate_chloropleth  <- st_cast(data_for_plot_bivariate, "MULTIPOLYGON")
    
    if (input$variable_ped == 3) {
      bivariate_chloropleth <- bivariate_chloropleth %>% 
        filter(prop_driving >= input$slider_ped[1] & prop_driving <= input$slider_ped[2])
    } else if (input$variable_ped == 2) {bivariate_chloropleth <- bivariate_chloropleth %>% 
      filter(social_distancing >= input$slider_ped[1] & social_distancing <= input$slider_ped[2]) 
    } else if (input$variable_ped == 1) {bivariate_chloropleth <- bivariate_chloropleth %>% 
      filter(pop_density >= input$slider_ped[1] & pop_density <= input$slider_ped[2]) 
    } else {bivariate_chloropleth <- bivariate_chloropleth %>% 
      filter(trip_scale >= input$slider_ped[1] & trip_scale <= input$slider_ped[2])}
  })
  
  ## Bivariate dot density map (reactive value)
  
  # bivariate_dotdensity <- reactive({
  #   data_for_plot_bi_dot <- sample_points_for_app_WSG %>%
  #     dplyr::select(social_distancing_capacity_pop_perc_2m_quant3, input$data_for_plot_ped)
  #   if(length(colnames(data_for_plot_bi_dot)) == 2){data_for_plot_bi_dot <- cbind(data_for_plot_bi_dot[,1], data_for_plot_bi_dot)[,1:3]}
  #   #print(head(data_for_plot_bi_dot))
  #   colnames(data_for_plot_bi_dot) <- c("left_variable", "right_variable",  "geometry")
  #   data_for_plot_bi_dotdensity <- data_for_plot_bi_dot %>%
  #     mutate(
  #       group = paste(
  #         as.numeric(left_variable), "-",
  #         as.numeric(right_variable)
  #       )
  #     ) %>%
  #     left_join(bivariate_color_scale, by = "group") %>% 
  #     drop_na()
  #   
  #   bivariate_dotdensity  <- st_cast(data_for_plot_bi_dotdensity, "POINT")
  # })
  
  ## Univariate chloropleth map
  
  data_for_plot_uni <- reactive({
    
    data_for_plot_uni <- census_analysis_quantile_WSG %>%
      dplyr::select(social_distancing_capacity_pop_perc_2m_quant3)
    
    colnames(data_for_plot_uni) <- c("left_variable", "geometry")
    
    data_for_plot_uni <- data_for_plot_uni %>%
      mutate(
        group = paste(
          as.numeric(left_variable)
        )
      ) %>%
      left_join(color_scale_2, by = "group") %>% 
      mutate(prop_driving = round(census_analysis_quantile_WSG$prop_driving, 0),
             pop_density = log(round(census_analysis_quantile_WSG$`pop_density(sqkm)`, 0)),
             trip_scale = census_analysis_quantile_WSG$trip_scale,
             social_distancing = census_analysis_quantile_WSG$social_distancing_capacity_pop_perc_2m)
      
    
    if (input$variable_ped == 3) {
    data_for_plot_uni <- data_for_plot_uni %>% 
      filter(prop_driving >= input$slider_ped[1] & prop_driving <= input$slider_ped[2])
    } else if (input$variable_ped == 2) {data_for_plot_uni <- data_for_plot_uni %>% 
      filter(social_distancing >= input$slider_ped[1] & social_distancing <= input$slider_ped[2]) 
    } else if (input$variable_ped == 1) {data_for_plot_uni <- data_for_plot_uni %>% 
      filter(pop_density >= input$slider_ped[1] & pop_density <= input$slider_ped[2]) 
    } else {data_for_plot_uni <- data_for_plot_uni %>% 
      filter(trip_scale >= input$slider_ped[1] & trip_scale <= input$slider_ped[2])}
    })

  
  # Legend
  
  legend_uni_chloro <- legend_element(
    variables = c("Low capacity", "Medium capacity", "High capacity"),
    colours = c('#CABED0', '#BC7C8F', '#AE3A4E'),
    colour_type = "fill",
    variable_type = "category",
    title = "Pedestrian Capacity for Social Distancing (2 meters)"
  )
  legend_uni_chloro <- mapdeck_legend(legend_uni_chloro)
  
  ## Create both VAS plans
  
  # May plan
  
  may_vas_plan <- original_plan_disaggregated %>% 
    st_transform(4326)
  
  # Legend
  # legend_vas_1 <- legend_element(
  #   variables = c(""),
  #   colours = c('#fac402'),
  #   colour_type = "stroke",
  #   variable_type = "category",
  #   title = "May 2020 Plan"
  # )
  # legend_vas_1 <- mapdeck_legend(legend_vas_1)
  
  # July plan
  july_vas_plan <- revised_plan %>% 
    st_transform(4326)
  
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
    data_for_plot_uni()
    #bivariate_dotdensity()
    input$vas_1
    input$vas_2
    input$variable_ped
    input$knob_ped
    input$switch_biv
    input$tabs}, {
      #print(bivariate_chloropleth())
      if( rz_pedestrian$zoom == "IN"){
        if (input$switch_biv == TRUE) {
          mapdeck_update(map_id = "PedestrianMap")  %>%
            #clear_scatterplot(layer_id = "dot_density") %>%
            clear_polygon(layer_id = "univariate_layer") %>% 
            clear_path(layer_id = "july_plan") %>%
            clear_path(layer_id = "may_plan") %>% 
            add_polygon(
              data = bivariate_chloropleth()
              , na_colour = "#FFFFFF" 
              ,stroke_colour = "#FFFFFF"
              ,stroke_width = 5
              , fill_colour = "fill"
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
          
          if(input$vas_1 == FALSE & input$vas_2 == FALSE) {
            
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "july_plan") %>%
              clear_path(layer_id = "may_plan")
            
          }
          
          if (input$vas_1 == TRUE & input$vas_2 == FALSE) {
            
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "july_plan") %>%
              add_path(
                data = may_vas_plan
                , layer_id = "may_plan"
                , stroke_colour = "#fac402"
                , stroke_width = 40
                , stroke_opacity = 1
                , update_view = FALSE
              )
          }
          
          if (input$vas_1 == FALSE & input$vas_2 == TRUE) {
            
            mapdeck_update(map_id = "PedestrianMap")  %>%
              clear_path(layer_id = "may_plan") %>% 
              add_path(
                data = july_vas_plan
                , layer_id = "july_plan"
                , stroke_colour = "#feeaa1"
                , stroke_width = 40
                , stroke_opacity = 1
                , update_view = FALSE
              )
          }  
          
          if (input$vas_1 == TRUE & input$vas_2 == TRUE) {
            
            mapdeck_update(map_id = "PedestrianMap")  %>%
              add_path(
                data = may_vas_plan
                , layer_id = "may_plan"
                , stroke_colour = "#fac402"
                , stroke_width = 40
                , stroke_opacity = 1
                , update_view = FALSE
              ) %>% 
              add_path(
                data = july_vas_plan
                , layer_id = "july_plan"
                , stroke_colour = "#feeaa1"
                , stroke_width = 40
                , stroke_opacity = 1
                , update_view = FALSE
              )
          }  
          
        }
        
        if (input$switch_biv == FALSE) {
            mapdeck_update(map_id = "PedestrianMap")  %>%
              #clear_scatterplot(layer_id = "dot_density") %>%
              clear_polygon(layer_id = "chloropleth_layer") %>% 
              clear_path(layer_id = "july_plan") %>%
              clear_path(layer_id = "may_plan") %>% 
              add_polygon(
                data = data_for_plot_uni()
                , na_colour = "#FFFFFF" 
                ,stroke_colour = "#FFFFFF"
                ,stroke_width = 5
                ,fill_colour = "fill"
                , fill_opacity = 1
                , update_view = FALSE
                , layer_id = "univariate_layer"
                , auto_highlight = TRUE
                , highlight_colour = '#FFFFFF90'
                , legend = legend_uni_chloro
                , light_settings =  list(
                  lightsPosition = c(0,0, 5000)
                  , numberOfLights = 1
                  , ambientRatio = 1
                ) 
              )
            
            if(input$vas_1 == FALSE & input$vas_2 == FALSE) {
              
              mapdeck_update(map_id = "PedestrianMap")  %>%
                clear_path(layer_id = "july_plan") %>%
                clear_path(layer_id = "may_plan")
              
            }
            
            if (input$vas_1 == TRUE & input$vas_2 == FALSE) {
              
              mapdeck_update(map_id = "PedestrianMap")  %>%
                clear_path(layer_id = "july_plan") %>%
                add_path(
                  data = may_vas_plan
                  , layer_id = "may_plan"
                  , stroke_colour = "#fac402"
                  , stroke_width = 40
                  , stroke_opacity = 1
                  , update_view = FALSE
                )
            }
            
            if (input$vas_1 == FALSE & input$vas_2 == TRUE) {
              
              mapdeck_update(map_id = "PedestrianMap")  %>%
                clear_path(layer_id = "may_plan") %>% 
                add_path(
                  data = july_vas_plan
                  , layer_id = "july_plan"
                  , stroke_colour = "#feeaa1"
                  , stroke_width = 40
                  , stroke_opacity = 1
                  , update_view = FALSE
                )
            }  
            
            if (input$vas_1 == TRUE & input$vas_2 == TRUE) {
              
              mapdeck_update(map_id = "PedestrianMap")  %>%
                add_path(
                  data = may_vas_plan
                  , layer_id = "may_plan"
                  , stroke_colour = "#fac402"
                  , stroke_width = 40
                  , stroke_opacity = 1
                  , update_view = FALSE
                ) %>% 
                add_path(
                  data = july_vas_plan
                  , layer_id = "july_plan"
                  , stroke_colour = "#feeaa1"
                  , stroke_width = 40
                  , stroke_opacity = 1
                  , update_view = FALSE
                )
            } 
          
        }
        
      }
        
        
        #             add_polygon(
        #               data = bivariate_chloropleth()
        #               , na_colour = "#FFFFFF"
        #               ,stroke_colour = "#000000"
        #               ,stroke_width = 5
        #               ,fill_colour = "#FFFFFF"
        #               , fill_opacity = 0
        #               , update_view = FALSE
        #               , layer_id = "chloropleth_layer"
        #               , auto_highlight = TRUE
        #               , highlight_colour = '#FFFFFF90'
        #               , legend = FALSE
        #               , light_settings =  list(
        #                 lightsPosition = c(0,0, 5000)
        #                 , numberOfLights = 1
        #                 , ambientRatio = 1
        #               )
        #             ) %>%
        #             add_scatterplot(data = bivariate_dotdensity()
        #                             , fill_colour = "fill"
        #                             , radius = 20
        #                             , layer_id = "dot_density"
        #                             , na_colour = "#000000"
        #                             , radius_min_pixels = 2
        #                             , update_view = FALSE
        #                             , palette = "ped_color_palette")
        #         }
        
      
      if( rz_pedestrian$zoom == "OUT") {
        mapdeck_update(map_id = "PedestrianMap")  %>%  
          clear_polygon(layer_id = "chloropleth_layer") %>% 
          clear_polygon(layer_id = "univariate_layer") %>% 
          clear_path(layer_id = "may_plan") %>% 
          #clear_scatterplot(layer_id = "dot_density") %>% 
          clear_path(layer_id = "july_plan")
        
      }  
      
      if( rz_pedestrian$zoom == "FINAL") {
        mapdeck_update(map_id = "PedestrianMap")  %>%  
          clear_polygon(layer_id = "chloropleth_layer") %>% 
          clear_polygon(layer_id = "univariate_layer") %>% 
          clear_path(layer_id = "may_plan") %>% 
          #clear_scatterplot(layer_id = "dot_density") %>% 
          clear_path(layer_id = "july_plan")
      } 
    })
  
  observeEvent(input$variable_ped,{
    if(input$variable_ped == 3){
      updateSliderInput(session = session,
                      inputId = "slider_ped",
                      label = "Work commutes by car (%)",
                      0, 100,
                      value = c(0, 100),
                      step = 1)
      updateSliderInput(session = session,
                        inputId = "slider_ped",
                        value = c(0, 100),
                        step = 1
      )
    }
    
    else if (input$variable_ped == 2) {
      updateSliderInput(session = session,
                        inputId = "slider_ped",
                        label = "Capacity of local population to make trips on foot while maintaining 2 meters distance (%)",
                        0, 1000,
                        value = c(0, 1000),
                        step = 25)
      updateSliderInput(session = session,
                        inputId = "slider_ped",
                        value = c(0, 1000),
                        step = 25
      )
    }
    
    else if (input$variable_ped == 1) {
      updateSliderInput(session = session,
                      inputId = "slider_ped",
                      label = "Log of Population density / km2",
                      0, 12,
                      value = c(0, 12),
                      step = 1)
      updateSliderInput(session = session,
                      inputId = "slider_ped",
                      value = c(0, 12),
                      step = 1
      )
    }
    
    else {updateSliderInput(session = session,
                            inputId = "slider_ped",
                            label = "Pedestrian trips per sqm of walkable space index (0 = average)",
                            -1, 6.5,
                            value = c(-1, 6.5),
                            step = 0.5)
      updateSliderInput(session = session,
                        inputId = "slider_ped",
                        value = c(-1, 6.5),
                        step = 0.5
      )}
      
      })
  
  #####################
  ## MODE
 
  ########Output#######
  output$qzmyMap <- renderMapdeck({
    mapdeck(token = "pk.eyJ1Ijoiemhhb3FpYW8wMTIwIiwiYSI6ImNrYXBnbHB3dTFtbDIycWxvZ285cjNmcG0ifQ.fieGPt1pLEgHs1AI8NvjYg",
            style = "mapbox://styles/zhaoqiao0120/ckh1hkzwe02br19nvzt9bvxcg", zoom=10,location=c(-73.611,45.526))
  })
  
  observeEvent(input$qzmyMap_view_change$zoom, {
    if( input$qzmyMap_view_change$zoom > 10){qz$zoom_level <- 'OUT'} else {
      qz$zoom_level <- 'ISO'}}
  )
  
  #observeEvent(input$myMap_view_change$zoom, {
    #print(rz$zoom)
  #})
  
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
    })
  
  
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
    input$tabs
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
