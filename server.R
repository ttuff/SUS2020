##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  ### Render images from files #################################################
  
  output$homepic <- renderImage({
    filename <- normalizePath(file.path("www/Sus logo transparent.png"))
    return(list(src = filename, contentType = "image/png",  width = 571,
                height = 551))
    }, deleteFile = FALSE)
  
  output$bivariate_legend <- renderImage({
    filename <- normalizePath(file.path("www/bivariate_legend_2.png"))
    return(list(src = filename, contentType = "image/png",  width = 200,
                height = 177))
  }, deleteFile = FALSE)
  
  output$bivariate_legend_ped <- renderImage({
    filename <- normalizePath(file.path("www/bivariate_legend_2.png"))
    return(list(src = filename, contentType = "image/png",  width = 200,
                height = 177))
  }, deleteFile = FALSE)
  
  output$Univariate_left_legend <- renderImage({
    filename <- normalizePath(file.path("www/Univariate_left.png"))
    return(list(src = filename, contentType = "image/png",  width = 200,
                height = 200))
  }, deleteFile = FALSE)
  
  output$Univariate_right_legend <- renderImage({
    filename <- normalizePath(file.path("www/Univariate_right.png"))
    return(list(src = filename, contentType = "image/png",  width = 200,
                height = 200))
  }, deleteFile = FALSE)
  
  output$exemplar_ped <- renderImage({
    filename <- normalizePath(file.path("www/Exemplar.png"))
    return(list(src = filename, contentType = "image/png",  width = 550,
                height = 600))
  }, deleteFile = FALSE)
  
  output$sidewalk_calculation <- renderImage({
    filename <- normalizePath(file.path("www/sidewalk_calc.png"))
    return(list(src = filename, contentType = "image/png",  width = 400,
                height = 400))
  }, deleteFile = FALSE)
  
  output$univariate_legend_ped <- renderImage({
    filename <- normalizePath(file.path("www/legend_social_distancing_cap.png"))
    return(list(src = filename, contentType = "image/png",  width = 180,
                height = 140))
  }, deleteFile = FALSE)
  
  output$sidewalk_legend_ped <- renderImage({
    filename <- normalizePath(file.path("www/legend_sidewalk.png"))
    return(list(src = filename, contentType = "image/png",  width = 250,
                height = 140))
  }, deleteFile = FALSE)
  
  
  ### Plot output calls for all 'left' plots ###################################
  
  # Active living potential
  output$active_map_left <- renderPlot({
    
    data_for_plot_left <- 
      data_bivar()
    
    p <-
      ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", 
              size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)]), na.value = "grey70") +
      theme_map() + 
      theme(legend.position = "none")
    
    ggdraw() + 
      draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
      draw_plot(p) +
      draw_image(uni_legend, scale = .45, vjust = 0.25, hjust = 0.25) 
    
    }, bg = "white")
  
  # Commuter mode shift
  output$commuter_map_left <- renderPlot({
    
    quant_car_share <- car_share %>% mutate(quant3 = ntile(car_share$Car_per, 3))
    
    p <- ggplot(quant_car_share) +
      geom_sf(aes(fill = as.factor(quant3)), color = "white", 
              size = 0.05) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_map()
    
    ggdraw() + 
      draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
      draw_plot(p, scale = .85) +
      draw_image(uni_legend, scale = .45, vjust = 0.25, hjust = 0.25) 
    
  })

  # Pedestrian social distancing capacity map
  output$pedestrian_map_left <- renderPlot({
    
    p <- 
      ggplot() +
      geom_sf(data = census_circular, fill = "transparent", color = "black", 
              size = 0.05) +
      geom_sf(data = census_analysis_quantile,
              aes(fill = as.factor(
                social_distancing_capacity_pop_perc_2m_quant3)),
              color = "white", size = 0.03) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_void() +
      theme(legend.position = "none")
    
    ggdraw() + 
      draw_image(dropshadow2, scale = 1.85, vjust = 0.01) +
      draw_plot(p) +
      draw_image(uni_legend, scale = .45, vjust = 0.3, hjust = 0.3)
    
    })
  
  
  ### Plot output calls for all 'right' plots ##################################
  
  # Active living potential
  output$active_map_right <- renderPlot({
    
    if (input$data_for_plot_right == " ") {
      
      p <- 
        data_bivar() %>% 
        ggplot() +
        geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
        theme_map()
      
      ggdraw() + 
        draw_image(dropshadow1, scale = 1.49, vjust = -0.003, hjust = -0.003) +
        draw_plot(p)
      
    } else {
      
      p <- 
        data_bivar() %>% 
        ggplot() +
        geom_sf(aes(fill = as.factor(right_variable)), color = "white", 
                size = 0.01) +
        scale_fill_manual(values = rev(colors[c(4:6)])) +
        theme_map()
      
      ggdraw() + 
        draw_image(dropshadow1, scale = 1.49, vjust = -0.003, hjust = -0.003) +
        draw_plot(p) +
        draw_image(uni_legend_right, scale = .5, vjust = 0.25, hjust = -0.25)
      
    }
    
    
  }, bg = "transparent")
  
  
  ### Active living potential ##################################################
  
  ## Create the data frame to generate bivariate maps --------------------------
  
  data_bivar <- reactive({
    
    if (rz$zoom == "IN") {
      data <- data_CT_large
    } else if (rz$zoom == "OUT") {
      data <- data_borough_large 
    } else if (rz$zoom == "ISO") {
      data <- data_DA_1_large
    } else if (rz$zoom == "ISO_2") {
      data <- data_DA_2_large
    }
    
    # Starting case for no selection
    if (input$data_for_plot_right == " ") {
      
      data <- 
        data %>% 
        select(ID, name, name_2, population, left_variable_full = ale_index,
               left_variable = ale_index_quant3, ale_class, width,
               group, fill, elevation, fill_opacity)
      
    } else {
      
      data <- 
        data %>%
        dplyr::select(
          ID, name, name_2, population, left_variable_full = ale_index, 
          left_variable = ale_index_quant3, ale_class,
          right_variable_full = input$data_for_plot_right, 
          right_variable = paste0(input$data_for_plot_right, "_quant3"), 
          width, group = paste0(input$data_for_plot_right, "_quant3_group"),
          fill = paste0(input$data_for_plot_right, "_quant3_fill"),
          elevation = paste0(input$data_for_plot_right, "_quant3_elevation"),
          fill_opacity = paste0(input$data_for_plot_right, 
                                "_quant3_fill_opacity"))
    }
    
    return(data)
  })
  
  ## Observe zoom and coalesce to four values ----------------------------------
  
  observeEvent(input$active_map_view_change$zoom, {
    
    rz$zoom <- case_when(
      input$active_map_view_change$zoom >= 10.5 && 
        input$active_map_view_change$zoom <= 12 ~ "IN",
      input$active_map_view_change$zoom > 12 &&
        input$active_map_view_change$zoom < 14 ~ "ISO",
      input$active_map_view_change$zoom >= 14 ~ "ISO_2",
      TRUE ~ "OUT")
    
  })
  
  
  ## Observe click status and produce reactive values --------------------------
  
  
  
  
  ## Render the map ------------------------------------------------------------
  
  output$active_map <- renderMapdeck({
    mapdeck(style = "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7",
            token = paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                           "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ"),
            zoom = 10.1, location = c(-73.58, 45.53), pitch = 0) 
    })
  
  
  ## Update link text ----------------------------------------------------------
  
  # More info
  output$more_info_status <- reactive({
    input$more_info %% 2 == 1
  })
  
  outputOptions(output, "more_info_status", suspendWhenHidden = FALSE)

  observeEvent(input$more_info, {
    
    print(data_bivar())
    
    if (input$more_info %% 2 == 1) txt <- "Hide" else txt <- "Learn more"
    updateActionButton(session, "more_info", label = txt)
    
  })
  
  # Hide compare status
  output$active_hide_compare_status <- reactive({
    input$active_hide_compare %% 2 == 0
  })
  
  outputOptions(output, "active_hide_compare_status", suspendWhenHidden = FALSE)

  observeEvent(input$active_hide_compare, {
    
    if (input$active_hide_compare %% 2 == 0) txt <- "Hide" else txt <- "Show"
    updateActionButton(session, "active_hide_compare", label = txt)
    
  })
  
  # Hide explore status
  output$active_hide_explore_status <- reactive({
    input$active_hide_explore %% 2 == 0
  })
  
  outputOptions(output, "active_hide_explore_status", suspendWhenHidden = FALSE)

  observeEvent(input$active_hide_explore, {
    
    if (input$active_hide_explore %% 2 == 0) txt <- "Hide" else txt <- "Show"
    updateActionButton(session, "active_hide_explore", label = txt)
    
  })
  
  # Hide DYK status
  output$active_hide_dyk_status <- reactive({
    input$active_hide_dyk %% 2 == 0
  })
  
  outputOptions(output, "active_hide_dyk_status", suspendWhenHidden = FALSE)
  
  observeEvent(input$active_hide_dyk, {
    
    if (input$active_hide_dyk %% 2 == 0) txt <- "Hide" else txt <- "Show"
    updateActionButton(session, "active_hide_dyk", label = txt)
    
  })
  
  
  ## Render the info table -----------------------------------------------------
  
  output$active_info <- renderUI({
    
    # Univariate case
    if (input$data_for_plot_right == " ") {
      
      scale_singular <- case_when(
        rz$zoom == "OUT" ~ "borough/city",
        rz$zoom == "IN" ~ "census tract",
        TRUE ~ "dissemination area"
      )
      
      scale_plural <- case_when(
        scale_singular == "borough/city" ~ "boroughs or cities",
        scale_singular == "census tract" ~ "census tracts",
        scale_singular == "dissemination area" ~ "dissemination areas"
      )
      
      vec <- 
        data_bivar() %>% 
        filter(!is.na(left_variable), !is.na(left_variable_full)) %>% 
        pull(left_variable_full)
      
      min_val <- round(min(vec), 2)
      max_val <- round(max(vec), 2)
      mean_val <- round(mean(vec), 2)
      median_val <- round(median(vec), 2)
      sd_val <- sd(vec)
      quant_low <- round(quantile(vec, c(1/3, 2/3))[1], 2)
      quant_high <- round(quantile(vec, c(1/3, 2/3))[2], 2)
      
      # Case for no poly selected
      if (!rz$poly_select) {
        
        HTML(
          glue("At the {scale_singular} scale, the CanALE index varies from ",
               "{min_val} to {max_val}, with an average value of {mean_val} ",
               "and a median value of {median_val}. ",
               "Two thirds of {scale_plural} have a score between {quant_low} ",
               "and {quant_high}."))  
        
      # Case for selected poly
      } else {
        
        dat <- data_bivar() %>% filter(ID == rz$click)
        
        place_name <- case_when(
          scale_singular == "borough/city" ~ 
            glue("{dat$name}"),
          scale_singular == "census tract" ~ 
            glue("Census tract {dat$name}"),
          scale_singular == "dissemination area" ~ 
            glue("Dissemination area {dat$name}")
        )
          
        place_heading <- 
          if_else(scale_singular == "borough/city",
                  glue("{dat$name_2} of {place_name}"),
                  glue("{place_name} ({dat$name_2})"))
        
        poly_value <- dat$left_variable_full
        
        quintile <- quantile(vec, c(0.2, 0.4, 0.6, 0.8))
        
        larger_smaller <- case_when(
          poly_value >= quintile[4] ~ "much larger than",
          poly_value >= quintile[3] ~ "larger than",
          poly_value >= quintile[2] ~ "almost the same as",
          poly_value >= quintile[1] ~ "smaller than",
          TRUE ~ "much smaller than"
        )
         
        poor_strong <- case_when(
          str_detect(larger_smaller, "larger") ~ "strong",
          str_detect(larger_smaller, "smaller") ~ "poor",
          TRUE ~ "moderate"
        )
        
        percentile <- 
          {length(vec[vec <= dat$left_variable_full]) / length(vec) * 100} %>% 
          round()
        
        HTML(glue("<strong>{place_heading}</strong>", 
                  
             "<p>{place_name} has a population of ",
             "{prettyNum(dat$population, ',')} and a CanALE index ",
             "score of {round(poly_value, 2)}, which is {larger_smaller} ",
             "the region-wide median of {median_val}.", 
             
             "<p>{place_name} has {poor_strong} potential for active ", 
             "living, with a CanALE index score higher than {percentile} ",
             "percent of {scale_plural} in the Montreal region."))
        
      }
      
      
    # Bivariate case
    } else {
      
      HTML(cor(data_bivar()$left_variable_full, 
               data_bivar()$right_variable_full))
  
    }
  })  
  
  ## Render the histogram/scatterplot ------------------------------------------
  
  output$bivariate_graph <- renderPlot({
    
    # Histogram for a single variable
    if (input$data_for_plot_right == " ") {
      
      if (!rz$poly_select) {
        
        data_bivar() %>%
          filter(!is.na(left_variable)) %>%
          ggplot(aes(left_variable_full)) +
          geom_histogram(aes(fill = fill), bins = 25) +
          scale_fill_manual(values = colors[c(1:3)],
                            na.translate = FALSE) +
          labs(x = "CanALE index", y = NULL) +
          theme_minimal() +
          theme(legend.position = "none",
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank())    
        
      } else {
        
        if ({data_bivar() %>% 
            filter(ID == rz$click) %>% 
            filter(!is.na(left_variable)) %>% 
            nrow()} == 0) {
          
          data_bivar() %>% 
            filter(!is.na(left_variable)) %>%
            ggplot(aes(left_variable_full)) +
            geom_histogram(bins = 25, fill = colors[3]) +
            labs(x = "CanALE index", y = NULL) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank())
          
        } else {
          
          data_bivar() %>%
            filter(!is.na(left_variable)) %>%
            ggplot(aes(left_variable_full)) +
            geom_histogram(aes(fill = round(left_variable_full) == 
                                 round(left_variable_full[ID == rz$click])), 
                           bins = 25) +
            scale_fill_manual(values = colors[c(3, 1)],
                              na.translate = FALSE) +
            labs(x = "CanALE index", y = NULL) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank())
          
        }
      }
      
    # Scatterplot for two variables
    } else {
      
      y_var_name <- as.character(input$data_for_plot_right)
      
      if (nrow(filter(data_bivar(), ID == rz$click)) != 1) {
        
        data_bivar() %>% 
          ggplot(aes(left_variable_full, right_variable_full)) +
          geom_smooth(se = FALSE, colour = "grey50") +
          geom_point(aes(colour = fill)) +
          labs(x = "CanALE index", y = y_var_name) +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else {
        
        data_bivar() %>% 
          ggplot(aes(left_variable_full, right_variable_full)) +
          geom_smooth(se = FALSE, colour = "grey50") +
          geom_point(aes(colour = fill)) +
          gghighlight(ID == rz$click, keep_scales = TRUE) +
          labs(x = "CanALE index", y = y_var_name) +
          theme_minimal() +
          theme(legend.position = "none")
        
      }
    }
  })
  
  
  ## Render the did-you-knows --------------------------------------------------
  
  output$did_you_know <- renderUI({
    
    did_you_know %>% 
      filter(right_variable == input$data_for_plot_right) %>% 
      slice_sample(n = 3) %>% 
      pull(text) %>% 
      paste("<li> ", ., collapse = "") %>% 
      paste0("<ul>", ., "</ul>") %>%
      HTML()
  })
  
  
  ## Clear click status --------------------------------------------------------
  
  observeEvent(input$active_clear_selection, {rz$poly_select <- FALSE})
  
  
  

  ## Update map in response to variable changes, zooming, or options -----------
  
  observeEvent(
    {
      data_bivar()
      input$tabs
      input$active_extrude
    },
    {
      if (!input$active_extrude) {
        
        mapdeck_update(map_id = "active_map")  %>%  
          clear_polygon(layer_id = "extrude") %>%
          add_polygon(
            data = data_bivar(), 
            stroke_width = "width",
            stroke_colour = "#FFFFFF",
            fill_colour = "fill_opacity", 
            update_view = FALSE,
            layer_id = "polylayer", 
            id = "ID",
            auto_highlight = TRUE, 
            highlight_colour = '#FFFFFF90', 
            legend = FALSE, 
            light_settings = list(
              lightsPosition = c(0,0, 5000), 
              numberOfLights = 1, 
              ambientRatio = 1))
        
      } else {
        mapdeck_update(map_id = "active_map")  %>%  
          clear_polygon(layer_id = "polylayer") %>%
          add_polygon(
            data = data_bivar(), 
            fill_colour = "fill", 
            elevation = "elevation", 
            update_view = FALSE, 
            layer_id = "extrude", 
            auto_highlight = TRUE, 
            highlight_colour = '#FFFFFF90', 
            legend = FALSE, 
            light_settings = list(
              lightsPosition = c(0,0, 5000), 
              numberOfLights = 1, 
              ambientRatio = 1)) #%>% 
        # mapdeck_view(zoom = input$active_map_view_change$zoom,
        #              location = c(input$active_map_view_change$longitude, 
        #                           input$active_map_view_change$latitude),
        #              bearing = input$active_map_view_change$bearing,
        #              pitch = 35)
        
      }
      
    })
  
  
  ## Observe polygon clicks and deliver output ---------------------------------
  
  observeEvent({input$active_map_polygon_click},{
    
    lst <- jsonlite::fromJSON(input$active_map_polygon_click)
    
    rz$click <- lst$object$properties$id
    
    rz$poly_select = TRUE
    
    # temporary_here <- data_bivar() 
    # #print(temporary_here$fill)
    # temporary_here[which(temporary_here$fill != lst$object$properties$fill_colour),5] <- 0
    # temporary_here[which(temporary_here$fill == lst$object$properties$fill_colour),5] <- 4000
    # # print("left_variable")
    # #print(crs(data_for_plot_bivariate))
    # 
    # if( rz$zoom == "ISO"){
    #   
    #   # mapdeck_update(map_id = "active_map") %>%  
    #   #  clear_polygon(layer_id = "polylayer")
    #   
    #   # print("left_variable")
    #   #print(crs(data_for_plot_bivariate))
    #   mapdeck_update(map_id = "active_map")  %>%  
    #     clear_polygon(layer_id = "polylayer") %>%
    #     add_polygon(data = isochrones,
    #                 fill_colour = "time",
    #                 fill_opacity = 0.5,
    #                 legend = TRUE,
    #                 layer_id = "isolayer",
    #                 update_view = FALSE)  
    # } 
    # 
    # if( rz$zoom == "IN"){
    #   
    #   # mapdeck_update(map_id = "active_map") %>%  
    #   #  clear_polygon(layer_id = "polylayer")
    #   
    #   # print("left_variable")
    #   #print(crs(data_for_plot_bivariate))
    #   mapdeck_update(map_id = "active_map")  %>%
    #     add_polygon(
    #       data = temporary_here
    #       , fill_colour = "fill"
    #       , fill_opacity = 0
    #       , elevation = "elevation"
    #       , update_view = FALSE
    #       , layer_id = "polylayer"
    #       , auto_highlight = TRUE
    #       , highlight_colour = '#FFFFFF90'
    #       , light_settings =  list(
    #         lightsPosition = c(-73.75,45, 5000, -74,45.5, 10000)
    #         , numberOfLights = 2
    #         , ambientRatio = 1
    #       ) 
    #     )  
    # }
    # if (rz$zoom == "OUT") {
    #   mapdeck_update(map_id = "active_map")  %>%  
    #     clear_polygon(layer_id = "polylayer") %>%  
    #     clear_polygon(layer_id = "isolayer")
    # } 
    
  })
  
  
  ## Output polygon select status --------------------------------------------
    
    output$active_poly_select <- reactive(rz$poly_select)
    
    outputOptions(output, "active_poly_select", suspendWhenHidden = FALSE)
    
    
    ## Clear polygon select on zoom change -------------------------------------
    
    observeEvent(rz$zoom, {rz$poly_select <- FALSE}, ignoreInit = TRUE)
    
    
    ## Flush status on tab change ----------------------------------------------
    
    observeEvent(input$tabs, {
      rz$poly_select <- FALSE
      rz$click <- NA
      }, ignoreInit = TRUE)
    
    
    ## Add highlight polygon on click ------------------------------------------

    observeEvent(
      {
        rz$poly_select
        rz$click
        }, 
      {
        if (rz$poly_select && !is.na(rz$click)) {
          
          print(rz$click)
          
          mapdeck_update(map_id = "active_map")  %>%
            add_polygon(
              data = {
                data_bivar() %>% 
                  filter(ID == rz$click)},
              stroke_width = "width",
              stroke_colour = "#000000",
              fill_colour = "fill",
              update_view = FALSE,
              layer_id = "poly_highlight",
              auto_highlight = TRUE,
              highlight_colour = '#FFFFFF90',
              legend = FALSE,
              light_settings = list(
                lightsPosition = c(0,0, 5000),
                numberOfLights = 1,
                ambientRatio = 1))
          
          } else {
            
            print("Removing selection")
            
            mapdeck_update(map_id = "active_map")  %>%
              clear_polygon(layer_id = "poly_highlight")
            
            }

      }, ignoreInit = TRUE)
    
  
  
  ### Pedestrian realm #########################################################
  
  ## Load MapBox Base Map  -------------------------------------------------
  output$PedestrianMap <- renderMapdeck({
    mapdeck(style = "mapbox://styles/skohn90/ckgjqwg1w00bv1bmorr5oad7q", 
            token = 'pk.eyJ1Ijoic2tvaG45MCIsImEiOiJja2JpNGZjMnUwYm9hMnFwN3Q2bmV5c3prIn0.M-AJKxYD1ETFiBB6swQmJw',
            zoom = 9,location = c(-73.75, 45), pitch = 35) 
    })
  
  ## Univariate chloropleth map + Legend  --------------------------------------

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

  # legend_uni_chloro <- legend_element(
  #   variables = c("0-1 m", "1-2 m", "2-4 m", "4-6 m", "6-10 m", "10-20 m"),
  #   colours = c('#feebe2', '#fcc5c0', '#fa9fb5', '#f768a1', '#c51b8a', '#7a0177'),
  #   colour_type = "stroke",
  #   variable_type = "gradient",
  #   title = "Sidewalk Width"
  # )
  # legend_uni_chloro <- mapdeck_legend(legend_uni_chloro)
  
  ## Bivariate chloropleth map -------------------------------------------------
  bivariate_chloropleth <- reactive({
    data_for_plot_bi <- census_analysis_quantile_WSG %>%
      dplyr::select(social_distancing_capacity_pop_perc_2m_quant3, 
                    input$data_for_plot_ped)
    if (length(colnames(data_for_plot_bi)) == 2){
      data_for_plot_bi <- cbind(data_for_plot_bi[,1], data_for_plot_bi)[,1:3]}
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
  
  ## Second variable plot -------------------------------------------------
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
      draw_plot(p) +
      draw_image(uni_legend_right, scale = .5, vjust = 0.25, hjust = -0.25)
    
  }, bg = "transparent")
  
  
  ## Create VAS plans  -------------------------------------------------
  
  # May plan
  may_vas_plan <- original_plan_disaggregated %>% 
    st_transform(4326)
  
  # July plan
  july_vas_plan <- revised_plan %>% 
    st_transform(4326)
  
  ## Set zoom bins  -------------------------------------------------
  observeEvent(input$PedestrianMap_view_change$zoom, {
    #print(rz_pedestrian$zoom)
    if (input$PedestrianMap_view_change$zoom >= 10.5 && 
        input$PedestrianMap_view_change$zoom <= 14) {
      rz_pedestrian$zoom <- 'IN'} else {
      if (input$PedestrianMap_view_change$zoom > 14) {
        rz_pedestrian$zoom <- 'FINAL'} else {
        rz_pedestrian$zoom <- 'OUT'}}
  })

  # Send reactive zoom variable back to the UI
  output$zoom <- reactive({
    return(rz_pedestrian$zoom)
  })
  outputOptions(output, "zoom", suspendWhenHidden = FALSE)
  
  ## Titles & and Link Text  -------------------------------------------------
  
  # Set title across zoom levels
  output$title_text_ped <- renderText({
    if( rz_pedestrian$zoom == "OUT"){
      paste0("Pedestrian Capacity for Social Distancing, Census Tracts")
    } else if (rz_pedestrian$zoom == "IN") {
      "Pedestrian Capacity for Social Distancing, Dissemination Area"  
    } else {"Explore Sidewalks and Parks"}
  })
  
  # Hide extra text
  output$more_info_ped_status <- reactive({
    input$more_info_ped %% 2 == 1
  })
  
  outputOptions(output, "more_info_ped_status", suspendWhenHidden = FALSE)
  
  observeEvent(input$more_info_ped, {

    if (input$more_info_ped %% 2 == 1) {
      txt <- "Hide"
    } else {
      txt <- "Learn more"
    }
    updateActionButton(session, "more_info_ped", label = txt)
    
  })
  
  # Hide explore status
  output$pedestrian_hide_explore_status <- reactive({
    input$pedestrian_hide_explore %% 2 == 0
  })
  
  outputOptions(output, "pedestrian_hide_explore_status", suspendWhenHidden = FALSE)
  
  observeEvent(input$pedestrian_hide_explore, {
    
    if (input$pedestrian_hide_explore %% 2 == 0) txt <- "Hide" else txt <- "Show"
    updateActionButton(session, "pedestrian_hide_explore", label = txt)
    
  })
  
  # Hide DYK status
  output$pedestrian_hide_dyk_status <- reactive({
    input$pedestrian_hide_dyk %% 2 == 0
  })
  
  outputOptions(output, "pedestrian_hide_dyk_status", suspendWhenHidden = FALSE)
  
  observeEvent(input$pedestrian_hide_dyk, {
    
    if (input$pedestrian_hide_dyk %% 2 == 0) txt <- "Hide" else txt <- "Show"
    updateActionButton(session, "pedestrian_hide_dyk", label = txt)
    
  })
  
  ## Update map if there is a zoom / dataframe / tab / input change  -----------
  
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
        
      }
      
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
                        label = paste0("Capacity of local population to make ",
                                       "trips on foot while maintaining 2 meters distance (%)"),
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
    mapdeck(token = 
              "pk.eyJ1Ijoiemhhb3FpYW8wMTIwIiwiYSI6ImNrYXBnbHB3dTFtbDIycWxvZ285cjNmcG0ifQ.fieGPt1pLEgHs1AI8NvjYg",
            style = "mapbox://styles/zhaoqiao0120/ckh1hkzwe02br19nvzt9bvxcg",
            zoom=10,location=c(-73.611,45.526))
  })
  
  observeEvent(input$qzmyMap_view_change$zoom, {
    if( input$qzmyMap_view_change$zoom > 10){qz$zoom_level <- 'OUT'} else {
      qz$zoom_level <- 'ISO'}}
  )
  
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
      # showNotification("A potentially cyclable trip:\n",
      #                  "A car trip where the cycling distance between its ",
      #                  "origin and destination is shorter than 4.4 kilometers",
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
