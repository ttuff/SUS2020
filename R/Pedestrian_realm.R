Pedestrian_realm_module_UI <- function(id, i18n ) {
  ns <- NS(id)
  tabItem(
    tags$head(tags$style(HTML('
          #title_bar_ped {border-width: 10px; border-color: rgb(255, 255, 255);}
          #input_control_right {border-width: 10px; 
          border-color: rgba(255,255,255,1);}
          #ped_legend_container {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}'))),
    
    mapdeckOutput(outputId = ns("PedestrianMap"), height = "1000px"),
    
   
    
    absolutePanel(
      id = ns("title_bar_ped"), class = "panel panel-default",
      draggable = FALSE, top = 70, left = 270, width = "40%",
      h3(textOutput(ns("title_text_ped"))),
      conditionalPanel(
        condition = "output.zoom == 'OUT'", ns = ns ,
        id = ns("plotContainer_ped"),
        p(title_text %>%
            filter(tab == "pedestrian_ct", type == "main") %>%
            pull(text))),
      conditionalPanel(
        condition = "output.zoom == 'IN'", ns = ns ,
        id = ns("plotContainer_ped"),
        p(title_text %>%
            filter(tab == "pedestrian_da", type == "main") %>%
            pull(text))),
      actionLink(ns("more_info_ped"), "Learn more"),
      conditionalPanel(
        condition = "output.more_info_ped_status == 1 && output.zoom == 'OUT'", ns = ns ,
        id = ns("plotContainer_ped"),
        HTML(title_text %>%
               filter(tab == "pedestrian_ct", type == "extra") %>%
               pull(text))),
      conditionalPanel(
        condition = "output.more_info_ped_status == 1 && output.zoom == 'IN'", ns = ns ,
        id = ns("plotContainer_ped"),
        p(title_text %>%
            filter(tab == "pedestrian_da", type == "extra") %>%
            pull(text)),
        imageOutput(ns("exemplar_ped"))),
      conditionalPanel(
        condition = "output.more_info_ped_status == 1 && output.zoom == 'FINAL'", ns = ns ,
        id = ns("plotContainer_ped"),
        p(title_text %>%
            filter(tab == "pedestrian_sidewalk", type == "extra") %>%
            pull(text)),
        imageOutput(ns("sidewalk_calculation")))),
    
    absolutePanel(
      id = ns("input_control_right"), style="z-index:501;", 
      class = "panel panel-default", top = 70, right = 50, width = 300,
      conditionalPanel(
        condition = "output.zoom == 'IN'", ns = ns ,
        id = ns("plotContainer_ped_control"),
        materialSwitch(inputId = ns("switch_biv"), 
                       label = h3(strong("Perform a Bivariate Analysis", 
                                         style = "color:#B2D235")), 
                       status = "primary", value = FALSE),
        conditionalPanel(
          condition = "input.switch_biv == true", ns = ns ,
          id = ns("plotContainer_ped_control"),
          selectInput(ns("data_for_plot_ped"), 
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
                   actionLink(inputId = ns("pedestrian_hide_second_variable"),
                              label = "Hide"))),
          conditionalPanel(
            condition = "output.pedestrian_hide_second_variable_status == 1", ns = ns ,
            plotOutput(ns("second_variable"), width = 250, height = 250))),
        hr(),
        fluidRow(
          column(width = 8,
                 h4("Montreal Covid-19 Expanded Active Transit Corridors")),
          column(width = 4, align = "right",
                 actionLink(inputId = ns("vas_hide_explore"),
                            label = "Hide"))),
        conditionalPanel(
          ## Adding impossible condition to turn off this box. restore to: "output.vas_hide_explore_status == 1"
          condition = "output.vas_hide_explore_status == 1", ns = ns ,
          materialSwitch(inputId = ns("vas_1"), 
                         label = "Original Plan (May 15, 2020)", 
                         status = "info", value = FALSE),
          materialSwitch(inputId = ns("vas_2"), 
                         label = "Revised Plan (July 25, 2020)", 
                         status = "info", value = FALSE)),
        hr(),
        selectInput(
          inputId = ns("variable_ped"),
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
           sliderInput(inputId = ns("slider_ped"), label = "", 0, 12, 
                       value = c(0, 12), step = 1))),
      hr(),
      fluidRow(
        column(width = 8,
               h4("Explore")),
        column(width = 4, align = "right",
               actionLink(inputId = ns("pedestrian_hide_explore"),
                          label = "Hide"))),
      conditionalPanel(
        condition = "output.pedestrian_hide_explore_status == 1", ns = ns ,
        htmlOutput(ns("pedestrian_info")),
        conditionalPanel(
          condition = "output.pedestrian_poly_selected == 1", ns = ns ,
          actionLink(inputId = ns("pedestrian_clear_selection"), 
                     label = "Clear selection")),
        plotOutput(ns("pedestrian_graph"), height = 150)),
      hr(),
      fluidRow(
        column(width = 8,
               h4("Did you know?")),
        column(width = 4, align = "right",
               actionLink(inputId = ns("pedestrian_hide_dyk"),
                          label = "Hide"))),
      conditionalPanel(
        condition = "output.pedestrian_hide_dyk_status == 1", ns = ns ,
        htmlOutput(ns("did_you_know_ped")))
    ),
    
    absolutePanel(
      id = ns("ped_legend_container"), class = "panel panel-default",
      style="z-index:500; border-color: #FFFFFF00; background-color: #FFFFFF00;", bottom = -200, left = 270, fixed = TRUE,
      conditionalPanel(
        condition = 
          "input.switch_biv == true && output.zoom == 'IN' && output.more_info_ped_status == 0", ns = ns ,
        id = ns("ped_legend"),
        imageOutput(ns("bivariate_legend_ped"))),
      conditionalPanel(
        condition = 
          "output.zoom == 'OUT' || output.zoom == 'IN' && input.switch_biv == false && output.more_info_ped_status == 0", ns = ns ,
        id = ns("ped_legend"),
        imageOutput(ns("univariate_legend_ped"))),
      conditionalPanel(condition = "output.zoom == 'FINAL' && output.more_info_ped_status == 0", ns = ns ,
                       id = ns("ped_legend"),
                       imageOutput(ns("sidewalk_legend_ped")))
    )
  
    
  )}


js_ped_1 <- "$(document).ready(function(){
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

Pedestrian_realm_module_server <- function(id) {
  moduleServer(id,function(input, output, session) {
    ns <- NS(id)
    
    output$bivariate_legend_ped <- renderImage({
      filename <- normalizePath(file.path("www/bivariate_legend_2.png"))
      return(list(src = filename, contentType = "image/png",  width = 200,
                  height = 177))
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
    
    
    did_you_know <- 
      read_csv("data/did_you_know.csv") 
     # mutate(right_variable = if_else(is.na(right_variable), " ", right_variable))
    
    variable_explanations <- 
      read_csv("data/variable_explanations.csv")
    
    census_analysis_quantile_WSG <- census_analysis_quantile %>% 
      st_transform(4326)
    
    dropshadow1 <- normalizePath(file.path("www/dropshadow1.png"))
    dropshadow2 <- normalizePath(file.path("www/dropshadow2.png"))
    
    uni_legend <- normalizePath(file.path("www/Univariate_left.png"))
    uni_legend_right <- normalizePath(file.path("www/Univariate_right.png"))
    
    
    rz_pedestrian <- reactiveValues(zoom = 'OUT',
                                    poly_selected = NA)
    
    
    ## Load MapBox Base Map  -------------------------------------------------
    output$PedestrianMap <- renderMapdeck({
      mapdeck(style = "mapbox://styles/skohn90/ckgjqwg1w00bv1bmorr5oad7q", 
              token = 'pk.eyJ1Ijoic2tvaG45MCIsImEiOiJja2JpNGZjMnUwYm9hMnFwN3Q2bmV5c3prIn0.M-AJKxYD1ETFiBB6swQmJw',
              zoom = 9.2,location = c(-73.65, 45.4), pitch = 35) 
    })
    
    ## Univariate chloropleth info CT Level   ------------------------------------
    
    census_analysis_ct_plot <- census_analysis_ct %>%
      dplyr::select(social_distancing_capacity_pop_perc_2m_quant3)
    
    colnames(census_analysis_ct_plot) <- c("left_variable", "geometry")
    
    census_analysis_ct_plot <- census_analysis_ct_plot %>%
      mutate(
        group = paste(
          as.numeric(left_variable)
        )
      ) %>%
      left_join(color_scale_2, by = "group") %>% 
      mutate(social_distancing = census_analysis_ct$social_distancing_capacity_pop_perc_2m,
             population = census_analysis_ct$population.x)
    
    ## Univariate chloropleth map + Legend  --------------------------------------
    
    data_for_plot_uni <- reactive({
      
      data_for_plot_uni <- census_analysis_quantile_WSG %>%
        dplyr::select(left_variable_full = social_distancing_capacity_pop_perc_2m,
                      left_variable = social_distancing_capacity_pop_perc_2m_quant3)
      
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
               social_distancing = census_analysis_quantile_WSG$social_distancing_capacity_pop_perc_2m,
               ID = census_analysis_quantile_WSG$GeoUID,
               population = census_analysis_quantile_WSG$population)
      
      
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
        dplyr::select(left_variable_full = social_distancing_capacity_pop_perc_2m, 
                      right_variable_full = input$data_for_plot_ped,
                      left_variable = social_distancing_capacity_pop_perc_2m_quant3,
                      right_variable = paste0(input$data_for_plot_ped, "_quant3"))
      
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
               social_distancing = census_analysis_quantile_WSG$social_distancing_capacity_pop_perc_2m,
               ID = census_analysis_quantile_WSG$GeoUID,
               population = census_analysis_quantile_WSG$population) %>% 
        left_join(bivariate_color_scale, by = "group") %>% 
        drop_na(right_variable) 
     # print("made it here 1")
      st_crs(data_for_plot_bivariate) <- 4326
      bivariate_chloropleth  <- st_cast(data_for_plot_bivariate, "MULTIPOLYGON")
     # print("made it here 2")
      
      if (input$variable_ped == 3) {
        bivariate_chloropleth <- bivariate_chloropleth %>% 
          filter(prop_driving >= input$slider_ped[1] & prop_driving <= input$slider_ped[2])
      } else if (input$variable_ped == 2) {bivariate_chloropleth <- bivariate_chloropleth %>% 
        filter(social_distancing >= input$slider_ped[1] & social_distancing <= input$slider_ped[2]) 
      } else if (input$variable_ped == 1) {bivariate_chloropleth <- bivariate_chloropleth %>% 
        filter(pop_density >= input$slider_ped[1] & pop_density <= input$slider_ped[2]) 
      } else {bivariate_chloropleth <- bivariate_chloropleth %>% 
        filter(trip_scale >= input$slider_ped[1] & trip_scale <= input$slider_ped[2])}
     # print("made it here 3")
      return(bivariate_chloropleth)
    })
    
    ## Second variable plot -------------------------------------------------
    output$second_variable <- renderPlot({
      
      colors <- color_scale$fill
      colors <- as.character(colors)
      
      data_for_plot_ped <- census_analysis_quantile %>%
        dplyr::select(right_variable = paste0(input$data_for_plot_ped, "_quant3"))
      
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
    
    ## Creating problem on server. See: https://github.com/r-spatial/sf/issues/1419
    ## also: https://stackoverflow.com/questions/61286108/error-in-cpl-transformx-crs-aoi-pipeline-reverse-ogrcreatecoordinatetrans 
    
    # # May plan
    st_crs(original_plan_disaggregated) = 32620
    may_vas_plan <- original_plan_disaggregated %>%
      st_transform(4326)
    # 
    # # July plan
    st_crs(revised_plan) = 32620
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
    
    ## Observe and change click status -------------------------------------------
    
    # Update poly_selected on click
    observeEvent(input$PedestrianMap_polygon_click, {
      lst_ped <- jsonlite::fromJSON(input$PedestrianMap_polygon_click)
      rz_pedestrian$poly_selected <- lst_ped$object$properties$id
    })
    
    # Clear click status if prompted
    observeEvent(input$pedestrian_clear_selection, {rz_pedestrian$poly_selected <- NA})
    
    # Output polygon select status
    output$pedestrian_poly_selected <- reactive({
      if (is.na(rz_pedestrian$poly_selected)) FALSE else TRUE})
    outputOptions(output, "pedestrian_poly_selected", suspendWhenHidden = FALSE)
    
    # Clear polygon select on zoom change
    observeEvent(rz_pedestrian$zoom, {rz_pedestrian$poly_selected <- NA}, ignoreInit = TRUE)
    
    # Clear polygon select on tab change
    observeEvent(input$tabs, {rz_pedestrian$poly_selected <- NA}, ignoreInit = TRUE)
    
    ## Titles & and Link Text  -------------------------------------------------
    
    # Set title across zoom levels
    output$title_text_ped <- renderText({
      if( rz_pedestrian$zoom == "OUT"){
        paste0("Pedestrian capacity for social distancing (census tracts)")
      } else if (rz_pedestrian$zoom == "IN") {
        "Pedestrian capacity for social distancing (dissemination areas)"  
      } else {"Explore sidewalks and parks"}
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
      updateActionButton(session, ns("more_info_ped"), label = txt)
      
    })
    
    # Hide explore status
    output$pedestrian_hide_explore_status <- reactive({
      input$pedestrian_hide_explore %% 2 == 0
    })
    
    outputOptions(output, "pedestrian_hide_explore_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$pedestrian_hide_explore, {
      
      if (input$pedestrian_hide_explore %% 2 == 0) txt <- "Hide" else txt <- "Show"
      updateActionButton(session, ns("pedestrian_hide_explore"), label = txt)
      
    })
    
    # Hide DYK status
    output$pedestrian_hide_dyk_status <- reactive({
      input$pedestrian_hide_dyk %% 2 == 0
    })
    
    outputOptions(output, "pedestrian_hide_dyk_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$pedestrian_hide_dyk, {
      
      if (input$pedestrian_hide_dyk %% 2 == 0) txt <- "Hide" else txt <- "Show"
      updateActionButton(session, ns("pedestrian_hide_dyk"), label = txt)
      
    })
    
    # Hide second variable map status
    output$pedestrian_hide_second_variable_status <- reactive({
      input$pedestrian_hide_second_variable %% 2 == 0
    })
    
    outputOptions(output, "pedestrian_hide_second_variable_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$pedestrian_hide_second_variable, {
      
      if (input$pedestrian_hide_second_variable %% 2 == 0) txt <- "Hide" else txt <- "Show"
      updateActionButton(session, ns("pedestrian_hide_second_variable"), label = txt)
      
    })
    
    # vas_hide_explore status
    output$vas_hide_explore_status <- reactive({
      input$vas_hide_explore %% 2 == 0
    })
    
    outputOptions(output, "vas_hide_explore_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$vas_hide_explore, {
      
      if (input$vas_hide_explore %% 2 == 0) txt <- "Hide" else txt <- "Show"
      updateActionButton(session, ns("vas_hide_explore"), label = txt)
      
    })
    
    ## Update map if there is a zoom / dataframe / tab / input change  -----------
    
    observeEvent({
      rz_pedestrian$zoom
      bivariate_chloropleth()
      data_for_plot_uni()
      #bivariate_dotdensity()
      input$vas_1
      input$vas_2
      input$variable_ped
      input$knob_ped
      input$switch_biv
      #input$tabs
      }, 
      {
        if( rz_pedestrian$zoom == "IN"){
          if (input$switch_biv == TRUE) {
            mapdeck_update(map_id = ns("PedestrianMap"))  %>%
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
                , id = "ID"
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
              
              mapdeck_update(map_id = ns("PedestrianMap"))  %>%
                clear_path(layer_id = "july_plan") %>%
                clear_path(layer_id = "may_plan")
              
            }
            
            if (input$vas_1 == TRUE & input$vas_2 == FALSE) {
              
              mapdeck_update(map_id = ns("PedestrianMap"))  %>%
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
              
              mapdeck_update(map_id = ns("PedestrianMap"))  %>%
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
              
              mapdeck_update(map_id = ns("PedestrianMap"))  %>%
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
            mapdeck_update(map_id = ns("PedestrianMap"))  %>%
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
                , id = "ID"
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
              
              mapdeck_update(map_id = ns("PedestrianMap"))  %>%
                clear_path(layer_id = "july_plan") %>%
                clear_path(layer_id = "may_plan")
              
            }
            
            if (input$vas_1 == TRUE & input$vas_2 == FALSE) {
              
              mapdeck_update(map_id = ns("PedestrianMap"))  %>%
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
              
              mapdeck_update(map_id = ns("PedestrianMap"))  %>%
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
              
              mapdeck_update(map_id = ns("PedestrianMap"))  %>%
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
          mapdeck_update(map_id = ns("PedestrianMap"))  %>%  
            clear_polygon(layer_id = "chloropleth_layer") %>% 
            clear_polygon(layer_id = "univariate_layer") %>% 
            clear_path(layer_id = "may_plan") %>% 
            #clear_scatterplot(layer_id = "dot_density") %>% 
            clear_path(layer_id = "july_plan")
          
        }  
        
        if( rz_pedestrian$zoom == "FINAL") {
          mapdeck_update(map_id = ns("PedestrianMap"))  %>%  
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
                          inputId = ns("slider_ped"),
                          label = "Work commutes by car (%)",
                          0, 100,
                          value = c(0, 100),
                          step = 1)
        updateSliderInput(session = session,
                          inputId = ns("slider_ped"),
                          value = c(0, 100),
                          step = 1
        )
      }
      
      else if (input$variable_ped == 2) {
        updateSliderInput(session = session,
                          inputId = ns("slider_ped"),
                          label = paste0("Capacity of local population to make ",
                                         "trips on foot while maintaining 2 meters distance (%)"),
                          0, 1000,
                          value = c(0, 1000),
                          step = 25)
        updateSliderInput(session = session,
                          inputId = ns("slider_ped"),
                          value = c(0, 1000),
                          step = 25
        )
      }
      
      else if (input$variable_ped == 1) {
        updateSliderInput(session = session,
                          inputId = ns("slider_ped"),
                          label = "Log of Population density / km2",
                          0, 12,
                          value = c(0, 12),
                          step = 1)
        updateSliderInput(session = session,
                          inputId = ns("slider_ped"),
                          value = c(0, 12),
                          step = 1
        )
      }
      
      else {updateSliderInput(session = session,
                              inputId = ns("slider_ped"),
                              label = "Pedestrian trips per sqm of walkable space index (0 = average)",
                              -1, 6.5,
                              value = c(-1, 6.5),
                              step = 0.5)
        updateSliderInput(session = session,
                          inputId = ns("slider_ped"),
                          value = c(-1, 6.5),
                          step = 0.5
        )}
      
    })
    
    ## Render the info table -----------------------------------------------------
    
    output$pedestrian_info <- renderUI({
      #print("ped render UI")
      census_analysis_ct_plot <- census_analysis_ct_plot %>% 
        filter(social_distancing != Inf,
               population >= 500)
      
      if (rz_pedestrian$zoom == "OUT") {
        
        min_ped_ct <- round(min(census_analysis_ct_plot$social_distancing), 2)
        max_ped_ct <- round(max(census_analysis_ct_plot$social_distancing), 2)
        mean_ped_ct <- round(mean(census_analysis_ct_plot$social_distancing), 2)
        median_ped_ct <- round(median(census_analysis_ct_plot$social_distancing), 2)
        sd_ped_ct <- sd(census_analysis_ct_plot$social_distancing)
        quant_low_ped_ct <- round(quantile(census_analysis_ct_plot$social_distancing, c(1/3, 2/3))[1], 2)
        quant_high_ped_ct <- round(quantile(census_analysis_ct_plot$social_distancing, c(1/3, 2/3))[2], 2)
        
        HTML(
          glue("At the census tract scale, after removing outliers with a ",
               "population below 500, the capacity for pedestrian social distancing varies from ",
               "{min_ped_ct}% to {max_ped_ct}%, with an average value of {mean_ped_ct}% ",
               "and a median value of {median_ped_ct}%. ",
               "Two thirds of census tracts have a score between {quant_low_ped_ct}% ",
               "and {quant_high_ped_ct}%. Out of the 532 census tracts, ",
               "227 of them have a capacity score below 100%, ",
               "while 85 of them have a capacity score below 50%. "))
      }
      
      else if (rz_pedestrian$zoom == "IN" & input$switch_biv == FALSE) {
        
        vec_ped_uni <- 
          data_for_plot_uni() %>% 
          filter(social_distancing != Inf,
                 population >= 100) %>% 
          pull(social_distancing)
        
        min_da_uni <- round(min(vec_ped_uni), 2)
        max_da_uni <- round(max(vec_ped_uni), 2)
        mean_da_uni <- round(mean(vec_ped_uni), 2)
        median_da_uni <- round(median(vec_ped_uni), 2)
        sd_da_uni <- sd(vec_ped_uni)
        quant_low_da_uni <- round(quantile(vec_ped_uni, c(1/3, 2/3))[1], 2)
        quant_high_da_uni <- round(quantile(vec_ped_uni, c(1/3, 2/3))[2], 2)
        
        # Case for no poly selected
        if (is.na(rz_pedestrian$poly_selected)) {
          
          HTML(
            glue("At the dissemination area scale, after removing outliers with a population below 100, the capacity for pedestrian social distancing varies from ",
                 "{min_da_uni}% to {max_da_uni}%, with an average value of {mean_da_uni}% ",
                 "and a median value of {median_da_uni}%. ",
                 "Two thirds of dissemination areas have a score between {quant_low_da_uni}% ",
                 "and {quant_high_da_uni}%."))  
          
          # Case for selected poly
        } else {
          
          dat_ped_uni <- data_for_plot_uni() %>% filter(ID == rz_pedestrian$poly_selected)
          
          poly_value_ped_uni <- dat_ped_uni$social_distancing
          
          quintile_ped_uni <- quantile(vec_ped_uni, c(0.2, 0.4, 0.6, 0.8))
          
          larger_smaller_ped_uni <- case_when(
            poly_value_ped_uni >= quintile_ped_uni[4] ~ "much larger than",
            poly_value_ped_uni >= quintile_ped_uni[3] ~ "larger than",
            poly_value_ped_uni >= quintile_ped_uni[2] ~ "almost the same as",
            poly_value_ped_uni >= quintile_ped_uni[1] ~ "smaller than",
            TRUE ~ "much smaller than"
          )
          
          poor_strong_ped_uni <- case_when(
            str_detect(larger_smaller_ped_uni, "larger") ~ "strong",
            str_detect(larger_smaller_ped_uni, "smaller") ~ "poor",
            TRUE ~ "moderate"
          )
          
          HTML(glue("The dissemination area {dat_ped_uni$ID} has a population of ",
                    "{prettyNum(dat_ped_uni$population, ',')} and a pedestrian social distancing capacity ",
                    "of {round(poly_value_ped_uni, 2)}%, which is {larger_smaller_ped_uni} ",
                    "the region-wide median of {median_da_uni}%.", 
                    
                    "<p>Dissemination area {dat_ped_uni$ID} offers a {poor_strong_ped_uni} capacity for its residents to practice social distancing in the local pedestrian realm."))
          
        }}
      
      else if (rz_pedestrian$zoom == "IN" & input$switch_biv == TRUE) {
        
        var_name_ped <- data_frame(code = c("agg_proximity_score", "net_median_income", "visible_minority_pop", "immigrants"),
                                   name = c("Walkable Access to Key Amenities", "Net Median Income", "Visible Minority Population",
                                            "Immigrant Population")) %>% 
          as_tibble() %>% 
          filter(code == input$data_for_plot_ped) %>%
          pull(name)
        
        var_code_ped <- data_frame(code = c("agg_proximity_score", "net_median_income", "visible_minority_pop", "immigrants"),
                                   name = c("Walkable Access to Key Amenities", "Net Median Income", "Visible Minority Population",
                                            "Immigrant Population")) %>% 
          as_tibble() %>% 
          filter(code == input$data_for_plot_ped) %>%
          pull(code)
        
        correlation_ped <-
          cor.test(bivariate_chloropleth()$left_variable_full,
                   bivariate_chloropleth()$right_variable_full, method = "spearman", exact = FALSE) 
        
        correlation_ped <- round(correlation_ped$estimate, 2) 
        
        pos_neg_ped <- if_else(correlation_ped > 0, "positive", "negative")
        
        strong_weak_ped <- case_when(
          abs(correlation_ped) > 0.6 ~ "strong",
          abs(correlation_ped) > 0.3 ~ "moderate",
          TRUE ~ "weak")
        
        higher_lower_ped <- if_else(pos_neg_ped == "positive", "higher", "lower")
        
        high_low_disclaimer_ped <- case_when(
          strong_weak_ped == "strong" ~ "with only a few exceptions",
          strong_weak_ped == "moderate" ~ "although with some exceptions",
          strong_weak_ped == "weak" ~ "although with many exceptions",
        )
        
        # Case for no poly selected
        if (is.na(rz_pedestrian$poly_selected)) {
          
          # If correlation is close to zero
          if (correlation_ped < 0.05 && correlation_ped > -0.05) {
            
            HTML(glue(
              "The capacity for pedestrian social distancing metric has effectively no correlation ",
              "({correlation_ped}) with {var_name_ped} at the dissemination area scale. ",
              "<p>This means that, at the dissemination area scale, ",
              "there is no relationship between the two variables."))
            
          } else {
            
            HTML(glue(
              "The capacity for pedestrian social distancing metric has a {strong_weak_ped} {pos_neg_ped} ",
              "correlation ({correlation_ped}) with '{tolower(var_name_ped)}' at the dissemination area scale. ",
              "<p>This means that, in general, dissemination areas with higher ",
              "capacities to allow for pedestrian social distancing tend to have {higher_lower_ped} ",
              "values for '{tolower(var_name_ped)}', {high_low_disclaimer_ped}."))
            
          }
        }}
      
      else if (rz_pedestrian$zoom == "FINAL") {
        min_sidewalk <- round(min(sidewalks_WSG$sidewalk_width), 2)
        max_sidewalk <- round(max(sidewalks_WSG$sidewalk_width), 2)
        mean_sidewalk <- round(mean(sidewalks_WSG$sidewalk_width), 2)
        median_sidewalk <- round(median(sidewalks_WSG$sidewalk_width), 2)
        sd_sidewalk <- sd(sidewalks_WSG$sidewalk_width)
        quant_low_sidewalk <- round(quantile(sidewalks_WSG$sidewalk_width, c(1/3, 2/3))[1], 2)
        quant_high_sidewalk <- round(quantile(sidewalks_WSG$sidewalk_width, c(1/3, 2/3))[2], 2)
        
        HTML(
          glue("Sidewalk width in Montreal varies from ",
               "{min_sidewalk} meters to {max_sidewalk} meters, ",
               "with an average value of {mean_sidewalk} meters ",
               "and a median value of {median_sidewalk} meters. ",
               "Two thirds of Montreal's sidewalks have widths ",
               "between {quant_low_sidewalk} meters and {quant_high_sidewalk} meters. "))
      }
      #print("end ped render UI")
    })
    
    ## Render the histogram/scatterplot ------------------------------------------
    
    output$pedestrian_graph <- renderPlot({
     # print("start ped graph")
      # Zoom out
      if (rz_pedestrian$zoom == "OUT") {
        
        census_analysis_ct_plot %>%
          ggplot(aes(social_distancing)) +
          geom_histogram(aes(fill = fill), bins = 25) +
          scale_fill_manual(values = colors[c(1:3)],
                            na.translate = FALSE) +
          scale_x_continuous(name = "Capacity for pedestrian social distancing",
                             limits = c(0, 500),
                             expand = c(0,0),
                             breaks = seq(0, 500, by = 100),
                             labels = c("0%", "100 %", "200 %", "300 %", "400 %", "500 %"),
                             oob = scales::squish) +
          labs(y = NULL) +
          theme_minimal() +
          theme(legend.position = "none",
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank())
      }
      
      else if (rz_pedestrian$zoom == "IN" & input$switch_biv == FALSE) {
        
        # If no poly is selected
        if (is.na(rz_pedestrian$poly_selected)) {
          
          data_for_plot_uni() %>%
            filter(!is.na(left_variable)) %>%
            ggplot(aes(left_variable_full)) +
            geom_histogram(aes(fill = fill), bins = 25) +
            scale_fill_manual(values = colors[c(1:3)],
                              na.translate = FALSE) +
            scale_x_continuous(name = "Capacity for pedestrian social distancing",
                               limits = c(0, 500),
                               expand = c(0,0),
                               breaks = seq(0, 500, by = 100),
                               labels = c("0%", "100 %", "200 %", "300 %", "400 %", "500 %"),
                               oob = scales::squish) +
            labs(y = NULL) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank())  
        }
        
        # If there is an active selection
        
        else {
          
          data_for_plot_uni() %>%
            filter(!is.na(left_variable)) %>%
            ggplot(aes(left_variable_full)) +
            geom_histogram(aes(
              fill = round(left_variable_full) == 
                round(left_variable_full[ID == rz_pedestrian$poly_selected])), 
              bins = 25) +
            scale_fill_manual(values = colors[c(3, 1)],
                              na.translate = FALSE) +
            scale_x_continuous(name = "Capacity for pedestrian social distancing",
                               limits = c(0, 500),
                               expand = c(0,0),
                               breaks = seq(0, 500, by = 100),
                               labels = c("0%", "100 %", "200 %", "300 %", "400 %", "500 %"),
                               oob = scales::squish) +
            labs(y = NULL) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank())
        }}
      
      else if (rz_pedestrian$zoom == "IN" & input$switch_biv == TRUE) {
        var_name_ped <- data_frame(code = c("agg_proximity_score", "net_median_income", "visible_minority_pop", "immigrants"),
                                   name = c("Walkable Access to Key Amenities", "Net Median Income", "Visible Minority Population",
                                            "Immigrant Population")) %>% 
          as_tibble() %>% 
          filter(code == input$data_for_plot_ped) %>%
          pull(name)
        
        var_code_ped <- data_frame(code = c("agg_proximity_score", "net_median_income", "visible_minority_pop", "immigrants"),
                                   name = c("Walkable Access to Key Amenities", "Net Median Income", "Visible Minority Population",
                                            "Immigrant Population")) %>% 
          as_tibble() %>% 
          filter(code == input$data_for_plot_ped) %>%
          pull(code)
        
        if (nrow(filter(bivariate_chloropleth(), ID == rz_pedestrian$poly_selected)) != 1) {
          
          bivariate_chloropleth() %>%
            drop_na() %>%
            ggplot(aes(left_variable_full, right_variable_full)) +
            geom_point(aes(colour = group)) +
            #geom_smooth(method = "lm", se = FALSE, colour = "grey50") +
            scale_colour_manual(values = deframe(bivariate_color_scale)) +
            scale_x_continuous(name = "Capacity for pedestrian social distancing",
                               limits = c(0, 500),
                               expand = c(0,0),
                               breaks = seq(0, 500, by = 100),
                               labels = c("0%", "100 %", "200 %", "300 %", "400 %", "500 %"),
                               oob = scales::squish) +
            labs(y = var_name_ped) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank())
          
        } else {
          
          bivariate_chloropleth() %>%
            drop_na() %>%
            ggplot(aes(left_variable_full, right_variable_full)) +
            geom_point(colour = bivariate_color_scale$fill[9]) +
            geom_smooth(method = "lm", se = FALSE, colour = "grey50") +
            geom_point(data = filter(bivariate_chloropleth(), ID == rz_pedestrian$poly_selected,
                                     !is.na(left_variable_full),
                                     !is.na(right_variable_full)),
                       colour = bivariate_color_scale$fill[1],
                       size = 3) +
            scale_x_continuous(name = "Capacity for pedestrian social distancing (%)",
                               limits = c(0, 200),
                               expand = c(0,0),
                               breaks = seq(0, 200, by = 25),
                               oob = scales::squish) +
            labs(y = var_name_ped) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank())
          
        }
      }
      
      else if (rz_pedestrian$zoom == "FINAL") {
        sidewalks_WSG %>%
          ggplot(aes(sidewalk_width)) +
          geom_histogram(aes(fill = rounded_sidewalk_width), bins = 25) +
          scale_fill_manual(values = c('#feebe2', '#fcc5c0', '#fa9fb5', '#f768a1', '#c51b8a', '#7a0177')) +
          labs(x = "Sidewalk width (meters)", y = NULL) +
          theme_minimal() +
          theme(legend.position = "none",
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank())
      }
     # print("end ped graph")
    })
    
    ## Render the did-you-knows --------------------------------------------------
    
    output$did_you_know_ped <- renderUI({
      if (rz_pedestrian$zoom == "OUT") {
        did_you_know %>% 
          filter(right_variable == "ct_ped") %>% 
          pull(text) %>% 
          paste("<li> ", ., collapse = "") %>% 
          paste0("<ul>", ., "</ul>") %>%
          HTML()
      }
      else if (rz_pedestrian$zoom == "IN" & input$switch_biv == FALSE) {
        did_you_know %>% 
          filter(right_variable == "da_ped") %>% 
          pull(text) %>% 
          paste("<li> ", ., collapse = "") %>% 
          paste0("<ul>", ., "</ul>") %>%
          HTML()
      }
      
      else if (rz_pedestrian$zoom == "IN" & input$switch_biv == TRUE) {
        did_you_know %>% 
          filter(right_variable == paste0(input$data_for_plot_ped, "_quant3")) %>% 
          pull(text) %>% 
          paste("<li> ", ., collapse = "") %>% 
          paste0("<ul>", ., "</ul>") %>%
          HTML()
      }
      # else {
      #   did_you_know %>%
      #     filter(right_variable == "sidewalk_ped") %>%
      #     pull(text) %>%
      #     paste("<li> ", ., collapse = "") %>%
      #     paste0("<ul>", ., "</ul>") %>%
      #     HTML()
      # }
      
    })
 })}