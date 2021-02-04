### CANALE MODULE ##############################################################

# Data and helper functions -----------------------------------------------

# Load bivariate census data
qs::qload("data/data_canale.qsm")

# Initialize reactive values
rv_canale <- reactiveValues(zoom = "OUT", poly_selected = NA)
data_canale <- reactive(
  data_canale_borough %>%
    dplyr::select(ID, name, name_2, population, left_variable_full = ale_index,
                  left_variable = ale_index_quant3, ale_class, width, group, 
                  fill, elevation, fill_opacity))

# Dropdown menu
var_right_list <- 
  list("----" = " ", 
       "Housing" = list("Tenant-occupied (%)" = "tenant_prop",
                        "Average rent" = "avg_rent",
                        "Average property value" = "avg_property_value",
                        "Unaffordable housing (%)" = "unaffordable_prop",
                        "Unsuitable housing (%)" = "unsuitable_prop"),
       "Income" = list("Median household income" = "median_income",
                       "Income under $50k (%)" = "income_50_prop",
                       "Income between $50k-$100k (%)" = "income_100_prop",
                       "Income above $100k (%)" = "income_high_prop"),
       "Immigration" = list("Immigrants (%)" =  "immigrant_prop",
                            "New immigrants (%)" = "immigrant_new_prop"),
       "Transportation" = list("Drive to work (%)" = "car_prop",
                               "Walk or cycle to work (%)" = "walk_or_bike_prop",
                               "Public transit to work (%)" = "transit_prop",
                               "15 minutes to work (%)" = "time_15_prop",
                               "15-30 minutes to work (%)" = "time_30_prop",
                               "30-45 minutes to work (%)" = "time_45_prop",
                               "45-60 minutes to work (%)" = "time_60_prop"))


# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  
  tabItem(
    tabName = "canale",
    
    # Style tags
    module_style,

    # Main map
    mapdeckOutput(NS(id, "map"), height = "91vh"),
    
    # Title bar
    title_UI(NS(id, "title")),
    
    # Right panel
    absolutePanel(
      id = NS(id, "right_panel"), style =
        "z-index:500; max-height: 88vh; overflow-y: auto; overflow-x:hidden; padding: 5px;",
      class = "panel panel-default", top = 70, right = 20, width = 300,
      
      # 3D switch
      materialSwitch(
        inputId = NS(id, "extrude"),
        label = i18n$t("View in 3D"),
        status = "danger",
        value = FALSE),
      
      hr(),

      # Compare panel
      fluidRow(
        column(width = 7, h4(i18n$t("Compare"))),
        column(width = 5, align = "right", 
               actionLink(inputId = NS(id, "hide_compare"),
                          label = i18n$t("Hide")))),
      
      conditionalPanel(
        condition = "output.hide_compare_status == 1", ns = NS(id),
        selectInput(NS(id, "var_right"),
                    label = NULL, choices = var_right_list),
        plotOutput(NS(id, "map_right"), height = 200)),
      
      conditionalPanel(
        condition = "input.extrude == 0", ns = NS(id), hr(),

        # Explore panel
        fluidRow(
          column(width = 7, h4(i18n$t("Explore"))),
          column(width = 5, align = "right",
                 actionLink(inputId = NS(id, "hide_explore"), 
                            label = i18n$t("Hide")))),
        
        conditionalPanel(
          condition = "output.hide_explore_status == 1", ns = NS(id),
          htmlOutput(NS(id, "info")),
          conditionalPanel(
            condition = "output.poly_selected == 1", ns = NS(id),
            actionLink(inputId = NS(id, "clear_selection"),
                       label = "Clear selection")),
          explore_graph_UI("canale")),
        
        hr(),

    #     # DYK panel
    #     fluidRow(
    #       column(
    #         width = 8,
    #         h4(i18n$t("Did you know?"))
    #       ),
    #       column(
    #         width = 4, align = "right",
    #         actionLink(
    #           inputId = NS(id, "hide_dyk"),
    #           label = i18n$t("Hide")
    #         )
    #       )
    #     ),
    #     conditionalPanel(
    #       condition = "output.hide_dyk_status == 1",
    #       htmlOutput(NS(id, "did_you_know"))
    #     )
      )
    ),
    # 
    # Floating legend
    absolutePanel(
      id = NS(id, "legend_container"), class = "panel panel-default",
      style = "z-index:500; background-color:rgba(0, 0, 0, 0); color: rgba(0, 0, 0, 0);", 
      bottom = 20, left = 260, fixed = TRUE,
      conditionalPanel(
        condition = 'input.var_right != " "', ns = NS(id),
        id = NS(id, "legend"),
        img(src = "bivariate_legend_2.png", width = 200, height = 177))
      )
  )
}


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Data
    data_canale <- reactive({
      
      if (input$extrude) {
       data <- data_canale_DA_1
      } else data <- switch(rv_canale$zoom, "OUT" = data_canale_borough, 
                            "IN" = data_canale_CT, "ISO" = data_canale_DA_1, 
                            "ISO_2" = data_canale_DA_2)
      
      if (input$var_right == " ") {
        data <-
          data %>%
          dplyr::select(ID, name, name_2, population,
                        left_variable_full = ale_index,
                        left_variable = ale_index_quant3, ale_class, width,
                        group, fill, elevation, fill_opacity)
        
        } else {
          data <-
            data %>%
            dplyr::select(
              ID, name, name_2, population,
              left_variable_full = ale_index, left_variable = ale_index_quant3, 
              ale_class, right_variable_full = input$var_right, 
              right_variable = paste0(input$var_right, "_quant3"), 
              width, group = paste0(input$var_right, "_quant3_group"),
              fill = paste0(input$var_right, "_quant3_fill"),
              elevation = paste0(input$var_right, "_quant3_elevation"),
              fill_opacity = paste0(input$var_right, 
                                    "_quant3_fill_opacity"))
          }
      
      return(data)
    })
    
    # Title bar
    title_server("title", "canale")
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      
      rv_canale$zoom <- case_when(
        input$map_view_change$zoom >= 10.5 && input$map_view_change$zoom <= 12 ~ "IN",
        input$map_view_change$zoom > 12 && input$map_view_change$zoom < 14 ~ "ISO",
        input$map_view_change$zoom >= 14 ~ "ISO_2",
        TRUE ~ "OUT")
      
      })
    
    # Translate drop-down list
    observe({
      updateSelectInput(
        session = session,
        inputId = "var_right",
        choices = sus_translate(var_right_list)
      )
    })

    # 
    # # did_you_know <-
    # #   read_csv("data/did_you_know.csv") %>%
    # #   mutate(right_variable = if_else(is.na(right_variable), " ", right_variable))
    # #
    # # variable_explanations <-
    # #   read_csv("data/variable_explanations.csv")
    # #
    # 
    
    ## Observe and change click status -------------------------------------------

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      rv_canale$poly_selected <- lst$object$properties$id
      })

    # Clear click status if prompted
    observeEvent(input$clear_selection, {rv_canale$poly_selected <- NA})

    # Output polygon select status
    output$poly_selected <- reactive({
      if (is.na(rv_canale$poly_selected)) FALSE else TRUE
      })
    outputOptions(output, "poly_selected", suspendWhenHidden = FALSE)

    # Clear polygon select on zoom change
    observeEvent(rv_canale$zoom, {rv_canale$poly_selected <- NA},
                 ignoreInit = TRUE)

    # Clear polygon select on tab change
    observeEvent(input$tabs, {rv_canale$poly_selected <- NA}, ignoreInit = TRUE)

    # Observe and react to change in extrude status
    observeEvent(input$extrude, {rv_canale$poly_selected <- NA})


    ## Render the map ------------------------------------------------------------
    
    output$map <- renderMapdeck({
      mapdeck(
        style = "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7",
        token = paste0(
          "pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
          "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ"),
        zoom = 10.1, location = c(-73.58, 45.53), pitch = 0) %>% 
        add_polygon(
          data = data_canale(),
          stroke_width = "width", stroke_colour = "#FFFFFF",
          fill_colour = "fill_opacity", update_view = FALSE,
          layer_id = "polylayer", id = "ID", auto_highlight = TRUE,
          highlight_colour = "#FFFFFF90", legend = FALSE)
      })
    
    
    # ## Render the info table -----------------------------------------------------
    # 
    # output$info <- renderUI({
    #   scale_singular <- case_when(
    #     rv_canale$zoom == "OUT" ~ sus_translate("borough/city"),
    #     rv_canale$zoom == "IN" ~ sus_translate("census tract"),
    #     TRUE ~ sus_translate("dissemination area")
    #   )
    # 
    #   scale_plural <- case_when(
    #     scale_singular == sus_translate("borough/city") ~ sus_translate("boroughs or cities"),
    #     scale_singular == sus_translate("census tract") ~ sus_translate("census tracts"),
    #     scale_singular == sus_translate("dissemination area") ~ sus_translate("dissemination areas")
    #   )
    # 
    #   vec <-
    #     data_canale() %>%
    #     filter(!is.na(left_variable), !is.na(left_variable_full)) %>%
    #     pull(left_variable_full)
    # 
    #   # Univariate case
    #   if (input$var_right == " ") {
    #     # print("stats")
    #     min_val <- round(min(vec), 2)
    #     max_val <- round(max(vec), 2)
    #     mean_val <- round(mean(vec), 2)
    #     median_val <- round(median(vec), 2)
    #     sd_val <- sd(vec)
    #     quant_low <- round(quantile(vec, c(1 / 3, 2 / 3))[1], 2)
    #     quant_high <- round(quantile(vec, c(1 / 3, 2 / 3))[2], 2)
    # 
    #     # Case for no poly selected
    #     if (is.na(rv_canale$poly_selected)) {
    #       HTML(
    #         glue(sus_translate(paste0(
    #           "At the {scale_singular} scale, the CanALE index varies from ",
    #           "{min_val} to {max_val}, with an average value of {mean_val} ",
    #           "and a median value of {median_val}. ",
    #           "Two thirds of {scale_plural} have a score between {quant_low} ",
    #           "and {quant_high}."
    #         )))
    #       )
    # 
    #       # Case for selected poly
    #     } else {
    #       dat <- data_canale() %>% filter(ID == rv_canale$poly_selected)
    # 
    #       place_name <- case_when(
    #         scale_singular == sus_translate("borough/city") ~
    #         glue("{dat$name}"),
    #         scale_singular == sus_translate("census tract") ~
    #         glue(sus_translate(paste0("Census tract {dat$name}"))),
    #         scale_singular == sus_translate("dissemination area") ~
    #         glue(sus_translate(paste0("Dissemination area {dat$name}")))
    #       )
    # 
    #       if (dat$name_2 == "Borough" | dat$name_2 == "City") {
    #         dat$name_2 <- sus_translate(glue("{dat$name_2}"))
    #       }
    # 
    #       place_heading <-
    #         if_else(scale_singular == sus_translate("borough/city"),
    #           glue(sus_translate(paste0("{dat$name_2} of {place_name}"))),
    #           glue("{place_name} ({dat$name_2})")
    #         )
    # 
    #       poly_value <- dat$left_variable_full
    # 
    #       quintile <- quantile(vec, c(0.2, 0.4, 0.6, 0.8))
    # 
    #       larger_smaller <- case_when(
    #         poly_value >= quintile[4] ~ sus_translate("much larger than"),
    #         poly_value >= quintile[3] ~ sus_translate("larger than"),
    #         poly_value >= quintile[2] ~ sus_translate("almost the same as"),
    #         poly_value >= quintile[1] ~ sus_translate("smaller than"),
    #         TRUE ~ sus_translate("much smaller than")
    #       )
    # 
    #       poor_strong <- case_when(
    #         str_detect(larger_smaller, sus_translate("larger")) ~ sus_translate("strong"),
    #         str_detect(larger_smaller, sus_translate("smaller")) ~ sus_translate("poor"),
    #         TRUE ~ sus_translate("moderate")
    #       )
    # 
    #       percentile <-
    #         {
    #           length(vec[vec <= dat$left_variable_full]) / length(vec) * 100
    #         } %>%
    #         round()
    # 
    #       # Special case for Kahnawake
    #       if (dat$ID %in% c(56, "4620832.00", 24670285)) {
    #         HTML(
    #           glue(sus_translate(paste0(
    #             "<strong>Kahnawake Mohawk Territory</strong>",
    #             "<p>Statistics Canada does not gather the same ",
    #             "data for indigenous reserves in the Census as it does ",
    #             "for other jurisdictions, so we cannot display findings ",
    #             "here."
    #           )))
    #         )
    #       } else {
    #         HTML(
    #           glue(sus_translate(paste0(
    #             "<strong>{place_heading}</strong>",
    # 
    #             "<p>{place_name} has a population of ",
    #             "{prettyNum(dat$population, ',')} and a CanALE index ",
    #             "score of {round(poly_value, 2)}, which is {larger_smaller} ",
    #             "the region-wide median of {median_val}.",
    # 
    #             "<p>{place_name} has {poor_strong} potential for active ",
    #             "living, with a CanALE index score higher than {percentile}% ",
    #             "of {scale_plural} in the Montreal region."
    #           )))
    #         )
    #       }
    #     }
    # 
    #     # Bivariate case
    #   } else {
    #     var_name <-
    #       sus_translate(variable_explanations %>%
    #         filter(var_code == input$var_right) %>%
    #         pull(var_name))
    # 
    #     var_explanation <-
    #       sus_translate(variable_explanations %>%
    #         filter(var_code == input$var_right) %>%
    #         pull(explanation))
    # 
    #     correlation <-
    #       cor(
    #         data_canale()$left_variable_full,
    #         data_canale()$right_variable_full
    #       ) %>%
    #       round(2)
    # 
    #     pos_neg <- if_else(correlation > 0, sus_translate("positive"), sus_translate("negative"))
    # 
    #     strong_weak <- case_when(
    #       abs(correlation) > 0.6 ~ sus_translate("strong"),
    #       abs(correlation) > 0.3 ~ sus_translate("moderate"),
    #       TRUE ~ "weak"
    #     )
    # 
    #     higher_lower <-
    #       if_else(pos_neg == sus_translate("positive"),
    #         sus_translate("higher"),
    #         sus_translate("lower")
    #       )
    # 
    #     high_low_disclaimer <- case_when(
    #       strong_weak == sus_translate("strong") ~ sus_translate("with only a few exceptions"),
    #       strong_weak == sus_translate("moderate") ~ sus_translate("although with some exceptions"),
    #       strong_weak == sus_translate("weak") ~ sus_translate("although with many exceptions"),
    #     )
    # 
    #     # Case for no poly selected
    #     if (is.na(rv_canale$poly_selected)) {
    #       # print("2nd order")
    #       # If correlation is close to zero
    #       if (correlation < 0.05 && correlation > -0.05) {
    #         HTML(
    #           glue(sus_translate(paste0(
    #             "<p>{var_explanation}",
    #             "<p>The CanALE index has effectively no correlation ",
    #             "({correlation}) with {var_name} at the ",
    #             "{scale_singular} scale.",
    #             "<p>This means that, at the {scale_singular} scale, ",
    #             "there is no relationship between the two variables."
    #           )))
    #         )
    #       } else {
    #         HTML(
    #           glue(sus_translate(paste0(
    #             "<p>{var_explanation}",
    #             "<p>The CanALE index has a {strong_weak} {pos_neg} ",
    #             "correlation ({correlation}) with '{tolower(var_name)}' at the ",
    #             "{scale_singular} scale.",
    #             "<p>This means that, in general, {scale_plural} with higher ",
    #             "potential for active living tend to have {higher_lower} ",
    #             "values for '{tolower(var_name)}', {high_low_disclaimer}."
    #           )))
    #         )
    #       }
    # 
    #       # Case for poly selected
    #     } else {
    #       dat <- data_canale() %>% filter(ID == rv_canale$poly_selected)
    # 
    #       vec_2 <-
    #         data_canale() %>%
    #         filter(!is.na(right_variable), !is.na(right_variable_full)) %>%
    #         pull(right_variable_full)
    # 
    #       poly_value_1 <- dat$left_variable_full
    #       poly_value_2 <- dat$right_variable_full
    # 
    #       # print("polyselect")
    #       place_name <- case_when(
    #         scale_singular == sus_translate("borough/city") ~
    #         glue("{dat$name}"),
    #         scale_singular == sus_translate("census tract") ~
    #         glue(sus_translate(paste0("Census tract {dat$name}"))),
    #         scale_singular == sus_translate("dissemination area") ~
    #         glue(sus_translate(paste0("Dissemination area {dat$name}")))
    #       )
    # 
    #       if (dat$name_2 == "Borough" | dat$name_2 == "City") {
    #         dat$name_2 <- sus_translate(glue("{dat$name_2}"))
    #       }
    # 
    #       place_heading <-
    #         if_else(scale_singular == sus_translate("borough/city"),
    #           glue(sus_translate(paste0("{dat$name_2} of {place_name}"))),
    #           glue("{place_name} ({dat$name_2})")
    #         )
    # 
    # 
    #       percentile_left <-
    #         {
    #           length(vec[vec <= dat$left_variable_full]) / length(vec) * 100
    #         } %>%
    #         round()
    # 
    #       percentile_right <-
    #         {
    #           length(vec_2[vec_2 <= dat$right_variable_full]) /
    #             length(vec_2) * 100
    #         } %>%
    #         round()
    # 
    #       relative_position <- case_when(
    #         abs(percentile_left - percentile_right) > 50 ~ sus_translate("dramatically different"),
    #         abs(percentile_left - percentile_right) > 30 ~ sus_translate("substantially different"),
    #         abs(percentile_left - percentile_right) > 10 ~ sus_translate("considerably different"),
    #         TRUE ~ sus_translate("similar")
    #       )
    # 
    #       # Special case for Kahnawake
    #       if (dat$ID %in% c(56, "4620832.00", 24670285)) {
    #         HTML(
    #           glue(sus_translate(paste0(
    #             "<strong>Kahnawake Mohawk Territory</strong>",
    #             "<p>Statistics Canada does not gather the same ",
    #             "data for indigenous reserves in the Census as it does ",
    #             "for other jurisdictions, so we cannot display findings ",
    #             "here."
    #           )))
    #         )
    #       } else {
    #         HTML(
    #           glue(sus_translate(paste0(
    #             "<strong>{place_heading}</strong>",
    # 
    #             "<p>{place_name} has a population of ",
    #             "{prettyNum(dat$population, ',')}, a CanALE index score ",
    #             "of {round(poly_value_1, 2)}, and a '{tolower(var_name)}' ",
    #             "value of {round(poly_value_2, 2)}. ",
    # 
    #             "<p>These two scores are {relative_position}, in relative ",
    #             "terms. {place_name} has a CanALE index score higher ",
    #             "than {percentile_left}% of {scale_plural} and ",
    #             "a '{tolower(var_name)}' score higher than ",
    #             "{percentile_right}% of {scale_plural} in the ",
    #             "Montreal region."
    #           )))
    #         )
    #       }
    #     }
    #   }
    # })

    # Render the histogram/scatterplot
    explore_graph_server("canale", data_canale, reactive(rv_canale$poly_selected))
    
    
    # 
    # 
    # 
    # ## Render the did-you-knows --------------------------------------------------
    # 
    # output$did_you_know <- renderUI({
    #   sus_translate(did_you_know %>%
    #     filter(right_variable == input$var_right) %>%
    #     slice_sample(n = 2) %>%
    #     pull(text)) %>%
    #     paste("<li> ", ., collapse = "") %>%
    #     paste0("<ul>", ., "</ul>") %>%
    #     HTML()
    # })
    # 
    # 
    ## Update map in response to variable changes, zooming, or options -----------

    observeEvent({
      input$var_right
      rv_canale$zoom
      input$extrude}, {
        if (!input$extrude) {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "extrude") %>%
            add_polygon(
              data = data_canale(),
              stroke_width = "width", stroke_colour = "#FFFFFF",
              fill_colour = "fill_opacity", update_view = FALSE,
              layer_id = "polylayer", id = "ID", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90", legend = FALSE)
        } else {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "polylayer") %>%
            add_polygon(
              data = data_canale(),
              fill_colour = "fill", elevation = "elevation",
              update_view = FALSE, layer_id = "extrude", id = "ID",
              auto_highlight = TRUE, highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(lightsPosition = c(0, 0, 5000),
                                    numberOfLights = 1, ambientRatio = 1))
          }
        }
      )


    ## Update map on click -------------------------------------------------------

    observeEvent(rv_canale$poly_selected, {

      # Mode if not in 3D
      if (!input$extrude) {
        if (!is.na(rv_canale$poly_selected)) {

          # print(paste0("Selecting polygon ", rv_canale$poly_selected))

          mapdeck_update(map_id = NS(id, "map")) %>%
            add_polygon(
              data = filter(data_canale(), ID == rv_canale$poly_selected),
              stroke_width = "width", stroke_colour = "#000000",
              fill_colour = "fill", update_view = FALSE, 
              layer_id = "poly_highlight", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90", legend = FALSE)
          }

        if (is.na(rv_canale$poly_selected)) {

          # print("Removing selection")

          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "poly_highlight")
        }

        # Mode if in 3D
      } else if (input$extrude) {
        if (!is.na(rv_canale$poly_selected)) {

          # print(paste0("Selecting 3D polygon ", rv_canale$poly_selected))

          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "polylayer") %>%
            clear_polygon(layer_id = "extrude") %>%
            add_polygon(
              data = {
                data_canale() %>%
                  mutate(elevation = if_else(
                    group == group[ID == rv_canale$poly_selected], 4000, 0))},
              fill_colour = "fill", elevation = "elevation",
              update_view = FALSE, layer_id = "extrude", id = "ID",
              auto_highlight = TRUE, highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(
                lightsPosition = c(0, 0, 5000),
                numberOfLights = 1,
                ambientRatio = 1))
        }

        if (is.na(rv_canale$poly_selected)) {

          # print("Removing 3D selection")

          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "poly_highlight") %>%
            clear_polygon(layer_id = "extrude") %>%
            add_polygon(
              data = data_canale(), fill_colour = "fill", 
              elevation = "elevation", update_view = FALSE, layer_id = "extrude",
              id = "ID", auto_highlight = TRUE, highlight_colour = "#FFFFFF90",
              legend = FALSE)
        }
      }
    })


    # ## Update link text ----------------------------------------------------------
    # 
    # # More info
    # output$more_info_status <- reactive(input$more_info %% 2 == 1)
    # outputOptions(output, "more_info_status", suspendWhenHidden = FALSE)
    # 
    # observe({
    #   if (input$more_info %% 2 == 1) {
    #     txt <- sus_translate("Hide")
    #   } else {
    #     txt <- sus_translate("Learn more")
    #   }
    #   updateActionButton(session, "more_info", label = txt)
    # })

    # Hide compare status
    output$hide_compare_status <- reactive(input$hide_compare %% 2 == 0)
    outputOptions(output, "hide_compare_status", suspendWhenHidden = FALSE)

    observeEvent(input$hide_compare, {
      if (input$hide_compare %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Show")
      updateActionButton(session, "hide_compare", label = txt)
      })

    # Hide explore status
    output$hide_explore_status <- reactive(input$hide_explore %% 2 == 0)
    outputOptions(output, "hide_explore_status", suspendWhenHidden = FALSE)

    observeEvent(input$hide_explore, {
      if (input$hide_explore %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Show")
      updateActionButton(session, "hide_explore", label = txt)
      })

    # # Hide DYK status
    # output$hide_dyk_status <- reactive(input$hide_dyk %% 2 == 0)
    # outputOptions(output, "hide_dyk_status", suspendWhenHidden = FALSE)
    # 
    # observeEvent(input$hide_dyk, {
    #   if (input$hide_dyk %% 2 == 0) {
    #     txt <- sus_translate("Hide")
    #   } else {
    #     txt <- sus_translate("Show")
    #   }
    #   updateActionButton(session, "hide_dyk", label = txt)
    # })
    # 
    
    # Left map
    left_map_server("canale", data_canale, reactive(rv_canale$zoom))

    # Right map
    output$map_right <- renderPlot({
      
        if (input$var_right == " ") {
          p <- 
            ggplot(data_canale()) +
            geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
            theme_map()

          ggdraw() +
            draw_image(dropshadow_right, scale = 1.17) +
            draw_plot(p)
        } else {
          p <-
            ggplot(data_canale()) +
            geom_sf(aes(fill = as.factor(right_variable)),
                    color = "white", size = 0.01) +
            scale_fill_manual(values = rev(colors[c(4:6)])) +
            theme_map()

          ggdraw() +
            draw_image(dropshadow_right, scale = 1.17) +
            draw_plot(p) +
            draw_image(uni_legend_right, scale = .45, vjust = 0.25, hjust = -0.25)
        }
      }, bg = "transparent") %>% bindCache(input$var_right, 
                                           rv_canale$zoom)
  })
}
