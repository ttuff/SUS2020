### CANALE MODULE ##############################################################

# Data and helper functions -----------------------------------------------

# Load bivariate census data
qs::qload("data/new_bivariate.qsm")

# Dropdown menu
var_list <- 
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
  ns <- NS(id)

  tabItem(
    # Style tags
    tags$head(tags$style(HTML("
          #title_bar {border-width: 10px; border-color: rgb(255, 255, 255);}
          #input_control_overlay {border-width: 10px; 
          border-color: rgba(255,255,255,1);}
          #input_control_left {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}
          #input_control_left2 {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}
          #active_legend_container {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}"))),

    # Main map
    mapdeckOutput(outputId = ns("active_map"), height = "91vh"),

    # Title bar
    absolutePanel(
      id = "title_bar", class = "panel panel-default", style = "padding:5px;",
      draggable = FALSE, top = 70, left = 270, width = "40%",
      h2(i18n$t("Active living potential: the CanALE index")),
      p(i18n$t(title_text %>%
        filter(tab == "active", type == "main") %>%
        pull(text))),
      actionLink(ns("more_info"), i18n$t("Learn more")),
      conditionalPanel(
        condition = "output.more_info_status == 1", ns = ns,
        uiOutput(outputId = ns("active_extra_html"))
      )
    ),
    # HTML(title_text %>%
    #        filter(tab == "active", type == "extra") %>%
    #        pull(text)))),

    # 3D switch
    absolutePanel(
      id = ns("input_control_overlay"), style =
        "z-index:500; max-height: 88vh; overflow-y: auto; overflow-x:hidden; padding: 5px;",
      class = "panel panel-default", top = 70, right = 50, width = 300,
      materialSwitch(
        inputId = ns("active_extrude"),
        label = i18n$t("View in 3D"),
        status = "danger",
        value = FALSE
      ),
      hr(),

      # Compare panel
      fluidRow(
        column(width = 8, h4(i18n$t("Compare"))),
        column(
          width = 4, align = "right",
          actionLink(
            inputId = ns("active_hide_compare"),
            label = i18n$t("Hide")
          )
        )
      ),
      conditionalPanel(
        condition = "output.active_hide_compare_status == 1", ns = ns,
        selectInput(ns("data_for_plot_right"),
          label = NULL,
          choices = var_list
        ),
        plotOutput(ns("active_map_right")), height = 250
      ),
      conditionalPanel(
        condition = "input.active_extrude == 0", ns = ns,
        hr(),

        # Explore panel
        fluidRow(
          column(
            width = 8,
            h4(i18n$t("Explore"))
          ),
          column(
            width = 4, align = "right",
            actionLink(
              inputId = ns("active_hide_explore"),
              label = i18n$t("Hide")
            )
          )
        ),
        conditionalPanel(
          condition = "output.active_hide_explore_status == 1", ns = ns,
          htmlOutput(ns("active_info")),
          conditionalPanel(
            condition = "output.active_poly_selected == 1", ns = ns,
            actionLink(
              inputId = ns("active_clear_selection"),
              label = "Clear selection"
            )
          ),
          plotOutput(ns("bivariate_graph"), height = 150)
        ),
        hr(),

        # DYK panel
        fluidRow(
          column(
            width = 8,
            h4(i18n$t("Did you know?"))
          ),
          column(
            width = 4, align = "right",
            actionLink(
              inputId = ns("active_hide_dyk"),
              label = i18n$t("Hide")
            )
          )
        ),
        conditionalPanel(
          condition = "output.active_hide_dyk_status == 1",
          htmlOutput(ns("did_you_know"))
        )
      )
    ),

    # Floating legend
    absolutePanel(
      id = ns("active_legend_container"), class = "panel panel-default",
      style = "z-index:500;", bottom = -200, left = 270, fixed = TRUE,
      conditionalPanel(
        condition = 'input.data_for_plot_right != " "', ns = ns,
        id = ns("active_legend"),
        img(src = "bivariate_legend_2.png", width = 200, height = 177)
      )
    )
  )
}


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Active extra html translation -------------------------------------------

    output$active_extra_html <-
      renderUI(HTML(sus_translate(
        title_text %>%
          filter(tab == "active", type == "extra") %>%
          pull(text)
      )))


    # Drop down list for variable selection -----------------------------------


    # List reactive translation
    observe({
      updateSelectInput(
        session = session,
        inputId = "data_for_plot_right",
        choices = sus_translate(var_list)
      )
    })



    # Load bivariate census data
    # qload("data/new_bivariate.qsm")

    # did_you_know <-
    #   read_csv("data/did_you_know.csv") %>%
    #   mutate(right_variable = if_else(is.na(right_variable), " ", right_variable))
    #
    # variable_explanations <-
    #   read_csv("data/variable_explanations.csv")
    #

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



    rz <- reactiveValues(
      zoom = "OUT",
      poly_selected = NA
    )


    ## Create the data frame to generate bivariate maps --------------------------


    data_bivar <- reactive({
      if (input$active_extrude) {
        data <- data_DA_1_large
      } else if (rz$zoom == "OUT") {
        data <- data_borough_large
      } else if (rz$zoom == "IN") {
        data <- data_CT_large
      } else if (rz$zoom == "ISO") {
        data <- data_DA_1_large
      } else if (rz$zoom == "ISO_2") {
        data <- data_DA_2_large
      }
      st_crs(data) <- 4326 # Starting case for no selection
      if (input$data_for_plot_right == " ") {
        data <-
          data %>%
          dplyr::select(ID, name, name_2, population,
            left_variable_full = ale_index,
            left_variable = ale_index_quant3, ale_class, width,
            group, fill, elevation, fill_opacity
          )
      } else {
        data <-
          data %>%
          dplyr::select(
            ID, name, name_2, population,
            left_variable_full = ale_index,
            left_variable = ale_index_quant3, ale_class,
            right_variable_full = input$data_for_plot_right,
            right_variable = paste0(input$data_for_plot_right, "_quant3"),
            width, group = paste0(input$data_for_plot_right, "_quant3_group"),
            fill = paste0(input$data_for_plot_right, "_quant3_fill"),
            elevation = paste0(input$data_for_plot_right, "_quant3_elevation"),
            fill_opacity = paste0(
              input$data_for_plot_right,
              "_quant3_fill_opacity"
            )
          )
      }
      st_crs(data) <- 4326
      st_crs(data$geometry) <- 4326
      # print(st_crs(data))
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
        TRUE ~ "OUT"
      )
    })


    ## Observe and change click status -------------------------------------------

    # Update poly_selected on click
    observeEvent(input$active_map_polygon_click, {
      lst <- jsonlite::fromJSON(input$active_map_polygon_click)
      rz$poly_selected <- lst$object$properties$id
    })

    # Clear click status if prompted
    observeEvent(input$active_clear_selection, {
      rz$poly_selected <- NA
    })

    # Output polygon select status
    output$active_poly_selected <- reactive({
      if (is.na(rz$poly_selected)) FALSE else TRUE
    })
    outputOptions(output, "active_poly_selected", suspendWhenHidden = FALSE)

    # Clear polygon select on zoom change
    observeEvent(rz$zoom,
      {
        rz$poly_selected <- NA
      },
      ignoreInit = TRUE
    )

    # Clear polygon select on tab change
    observeEvent(input$tabs,
      {
        rz$poly_selected <- NA
      },
      ignoreInit = TRUE
    )


    ## Observe and react to change in extrude status -----------------------------

    observeEvent(input$active_extrude, {
      rz$poly_selected <- NA
    })


    ## Render the map ------------------------------------------------------------

    output$active_map <- renderMapdeck({
      mapdeck(
        style = "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7",
        token = paste0(
          "pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
          "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ"
        ),
        zoom = 10.1, location = c(-73.58, 45.53), pitch = 0
      )
    })


    ## Render the info table -----------------------------------------------------

    output$active_info <- renderUI({
      scale_singular <- case_when(
        rz$zoom == "OUT" ~ sus_translate("borough/city"),
        rz$zoom == "IN" ~ sus_translate("census tract"),
        TRUE ~ sus_translate("dissemination area")
      )

      scale_plural <- case_when(
        scale_singular == sus_translate("borough/city") ~ sus_translate("boroughs or cities"),
        scale_singular == sus_translate("census tract") ~ sus_translate("census tracts"),
        scale_singular == sus_translate("dissemination area") ~ sus_translate("dissemination areas")
      )

      vec <-
        data_bivar() %>%
        filter(!is.na(left_variable), !is.na(left_variable_full)) %>%
        pull(left_variable_full)

      # Univariate case
      if (input$data_for_plot_right == " ") {
        # print("stats")
        min_val <- round(min(vec), 2)
        max_val <- round(max(vec), 2)
        mean_val <- round(mean(vec), 2)
        median_val <- round(median(vec), 2)
        sd_val <- sd(vec)
        quant_low <- round(quantile(vec, c(1 / 3, 2 / 3))[1], 2)
        quant_high <- round(quantile(vec, c(1 / 3, 2 / 3))[2], 2)

        # Case for no poly selected
        if (is.na(rz$poly_selected)) {
          HTML(
            glue(sus_translate(paste0(
              "At the {scale_singular} scale, the CanALE index varies from ",
              "{min_val} to {max_val}, with an average value of {mean_val} ",
              "and a median value of {median_val}. ",
              "Two thirds of {scale_plural} have a score between {quant_low} ",
              "and {quant_high}."
            )))
          )

          # Case for selected poly
        } else {
          dat <- data_bivar() %>% filter(ID == rz$poly_selected)

          place_name <- case_when(
            scale_singular == sus_translate("borough/city") ~
            glue("{dat$name}"),
            scale_singular == sus_translate("census tract") ~
            glue(sus_translate(paste0("Census tract {dat$name}"))),
            scale_singular == sus_translate("dissemination area") ~
            glue(sus_translate(paste0("Dissemination area {dat$name}")))
          )

          if (dat$name_2 == "Borough" | dat$name_2 == "City") {
            dat$name_2 <- sus_translate(glue("{dat$name_2}"))
          }

          place_heading <-
            if_else(scale_singular == sus_translate("borough/city"),
              glue(sus_translate(paste0("{dat$name_2} of {place_name}"))),
              glue("{place_name} ({dat$name_2})")
            )

          poly_value <- dat$left_variable_full

          quintile <- quantile(vec, c(0.2, 0.4, 0.6, 0.8))

          larger_smaller <- case_when(
            poly_value >= quintile[4] ~ sus_translate("much larger than"),
            poly_value >= quintile[3] ~ sus_translate("larger than"),
            poly_value >= quintile[2] ~ sus_translate("almost the same as"),
            poly_value >= quintile[1] ~ sus_translate("smaller than"),
            TRUE ~ sus_translate("much smaller than")
          )

          poor_strong <- case_when(
            str_detect(larger_smaller, sus_translate("larger")) ~ sus_translate("strong"),
            str_detect(larger_smaller, sus_translate("smaller")) ~ sus_translate("poor"),
            TRUE ~ sus_translate("moderate")
          )

          percentile <-
            {
              length(vec[vec <= dat$left_variable_full]) / length(vec) * 100
            } %>%
            round()

          # Special case for Kahnawake
          if (dat$ID %in% c(56, "4620832.00", 24670285)) {
            HTML(
              glue(sus_translate(paste0(
                "<strong>Kahnawake Mohawk Territory</strong>",
                "<p>Statistics Canada does not gather the same ",
                "data for indigenous reserves in the Census as it does ",
                "for other jurisdictions, so we cannot display findings ",
                "here."
              )))
            )
          } else {
            HTML(
              glue(sus_translate(paste0(
                "<strong>{place_heading}</strong>",

                "<p>{place_name} has a population of ",
                "{prettyNum(dat$population, ',')} and a CanALE index ",
                "score of {round(poly_value, 2)}, which is {larger_smaller} ",
                "the region-wide median of {median_val}.",

                "<p>{place_name} has {poor_strong} potential for active ",
                "living, with a CanALE index score higher than {percentile}% ",
                "of {scale_plural} in the Montreal region."
              )))
            )
          }
        }

        # Bivariate case
      } else {
        var_name <-
          sus_translate(variable_explanations %>%
            filter(var_code == input$data_for_plot_right) %>%
            pull(var_name))

        var_explanation <-
          sus_translate(variable_explanations %>%
            filter(var_code == input$data_for_plot_right) %>%
            pull(explanation))

        correlation <-
          cor(
            data_bivar()$left_variable_full,
            data_bivar()$right_variable_full
          ) %>%
          round(2)

        pos_neg <- if_else(correlation > 0, sus_translate("positive"), sus_translate("negative"))

        strong_weak <- case_when(
          abs(correlation) > 0.6 ~ sus_translate("strong"),
          abs(correlation) > 0.3 ~ sus_translate("moderate"),
          TRUE ~ "weak"
        )

        higher_lower <-
          if_else(pos_neg == sus_translate("positive"),
            sus_translate("higher"),
            sus_translate("lower")
          )

        high_low_disclaimer <- case_when(
          strong_weak == sus_translate("strong") ~ sus_translate("with only a few exceptions"),
          strong_weak == sus_translate("moderate") ~ sus_translate("although with some exceptions"),
          strong_weak == sus_translate("weak") ~ sus_translate("although with many exceptions"),
        )

        # Case for no poly selected
        if (is.na(rz$poly_selected)) {
          # print("2nd order")
          # If correlation is close to zero
          if (correlation < 0.05 && correlation > -0.05) {
            HTML(
              glue(sus_translate(paste0(
                "<p>{var_explanation}",
                "<p>The CanALE index has effectively no correlation ",
                "({correlation}) with {var_name} at the ",
                "{scale_singular} scale.",
                "<p>This means that, at the {scale_singular} scale, ",
                "there is no relationship between the two variables."
              )))
            )
          } else {
            HTML(
              glue(sus_translate(paste0(
                "<p>{var_explanation}",
                "<p>The CanALE index has a {strong_weak} {pos_neg} ",
                "correlation ({correlation}) with '{tolower(var_name)}' at the ",
                "{scale_singular} scale.",
                "<p>This means that, in general, {scale_plural} with higher ",
                "potential for active living tend to have {higher_lower} ",
                "values for '{tolower(var_name)}', {high_low_disclaimer}."
              )))
            )
          }

          # Case for poly selected
        } else {
          dat <- data_bivar() %>% filter(ID == rz$poly_selected)

          vec_2 <-
            data_bivar() %>%
            filter(!is.na(right_variable), !is.na(right_variable_full)) %>%
            pull(right_variable_full)

          poly_value_1 <- dat$left_variable_full
          poly_value_2 <- dat$right_variable_full

          # print("polyselect")
          place_name <- case_when(
            scale_singular == sus_translate("borough/city") ~
            glue("{dat$name}"),
            scale_singular == sus_translate("census tract") ~
            glue(sus_translate(paste0("Census tract {dat$name}"))),
            scale_singular == sus_translate("dissemination area") ~
            glue(sus_translate(paste0("Dissemination area {dat$name}")))
          )

          if (dat$name_2 == "Borough" | dat$name_2 == "City") {
            dat$name_2 <- sus_translate(glue("{dat$name_2}"))
          }

          place_heading <-
            if_else(scale_singular == sus_translate("borough/city"),
              glue(sus_translate(paste0("{dat$name_2} of {place_name}"))),
              glue("{place_name} ({dat$name_2})")
            )


          percentile_left <-
            {
              length(vec[vec <= dat$left_variable_full]) / length(vec) * 100
            } %>%
            round()

          percentile_right <-
            {
              length(vec_2[vec_2 <= dat$right_variable_full]) /
                length(vec_2) * 100
            } %>%
            round()

          relative_position <- case_when(
            abs(percentile_left - percentile_right) > 50 ~ sus_translate("dramatically different"),
            abs(percentile_left - percentile_right) > 30 ~ sus_translate("substantially different"),
            abs(percentile_left - percentile_right) > 10 ~ sus_translate("considerably different"),
            TRUE ~ sus_translate("similar")
          )

          # Special case for Kahnawake
          if (dat$ID %in% c(56, "4620832.00", 24670285)) {
            HTML(
              glue(sus_translate(paste0(
                "<strong>Kahnawake Mohawk Territory</strong>",
                "<p>Statistics Canada does not gather the same ",
                "data for indigenous reserves in the Census as it does ",
                "for other jurisdictions, so we cannot display findings ",
                "here."
              )))
            )
          } else {
            HTML(
              glue(sus_translate(paste0(
                "<strong>{place_heading}</strong>",

                "<p>{place_name} has a population of ",
                "{prettyNum(dat$population, ',')}, a CanALE index score ",
                "of {round(poly_value_1, 2)}, and a '{tolower(var_name)}' ",
                "value of {round(poly_value_2, 2)}. ",

                "<p>These two scores are {relative_position}, in relative ",
                "terms. {place_name} has a CanALE index score higher ",
                "than {percentile_left}% of {scale_plural} and ",
                "a '{tolower(var_name)}' score higher than ",
                "{percentile_right}% of {scale_plural} in the ",
                "Montreal region."
              )))
            )
          }
        }
      }
    })

    ## Render the histogram/scatterplot ------------------------------------------

    output$bivariate_graph <- renderPlot({

      # Histogram for a single variable
      if (input$data_for_plot_right == " ") {

        # If no poly is selected
        if (is.na(rz$poly_selected)) {
          data_bivar() %>%
            filter(!is.na(left_variable)) %>%
            ggplot(aes(left_variable_full)) +
            geom_histogram(aes(fill = fill), bins = 25) +
            scale_fill_manual(
              values = colors[c(1:3)],
              na.translate = FALSE
            ) +
            labs(x = "CanALE index", y = NULL) +
            theme_minimal() +
            theme(
              legend.position = "none",
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()
            )

          # If there is an active selection
        } else {

          # If the selection is NA
          if ({
            data_bivar() %>%
              filter(ID == rz$poly_selected) %>%
              filter(!is.na(left_variable)) %>%
              nrow()
          } == 0) {
            data_bivar() %>%
              filter(!is.na(left_variable)) %>%
              ggplot(aes(left_variable_full)) +
              geom_histogram(bins = 25, fill = colors[3]) +
              labs(x = "CanALE index", y = NULL) +
              theme_minimal() +
              theme(
                legend.position = "none",
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank()
              )

            # If the selection should be plotted
          } else {
            data_bivar() %>%
              filter(!is.na(left_variable)) %>%
              ggplot(aes(left_variable_full)) +
              geom_histogram(aes(
                fill = round(left_variable_full) ==
                  round(left_variable_full[ID == rz$poly_selected])
              ),
              bins = 25
              ) +
              scale_fill_manual(values = colors[c(3, 1)], na.translate = FALSE) +
              labs(x = "CanALE index", y = NULL) +
              theme_minimal() +
              theme(
                legend.position = "none",
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank()
              )
          }
        }

        # Scatterplot for two variables
      } else {
        var_name <-
          sus_translate(variable_explanations %>%
            filter(var_code == input$data_for_plot_right) %>%
            pull(var_name))


        if (nrow(filter(data_bivar(), ID == rz$poly_selected)) != 1) {
          data_bivar() %>%
            drop_na() %>%
            ggplot(aes(left_variable_full, right_variable_full)) +
            geom_point(aes(colour = group)) +
            # geom_smooth(method = "lm", se = FALSE, colour = "grey50") +
            scale_colour_manual(values = deframe(bivariate_color_scale)) +
            labs(x = "CanALE index", y = var_name) +
            theme_minimal() +
            theme(
              legend.position = "none",
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()
            )
        } else {
          data_bivar() %>%
            drop_na() %>%
            ggplot(aes(left_variable_full, right_variable_full)) +
            geom_point(colour = bivariate_color_scale$fill[9]) +
            # geom_smooth(method = "lm", se = FALSE, colour = "grey50") +
            geom_point(
              data = filter(
                data_bivar(), ID == rz$poly_selected,
                !is.na(left_variable_full),
                !is.na(right_variable_full)
              ),
              colour = bivariate_color_scale$fill[1],
              size = 3
            ) +
            labs(x = "CanALE index", y = var_name) +
            theme_minimal() +
            theme(
              legend.position = "none",
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()
            )
        }
      }
    })


    ## Render the did-you-knows --------------------------------------------------

    output$did_you_know <- renderUI({
      sus_translate(did_you_know %>%
        filter(right_variable == input$data_for_plot_right) %>%
        slice_sample(n = 2) %>%
        pull(text)) %>%
        paste("<li> ", ., collapse = "") %>%
        paste0("<ul>", ., "</ul>") %>%
        HTML()
    })


    ## Update map in response to variable changes, zooming, or options -----------

    observeEvent(
      {
        data_bivar()
        input$tabs
        input$active_extrude
      },
      {
        if (!input$active_extrude) {
          mapdeck_update(map_id = ns("active_map")) %>%
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
              highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(
                lightsPosition = c(0, 0, 5000),
                numberOfLights = 1,
                ambientRatio = 1
              )
            )
        } else {
          mapdeck_update(map_id = ns("active_map")) %>%
            clear_polygon(layer_id = "polylayer") %>%
            add_polygon(
              data = data_bivar(),
              fill_colour = "fill",
              elevation = "elevation",
              update_view = FALSE,
              layer_id = "extrude",
              id = "ID",
              auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(
                lightsPosition = c(0, 0, 5000),
                numberOfLights = 1,
                ambientRatio = 1
              )
            )
        }
      }
    )


    ## Update map on click -------------------------------------------------------

    observeEvent(rz$poly_selected, {

      # Mode if not in 3D
      if (!input$active_extrude) {
        if (!is.na(rz$poly_selected)) {

          # print(paste0("Selecting polygon ", rz$poly_selected))

          mapdeck_update(map_id = ns("active_map")) %>%
            add_polygon(
              data = {
                data_bivar() %>%
                  filter(ID == rz$poly_selected)
              },
              stroke_width = "width",
              stroke_colour = "#000000",
              fill_colour = "fill",
              update_view = FALSE,
              layer_id = "poly_highlight",
              auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(
                lightsPosition = c(0, 0, 5000),
                numberOfLights = 1,
                ambientRatio = 1
              )
            )
        }

        if (is.na(rz$poly_selected)) {

          # print("Removing selection")

          mapdeck_update(map_id = ns("active_map")) %>%
            clear_polygon(layer_id = "poly_highlight")
        }

        # Mode if in 3D
      } else if (input$active_extrude) {
        if (!is.na(rz$poly_selected)) {

          # print(paste0("Selecting 3D polygon ", rz$poly_selected))

          mapdeck_update(map_id = ns("active_map")) %>%
            clear_polygon(layer_id = "polylayer") %>%
            clear_polygon(layer_id = "extrude") %>%
            add_polygon(
              data = {
                data_bivar() %>%
                  mutate(elevation = if_else(
                    group == group[ID == rz$poly_selected], 4000, 0
                  ))
              },
              fill_colour = "fill",
              elevation = "elevation",
              update_view = FALSE,
              layer_id = "extrude",
              id = "ID",
              auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(
                lightsPosition = c(0, 0, 5000),
                numberOfLights = 1,
                ambientRatio = 1
              )
            )
        }

        if (is.na(rz$poly_selected)) {

          # print("Removing 3D selection")

          mapdeck_update(map_id = ns("active_map")) %>%
            clear_polygon(layer_id = "poly_highlight") %>%
            clear_polygon(layer_id = "extrude") %>%
            add_polygon(
              data = data_bivar(),
              fill_colour = "fill",
              elevation = "elevation",
              update_view = FALSE,
              layer_id = "extrude",
              id = "ID",
              auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(
                lightsPosition = c(0, 0, 5000),
                numberOfLights = 1,
                ambientRatio = 1
              )
            )
        }

        # Mode if in 3D
      }
    })


    ## Update link text ----------------------------------------------------------

    # More info
    output$more_info_status <- reactive(input$more_info %% 2 == 1)
    outputOptions(output, "more_info_status", suspendWhenHidden = FALSE)

    observe({
      if (input$more_info %% 2 == 1) {
        txt <- sus_translate("Hide")
      } else {
        txt <- sus_translate("Learn more")
      }
      updateActionButton(session, "more_info", label = txt)
    })

    # Hide compare status
    output$active_hide_compare_status <-
      reactive(input$active_hide_compare %% 2 == 0)
    outputOptions(output, "active_hide_compare_status", suspendWhenHidden = FALSE)

    observeEvent(input$active_hide_compare, {
      if (input$active_hide_compare %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else {
        txt <- sus_translate("Show")
      }
      updateActionButton(session, "active_hide_compare", label = txt)
    })

    # Hide explore status
    output$active_hide_explore_status <-
      reactive(input$active_hide_explore %% 2 == 0)
    outputOptions(output, "active_hide_explore_status", suspendWhenHidden = FALSE)

    observeEvent(input$active_hide_explore, {
      if (input$active_hide_explore %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else {
        txt <- sus_translate("Show")
      }
      updateActionButton(session, "active_hide_explore", label = txt)
    })

    # Hide DYK status
    output$active_hide_dyk_status <- reactive(input$active_hide_dyk %% 2 == 0)
    outputOptions(output, "active_hide_dyk_status", suspendWhenHidden = FALSE)

    observeEvent(input$active_hide_dyk, {
      if (input$active_hide_dyk %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else {
        txt <- sus_translate("Show")
      }
      updateActionButton(session, "active_hide_dyk", label = txt)
    })


    ### Plot output calls for left plot TKTK NOT CURRENTLY WORKING ##################################
    
    # output$canale_map_left <- renderCachedPlot({
    #   
    #   # data_for_plot_left <- 
    #   #   data_bivar()
    #   # 
    #   # p <-
    #   #   ggplot(data_for_plot_left) +
    #   #   geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
    #   #   scale_fill_manual(values = rev(colors[c(1:3)]), na.value = "grey70") +
    #   #   theme_map() + 
    #   #   theme(legend.position = "none")
    #   # 
    #   # ggdraw() + 
    #   #   draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
    #   #   draw_plot(p) +
    #   #   draw_image(uni_legend, scale = .45, vjust = 0.25, hjust = 0.25) 
    #   
    # }, 
    # cacheKeyExpr = paste(rz$zoom, "left", sep = "_"),
    # cache = diskCache("./app-cache"),
    # bg = "white")
    
    ### Plot output calls for all 'right' plots ##################################

    # Active living potential
    output$active_map_right <- renderCachedPlot(
      {
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
            geom_sf(aes(fill = as.factor(right_variable)),
              color = "white",
              size = 0.01
            ) +
            scale_fill_manual(values = rev(colors[c(4:6)])) +
            theme_map()

          ggdraw() +
            draw_image(dropshadow1, scale = 1.49, vjust = -0.003, hjust = -0.003) +
            draw_plot(p) +
            draw_image(uni_legend_right, scale = .5, vjust = 0.25, hjust = -0.25)
        }
      },
      cacheKeyExpr = paste(rz$zoom, input$data_for_plot_right, sep = "_"),
      cache = diskCache("./app-cache"),
      bg = "transparent"
    )
  })
}
