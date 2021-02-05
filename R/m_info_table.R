#### INFO TABLE MODULE #########################################################

info_table_UI <- function(id) {
  htmlOutput(NS(id, "info_table"))
}

info_table_server <- function(id, x, var_right, select, zoom, title) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select))
  stopifnot(is.reactive(zoom))
  stopifnot(!is.reactive(title))
  
  moduleServer(id, function(input, output, session) {
    
    output$info_table <- renderUI({
      
      title <- sus_translate(title)
      
      scale_singular <- switch(zoom(), "OUT" = sus_translate("borough/city"),
                               "IN" = sus_translate("census tract"),
                               sus_translate("dissemination area"))
    
      scale_plural <- case_when(
        scale_singular == sus_translate("borough/city") ~ 
          sus_translate("boroughs or cities"),
        scale_singular == sus_translate("census tract") ~ 
          sus_translate("census tracts"),
        scale_singular == sus_translate("dissemination area") ~ 
          sus_translate("dissemination areas")
      )
      
      vec <-
        x() %>%
        filter(!is.na(left_variable), !is.na(left_variable_full)) %>%
        pull(left_variable_full)
      
      # Univariate case
      if (var_right() == " ") {
        min_val <- round(min(vec), 2)
        max_val <- round(max(vec), 2)
        mean_val <- round(mean(vec), 2)
        median_val <- round(median(vec), 2)
        sd_val <- sd(vec)
        quant_low <- round(quantile(vec, c(1 / 3, 2 / 3))[1], 2)
        quant_high <- round(quantile(vec, c(1 / 3, 2 / 3))[2], 2)
        
        # Case for no poly selected
        if (is.na(select())) {
          HTML(
            glue(sus_translate(paste0(
              "At the {scale_singular} scale, the {title} varies from ",
              "{min_val} to {max_val}, with an average value of {mean_val} ",
              "and a median value of {median_val}. ",
              "Two thirds of {scale_plural} have a score between {quant_low} ",
              "and {quant_high}."
            )))
          )
          
          # Case for selected poly
        } else {
          dat <- x() %>% filter(ID == select())
          
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
            stringr::str_detect(larger_smaller, sus_translate("larger")) ~ 
              sus_translate("strong"),
            stringr::str_detect(larger_smaller, sus_translate("smaller")) ~ 
              sus_translate("poor"),
            TRUE ~ sus_translate("moderate")
          )
          
          percentile <-
            round(length(vec[vec <= dat$left_variable_full]) / length(vec) * 100)
          
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
                "{prettyNum(dat$population, ',')} and a {title} ",
                "score of {round(poly_value, 2)}, which is {larger_smaller} ",
                "the region-wide median of {median_val}.",
                
                "<p>{place_name} has {poor_strong} potential for active ",
                "living, with a {title} score higher than {percentile}% ",
                "of {scale_plural} in the Montreal region."
              )))
            )
          }
        }
        
        # Bivariate case
      } else {
        var_name <- sus_translate(variable_explanations %>% 
                                    filter(var_code == var_right()) %>%
                                    pull(var_name))
        
        var_explanation <- sus_translate(variable_explanations %>%
                                           filter(var_code == var_right()) %>%
                                           pull(explanation))
        
        correlation <- cor(x()$left_variable_full, x()$right_variable_full) %>%
          round(2)
        
        pos_neg <- if_else(correlation > 0, sus_translate("positive"), 
                           sus_translate("negative"))
        
        strong_weak <- case_when(abs(correlation) > 0.6 ~ sus_translate("strong"),
                                 abs(correlation) > 0.3 ~ sus_translate("moderate"),
                                 TRUE ~ "weak")
        
        higher_lower <- if_else(pos_neg == sus_translate("positive"),
                                sus_translate("higher"),
                                sus_translate("lower"))
        
        high_low_disclaimer <- case_when(
          strong_weak == sus_translate("strong") ~ 
            sus_translate("with only a few exceptions"),
          strong_weak == sus_translate("moderate") ~ 
            sus_translate("although with some exceptions"),
          strong_weak == sus_translate("weak") ~ 
            sus_translate("although with many exceptions"))
        
        # Case for no poly selected
        if (is.na(select())) {
          # If correlation is close to zero
          if (correlation < 0.05 && correlation > -0.05) {
            HTML(
              glue(sus_translate(paste0(
                "<p>{var_explanation}",
                "<p>The {title} has effectively no correlation ",
                "({correlation}) with {var_name} at the ",
                "{scale_singular} scale.",
                "<p>This means that, at the {scale_singular} scale, ",
                "there is no relationship between the two variables."
              )))
            )
          } else {
            HTML(glue(sus_translate(paste0(
              "<p>{var_explanation}",
              "<p>The {title} has a {strong_weak} {pos_neg} ",
              "correlation ({correlation}) with '{tolower(var_name)}' at the ",
              "{scale_singular} scale.",
              "<p>This means that, in general, {scale_plural} with higher ",
              "potential for active living tend to have {higher_lower} ",
              "values for '{tolower(var_name)}', {high_low_disclaimer}.")))
              )
            }
          
          # Case for poly selected
        } else {
          dat <- x() %>% filter(ID == select())
          
          vec_2 <-
            x() %>%
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
                    glue("{place_name} ({dat$name_2})"))
          
          percentile_left <- 
            round(length(vec[vec <= dat$left_variable_full]) / length(vec) * 100)
          
          percentile_right <- 
            round(length(vec_2[vec_2 <= dat$right_variable_full]) / length(vec_2) * 100)
          
          relative_position <- case_when(
            abs(percentile_left - percentile_right) > 50 ~ 
              sus_translate("dramatically different"),
            abs(percentile_left - percentile_right) > 30 ~ 
              sus_translate("substantially different"),
            abs(percentile_left - percentile_right) > 10 ~ 
              sus_translate("considerably different"),
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
                "{prettyNum(dat$population, ',')}, a {title} score ",
                "of {round(poly_value_1, 2)}, and a '{tolower(var_name)}' ",
                "value of {round(poly_value_2, 2)}. ",
                
                "<p>These two scores are {relative_position}, in relative ",
                "terms. {place_name} has a {title} score higher ",
                "than {percentile_left}% of {scale_plural} and ",
                "a '{tolower(var_name)}' score higher than ",
                "{percentile_right}% of {scale_plural} in the ",
                "Montreal region.")))
            )
          }
        }
      }
    })
  })
}
