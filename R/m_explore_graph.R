#### EXPLORE GRAPH MODULE ######################################################

explore_graph_UI <- function(id) {
  plotOutput(NS(id, "explore_graph"), height = 150)
}

explore_graph_server <- function(id, x, var_right, select, title) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select))
  stopifnot(!is.reactive(title))
  
  moduleServer(id, function(input, output, session) {
    
    output$explore_graph <- renderPlot({
      
      # Histogram for a single variable
      if (var_right() == " ") {
        
        # If no poly is selected
        if (is.na(select())) {
          x() %>%
            filter(!is.na(left_variable)) %>%
            ggplot(aes(left_variable_full)) +
            geom_histogram(aes(fill = fill), bins = 25) +
            scale_fill_manual(
              values = colors[c(1:3)],
              na.translate = FALSE
            ) +
            labs(x = sus_translate(title), y = NULL) +
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
          if (nrow(filter(x(), ID == select(), !is.na(left_variable))) == 0) {
            x() %>%
              filter(!is.na(left_variable)) %>%
              ggplot(aes(left_variable_full)) +
              geom_histogram(bins = 25, fill = colors[3]) +
              labs(x = title, y = NULL) +
              theme_minimal() +
              theme(legend.position = "none",
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.y = element_blank())
            
            # If the selection should be plotted
          } else {
            x() %>%
              filter(!is.na(left_variable)) %>%
              ggplot(aes(left_variable_full)) +
              geom_histogram(aes(fill = round(left_variable_full) ==
                                   round(left_variable_full[ID == select()])),
                             bins = 25) +
              scale_fill_manual(values = colors[c(3, 1)], na.translate = FALSE) +
              labs(x = title, y = NULL) +
              theme_minimal() +
              theme(legend.position = "none",
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.y = element_blank())
            }
          }
        
        # Scatterplot for two variables
      } else {
        var_name <- sus_translate(variable_explanations %>%
                                    filter(var_code == var_right()) %>%
                                    pull(var_name))
        
        if (nrow(filter(x(), ID == select())) != 1) {
          x() %>%
            tidyr::drop_na() %>%
            ggplot(aes(left_variable_full, right_variable_full)) +
            geom_point(aes(colour = group)) +
            scale_colour_manual(values = tibble::deframe(bivariate_color_scale)) +
            labs(x = title, y = var_name) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank())
          
        } else {
          x() %>%
            tidyr::drop_na() %>%
            ggplot(aes(left_variable_full, right_variable_full)) +
            geom_point(colour = bivariate_color_scale$fill[9]) +
            geom_point(data = filter(x(), ID == select(), 
                                     !is.na(left_variable_full),
                                     !is.na(right_variable_full)),
                       colour = bivariate_color_scale$fill[1], size = 3) +
            labs(x = title, y = var_name) +
            theme_minimal() +
            theme(legend.position = "none", 
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank())
          }
      }
    }, bg = "white")
  })
}
