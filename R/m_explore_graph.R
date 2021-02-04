#### EXPLORE GRAPH MODULE ######################################################

explore_graph_UI <- function(id) {
  plotOutput(NS(id, "explore_graph"), height = 150)
}

explore_graph_server <- function(id, x, select = NULL, title = NULL) {
  stopifnot(is.reactive(x))
  # stopifnot(is.reactive(select))
  # stopifnot(!is.reactive(title))
  
  moduleServer(id, function(input, output, session) {
    
    output$explore_graph <- renderPlot({
      
      ggplot(x()) +
        geom_sf()
      
    }, bg = "white")
  })
}




# output$bivariate_graph <- renderPlot({
# 
#   # Histogram for a single variable
#   if (input$var_right == " ") {
# 
#     # If no poly is selected
#     if (is.na(rv_canale$poly_selected)) {
#       data_canale() %>%
#         filter(!is.na(left_variable)) %>%
#         ggplot(aes(left_variable_full)) +
#         geom_histogram(aes(fill = fill), bins = 25) +
#         scale_fill_manual(
#           values = colors[c(1:3)],
#           na.translate = FALSE
#         ) +
#         labs(x = sus_translate("CanALE index"), y = NULL) +
#         theme_minimal() +
#         theme(
#           legend.position = "none",
#           panel.grid.minor.x = element_blank(),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.y = element_blank()
#         )
# 
#       # If there is an active selection
#     } else {
# 
#       # If the selection is NA
#       if ({
#         data_canale() %>%
#           filter(ID == rv_canale$poly_selected) %>%
#           filter(!is.na(left_variable)) %>%
#           nrow()
#       } == 0) {
#         data_canale() %>%
#           filter(!is.na(left_variable)) %>%
#           ggplot(aes(left_variable_full)) +
#           geom_histogram(bins = 25, fill = colors[3]) +
#           labs(x = "CanALE index", y = NULL) +
#           theme_minimal() +
#           theme(
#             legend.position = "none",
#             panel.grid.minor.x = element_blank(),
#             panel.grid.major.x = element_blank(),
#             panel.grid.minor.y = element_blank()
#           )
# 
#         # If the selection should be plotted
#       } else {
#         data_canale() %>%
#           filter(!is.na(left_variable)) %>%
#           ggplot(aes(left_variable_full)) +
#           geom_histogram(aes(
#             fill = round(left_variable_full) ==
#               round(left_variable_full[ID == rv_canale$poly_selected])
#           ),
#           bins = 25
#           ) +
#           scale_fill_manual(values = colors[c(3, 1)], na.translate = FALSE) +
#           labs(x = "CanALE index", y = NULL) +
#           theme_minimal() +
#           theme(
#             legend.position = "none",
#             panel.grid.minor.x = element_blank(),
#             panel.grid.major.x = element_blank(),
#             panel.grid.minor.y = element_blank()
#           )
#       }
#     }
# 
#     # Scatterplot for two variables
#   } else {
#     var_name <-
#       sus_translate(variable_explanations %>%
#         filter(var_code == input$var_right) %>%
#         pull(var_name))
# 
# 
#     if (nrow(filter(data_canale(), ID == rv_canale$poly_selected)) != 1) {
#       data_canale() %>%
#         drop_na() %>%
#         ggplot(aes(left_variable_full, right_variable_full)) +
#         geom_point(aes(colour = group)) +
#         # geom_smooth(method = "lm", se = FALSE, colour = "grey50") +
#         scale_colour_manual(values = deframe(bivariate_color_scale)) +
#         labs(x = "CanALE index", y = var_name) +
#         theme_minimal() +
#         theme(
#           legend.position = "none",
#           panel.grid.minor.x = element_blank(),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.y = element_blank()
#         )
#     } else {
#       data_canale() %>%
#         drop_na() %>%
#         ggplot(aes(left_variable_full, right_variable_full)) +
#         geom_point(colour = bivariate_color_scale$fill[9]) +
#         # geom_smooth(method = "lm", se = FALSE, colour = "grey50") +
#         geom_point(
#           data = filter(
#             data_canale(), ID == rv_canale$poly_selected,
#             !is.na(left_variable_full),
#             !is.na(right_variable_full)
#           ),
#           colour = bivariate_color_scale$fill[1],
#           size = 3
#         ) +
#         labs(x = "CanALE index", y = var_name) +
#         theme_minimal() +
#         theme(
#           legend.position = "none",
#           panel.grid.minor.x = element_blank(),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.y = element_blank()
#         )
#     }
#   }
# })
