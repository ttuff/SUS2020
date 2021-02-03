#### LEFT MAP MODULE ###########################################################

left_map_UI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("left_map"), height = 200)
}

left_map_server <- function(id, x, cache_id = x) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(cache_id))

  moduleServer(id, function(input, output, session) {
    
    output$left_map <- renderPlot({
      
      p <-
        ggplot(x()) +
        geom_sf(aes(fill = as.factor(left_variable)), color = "white", size = 0.01) +
        scale_fill_manual(values = rev(colors[c(1:3)]), na.value = "grey70") +
        theme_map() +
        theme(legend.position = "none")
      
      ggdraw() +
        draw_image(dropshadow_left, scale = 1.41) +
        draw_plot(p) +
        draw_image(uni_legend, scale = .45, vjust = 0.25, hjust = 0.25)
      
    }, bg = "white") %>% bindCache(cache_id())
  })
}


# cacheKeyExpr = paste(rz$zoom, "left", sep = "_"),
