#### LEFT MAP MODULE ###########################################################

left_map_UI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("map_left"), height = 200)
}

left_map_server <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    
    p <-
      ggplot(x) +
      geom_sf(aes(fill = as.factor(ale_class)), color = "white", size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)]), na.value = "grey70") +
      theme_map() +
      theme(legend.position = "none")
    
    output$map_left <- renderPlot({
      ggdraw() +
        # draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
        draw_plot(p) #+
        # draw_image(uni_legend, scale = .45, vjust = 0.25, hjust = 0.25)
    }, bg = "white")
  })
}


# cacheKeyExpr = paste(rz$zoom, "left", sep = "_"),
# cache = diskCache("./app-cache"),
# bg = "white")
