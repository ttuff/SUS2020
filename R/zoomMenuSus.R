# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.


zoomMenuSus_UI <- function(id,i18n
){ 
  
  ns <- NS(id)
  
  tagList(
    scrollingTimelineSus_UI(ns("scroll"), i18n =  i18n),
    zoomMenuItemSus_UI(ns("item"), i18n =  i18n)
    
      
    
  )
}


zoomMenuSus_Server <- function(input, output, session, zoom) {
  #observe(print(zoom()))
  
  callModule(scrollingTimelineSus_Server, "scroll", zoom = zoom)
  
 
  callModule(zoomMenuItemSus_Server, "item", zoom = zoom)
  
}


#  ui <- fluidPage(
#    scrollingTimelineSus_UI("language_button", i18n = i18n)
#  )
# 
# server <- function(input, output, session) {
#   output$count <- renderText(as.character(input$submit))
#   zoomer <- reactive(input$BiodiversityMap_view_change$zoom)
#   callModule(scrollingTimelineSus_Server, "timelinePanel", zoom = zoomer)
# }
# 
# shinyApp(ui, server)
