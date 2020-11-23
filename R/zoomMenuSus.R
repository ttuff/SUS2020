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
    zoomMenuItemSus_UI(ns("item1"), i18n =  i18n, from_the_top = 105, provided_icon = icon("globe"),provided_label="Globe"),
    zoomMenuItemSus_UI(ns("item2"), i18n =  i18n, from_the_top = 145, provided_icon = icon("globe"),provided_label="Continent"),
    zoomMenuItemSus_UI(ns("item3"), i18n =  i18n, from_the_top = 210, provided_icon = icon("globe"),provided_label="Region"),
    zoomMenuItemSus_UI(ns("item4"), i18n =  i18n, from_the_top = 275, provided_icon = icon("globe"),provided_label="Sub-region"),
    zoomMenuItemSus_UI(ns("item5"), i18n =  i18n, from_the_top = 330, provided_icon = icon("globe"),provided_label="Community"),
    zoomMenuItemSus_UI(ns("item6"), i18n =  i18n, from_the_top = 410, provided_icon = icon("globe"),provided_label="Population"),
    zoomMenuItemSus_UI(ns("item7"), i18n =  i18n, from_the_top = 480, provided_icon = icon("globe"),provided_label="Park"),
    zoomMenuItemSus_UI(ns("item8"), i18n =  i18n, from_the_top = 545, provided_icon = icon("globe"),provided_label="Individual")
    
      
    
  )
}


zoomMenuSus_Server <- function(input, output, session, zoom) {
  #observe(print(zoom()))
  
  callModule(scrollingTimelineSus_Server, "scroll", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item1", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item2", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item3", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item4", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item5", zoom = zoom)
  
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
