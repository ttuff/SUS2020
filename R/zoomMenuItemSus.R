# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.


zoomMenuItemSus_UI <- function(id,i18n
){ 
  
  ns <- NS(id)
  
  tagList(
    
    absolutePanel(
      id = "timeline", class = "panel panel-default",
      draggable = TRUE, top  = 150, right  = 200, 
      width = 300, height = 100,
      style="
      z-index:600;
        color: #000000; 
        background-color: #3C3C3B00; 
        border-color: #0096C900; 
        border-radius: 30px; 
        border-width: 1px;  
        padding:5px; 
        font-size:100%;
      vertical-align: top;
      horizontal-align: left;",
      actionBttn(
        inputId = "Id114",
        label = "Global", 
        style = "gradient",
        color = "default",
        icon = icon("globe")
      )),
      tags$style(HTML("
                  #dropPanel {
                    height:600px;
                    overflow-y:scroll
                  }
                  ")),
      absolutePanel(
        id = "dropPanel", class = "panel panel-default",
        draggable = TRUE, top = 300, left = 270, 
        width = "60%", height = "100%",
        style="z-index:10000;
        overflow-y:scroll:
        color: #FFFFFF; 
        background-color: #3C3C3B95; 
        border-color: #0096C995; 
        border-radius: 30px; 
        border-width: 1px;  
        padding:5px; 
        font-size:100%",
        
        includeMarkdown("02-Vignette.md")
          
        
        
      )
      
    
  )
}


zoomMenuItemSus_Server <- function(input, output, session, zoom) {
  
  #observe(print(zoom()))
  
  
  
  
  
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
