# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.


zoomMenuItemSus_UI <- function(id,
                               i18n, 
                               from_the_top,
                               provided_icon,
                               provided_label
){ 
  
  ns <- NS(id)
  
  tagList(
    tags$i(
      class = "fa fa-globe", 
      style = "color: #3C3C3B"
    ),
   # "www/logo.png"
    absolutePanel(
      id = "timeline", class = "panel panel-default",
      draggable = FALSE, top  = from_the_top, right = 10, 
      width = 120, height = 34,
      style="
      z-index:600;
        color: #000000; 
        background-color: #3C3C3B00; 
        border-color: #0096C900; 
        border-radius: 30px; 
        border-width: 1px;  
        padding:5px; 
        font-size:100%;
      vertical-align: center;
      horizontal-align: left;",
      actionBttn(
        inputId = ns("Id114"),
        label = provided_label,
        style = "gradient",
        color = "default",
        icon = icon("globe"),
        block = FALSE,
        size = "xs"
      )
      ),
      # tags$style(HTML("
      #             #dropPanel {
      #               height:600px;
      #               overflow-y:scroll
      #             }
      #             ")),
    conditionalPanel(
      condition = "output.bttn", 
      ns = ns ,
      id = ns("bttn_panel"),
      absolutePanel(
        id = "dropPanel", class = "panel panel-default",
        draggable = TRUE, top = 240, left = 270, 
        width = "40%", height = "50%",
        style="z-index:600;
       # overflow-y:scroll;
        color: #FFFFFF; 
        background-color: #3C3C3B95; 
        border-color: #0096C995; 
        border-radius: 30px; 
        border-width: 1px;  
        padding:5px; 
        font-size:100%",
        tags$iframe(src = './02-Vignette.html', # put myMarkdown.html to /www
                    width = '100%', height = '100%',
                    frameborder = 0, scrolling = 'auto',
                    style="z-index:600;
       # overflow-y:scroll;
        color: #FFFFFF; 
        background-color: #3C3C3B95; 
        border-color: #0096C995; 
        border-radius: 30px; 
        border-width: 1px;  
        padding:5px; 
        font-size:100%"
        )
        
        )
          
        
        
      )
      
    
  )
}


zoomMenuItemSus_Server <- function(input, output, session, zoom) {
  
 # observe(print(zoom()))
  #observe(print(input$Id114[1]))
  #observe(print(input$bttn1))
  output$bttn <- reactive({
    
    even <- input$Id114[1] %% 2 != 0
    print(even)
    return(even)
    
  })
  outputOptions(output, "bttn", suspendWhenHidden = FALSE)
  
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
