# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.


dropoutPanelSus_UI <- function(id,i18n
                            , title = i18n$t("Add title as titletextSus_UI(title = 'my title')"), 
                            textAboveSplit = i18n$t("Add text as titletextSus_UI(textAboveSplit = 'short description')"), 
                            textBelowSplit = i18n$t("Add text as titletextSus_UI(textBelowSplit = 'long description')")
){ 
  
  ns <- NS(id)
  
  tagList(
    
    absolutePanel(
      id = "dropPanel", class = "panel panel-default",
      draggable = TRUE, top = 70, right = 270, 
      width = 0, height = 0,
      style="vertical-align: top;
        color: #FFFFFF; 
        background-color: #3C3C3B00; 
        border-color: #0096C900; 
        border-radius: 30px; 
        border-width: 1px;  
        padding:5px; 
        font-size:100%",
      
      dropdown(
        #includeMarkdown("02-Vignette.md"),
        tags$iframe(src = './02-Vignette.html', # put myMarkdown.html to /www
                    width = '100%', height = '800px', 
                    frameborder = 0, scrolling = 'auto'
        ),
        style = "unite", icon = icon("dot-circle"),
        status = "primary", width = "800px",
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInLeftBig,
          exit = animations$fading_exits$fadeOutRightBig
        )
        
      )
     
    )
  )
}

#addResourcePath("www", getwd())
dropoutPanelSus_Server <- function(id,i18n) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
               
               })
}

