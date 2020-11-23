# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.


scrollingTimelineSus_UI <- function(id,i18n
                               ){ 
  
  ns <- NS(id)
  
  tagList(
    
    absolutePanel(
      id = "timeline", class = "panel panel-default",
      draggable = TRUE, top  = 50, right  = 50, 
      width = 30, height = 600,
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
      plotOutput(ns("timelinePlot"), width = 30, height = 600),
     
      
    )
  )
}

#addResourcePath("www", getwd())
scrollingTimelineSus_Server <- function(input, output, session, zoom) {

                 #observe(print(zoom()))
                initial_position <- 8
    
                 output$timelinePlot <- renderCachedPlot({
                   par(mar=c(0,0,0,0))
                   plot(x=c(0,0), y=c(1,20), type="l", lwd=10, col="#3C3C3B90",  
                        bty="n", xaxt="n", yaxt="n", xlab="", ylab="", 
                        xlim=c(-1,1), ylim=c(-2,22))
                   if(is.null(zoom()) != TRUE){
                   points(0,21-round(zoom(), digits = 2), pch=19, cex=6, col="#3C3C3B90", bg=NA)
                   points(0,21-round(zoom(), digits = 2), pch=19, cex=, col="#3C3C3B90", bty="n")
                   } else {
                     points(0,initial_position, pch=19, cex=6, col="#3C3C3B90", bg=NA)
                     points(0,initial_position, pch=19, cex=, col="#3C3C3B90", bty="n")
                   }
                 }, 
                 cacheKeyExpr = if(is.null(zoom()) != TRUE){round(zoom(), digits = 2)}else{"base"},
                 cache = diskCache("./app-cache"),
                 bg = "transparent")
                 
              
      
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
