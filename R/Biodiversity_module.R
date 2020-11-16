Biodiversity_module_UI <- function(id) {
  ns <- NS(id)
  
  mapdeckOutput(outputId = ns("BiodiversityMap"), height = "1000px")
  
}

Biodiversity_module_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 
                 output$BiodiversityMap <- renderMapdeck({
                   mapdeck(style = "mapbox://styles/ttuff/ckg1hclfr0az819plu73iomnn", 
                           token = 'pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ',
                           zoom = 9.2,location = c(-73.65, 45.4), pitch = 35) 
                   
                   #return(output$BiodiversityMap)
                 })
                 
               }
  )}