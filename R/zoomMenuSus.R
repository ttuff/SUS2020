# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.
 

 break_data <- as.data.frame( seq(1,20,by=0.1)) %>% rename(c("zoom" = "seq(1, 20, by = 0.1)")) %>%
  mutate(zoom_level = cut(zoom,
                          breaks = c(0,2.1,4.3,6.5,9.5, 12.5,15.5, 18, 20),
                          labels = c("a","b","c","d", "e","f","g","h"),
                          include.lowest = T))

zoomMenuSus_UI <- function(id,i18n, zoom 
){ 
  
  ns <- NS(id)
  
  tagList(
    scrollingTimelineSus_UI(ns("scroll"), i18n =  i18n),
    conditionalPanel(
      condition = "output.zoom_back == 'a'", ns = ns ,
      id = ns("bttn1"),
    zoomMenuItemSus_UI(ns("item1"), i18n =  i18n, from_the_top = 105, 
                       link = "./Rmarkdown_Knits/Biodiversity_module/global/global_biodiversity.html",
                       provided_icon = icon("globe"),provided_label="Globe")),
    conditionalPanel(
      condition = "output.zoom_back == 'b'", ns = ns ,
      id = ns("bttn2"),
    zoomMenuItemSus_UI(ns("item2"), i18n =  i18n, from_the_top = 145, 
                       link = "./Rmarkdown_Knits/Biodiversity_module/individual/continental_biodiversity.html",
                       provided_icon = icon("globe"),provided_label="Continent")),
    conditionalPanel(
      condition = "output.zoom_back == 'c'", ns = ns ,
      id = ns("bttn3"),
    zoomMenuItemSus_UI(ns("item3"), i18n =  i18n, from_the_top = 210, 
                       link = "./Rmarkdown_Knits/Biodiversity_module/individual/Individual_biodiversity.html",
                       provided_icon = icon("globe"),provided_label="Region")),
    conditionalPanel(
      condition = "output.zoom_back == 'd'", ns = ns ,
      id = ns("bttn4"),
    zoomMenuItemSus_UI(ns("item4"), i18n =  i18n, from_the_top = 275, 
                       link = "./Rmarkdown_Knits/Biodiversity_module/individual/Individual_biodiversity.html",
                       provided_icon = icon("globe"),provided_label="Sub-region")),
    conditionalPanel(
      condition = "output.zoom_back == 'e'", ns = ns ,
      id = ns("bttn5"),
    zoomMenuItemSus_UI(ns("item5"), i18n =  i18n, from_the_top = 330, 
                       link = "./Rmarkdown_Knits/Biodiversity_module/individual/Individual_biodiversity.html",
                       provided_icon = icon("globe"),provided_label="Community")),
    conditionalPanel(
      condition = "output.zoom_back == 'f'", ns = ns ,
      id = ns("bttn6"),
    zoomMenuItemSus_UI(ns("item6"), i18n =  i18n, from_the_top = 410, 
                       link = "./Rmarkdown_Knits/Biodiversity_module/individual/Individual_biodiversity.html",
                       provided_icon = icon("globe"),provided_label="Population")),
    conditionalPanel(
      condition = "output.zoom_back == 'g'", ns = ns ,
      id = ns("bttn7"),
    zoomMenuItemSus_UI(ns("item7"), i18n =  i18n, from_the_top = 480, 
                       link = "./Rmarkdown_Knits/Biodiversity_module/park/park_biodiversity.html",
                       provided_icon = icon("globe"),provided_label="Park")),
    conditionalPanel(
      condition = "output.zoom_back == 'h'", ns = ns ,
      id = ns("bttn8"),
    zoomMenuItemSus_UI(ns("item8"), i18n =  i18n, from_the_top = 545, 
                       link = "./Rmarkdown_Knits/Biodiversity_module/individual/Individual_biodiversity.html",
                       provided_icon = icon("globe"),provided_label="Individual"))
    
      
    
  )
}


zoomMenuSus_Server <- function(input, output, session, zoom) {
  observe(print(zoom()))
  zoom_menu_zoom <- reactiveValues(zoom = 'h')
  
  output$zoomBreak <- reactive({
    #break_data
    req(zoom())
    even <- break_data[which(break_data[,1] %in% round(zoom(), digits=1) == TRUE),2]
    print(length(even))
    if(length(even) != 0){
    zoom_menu_zoom$zoom <- even
    }
    return(even)
    
  })
  outputOptions(output, "zoomBreak", suspendWhenHidden = FALSE)
  
  output$zoom_back <- reactive({
    return(zoom_menu_zoom$zoom)
    
  })
  outputOptions(output, "zoom_back", suspendWhenHidden = FALSE)
  
  
  callModule(scrollingTimelineSus_Server, "scroll", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item1", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item2", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item3", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item4", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item5", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item6", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item7", zoom = zoom)
  callModule(zoomMenuItemSus_Server, "item8", zoom = zoom)
  
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
