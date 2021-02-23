# library(sf)
# library(shiny)
# library(shinyWidgets)
# library(shinydashboard)
# library(mapdeck)
# library(tidyverse)

Biodiversity_module_UI <- function(id, i18n) {
 # usei18n(i18n)
  ns <- NS(id)
  
  tabItem(
    tags$head(tags$style(HTML('
          #title_bar_ped {border-width: 10px; border-color: rgb(255, 255, 255);}
          #input_control_right {border-width: 10px;
          border-color: rgba(255,255,255,1);}
          #ped_legend_container {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}'))
          ),
    
    mapdeckOutput(outputId = ns("BiodiversityMap"), height = "1000px"
          ),

    titletextSus_UI(id = ns("title"), i18n  = i18n
                    ,
          title = i18n$t("Biodiversity"),
          textAboveSplit = i18n$t("The whole of an ecosystem is more than the sum of its parts. The health and resilience of our urban green spaces are determined by the quantity, quality, and composition of the species with cohabitat with."),
          textBelowSplit = i18n$t("Montreal's biodiversity is the result of many competing factors...")
          )
    ,

    rightPanelSus_UI(id = ns("Bio_right_panel"),i18n = i18n, var_list_bio_raster= var_list_bio_raster),
  
   #dropoutPanelSus_UI("dropPanel_1", i18n = i18n),
   zoomMenuSus_UI(ns("zoom_menu"), i18n = i18n)
  
  
  )
}

# library(rgdal)
# library(raster)
# library(mapview)
#montreal_ES <- st_read("/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/data/Eco2Urb/montreal-agglomeration-muni-100028-div-vf1-20201125.shp")
load("data/Montreal_Ecosystem_services_by_borough.Rdata")
load("data/Montreal_ecosystem_services_by_hex.Rdata")

# class(montreal_ES)
 st_crs(Montreal_ecosystem_services_by_borough) <- 2950
# #mapview(Montreal_ecosystem_services_by_borough)
  key <- 'pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ'
  Montreal_ES <- Montreal_ecosystem_services_by_borough %>%
   st_transform(4326)

Biodiversity_module_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$BiodiversityMap <- renderMapdeck({
                    mapdeck(style = "mapbox://styles/ttuff/ckg1hclfr0az819plu73iomnn", 
                           token = key,
                           zoom = 22,location = c(-73.5686, 45.5250), pitch = 70, bearing = 15)   
                   
                    })
                 
                 
                 
                 bio_raster <- rightPanelSus_Server(id = "Bio_right_panel", var_list_bio_raster)
                 #bio_raster <- reactive(bio_raster)
                 observe(print(bio_raster))
                 
                 
                 
                 titletextSus_Server(id = "title")
                 #dropoutPanelSus_Server("dropPanel_1", i18n = i18n) #turn imbedded html on and off
                 #observe(print(input$BiodiversityMap_view_change$zoom))
                 zoomer <- reactive(input$BiodiversityMap_view_change$zoom)
                 callModule(zoomMenuSus_Server, "zoom_menu", zoom = zoomer)
                 
                 bio_reactive <- reactiveValues(zoom = 'OUT',
                                                 poly_selected = NA)
                 
                 
                 
                 observeEvent(zoomer(), {
                   #print(rz_pedestrian$zoom)
                   if (zoomer() >= 12.3 &&
                       zoomer() <= 16.3) {
                     bio_reactive$zoom <- 'RASTER_WINDOW'} else {
                       if (zoomer() < 12 &&
                           zoomer() > 9) {
                         bio_reactive$zoom <- 'CHOROPLETH_WINDOW'} else {
                           bio_reactive$zoom <- 'OUT'}}
                 })
                 
                 # Send reactive zoom variable back to the UI
                 # output$zoom <- reactive({
                 #   return(bio_reactive$zoom)
                 #   
                 # })
                 # outputOptions(output, "zoom", suspendWhenHidden = FALSE)
                 # 
                 
                 observe(print(bio_reactive$zoom))
                 observeEvent({
                   bio_reactive$zoom
                   bio_raster()
                 },
                 {
                   print(bio_raster())
                   
                 if( bio_reactive$zoom ==  'CHOROPLETH_WINDOW'){

                 mapdeck_update(map_id = ns("BiodiversityMap"))   %>%
                     clear_polygon("raster1") %>%
                   add_polygon(
                      data = Montreal_ES
                     , na_colour = "#CECECD"
                     ,stroke_colour = "#000000"
                     ,stroke_width = 5
                     ,fill_colour = "can_area_quant3_fill"
                     , fill_opacity = 1
                     , update_view = FALSE
                     , layer_id = "tree_agg_layer"
                     , id = "ID"
                     , auto_highlight = FALSE
                     , highlight_colour = '#00000090'
                     , legend = FALSE
                     , light_settings =  list(
                       lightsPosition = c(0,0, 5000)
                       , numberOfLights = 1
                       , ambientRatio = 1
                     )
                   )
                 } else {mapdeck_update(map_id = ns("BiodiversityMap"))   %>%
                     clear_polygon("tree_agg_layer")}





                   if( bio_reactive$zoom == "RASTER_WINDOW"){

                     
                     
                     mapdeck_update(map_id = ns("BiodiversityMap"))  %>%
                       clear_polygon("tree_agg_layer") %>%
                       add_polygon(
                         data = Montreal_ecosystem_services_by_hex
                         , na_colour = "#000000"
                         ,fill_colour = bio_raster()
                        
                         , fill_opacity = 1
                         , update_view = FALSE
                         , layer_id = "raster1"
                         , id = "ID"
                         , auto_highlight = FALSE
                         , highlight_colour = '#00000090'
                         , legend = FALSE
                         , light_settings =  list(
                           lightsPosition = c(0,0, 5000)
                           , numberOfLights = 1
                           , ambientRatio = 1
                         )
                       )
                       
                   }  else {mapdeck_update(map_id = ns("BiodiversityMap"))   %>%
                       clear_polygon("raster1")}
                 },ignoreInit = TRUE)
                 
              
               }
               
               
  )}




# source("/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/R/titletextSus.R")
# source("/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/R/zoomMenuItemSus.R")
# source("/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/R/zoomMenuSus.R")
# source("/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/R/scrollingTimelingSus.R")
# 
# 
# ui <- fluidPage(
#   Biodiversity_module_UI("biodiversity_module", i18n = i18n)
# )
# 
# server <- function(input, output, session) {
#   Biodiversity_module_server("biodiversity_module")
# }
# 
# shinyApp(ui, server)
