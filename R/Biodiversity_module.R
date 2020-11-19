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
          ),
    
    rightPanelSus_UI(id = ns("Bio_right_panel"),i18n = i18n)
  
    
  
  
  
  )
}

Biodiversity_module_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$BiodiversityMap <- renderMapdeck({
                    mapdeck(style = "mapbox://styles/ttuff/ckg1hclfr0az819plu73iomnn", 
                           token = 'pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ',
                           zoom = 9.2,location = c(-73.65, 45.4), pitch = 35) 
                   #return(output$BiodiversityMap)
                    })
                 titletextSus_Server(id = "title")
                 rightPanelSus_Server(id = "Bio_right_panel")  
               }
  )}