#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(mapdeck)
library(DT)
library(dplyr)


loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
}

qz <- reactiveValues(zoom_level = 'IN')
# Define UI for application that draws a histogram
ui <- fluidPage(
    # tags$head(
    #     tags$style(
    #         HTML(".shiny-notification {
    #          position:fixed;
    #          top: calc(10%);
    #          left: calc(25%);
    #          }
    #          "
    #         )
    #     )
    # ),
    mapdeckOutput(outputId = "qzmyMap",
                  height = "800px"),
    absolutePanel(
        id = "controls", class = "panel panel-default",
        draggable = FALSE, top = 55, left = "4%",
        right = "auto", bottom = "auto",
        width = 0, height = 0,
        dropdownButton(
            label = "",
            inputId = "drop",
            icon = icon("gear"),
            status = "primary",
            circle = TRUE,
            width = 330,
            h4(strong("Modal Shift Scenarios")),
            radioGroupButtons("radio1",label = "Predefined Scenarios",
                              checkIcon = list(
                                  yes = tags$i(class = "fa fa-check-square", 
                                               style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-square-o", 
                                              style = "color: steelblue")),
                              choices = list("Scenario 1" = 1,"Scenario 2" = 2, "Reset" = 3),
                              selected = 3),
            sliderTextInput(
                inputId = "slider1",
                label = "Cycling Distance (km):", 
                choices = seq(from = 1,
                              to = 10,
                              by = 0.1),
                grid = TRUE
            ),
            
            sliderTextInput(
                inputId = "slider2",
                label = "Elevation Gain (m):", 
                choices = seq(from = 10,
                              to = 55,
                              by = 5),
                grid = TRUE
            ),
            sliderTextInput(
                inputId = "slider3",
                label = "Time Ratio:", 
                choices = seq(from = 1.0,
                              to = 3.0,
                              by = 0.2),
                grid = TRUE
            ),
            # materialSwitch(inputId = "switch1", label = "Modelled Cycling Route", status = "primary", value = FALSE),
            hr(),
            materialSwitch(inputId = "switch2", label = "Cycling Network", status = "primary", value = TRUE)
            
        )
    ),
    absolutePanel(
        id="panel1",
        style="z-index:500;",
        class = "panel panel-default",
        draggable = FALSE, 
        top = 60, right = 50,
        widtth=60,
        conditionalPanel(
            condition = "output.zoom_level == 'ISO'",
            h4(strong("Choropleth Map")),
            pickerInput(
                inputId = "variable",
                label = "Select a variable:", 
                choices = list("Share of Car Trips" = 2, "Average Commuting Distance" = 3, "Access to Cycling Infrastructure" = 1),
                selected = 2
            ),
            knobInput(
                inputId = "knob1",
                label = "Car Share by Origin Census Tract:",
                step = 0.5,
                min = 4,
                max = 17,
                value = 17,
                displayPrevious = TRUE,
                lineCap = "round",
                fgColor = "#B2D235",
                inputColor = "#B2D235"
            )
        ),
        conditionalPanel(
            condition = "output.zoom_level == 'OUT' & input.radio1 <3",
            h4(strong("VMT Reduction")),
            DT::DTOutput("table")
        )
    )
)
server <- function(input, output, session) {
    cycling1 <- loadRData("Rdata/car_1_finals.Rdata")
    cycling2 <- loadRData("Rdata/car_3_finals.Rdata")
    cycling_network <- loadRData("Rdata/reseau_cyclable.Rdata")
    car_share <- loadRData("Rdata/Car_Share.Rdata")
    cycling_access <- loadRData("Rdata/Cycling_Access.Rdata")
    trip_distance <- loadRData("Rdata/Trip_Distance.Rdata")
    scenario1 <- data.frame(c("Criteria: Cycling Distance (km)","Potential Cyclable Trips (per day)", "VMT Savings (per day)"), c(4.4, 60460, 102862))
    scenario2 <- data.frame(c("Criteria: Cycling Distance (km)","Criteria: Elevation Gain (m)", "Criteria: Time Ratio","Potential Cyclable Trips (per day)", "VMT Savings (per day)"), c(4.4,45,2.4, 44205, 72992))
    ###########legend#####
    df_pal1 <- data.frame(
        color = c(1,2,3,4,5),
        color_value = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
        stringsAsFactors = F
    )
    
    cycling_access <- left_join(cycling_access, df_pal1, by = "color")
    
    legend_po1 <- legend_element(
        variables = c("0 - 0.87","0.88 - 1.91","1.92 - 3.08","3.09 - 4.61","4.62 - 16.8"),
        colours = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
        colour_type = "fill",
        variable_type = "category",
        title = "Access to Cycling Infrastructure (km/sq.km)"
    )
    legend1 <- mapdeck_legend(legend_po1)
    
    df_pal2 <- data.frame(
        color = c(1,2,3,4,5),
        color_value = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
        stringsAsFactors = F
    )
    
    car_share <- left_join(car_share, df_pal2, by = "color")
    
    legend_po2 <- legend_element(
        variables = c("4% - 21%","22% - 33%","34% - 47%","48% - 61%","62% - 91%"),
        colours = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
        colour_type = "fill",
        variable_type = "category",
        title = "Share of Car Trips by Origin (%)"
    )
    legend2 <- mapdeck_legend(legend_po2)
    
    df_pal3 <- data.frame(
        color = c(1,2,3,4,5),
        color_value = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
        stringsAsFactors = F
    )

    trip_distance <- left_join(trip_distance, df_pal3, by = "color")

    legend_po3 <- legend_element(
        variables = c("2.3 - 6.4","6.5 - 7.8","7.9 - 8.9","9.0 - 10.4","10.5 - 22.6"),
        colours = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
        colour_type = "fill",
        variable_type = "category",
        title = "Average Commuting Distance (km)"
    )
    legend3 <- mapdeck_legend(legend_po3)
    ########Output#######
    output$qzmyMap <- renderMapdeck({
        mapdeck(token = "pk.eyJ1Ijoiemhhb3FpYW8wMTIwIiwiYSI6ImNrYXBnbHB3dTFtbDIycWxvZ285cjNmcG0ifQ.fieGPt1pLEgHs1AI8NvjYg",
                style = "mapbox://styles/zhaoqiao0120/ckh1hkzwe02br19nvzt9bvxcg", zoom=10,location=c(-73.611,45.526))
    })
    observeEvent(input$qzmyMap_view_change$zoom, {
        if( input$qzmyMap_view_change$zoom > 10){qz$zoom_level <- 'OUT'} else {
            qz$zoom_level <- 'ISO'}}
    )
    
    output$zoom_level <- reactive({
        
        return(qz$zoom_level)
        
    })
    
    outputOptions(output, "zoom_level", suspendWhenHidden = FALSE)
    
    observeEvent(input$radio1, {
        if(input$radio1 == 1){
            updateSliderTextInput(session = session,
                                  inputId = "slider1",
                                  selected = 4.4)
            updateSliderTextInput(session = session,
                                  inputId = "slider2",
                                  selected = 55)
            updateSliderTextInput(session = session,
                                  inputId = "slider3",
                                  selected = 3)
            # showNotification("yayaya",
            #                  type = "message", duration = 3)
            }
      
        else if (input$radio1 == 2){
            updateSliderTextInput(session = session,
                                  inputId = "slider1",
                                  selected = 4.4)
            updateSliderTextInput(session = session,
                                  inputId = "slider2",
                                  selected = 45)
            updateSliderTextInput(session = session,
                                  inputId = "slider3",
                                  selected = 2.4)
            # showNotification("A potentially cyclable trip:\nA car trip where the cycling distance between its origin and destination is shorter than 4.4 kilometers",
            #                  type = "message", duration = 3)
            }
            
        }
    )
    observeEvent(input$switch2, {
        if(input$switch2 == TRUE){
            mapdeck_update(map_id = "qzmyMap")  %>%
                add_path(data = cycling_network,
                         stroke_colour = "#EA3546",
                         stroke_width = 150,
                         layer_id = "network",
                         update_view = FALSE)
        } else {
            mapdeck_update(map_id = "qzmyMap")  %>%
                clear_path(layer_id = "network")
        }
    })
    observeEvent(input$variable,{
        if(input$variable == 1){
            updateKnobInput(session = session,
                            inputId = "knob1",
                            label = "Access to Cycling Infrastructure (km/sq.km):",
                            options = list(
                                step = 0.5,
                                min = 4,
                                max = 17,
                                displayPrevious = TRUE,
                                lineCap = "round",
                                fgColor = "#B2D235",
                                inputColor = "#B2D235"))
            updateKnobInput(session = session,
                            inputId = "knob1",
                            value = 17
            )
        } else if(input$variable == 2){
            updateKnobInput(session = session,
                            inputId = "knob1",
                            label = "Car Share by Origin Census Tract (%):",
                            options = list(
                                step = 1,
                                max = 91,
                                min = 4,
                                lineCap = "round",
                                fgColor = "#1983B0",
                                inputColor = "#1983B0"
                            ))
            updateKnobInput(session = session,
                            inputId = "knob1",
                            value = 91
                            )
        } else {
            updateKnobInput(session = session,
                            inputId = "knob1",
                            label = "Average Commuting Distance (km):",
                            options = list(
                                step = 0.5,
                                max = 23.0,
                                min = 3.5,
                                lineCap = "round",
                                fgColor = "#C56F34",
                                inputColor = "#C56F34"
                            )
            )
            updateKnobInput(session = session,
                            inputId = "knob1",
                            value = 23.0
            )
        }
    })
    
    observe({
        
        if( qz$zoom_level == "ISO"){
            updateMaterialSwitch(session = session,
                                 inputId = "switch2",
                                 value = FALSE)
            if (input$variable == 1) {
                cycling_access_select <- cycling_access[which(cycling_access$cycling_ac <= input$knob1),]
                mapdeck_update(map_id = "qzmyMap")  %>%
                    #clear_polygon(layer_id = "choropleth")%>%
                    clear_path(layer_id = "cyclable") %>%
                    add_polygon(data = cycling_access_select,
                                fill_opacity = 150,
                                fill_colour = "color_value",
                                stroke_colour = "#868683",
                                stroke_width = 100,
                                layer_id = "choropleth",
                                legend = legend1,
                                highlight_colour  =  "#AAFFFFFF",
                                auto_highlight = TRUE,
                                update_view = FALSE)
            } else if (input$variable == 2) {
                car_share_select <- car_share[which(car_share$Car_per <= input$knob1),]
                mapdeck_update(map_id = "qzmyMap")  %>%
                    #clear_polygon(layer_id = "choropleth")%>%
                    clear_path(layer_id = "cyclable") %>%
                    add_polygon(data = car_share_select,
                                fill_opacity = 150,
                                fill_colour = "color_value",#car_share
                                stroke_colour = "#CCD1D1",
                                stroke_width = 100,
                                layer_id = "choropleth",
                                legend = legend2,
                                highlight_colour  =  "#AAFFFFFF",
                                auto_highlight = TRUE,
                                update_view = FALSE)
            } else {
                trip_distance_select <- trip_distance[which(trip_distance$avg_dist <= input$knob1),]
                mapdeck_update(map_id = "qzmyMap")  %>%
                    #clear_polygon(layer_id = "choropleth")%>%
                    clear_path(layer_id = "cyclable") %>%
                    add_polygon(data = trip_distance_select,
                                fill_opacity = 150,
                                fill_colour = "color_value",#car_share
                                stroke_colour = "#CCD1D1",
                                stroke_width = 100,
                                layer_id = "choropleth",
                                legend = legend3,
                                highlight_colour  =  "#AAFFFFFF",
                                auto_highlight = TRUE,
                                update_view = FALSE)
            }
            
            
        }
        if(qz$zoom_level == "OUT") {
            
            # updateMaterialSwitch(session = session,
            #                      inputId = "switch2",
            #                      value = TRUE)
            if(input$radio1 == 1){
                mapdeck_update(map_id = "qzmyMap")  %>%
                    clear_polygon(layer_id = "choropleth") %>%
                    add_path(data = cycling1,
                             stroke_width  = "total_car",
                             stroke_colour = "#0061FF80",
                             layer_id = "cyclable",
                             update_view = FALSE)
                
                output$table <- renderDT({
                    DT::datatable(scenario1,
                        rownames = FALSE, colnames = c("",""), filter = "none",
                        style = "bootstrap",
                        options = list(
                            dom = 'b', ordering = FALSE
                        )
                    )
                })
            } else if (input$radio1 == 2){
                mapdeck_update(map_id = "qzmyMap")  %>%
                    clear_polygon(layer_id = "choropleth") %>%
                    add_path(data = cycling2,
                             stroke_width  = "total_car",
                             stroke_colour = "#722AEE80",
                             layer_id = "cyclable",
                             update_view = FALSE)
                output$table <- renderDT({
                    DT::datatable(scenario2,
                                  rownames = FALSE, colnames = c("",""), filter = "none",
                                  style = "bootstrap",
                                  options = list(
                                      dom = 'b', ordering = FALSE
                                  )
                    )
                })
            } else {
                mapdeck_update(map_id = "qzmyMap")  %>%
                    clear_polygon(layer_id = "choropleth") %>%
                    clear_path(layer_id = "cyclable")
            }
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



