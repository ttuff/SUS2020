library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(sf)
library(ggplot2)
library(dplyr)
library(nngeo)
library(stplanr)
library(htmltools)
library(DT)
library(jsonify)
library(mapdeck)

Accessibility_module_UI <- function(id, i18n){
    ns <- NS(id)
    tabItem(
        tags$head(tags$style(HTML('
          #title_bar_commute {border-width: 10px; border-color: rgb(255, 255, 255);}
          #commute_right_bar {border-width: 10px; 
          border-color: rgba(255,255,255,1);}'))),
        #Main Map
        mapdeckOutput(outputId = ns("mymap"),
                      height = "88vh"),
        #Right Panel
        absolutePanel(
            id = "commute_right_bar", style = "z-index:500; max-height: 88vh; overflow-y: auto; overflow-x:hidden; padding:5px;",
            class = "panel panel-default", top = 50, right = 50, width = "17%",bottom = "auto",
            h5(strong("Access to Urban Opportunities")),
            radioGroupButtons(inputId = ns("radio3"),
                              label = "",
                              checkIcon = list(
                                  yes = icon("ok",lib = "glyphicon")),
                              choices = list("Dissemination Area" = 1,"Route Planner" = 2),
                              selected = 1,
                              status = "primary"),
            hr(),
            materialSwitch(inputId = ns("switch1"), label = "Cycling Network", status = "primary", value = FALSE),
            conditionalPanel(
                condition = "input.radio3 == 2",ns = ns,
                selectInput(
                    inputId = ns("select"),
                    label = "Select your destination", 
                    choices = c("COVID-19 Testing Centre", "Health Care", "Grocery Store", "Pharmacy", "Eating Place"),
                    selected = "COVID-19 Testing Centre"),
                radioGroupButtons(inputId = ns("radio2"),
                                  checkIcon = list(
                                      yes = icon("ok",lib = "glyphicon")),
                                  choices = list("Shortest Route" = 1,"Safest Route" = 2),
                                  selected = 1)
            ),
            conditionalPanel(
                condition = "output.route != null & input.radio3 == 2",ns = ns,
                h5(strong("Route Information")),
                DT::DTOutput(ns("table")),
                h5(strong("Elevation Profile")),
                plotOutput(ns("line_plot"), height = 200)
            ),
            conditionalPanel(
                condition = "input.radio3 == 1",ns = ns,
                pickerInput(
                    inputId = ns("variable"),
                    label = "Select a variable:", 
                    choices = list("Travel Time to Closest Health Care" = 1, "Travel Time to Closest Grocery" = 2, "Travel Time to Closest Pharmacy" = 3, "Number of Accessible Eating Places" = 4),
                    selected = 1
                )
            ),
            conditionalPanel(
                condition = "output.da != null & input.radio3 == 1",ns = ns,
                DT::DTOutput(ns("table2"))
            )
            
        ),
        #Text Panel
        absolutePanel(
            id=ns("panel3"),class = "panel panel-default",
            fixed = FALSE, draggable = TRUE,
            top = 55, left = "10%",
            right = "auto", bottom = "auto",
            width = "auto", height = "auto",
            conditionalPanel(
                condition = "input.radio3 == 1",ns = ns,
                htmlOutput(ns("text"))
            )
        )

    )
}

load(file="D:/SUS2020/SUS2020/data/Road_wo_weight.Rdata")
load(file="D:/SUS2020/SUS2020/data/Road_weighted.Rdata")
load(file="D:/SUS2020/SUS2020/data/hospital.Rdata")
load(file="D:/SUS2020/SUS2020/data/grocery.Rdata")
load(file="D:/SUS2020/SUS2020/data/pharmacy.Rdata")
load(file="D:/SUS2020/SUS2020/data/covid.Rdata")
load(file="D:/SUS2020/SUS2020/data/DA.Rdata")
load(file="D:/SUS2020/SUS2020/data/reseau_cyclable.Rdata")

Accessibility_module_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- NS(id)
            #Generate Route Info Table
            Route_Table <- function(route,ratio){
                length <- paste0(sum(round(route$length,1))," m")
                time <- paste0(round((sum(route$length)/4.4704/60),0)," minutes")
                cyc_perc <- round((sum(route[which(route$weight <5),]$length)/sum(route$length)*100),0)
                cyc <- paste0(cyc_perc,"%")
                co2 <- paste0(round((30/160934.4*sum(route$length)),2)," kg")
                dist <- c(time, length, cyc, co2, ratio)
                route_info <- data.frame(c("Journey Time","Route Distance","Cycling Facility", "CO2 Avoided", "Safest Route/Fastest Route"), dist)
                return(route_info)
            }
            output$mymap <- renderMapdeck({
                mapdeck(token = "pk.eyJ1Ijoiemhhb3FpYW8wMTIwIiwiYSI6ImNrYXBnbHB3dTFtbDIycWxvZ285cjNmcG0ifQ.fieGPt1pLEgHs1AI8NvjYg",
                        style = "mapbox://styles/zhaoqiao0120/ckjj0fbc205z319p36spdtnv9",zoom=10.5,location=c(-73.611,45.526))
            })
            ##Legend--------
            df_pal1 <- data.frame(
                col_hosp = c(1,2,3,4,5),
                color_hosp= c('#E8EAF6','#c3dce8','#91bde1','#60a3d0','#4c81bf'),
                stringsAsFactors = F
            )
            
            DA <- left_join(DA, df_pal1, by = "col_hosp")
            
            legend_po1 <- legend_element(
                variables = c("0 - 5","5 - 10","10 - 15","15 - 20","20 - 50"),
                colours = c('#E8EAF6','#c3dce8','#91bde1','#60a3d0','#4c81bf'),
                colour_type = "fill",
                variable_type = "category",
                title = "Travel Time to Closest Health Care Facility (minutes)"
            )
            legend1 <- mapdeck_legend(legend_po1)
            
            df_pal2 <- data.frame(
                col_grocery = c(1,2,3,4,5),
                color_grocery = c('#e5efe5','#cce4c3','#99cd95','#6ab579','#3c9562'),
                stringsAsFactors = F
            )
            
            DA <- left_join(DA, df_pal2, by = "col_grocery")
            
            legend_po2 <- legend_element(
                variables = c("0 - 2","2 - 4","4 - 8","8 - 10","10 - 19"),
                colours = c('#e5efe5','#cce4c3','#99cd95','#6ab579','#3c9562'),
                colour_type = "fill",
                variable_type = "category",
                title = "Travel Time to Closest Grocery Store (minutes)"
            )
            legend2 <- mapdeck_legend(legend_po2)
            
            df_pal3 <- data.frame(
                col_phar = c(1,2,3,4,5),
                color_phar= c('#f1f6bb','#f3d57f','#f5a966','#f6685c','#d93c5f'),
                stringsAsFactors = F
            )
            
            DA <- left_join(DA, df_pal3, by = "col_phar")
            
            legend_po3 <- legend_element(
                variables = c("0 - 2","2 - 5","5 - 7","7 - 10","10 - 24"),
                colours = c('#f1f6bb','#f3d57f','#f5a966','#f6685c','#d93c5f'),
                colour_type = "fill",
                variable_type = "category",
                title = "Travel Time to Closest Pharmacy (minutes)"
            )
            legend3 <- mapdeck_legend(legend_po3)
            
            df_pal4 <- data.frame(
                col_eat = c(1,2,3,4,5),
                color_eat= c('#fbf6bf','#fbda75','#f7ba51','#d18b3d','#8f4a1e'),
                stringsAsFactors = F
            )
            
            DA <- left_join(DA, df_pal4, by = "col_eat")
            
            legend_po4 <- legend_element(
                variables = c("0 - 142","143 - 266","267 - 442","443 - 1010","1010 - 3035"),
                colours = c('#fbf6bf','#fbda75','#f7ba51','#d18b3d','#8f4a1e'),
                colour_type = "fill",
                variable_type = "category",
                title = "Number of Eating Places within 15-minutes Cycling"
            )
            legend4 <- mapdeck_legend(legend_po4)
            
            ##Reset Click Event--------   
            click <- reactiveValues(clicked=NULL)
            da_click <- reactiveValues(da_clicked=NULL)
            
            ##Cycling Network--------
            observeEvent(input$switch1, {
                if(input$switch1 == TRUE){
                    mapdeck_update(map_id = ns("mymap"))  %>%
                        add_path(data = a,
                                 stroke_colour = "#EA3546",
                                 stroke_width = 2,
                                 width_units = "pixels",
                                 layer_id = "network",
                                 update_view = FALSE)
                } else {
                    mapdeck_update(map_id = ns("mymap"))  %>%
                        clear_path(layer_id = "network")
                }
            })
            ##DA Or Route Level-------- 
            observeEvent(input$radio3, {
                if (input$radio3 == 1){
                    da_click$da_clicked <- NULL
                    mapdeck_update(map_id = ns("mymap"))  %>%
                        clear_polygon(layer_id = "base") %>%
                        clear_scatterplot(layer_id ="point") %>%
                        clear_scatterplot(layer_id ="ori_des1") %>%
                        clear_scatterplot(layer_id ="ori_des2") %>%
                        clear_path(layer_id ="route1") %>%
                        clear_path(layer_id ="route2") %>%
                        mapdeck_view(zoom=10.5,location=c(-73.611,45.526))
                    
                } else {
                    da_click$da_clicked <- NULL
                    click$clicked <- NULL
                    mapdeck_update(map_id = ns("mymap"))  %>%
                        clear_legend(layer_id = "choropleth") %>%
                        clear_polygon(layer_id = "choropleth") %>%
                        add_polygon(data = DA,
                                    fill_opacity = 1,
                                    fill_colour = "DAUID",
                                    stroke_opacity = 1,
                                    layer_id = "base",
                                    legend = FALSE,
                                    auto_highlight = FALSE,
                                    update_view = FALSE) %>%
                        mapdeck_view(zoom=11,location=c(-73.611,45.526))
                    
                }
            })
            ##Click Event--------
            observeEvent(input$variable,{
                da_click$da_clicked <- NULL
            })
            observeEvent(input$mymap_polygon_click,{
                js <- input$mymap_polygon_click
                lst <- jsonlite::fromJSON(js)
                da_click$da_clicked <-lst$object$properties
                click$clicked <- lst
                
            })
            output$route <- reactive({
                return(click$clicked)
            })
            output$da <- reactive({
                return(da_click$da_clicked)
            })
            outputOptions(output, "route", suspendWhenHidden = FALSE)
            outputOptions(output, "da", suspendWhenHidden = FALSE)
            
            observe({
                ##DA Level--------
                if (input$radio3 == 1){
                    if (input$variable == 1){
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_polygon(data = DA,
                                        # fill_opacity = 300,
                                        fill_colour = "color_hosp",
                                        stroke_colour = "#868683",
                                        stroke_width = 50,
                                        layer_id = "choropleth",
                                        id = "DAUID",
                                        legend = legend1,
                                        highlight_colour  =  "#FFFFFFFF",
                                        auto_highlight = TRUE,
                                        update_view = FALSE)

                        output$text <- renderUI({
                            HTML("Residents of the Island live an average of 1.8 kms from the nearest health care facility,
                     <br/>that works out to a 7-minutes cycling. Overall, 19% of people live more than
                     <br/>10 minutes away from their nearest health care facility, while 22% live between 5 and
                     <br/>10 minutes away and 59% live less than 5 minutes away. Low-income
                     <br/>households are more likely to live closer to the nearest health care facility.")
                        })
                    } else if (input$variable == 2){
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_polygon(data = DA,
                                        # fill_opacity = 300,
                                        fill_colour = "color_grocery",
                                        stroke_colour = "#868683",
                                        stroke_width = 50,
                                        layer_id = "choropleth",
                                        id = "DAUID",
                                        legend = legend2,
                                        highlight_colour  =  "#FFFFFFFF",
                                        auto_highlight = TRUE,
                                        update_view = FALSE)

                        output$text <- renderUI({
                            HTML("Residents of the Island live an average of 0.4 kms from the nearest grocery store,
                     <br/>that works out to a 2-minutes cycling. Overall, 97% of people live less than
                     <br/>5 minutes away from their nearest grocery store.")
                        })
                    }else if (input$variable == 3){
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_polygon(data = DA,
                                        # fill_opacity = 300,
                                        fill_colour = "color_phar",
                                        stroke_colour = "#868683",
                                        stroke_width = 50,
                                        layer_id = "choropleth",
                                        id = "DAUID",
                                        legend = legend3,
                                        highlight_colour  = "#FFFFFFFF",
                                        auto_highlight = TRUE,
                                        update_view = FALSE)
                        output$text <- renderUI({
                            HTML("Residents of the Island live an average of 0.7 kms from the nearest pharmacy,
                         <br/>that works out to a 3-minutes cycling. Overall, 92% of people live less than
                         <br/>5 minutes away from their nearest pharmacy.")
                        })
                    } else {
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_polygon(data = DA,
                                        fill_colour = "color_eat",
                                        stroke_colour = "#868683",
                                        stroke_width = 50,
                                        layer_id = "choropleth",
                                        id = "DAUID",
                                        legend = legend4,
                                        highlight_colour  = "#FFFFFFFF",
                                        auto_highlight = TRUE,
                                        update_view = FALSE)
                    }
                    ##DA Info Table--------
                    if(!is.null(da_click$da_clicked)){
                        selected <- DA[which(DA$DAUID == da_click$da_clicked$id),]
                        Pop <- selected$Pop2016
                        LI_Rate <- paste0(selected$LI_Rate,"%")
                        Cyc_Rate <- paste0(round(selected$Cyc_Rate,1),"%")
                        Cyc_dent <- paste0(round(selected$cyc_dent,1)," km/sq(km)")

                        info <- c(Pop,LI_Rate,Cyc_Rate, Cyc_dent,
                                  # paste0(selected$to_hosp, " m"), paste0(selected$to_grocery, " m"), paste0(selected$to_phar, "m"),
                                  paste0(selected$time_hos, " minutes"), paste0(selected$time_groc, " minutes"), paste0(selected$time_phar, " minutes"),
                                  selected$eat_15min)
                        DA_info <- data.frame(c("Population (2016)", "Low Income Rate (2016)", "Cycling Rate", "Density of Cycling Facility",
                                                # "Distance to Closest Health Care", "Distance to Closest Grocery","Distance to Closest Pharmacy",
                                                "Travel Time to Closest Health Care", "Travel Time to Closest Grocery",
                                                "Travel Time to Closest Pharmacy",  "Number of Accessible Eating Places"),
                                              info)
                        output$table2 <- renderDT({
                            DT::datatable(DA_info,
                                          rownames = FALSE, colnames = c("",""), filter = "none",
                                          style = "bootstrap",
                                          options = list(
                                              dom = 'b', ordering = FALSE
                                          )
                            )
                        })
                    }
                    
                } 
                ##Route Level--------
                else {
                    ##Route to Health Care--------
                    if(input$select == "Health Care"){
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_scatterplot(
                                data = hospital,
                                radius = 100,
                                radius_max_pixels = 10,
                                radius_min_pixels = 2,
                                fill_colour = "#0084ff80",
                                layer_id = "point",
                                update_view = FALSE,
                            )
                        ##Calculate Route--------
                        if (!is.null(click$clicked)){
                            from_point <- st_point(c(click$clicked$lon,click$clicked$lat))%>%
                                st_sfc(crs = 4326)%>%
                                st_sf()
                            nn <-  st_nn(from_point, hospital, k = 5, progress = FALSE)
                            close_id <- do.call(rbind, nn[1])[,1]
                            nn_point <- hospital[which(hospital$rownumber== close_id),]
                            coords <- od_coords(from_point, nn_point)
                            nodes_near = find_network_nodes(sln = Road_wo_weight, x = as.vector(coords[, c(1, 3)]),
                                                            y = as.vector(coords[, c(2, 4)]))
                            if (nodes_near[1] == nodes_near[2]){
                                length <- 0
                                mapdeck_update(map_id = ns("mymap")) %>%
                                    clear_path(layer_id = "route1")%>%
                                    clear_path(layer_id = "route2")%>%
                                    add_scatterplot(
                                        data = from_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#00FF00",
                                        layer_id = "ori_des1",
                                        update_view = FALSE
                                    )%>%
                                    add_scatterplot(
                                        data = nn_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#ff1e00",
                                        layer_id = "ori_des2",
                                        update_view = FALSE
                                    )

                            } else {
                                route <- route_local(Road_wo_weight,from_point,nn_point)
                                route2 <- route_local(Road_weighted,from_point,nn_point)
                                ratio <- round((sum(route2$length)/sum(route$length)),1)
                                ##Shortest Route--------
                                if (input$radio2 == 1){
                                    route$cumdist <- cumsum(route$length)
                                    route_info <- Route_Table(route,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route2")%>%
                                        add_path(
                                            data = route,
                                            stroke_colour = "#ffa500",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route1",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      style = "bootstrap",
                                                      caption = "Shortest Route",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="chocolate", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                } else {
                                    ##Safesest Route--------
                                    route2$cumdist <- cumsum(route2$length)
                                    route_info <- Route_Table(route2,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route1")%>%
                                        add_path(
                                            data = route2,
                                            stroke_colour = "#3E8534",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route2",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      caption = "Safest Route",
                                                      style = "bootstrap",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route2,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="#3E8534", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                }

                            }
                        }
                    }
                    ##Route to Grocery--------
                    else if (input$select == "Grocery Store") {
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_scatterplot(
                                data = grocery,
                                radius = 100,
                                radius_max_pixels = 10,
                                radius_min_pixels = 2,
                                fill_colour = "#09940b80",
                                layer_id = "point",
                                update_view = FALSE,
                            )


                        if (!is.null(click$clicked)){
                            from_point <- st_point(c(click$clicked$lon,click$clicked$lat))%>%
                                st_sfc(crs = 4326)%>%
                                st_sf()
                            nn <-  st_nn(from_point, grocery, k = 5, progress = FALSE)
                            close_id <- do.call(rbind, nn[1])[,1]
                            nn_point <- grocery[which(grocery$rownumber== close_id),]
                            coords <- od_coords(from_point, nn_point)
                            nodes_near = find_network_nodes(sln = Road_wo_weight, x = as.vector(coords[, c(1, 3)]),
                                                            y = as.vector(coords[, c(2, 4)]))
                            if (nodes_near[1] == nodes_near[2]){
                                length <- 0
                                mapdeck_update(map_id = ns("mymap")) %>%
                                    clear_path(layer_id = "route1")%>%
                                    clear_path(layer_id = "route2")%>%
                                    add_scatterplot(
                                        data = from_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#00FF00",
                                        layer_id = "ori_des1",
                                        update_view = FALSE
                                    )%>%
                                    add_scatterplot(
                                        data = nn_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#ff1e00",
                                        layer_id = "ori_des2",
                                        update_view = FALSE
                                    )

                            } else {
                                route <- route_local(Road_wo_weight,from_point,nn_point)
                                route2 <- route_local(Road_weighted,from_point,nn_point)
                                ratio <- round((sum(route2$length)/sum(route$length)),1)

                                if (input$radio2 == 1){
                                    route$cumdist <- cumsum(route$length)
                                    route_info <- Route_Table(route,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route2")%>%
                                        add_path(
                                            data = route,
                                            stroke_colour = "#ffa500",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route1",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      style = "bootstrap",
                                                      caption = "Shortest Route",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="chocolate", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                } else {
                                    route2$cumdist <- cumsum(route2$length)
                                    route_info <- Route_Table(route2,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route1")%>%
                                        add_path(
                                            data = route2,
                                            stroke_colour = "#3E8534",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route2",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      caption = "Safest Route",
                                                      style = "bootstrap",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route2,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="#3E8534", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                }

                            }

                        }
                    }
                    ##Route to Pharmacy--------
                    else if (input$select == "Pharmacy") {
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_scatterplot(
                                data = pharmacy,
                                radius = 100,
                                radius_max_pixels = 10,
                                radius_min_pixels = 2,
                                fill_colour ="#ff1e0080" ,
                                layer_id = "point",
                                update_view = FALSE,
                            )

                        if (!is.null(click$clicked)){
                            from_point <- st_point(c(click$clicked$lon,click$clicked$lat))%>%
                                st_sfc(crs = 4326)%>%
                                st_sf()
                            nn <-  st_nn(from_point, pharmacy, k = 5, progress = FALSE)
                            close_id <- do.call(rbind, nn[1])[,1]
                            nn_point <- pharmacy[which(pharmacy$rownumber== close_id),]
                            coords <- od_coords(from_point, nn_point)
                            nodes_near = find_network_nodes(sln = Road_wo_weight, x = as.vector(coords[, c(1, 3)]),
                                                            y = as.vector(coords[, c(2, 4)]))
                            if (nodes_near[1] == nodes_near[2]){
                                length <- 0
                                mapdeck_update(map_id = ns("mymap")) %>%
                                    clear_path(layer_id = "route1")%>%
                                    clear_path(layer_id = "route2")%>%
                                    add_scatterplot(
                                        data = from_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#00FF00",
                                        layer_id = "ori_des1",
                                        update_view = FALSE
                                    )%>%
                                    add_scatterplot(
                                        data = nn_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#ff1e00",
                                        layer_id = "ori_des2",
                                        update_view = FALSE
                                    )

                            } else {
                                route <- route_local(Road_wo_weight,from_point,nn_point)
                                route2 <- route_local(Road_weighted,from_point,nn_point)
                                ratio <- round((sum(route2$length)/sum(route$length)),1)

                                if (input$radio2 == 1){
                                    route$cumdist <- cumsum(route$length)
                                    route_info <- Route_Table(route,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route2")%>%
                                        add_path(
                                            data = route,
                                            stroke_colour = "#ffa500",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route1",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      style = "bootstrap",
                                                      caption = "Shortest Route",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="chocolate", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                } else {
                                    route2$cumdist <- cumsum(route2$length)
                                    route_info <- Route_Table(route2,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route1")%>%
                                        add_path(
                                            data = route2,
                                            stroke_colour = "#3E8534",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route2",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      caption = "Safest Route",
                                                      style = "bootstrap",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route2,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="#3E8534", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                }

                            }

                        }
                    }
                    ##Route to Eating Place--------
                    else if (input$select == "Eating Place") {
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_scatterplot(
                                data = eating,
                                radius = 100,
                                radius_max_pixels = 10,
                                radius_min_pixels = 2,
                                fill_colour = "#b60fdb80",
                                layer_id = "point",
                                update_view = FALSE,
                            )

                        if (!is.null(click$clicked)){
                            from_point <- st_point(c(click$clicked$lon,click$clicked$lat))%>%
                                st_sfc(crs = 4326)%>%
                                st_sf()
                            nn <-  st_nn(from_point, eating, k = 5, progress = FALSE)
                            close_id <- do.call(rbind, nn[1])[,1]
                            nn_point <- eating[which(eating$rownumber== close_id),]
                            coords <- od_coords(from_point, nn_point)
                            nodes_near = find_network_nodes(sln = Road_wo_weight, x = as.vector(coords[, c(1, 3)]),
                                                            y = as.vector(coords[, c(2, 4)]))
                            if (nodes_near[1] == nodes_near[2]){
                                length <- 0
                                mapdeck_update(map_id = ns("mymap")) %>%
                                    clear_path(layer_id = "route1")%>%
                                    clear_path(layer_id = "route2")%>%
                                    add_scatterplot(
                                        data = from_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#00FF00",
                                        layer_id = "ori_des1",
                                        update_view = FALSE
                                    )%>%
                                    add_scatterplot(
                                        data = nn_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#ff1e00",
                                        layer_id = "ori_des2",
                                        update_view = FALSE
                                    )

                            } else {
                                route <- route_local(Road_wo_weight,from_point,nn_point)
                                route2 <- route_local(Road_weighted,from_point,nn_point)
                                ratio <- round((sum(route2$length)/sum(route$length)),1)

                                if (input$radio2 == 1){
                                    route$cumdist <- cumsum(route$length)
                                    route_info <- Route_Table(route,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route2")%>%
                                        add_path(
                                            data = route,
                                            stroke_colour = "#ffa500",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route1",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      style = "bootstrap",
                                                      caption = "Shortest Route",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="chocolate", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                } else {
                                    route2$cumdist <- cumsum(route2$length)
                                    route_info <- Route_Table(route2,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route1")%>%
                                        add_path(
                                            data = route2,
                                            stroke_colour = "#3E8534",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route2",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      caption = "Safest Route",
                                                      style = "bootstrap",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route2,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="#3E8534", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                }

                            }

                        }
                    }
                    ##Route to Covid Testing--------
                    else if (input$select == "COVID-19 Testing Centre") {
                        mapdeck_update(map_id = ns("mymap"))  %>%
                            add_scatterplot(
                                data = covid,
                                radius = 100,
                                radius_max_pixels = 10,
                                radius_min_pixels = 2,
                                fill_colour = "#ff8800",
                                fill_opacity = 150,
                                layer_id = "point",
                                update_view = FALSE,
                            )
                        
                        if (!is.null(click$clicked)){
                            from_point <- st_point(c(click$clicked$lon,click$clicked$lat))%>%
                                st_sfc(crs = 4326)%>%
                                st_sf()
                            nn <-  st_nn(from_point, covid, k = 5, progress = FALSE)
                            close_id <- do.call(rbind, nn[1])[,1]
                            nn_point <- covid[which(covid$rownumber== close_id),]
                            coords <- od_coords(from_point, nn_point)
                            nodes_near = find_network_nodes(sln = Road_wo_weight, x = as.vector(coords[, c(1, 3)]),
                                                            y = as.vector(coords[, c(2, 4)]))
                            if (nodes_near[1] == nodes_near[2]){
                                length <- 0
                                mapdeck_update(map_id = ns("mymap")) %>%
                                    clear_path(layer_id = "route1")%>%
                                    clear_path(layer_id = "route2")%>%
                                    add_scatterplot(
                                        data = from_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#00FF00",
                                        layer_id = "ori_des1",
                                        update_view = FALSE
                                    )%>%
                                    add_scatterplot(
                                        data = nn_point,
                                        radius = 200,
                                        radius_max_pixels = 10,
                                        radius_min_pixels = 2,
                                        fill_colour = "#ff1e00",
                                        layer_id = "ori_des2",
                                        update_view = FALSE
                                    )

                            } else {
                                route <- route_local(Road_wo_weight,from_point,nn_point)
                                route2 <- route_local(Road_weighted,from_point,nn_point)
                                ratio <- round((sum(route2$length)/sum(route$length)),1)

                                if (input$radio2 == 1){
                                    route$cumdist <- cumsum(route$length)
                                    route_info <- Route_Table(route,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route2")%>%
                                        add_path(
                                            data = route,
                                            stroke_colour = "#ffa500",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route1",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      style = "bootstrap",
                                                      caption = "Shortest Route",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="chocolate", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                } else {
                                    route2$cumdist <- cumsum(route2$length)
                                    route_info <- Route_Table(route2,ratio)
                                    mapdeck_update(map_id = ns("mymap")) %>%
                                        clear_path(layer_id = "route1")%>%
                                        add_path(
                                            data = route2,
                                            stroke_colour = "#3E8534",
                                            stroke_width = 6,
                                            width_units = "pixels",
                                            layer_id = "route2",
                                            update_view = FALSE,
                                            focus_layer = TRUE
                                        )%>%
                                        add_scatterplot(
                                            data = from_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#00FF00",
                                            layer_id = "ori_des1",
                                            update_view = FALSE
                                        )%>%
                                        add_scatterplot(
                                            data = nn_point,
                                            radius = 200,
                                            radius_max_pixels = 10,
                                            radius_min_pixels = 2,
                                            fill_colour = "#ff1e00",
                                            layer_id = "ori_des2",
                                            update_view = FALSE
                                        )
                                    output$table <- renderDT({
                                        DT::datatable(route_info,
                                                      rownames = FALSE, colnames = c("",""), filter = "none",
                                                      caption = "Safest Route",
                                                      style = "bootstrap",
                                                      options = list(
                                                          dom = 'b', ordering = FALSE
                                                      )
                                        )
                                    })
                                    output$line_plot <- renderPlot({
                                        ggplot(route2,aes(x=cumdist, y=Z_Mean))+
                                            geom_line(color="#3E8534", lwd = 1)+
                                            labs(x = "Distance (m)", y = "Elevation (m)")+
                                            ylim(7,220)+
                                            theme(axis.title=element_text(face="bold"))
                                    })

                                }

                            }

                        }
                    }
                }
                
            })

        }
    )
}    

ui <- fluidPage(
    Accessibility_module_UI("accessibility_module", i18n = i18n)
)
server <- function(input, output, session) {
    Accessibility_module_server("accessibility_module")
}

# Run the application 
shinyApp(ui = ui, server = server)
