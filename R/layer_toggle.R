#### SELECT VARIABLE MODULE ####################################################


select_var_UI <- function(id, var_list_bio_raster) {
  selectInput(NS(id, "var"), label = NULL, choices = var_list_bio_raster)
}



select_var_server <- function(id, var_list_bio_raster) {
  stopifnot(!is.reactive(var_list_bio_raster))
  moduleServer(id, function(input, output, session) {
    observe({updateSelectInput(session, "var", 
                               choices = var_list_bio_raster)})
    reactive(input$var)
  })
}