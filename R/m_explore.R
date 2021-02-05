#### EXPLORE MODULE ############################################################

explore_UI <- function(id) {
  
  tagList(
    fluidRow(column(width = 7, h4(i18n$t("Explore"))),
             column(width = 5, align = "right", 
                    actionLink(inputId = NS(id, "hide"), label = i18n$t("Hide")))),
    
    conditionalPanel(
      condition = "output.hide_status == 1", ns = NS(id),
      info_table_UI(NS(id, "explore")),
      explore_graph_UI(NS(id, "explore")),
      conditionalPanel(
        condition = "output.poly_selected == 1", ns = NS(id),
        actionLink(inputId = NS(id, "clear_selection"),
                   label = "Clear selection")))
    )
}

explore_server <- function(id, x, var_right, select, zoom, title) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select))
  stopifnot(is.reactive(zoom))
  stopifnot(!is.reactive(title))
  
  moduleServer(id, function(input, output, session) {
    
    # Render info table
    info_table_server("explore", x, var_right, select, zoom, title)
    
    # Render the histogram/scatterplot
    explore_graph_server("explore", x, var_right, select, title)
    
    # Hide explore status
    output$hide_status <- reactive(input$hide %% 2 == 0)
    outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$hide, {
      if (input$hide %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Show")
      updateActionButton(session, "hide", label = txt)
    })
    
  })
}
