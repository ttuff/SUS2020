
# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.


rightPanelSus_UI <- function(id,i18n){ 
  
  ns <- NS(id)
  
  tagList(
    absolutePanel(
      id = ns("input_control_overlay"), style = "z-index:500;",
      class = "panel panel-default", top = 70, right = 50, width = 300,
      materialSwitch(
        inputId = ns("active_extrude"), 
        label = "View in 3D", 
        status = "danger",
        value = FALSE),
      hr(),
      
      # Compare panel
      fluidRow(
        column(width = 8, h4("Compare")),
        column(width = 4, align = "right",
               actionLink(inputId = ns("active_hide_compare"), 
                          label = "Hide"))),
      conditionalPanel(
        condition = "output.active_hide_compare_status == 1", ns=ns,
        selectInput(ns("data_for_plot_right"), label = NULL, 
                    choices = var_list),
        #plotOutput(ns("active_map_right")), height = 250),
      conditionalPanel(
        condition = "input.active_extrude == 0",ns=ns,
        hr(),
        HTML(title_text %>% 
               filter(tab == "active", type == "extra") %>% 
               pull(text))))
      )
    )
  
}



rightPanelSus_Server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 
                 ## Show/hide more info panel in title bar ------------------------------------
                 
                 # More info button
                 
               })
}






