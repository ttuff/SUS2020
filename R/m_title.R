#### TITLE BAR MODULE ##########################################################

title_UI <- function(id) {
  ns <- NS(id)
  
  absolutePanel(
    id = "title_bar", class = "panel panel-default", style = "padding:5px;",
    draggable = FALSE, top = 70, left = 270, width = "40%",
    uiOutput(ns("title")),
    uiOutput(ns("title_main")),
    actionLink(ns("more_info"), i18n$t("Learn more")),
    conditionalPanel(
      condition = "output.more_info_status == 1", ns = ns,
      uiOutput(ns("title_extra"))
    )
  )
}

title_server <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    
    title <- filter(title_text, tab == x)
    
    # More info
    output$more_info_status <- reactive(input$more_info %% 2 == 1)
    outputOptions(output, "more_info_status", suspendWhenHidden = FALSE)

    observe({
      if (input$more_info %% 2 == 1) {
        txt <- sus_translate("Hide")
      } else {
        txt <- sus_translate("Learn more")
      }
      updateActionButton(session, "more_info", label = txt)
    })
    
    output$title <- 
      renderUI(h2(sus_translate(filter(title, type == "title") %>% pull(text))))
    
    output$title_main <- 
      renderUI(p(sus_translate(filter(title, type == "main") %>% pull(text))))
    
    output$title_extra <-
      renderUI(HTML(
        sus_translate(
        filter(title, type == "extra") %>% pull(text)
        )
      ))
  })
}
