#### DID YOU KNOW MODULE ######################################################

dyk <- qread("data/dyk.qs")

dyk_UI <- function(id) {
  
  tagList(
    fluidRow(column(width = 7, h4(i18n$t("Did you know?"))),
             column(width = 5, align = "right",
                    actionLink(inputId = NS(id, "hide"), label = i18n$t("Hide")))),
    conditionalPanel(condition = "output.hide_status == 1", ns = NS(id),
                     htmlOutput(NS(id, "dyk")))
    )
  }

dyk_server <- function(id, var_left, var_right) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  
  moduleServer(id, function(input, output, session) {
    
    output$dyk <- renderUI({
      sus_translate(
        dyk %>% 
          filter(left_variable == var_left(), right_variable == var_right()) %>%
          slice_sample(n = 2) %>%
          pull(text)) %>%
        paste("<li> ", ., collapse = "") %>%
        paste0("<ul>", ., "</ul>") %>%
        HTML()
    })
    
    # Hide DYK status
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
