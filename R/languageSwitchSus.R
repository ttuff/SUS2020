
  # library(shiny)
  # library(shiny.i18n)
  # # for this example to run make sure that you have a translation file # in the same path
  # i18n <- Translator$new(translation_json_path = "/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/www/translation.json")
  # i18n$set_translation_language("fr")

  languageButton_UI <- function(id, i18n) {
    ns <- NS(id)
    absolutePanel(
      id = "language_button", style="z-index:10000;", 
      class = "panel panel-default", top = 10, right = 70, width = 0.01, height = 0.01,
      tagList( usei18n(i18n), 
               actionButton(ns("go"), "English",style="color: #3C3C3B; background-color: #0096C950; border-color: #FFFFFF;border-radius: 100px; border-width: 1px;  padding:7px; font-size:100%")
               
      )
    
    )
  }
  
  languageButton_Server <- function(id, global_session) {
    moduleServer(
      id,
      function(input, output, session) {
        ns <- NS(id)
        observeEvent(input$go,{
          print(input$go[1])
          if((input$go[1] %% 2) != 0){
          #if(input$go != FALSE){
            updateActionButton(session, "go",
                               label = "Français")
            update_lang(global_session, "en")
          } else {
            updateActionButton(session, "go",
                               label = "English")
            update_lang(global_session, "fr")
          }
        })
      }
    )
  }
  
  #  ui <- fluidPage(
  #    languageButton_UI("language_button", i18n = i18n) , 
  #      h2(i18n$t("Hello Shiny!"))
  #  )
  #  
  # server <- function(input, output, session) {
  #   languageButton_Server("language_button", global_session = session)
  # }
  # 
  # shinyApp(ui, server)
  # 
