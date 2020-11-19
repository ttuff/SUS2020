# 
#   library(shiny)
#   library(shiny.i18n)
#   # for this example to run make sure that you have a translation file # in the same path
#   i18n <- Translator$new(translation_json_path = "/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/www/translation.json")
#   i18n$set_translation_language("fr")
#   
#   languageButton_UI <- function(id, i18n) {
#     ns <- NS(id)
#   tagList( usei18n(i18n), actionButton(ns("go"), "English"), h2(i18n$t("Hello Shiny!"))
#   )
#   }
#   
#   
#   languageButton_Server <- function(id, i18n) {
#     moduleServer(
#       id,
#       function(input, output, session) {
#         
#         ns <- NS(id)
#         observeEvent(input$go,{
#           print(input$go[1])
#           if((input$go[1] %% 2) != 0){
#             updateActionButton(session, "go",
#                                label = "Français")
#             update_lang(session, "en")  # This line is not updating properly in module
#           } else {
#             updateActionButton(session, "go",
#                                label = "English")
#             update_lang(session, "fr") # This line is not updating properly in module
#             }
#         })
#       }
#     )
#   }
#   
#  
#   
#   
#   ui <- fluidPage(
#     languageButton_UI("language_button", i18n = i18n)
#   )
#   
#   server <- function(input, output, session) {
#     languageButton_Server("language_button", i18n = i18n)
#   }
#   
#   shinyApp(ui, server)
