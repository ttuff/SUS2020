Meet_the_team_UI <- function(id, i18n) {
  ns <- NS(id)
  absolutePanel(
    h2(i18n$t("Meet the team")),
    uiOutput(outputId = ns("meet_the_team_html")),
    left = "30vh", right = "0vh",
    style = "max-height: 88vh; overflow-y: auto;"
  )
    
}



Meet_the_team_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 
                 
                 output$meet_the_team_html <- renderUI({
                   
                   if (sus_reactive_variables$active_language() == "en") {
                     includeMarkdown("R/meet_the_team/meet_the_team_en.md")
                     
                   } else if (sus_reactive_variables$active_language() == "fr") {
                     includeMarkdown("R/meet_the_team/meet_the_team_fr.md")
                   }
                 })
                 
               })
  
}