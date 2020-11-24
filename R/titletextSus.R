# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.


titletextSus_UI <- function(id,i18n
                            , title = i18n$t("Add title as titletextSus_UI(title = 'my title')"), 
                            textAboveSplit = i18n$t("Add text as titletextSus_UI(textAboveSplit = 'short description')"), 
                            textBelowSplit = i18n$t("Add text as titletextSus_UI(textBelowSplit = 'long description')")
                            ){ 
  
  ns <- NS(id)
  
  tagList(
    
    absolutePanel(
      id = "title_bar", class = "panel panel-default",
      draggable = TRUE, top = 70, left = "270", width = "40%",
      style="vertical-align: top;
        color: #FFFFFF; 
        background-color: #3C3C3B98; 
        border-color: #0096C995; 
        border-radius: 30px; 
        border-width: 1px;  
        padding:5px; 
        font-size:100%",
      h1(title),
      p(textAboveSplit),
      actionLink(ns("title_more_info"), i18n$t("Learn more"),
                 style="vertical-align: top;
        color: #B2D235;font-size:110%;" ),
      h1(),
      conditionalPanel(
        condition = "output.title_more_info_status == 1",ns = ns ,
        p(textBelowSplit)
        )
      
    )
  )
}



titletextSus_Server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 
                 ## Show/hide more info panel in title bar ------------------------------------
                 
                 # More info button
                 output$title_more_info_status <- reactive(input$title_more_info %% 2 == 1)
                 outputOptions(output, "title_more_info_status", suspendWhenHidden = FALSE)
                 
                 observeEvent(input$title_more_info, {
print("title more info")
                   if (input$title_more_info %% 2 == 1) txt <- "Hide" else txt <- "Learn more"
                   updateActionButton(session, ns("title_more_info"), label = txt)
                   
                  })
  })
}
 
