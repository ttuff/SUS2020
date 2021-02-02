##### SUS UI SCRIPT ############################################################

dbHeader <- dashboardHeader(tags$li(
  class = "dropdown", 
  tags$style(".main-header {max-height: 50px}"),
  tags$style(".main-header .logo {height: 50px}")),
  title = "SUS",
  titleWidth = "13%")

dbHeader$children[[2]]$children <-
  fluidRow(column(width = 4, loadingLogo(
    'http:www.drtuff.com', 'logo.png', 'spinning_logo.gif', 50, 50, 50)), 
    column(width = 2), column(width = 6 ))

ui <- dashboardPage(
  
  dbHeader,
  
  ## Left sidebar ------------------------------------------------------------
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs", shiny.i18n::usei18n(i18n),
      
      menuItem(i18n$t("SUS Preview"), tabName = "home", icon = icon("balance-scale")),
      
      menuItem(i18n$t("Active living potential"), icon = icon("child"), 
               tabName = "canale", badgeLabel = i18n$t("Built environment"), 
               badgeColor = "purple"),
      
      conditionalPanel(condition = "input.tabs == 'canale'",
                       left_map_UI("canale_left_map")),
      
      menuItem(i18n$t("Commuter mode switching"), icon = icon("biking"), 
               tabName = "mode", badgeLabel = i18n$t("Simulation"),
               badgeColor = "aqua"),
      
      conditionalPanel(condition = "input.tabs == 'mode'", 
                       plotOutput("commuter_map_left", height = 200)),
      
      menuItem(i18n$t("Pedestrian realm"), icon = icon("walking"), 
               tabName = "Pedestrian", badgeLabel = "Covid-19", 
               badgeColor = "red"),
      
      conditionalPanel(condition = "input.tabs == 'Pedestrian'",
                       plotOutput("pedestrian_map_left", height = 200)),
      
      menuItem(i18n$t("Biodiversity"), icon = icon("bug"), 
               tabName = "biodiversity", 
               badgeLabel = i18n$t("Nature-based solutions")),
      
      hr(),
      
      menuItem(i18n$t("Why a dashboard?"), tabName = "why_dashboard"),
      
      menuItem(i18n$t("Meet the team"), tabName = "meet_the_team")), 
    
    collapsed = FALSE),
  
  
  ## Body --------------------------------------------------------------------
  
  dashboardBody(
    waiter::use_waiter(),
    waiter::use_hostess(),
    waiter::waiter_show_on_load(
      html = shiny::tagList(img(src = "Sus logo transparent.png", height = "400px"), shiny::br(),
        shiny::strong(shiny::h4(
          "Please wait, this may take a few minutes", style =
            "color:#002532; ")), shiny::br(), spin_folding_cube(),
        # shiny::span(waiter::hostess_loader("dup_1", preset = "circle",
        #                                    text_color = "#002532",
        #                                    class = "label-center",
        #                                    stroke_color = "#002532",
        #                                    center_page = TRUE))
        ), 
      color = "#D8F5FF"),
    
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png")),
    tags$head(tags$script(HTML(js))),
    tags$head(tags$script(HTML(js2))),
    tags$head(tags$script(HTML(js3))),
    tags$head(tags$style(HTML(styler))),
    
    absolutePanel(
      id = "language_button", 
      style = "z-index:10000; border-color: #FFFFFF00; background-color: #FFFFFF00;", 
      class = "panel panel-default", top = 10, right = 70, width = 0, height = 0,
      tagList(usei18n(i18n), actionButton(
        "language_button", label = "English", 
        style = "color: #3C3C3B; background-color: #0096C950; 
        border-color: #FFFFFF;border-radius: 50px; 
        border-width: 1px;  padding:7px; font-size:100%"))
    ),
    
    tabItems(
      
      # Home page
      tabItem(tabName = "home", fluidPage(
        id = 'home', tags$style('#home {background-color: #FFFFFF;}'),
        fluidRow(img(src = "SUSLOGO.png", height = 600), align = "center"),
        fluidRow(hr()),
        fluidRow(br()),
        fluidRow(img(src = "mssi_logo.png", height = 80), align = "center"),
        fluidRow(HTML(paste0(
          "<h5>An initiative of the <a href = 'https://www.mcgill.ca/mssi/'>McGill ",
          "Sustainability Systems Initiative</a></h5>")), align = "center")
      )), 
      
      # Modules
      tabItem(tabName = "why_dash", why_dash_UI("why_dash")),
      tabItem(tabName = "canale", canale_UI("canale")),
      tabItem(tabName = "Pedestrian", #Pedestrian_realm_module_UI("Pedestrian_realm_module", i18n = i18n)
      ),
      tabItem(tabName = "mode", Mode_switch_module_UI("Mode_switch_module")),
      tabItem(tabName = "biodiversity", Biodiversity_module_UI("biodiversity_module", i18n = i18n)),
      tabItem(tabName = "meet_the_team", Meet_the_team_UI("meet_the_team_module", i18n = i18n))
      
    )
  ),
  
  
  skin = "black", title = "Sus - for sustainable decision making"
)
