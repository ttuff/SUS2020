##### SUS UI SCRIPT ############################################################

dbHeader <- dashboardHeader(tags$li(class = "dropdown",
                                    tags$style(".main-header {max-height: 50px}"),
                                    tags$style(".main-header .logo {height: 50px}")),
                            title = "SUS",
                            titleWidth = "13%")

#dbHeader$children[[2]]$children <-  loadingLogo('http:www.drtuff.com', 'logo.png',
#                                                'spinning_logo.gif', 50, 50, 50)

dbHeader$children[[2]]$children <-  fluidRow(column(width = 4, loadingLogo('http:www.drtuff.com', 'logo.png',
                                                                            'spinning_logo.gif', 50, 50, 50)),
                                             column(width = 2),
                                              column(width = 6 ))


shinyUI(
  
  dashboardPage(

    skin = "black", title="Sus - for sustainable decision making",

    dbHeader,
    ### https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
    
    
    
    ## Left sidebar ------------------------------------------------------------
    
    dashboardSidebar(
     # HTML(as.character(usei18n(i18n))),
      width = 250,
      sidebarMenu(id = "tabs", 
                  shiny.i18n::usei18n(i18n),
                  menuItem(i18n$t("SUS Preview"), tabName = "home", 
                           icon = icon("balance-scale")),
                  
                  # menuItem("Access to green space", icon = icon("envira"),
                  #          tabName = "green", badgeLabel = "new",
                  #          badgeColor = "teal"),
                  
                  menuItem(i18n$t("Active living potential"), icon = icon("child"), 
                           tabName = "active", badgeLabel = i18n$t("Built environment"), 
                           badgeColor = "purple"),
                  conditionalPanel(condition = "input.tabs == 'active'",
                                   plotOutput("active_map_left", 
                                              height = 200)),
                  
                  
                  # 
                  # menuItem("Climate-change risk",
                  #          icon = icon("globe-americas"),
                  #          badgeColor = "yellow"),
                  
                  menuItem(i18n$t("Commuter mode switching"), icon = icon("biking"), 
                           tabName = "mode", badgeLabel = i18n$t("Simulation"), badgeColor = "aqua"),
                  conditionalPanel(condition = "input.tabs == 'mode'", 
                                   plotOutput("commuter_map_left", 
                                              height = 200)),
                  
                  # menuItem("Covid-19", icon = icon("head-side-mask"),
                  #          tabName = "Covid", badgeLabel = "health and safety",
                  #          badgeColor = "red"),
                  # 
                  # menuItem("Economic health", icon = icon("dollar-sign"),
                  #          tabName = "Economic"),
                  # 
                  # menuItem("Energy consumption", icon = icon("fire-alt"),
                  #          tabName = "Energy"),
                  # 
                  # menuItem("Food availability", icon = icon("cheese"),
                  #          tabName = "Food"),
                  # 
                  # menuItem("Housing", icon = icon("home"),
                  #          tabName = "rentals"),
                  # 
                  # menuItem("Land use", icon = icon("warehouse"),
                  #          tabName = "Land"),
                  
                  menuItem(i18n$t("Pedestrian realm"), icon = icon("walking"), 
                           tabName = "Pedestrian", badgeLabel = "Covid-19", 
                           badgeColor = "red"),
                  conditionalPanel(condition = "input.tabs == 'Pedestrian'",
                                   plotOutput("pedestrian_map_left", 
                                              height = 200))
                  ,
                  menuItem(i18n$t("Biodiversity"), icon = icon("bug"),
                           tabName = "biodiversity", badgeLabel = i18n$t("Nature-based solutions")),
                  
                  # menuItem("Public transit", icon = icon("train"),
                  #          tabName = "transit", badgeLabel = "on the ballot",
                  #          badgeColor = "teal"),
                  # 
                  br(),

                  menuItem(i18n$t("Why a dashboard?"), tabName = "why_dashboard"),
                  menuItem(i18n$t("Meet the team"), tabName = "meet_the_team")
                  
      ), collapsed = FALSE),
    
    
    ## Body --------------------------------------------------------------------
    
    dashboardBody(
      
      #add_busy_bar(color = "red", height = "8px"),
       
      tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png")),
      tags$head(tags$script(HTML(js))),
      tags$head(tags$script(HTML(js2))),
      tags$head(tags$script(HTML(js3))),
      tags$head(tags$style(HTML('
      /* logo */
      .skin-black .main-header .logo {
      background-color: #FFFFFF;
      }
      
      /* logo when hovered */
      .skin-black .main-header .logo:hover {
      background-color: #FFFFFF;
      }
      
      /* navbar (rest of the header) */
      .skin-black .main-header .navbar {
      background-color: #FFFFFF;
      }
      
      /* main sidebar */
      .skin-black .main-sidebar {
      background-color: #FFFFFF;
      
      }
      
      /* active selected tab in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
      background-color: #0096C9;
      color: #FFFFFF;
      
      }
      
      /* other links in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu a{
      background-color: #FFFFFF50;
      color: #3C3C3B;
      height: 60px;
      }
      
      /* other links in the sidebarmenu when hovered */
      .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
      background-color: #0096C910;
      }
      
      /* toggle button when hovered  */
      .skin-black .main-header .navbar .sidebar-toggle:hover{
      background-color: #FFFFFF;
      }
      
      /* body */
      .content-wrapper, .right-side {
      background-color: #FFFFFF;
      }
                                '))),
      absolutePanel(
        id = "language_button", style="z-index:10000; border-color: #FFFFFF00; background-color: #FFFFFF00;", 
        class = "panel panel-default", top = 10, right = 70, width = 0, height = 0,
        tagList(usei18n(i18n),
                actionButton("language_button", label = "English",
                             style="color: #3C3C3B; background-color: #0096C950; 
                       border-color: #FFFFFF;border-radius: 50px; 
                       border-width: 1px;  padding:7px; font-size:100%")
                
        )
      ),
      
      
      
      tabItems(
        
        ## Home page -----------------------------------------------------------
        
        tabItem(tabName = "home", 
                fluidPage(id = 'home',
                          tags$style('#home {background-color: #FFFFFF;}'),
                  fluidRow(imageOutput("homepic", height = 600), 
                           align = "center"),
                  fluidRow(hr()),
                  fluidRow(br()),
                  fluidRow(imageOutput("mssipic", height = 80), 
                           align = "center"),
                  fluidRow(HTML(paste0(
                    "<h5>An initiative of the <a href = ", "
                    'https://www.mcgill.ca/mssi/'>McGill ",
                    "Sustainability Systems Initiative</a></h5>" 
                  )),
                    align = "center")
                  )),
        
        
        ## Why a dashboard? ----------------------------------------------------
        
        tabItem(tabName = "why_dashboard",
                tags$head(tags$style(HTML('#why_title_bar {border-width: 10px; 
                                          border-color: rgb(255, 255, 255);}'))),
                absolutePanel(
                  id = "why_title_bar", class = "panel panel-default", 
                  draggable = FALSE, top = 70, left = 270, width = "40%",
                  h2("Why a dashboard? The science behind Sus"),
                  imageOutput("glamour_shot", height = 300),
                  p(i18n$t(paste0("Dashboards offer a tool for communicating ", 
                           "sustainability data in a visually based digital ", 
                           "platform. We see a gap in current dashboards ",
                           "going beyond the visualization of pre-existing ",
                           "data at static scales, leaving room for a more ",
                           "future-oriented, scalable, and interactive model."))),
                  p(i18n$t(paste0("Existing data-driven approaches to urban ",
                           "sustainability are characterized by static data, ",
                           "limited user interaction, and the ",
                           "oversimplification of complex urban issues. ", 
                           "They provide little opportunity for user ", 
                           "engagement and exploration of questions ",
                           "connecting different data and issues."))),
                  p(i18n$t(paste0("Some of the limitations of existing dashboards ",
                           "include a bias towards quantifiable, measurable ",
                           "components of sustainability, and a reliance on ",
                           "data with potential bias. Furthermore, they often ",
                           "attempt to play the role of a neutral force to ",
                           "communicate “objective” information on cities."))),
                  p(i18n$t(paste0("Sustainability dashboards should build upon best ",
                           "practices to provide useful tools for individuals ",
                           "and cities alike to examine the many facets of ", 
                           "urban sustainability and question existing ",
                           "assumptions."))),
                  p(i18n$t(paste0("Maintaining transparency with data and ", 
                           "methodologies, ensuring public participation and ",
                           "accurate representation of underprivileged ", 
                           "communities, and using engaging and accessible ", 
                           "tools contribute to the success of a dashboard."))),
                  p(i18n$t(paste0("Sus aims to more accurately represent and better ", 
                           "engage urban residents in order to harness the ", 
                           "momentum surrounding technologically-based ", 
                           "approaches to sustainability for public good."))),
                  br(),
                  p(i18n$t("Further resources:")),
                  HTML(paste0("<ul><li><a href= ''>Robin Basalaev-Binder ",
                              "and David Wachsmuth. 2020. 'Progress in ",
                              "data-driven urban sustainability'. ",
                  "Working paper.</a> <b>(MSSI research)</b></ul>"))
                  
                  )),
        
        ## Active living potential ---------------------------------------------
        
        tabItem(
          tabName = "active",
          
          CanALE_module_UI("CanALE_module")
        
          ),
        
        ## Pedestrian realm ----------------------------------------------------
        
        tabItem(
          tabName = "Pedestrian",
          
          Pedestrian_realm_module_UI("Pedestrian_realm_module", i18n = i18n)
          
          ),
        
        ## Commuting mode switch -----------------------------------------------
        
        tabItem(
          tabName = "mode",
          
          Mode_switch_module_UI("Mode_switch_module")
          
          ),
        
        ## Biodiversity -----------------------------------------------
        tabItem(
          tabName = "biodiversity",
          
          Biodiversity_module_UI("biodiversity_module", i18n = i18n)
        
          )
        
        
        )
      )
    )
  )
