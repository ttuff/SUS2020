# Shiny options -----------------------------------------------------------

shinyOptions(cache = diskCache("./app-cache"))

# Packages ----------------------------------------------------------------

#getDependencies("mapboxapi", installed=TRUE, available=FALSE)

library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinyjqui)
library(shinyWidgets)
library(shinythemes)
library(extrafont)
library(shiny.i18n)
library(waiter)

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(gghighlight)
library(ggthemes)

library(sf)
library(mapdeck) 
library(mapboxapi)
library(geojsonsf)
library(jsonify)
library(raster)
library(RColorBrewer)

library(markdown)
library(png)
library(cowplot)
library(classInt)
library(scales)

library(DT)
library(qs)
library(glue)
library(aniview)
library(data.table)

#library(shinipsum)
# library(fakir)
#library(googleLanguageR)
#library(shinyanimate)
# library(shinycssloaders)


# Translation -------------------------------------------------------------

i18n <- Translator$new(translation_csvs_path = "translations/")
# change this to the target language
i18n$set_translation_language("fr")
print(i18n$t("Learn more"))
# load translations
translation_fr <- read_csv("translations/translation_fr.csv")

# creation o r to store all our reactive values like active_language which will
# be called with sus_reactive_variables$active_language()
sus_reactive_variables <- reactiveValues() # r to store all our reactive values


# When debugging, switch to TRUE and get detailed error log on server
options(shiny.trace = FALSE)


# Functions ---------------------------------------------------------------

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

theme_map <- function(...) {
  default_background_color <- "transparent"
  default_font_color <- "black"
  default_font_family <- "Helvetica"
  
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family, color = default_font_color),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = default_background_color, color = NA),
      panel.background = element_rect(fill = default_background_color, color = NA),
      legend.background = element_rect(fill = default_background_color, color = NA),
      legend.position = "none",
      plot.margin = unit(c(0, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 22, hjust = 0, color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5, color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = default_font_color,
                                   margin = margin(b = -0.1, t = -0.1, l = 2, unit = "cm"),
                                   debug = F),
      plot.caption = element_text(size = 7, hjust = .5, 
                                  margin = margin(t = 0.2, b = 0, unit = "cm"),
                                  color = "#939184"),
      ...)
}


# Colours -----------------------------------------------------------------

bivariate_color_scale <- tibble(
  "3 - 3" = "#2A5A5B", "2 - 3" = "#567994", "1 - 3" = "#6C83B5", 
  "3 - 2" = "#5A9178", "2 - 2" = "#90B2B3", "1 - 2" = "#B5C0DA",
  "3 - 1" = "#73AE80", "2 - 1" = "#B8D6BE", "1 - 1" = "#E8E8E8") %>%
  pivot_longer(everything(), "group", "fill")

color_scale <- tibble(
  "6" = "#73AE80", "5" = "#B8D6BE", "4" = "#E8E8E8", "3" = "#6C83B5",
  "2" = "#B5C0DA", "1" = "#E8E8E8") %>%
  pivot_longer(everything(), "group", "fill")

color_scale_2 <- tibble(
  "3" = "#73AE80", "2" = "#B8D6BE", "1" = "#E8E8E8") %>%
  pivot_longer(everything(), "group", "fill")

colors <- as.character(color_scale$fill)

default_background_color <- "transparent"
default_font_color <- "black"
default_font_family <- "Helvetica"


# Animation ---------------------------------------------------------------

animateCSS <- function(effect, delay = 0, duration = 500, then = NULL) {
  
  effect <- match.arg(effect, c(
    "bounce", "flash", "pulse", "rubberBand", "shakeX", "shakeY", "headShake",
    "swing", "tada", "wobble", "jello", "heartBeat", "backInDown",
    "backInLeft", "backInRight", "backInUp", "backOutDown", "backOutLeft",
    "backOutRight", "backOutUp", "bounceIn", "bounceInDown", "bounceInLeft",
    "bounceInRight", "bounceInUp", "bounceOut", "bounceOutDown",
    "bounceOutLeft", "bounceOutRight", "bounceOutUp", "fadeIn", "fadeInDown",
    "fadeInDownBig", "fadeInLeft", "fadeInLeftBig", "fadeInRight",
    "fadeInRightBig", "fadeInUp", "fadeInUpBig", "fadeInTopLeft",
    "fadeInTopRight", "fadeInBottomLeft", "fadeInBottomRight", "fadeOut",
    "fadeOutDown", "fadeOutDownBig", "fadeOutLeft", "fadeOutLeftBig",
    "fadeOutRight", "fadeOutRightBig", "fadeOutUp", "fadeOutUpBig",
    "fadeOutTopLeft", "fadeOutTopRight", "fadeOutBottomRight",
    "fadeOutBottomLeft", "flip", "flipInX", "flipInY", "flipOutX", "flipOutY",
    "lightSpeedInRight", "lightSpeedInLeft", "lightSpeedOutRight",
    "lightSpeedOutLeft", "rotateIn", "rotateInDownLeft", "rotateInDownRight",
    "rotateInUpLeft", "rotateInUpRight", "rotateOut", "rotateOutDownLeft",
    "rotateOutDownRight", "rotateOutUpLeft", "rotateOutUpRight", "hinge",
    "jackInTheBox", "rollIn", "rollOut", "zoomIn", "zoomInDown", "zoomInLeft",
    "zoomInRight", "zoomInUp", "zoomOut", "zoomOutDown", "zoomOutLeft",
    "zoomOutRight", "zoomOutUp", "slideInDown", "slideInLeft", "slideInRight",
    "slideInUp", "slideOutDown", "slideOutLeft", "slideOutRight",
    "slideOutUp"))
  
  js <- paste(
    "    $this.animateCSS('%s', {",
    "      delay: %d,",
    "      duration: %d,",
    "      callback: function(){",
    "        %s",
    "      }",
    "    });",
    sep = "\n"
  )
  
  sprintf(js, effect, delay, duration, ifelse(is.null(then), "", then))
}

onShowJS <- function(animation, fadeDuration){
  sprintf(paste(
    "$('#%%s>div').on('show', function(){",
    "  var $this = $(this);",
    "  $this.css('opacity', 0).animate({opacity: 1}, %d, function(){",
    animation,
    "  });",
    "});",
    sep = "\n"
  ), fadeDuration)
}

onHideJS <- function(animation, fadeDuration){
  paste(
    "$('#%s>div').on('hide', function(){",
    "  var $this = $(this);",
    "  setTimeout(function(){",
    sub(
      "^(\\s.*?\\$this\\.animateCSS)",
      "$this.show().animateCSS",
      sub(
        "\\{\n        \n      \\}",
        sprintf("{$this.hide(%d);}", fadeDuration),
        animation
      )
    ),
    "  }, 0);",
    "});",
    sep = "\n"
  )
}

animatedConditionalPanel <-
  function(condition, ..., onShow = NULL, fadeIn = 600, onHide = NULL, 
           fadeOut = 400) {
    id <- paste0("animateCSS-", stringi::stri_rand_strings(1, 15))
    jsShow <- ifelse(!is.null(onShow), sprintf(onShowJS(onShow, fadeIn), id), "")
    jsHide <- ifelse(!is.null(onHide), sprintf(onHideJS(onHide, fadeOut), id), "")
    script <- tags$script(HTML(paste(jsShow,jsHide,sep = "\n")))
    condPanel <- conditionalPanel(condition, ...)
    tags$div(id = id, tagList(condPanel, script))
  }


# Drop down list for variable selection -----------------------------------

#Pedestrian realm
var_list_ped <- list("Walkable Access to Key Amenities" = 
                       "agg_proximity_score",
                     "Net Median Income" = 
                       "net_median_income",
                     "Visible Minority Population Proportion" = 
                       "minority_percent", 
                     "Immigrant Population Proportion" = 
                       "immigrant_percent")

var_list_slider <- list("Population density per square km" = 1, 
                        "Pedestrian social distancing capacity" = 2, 
                        "Work commutes by car (%)" = 3, 
                        "Trajet MTL 2016 data on pedestrian flows" = 4)

loadingLogo <- 
  function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
    tagList(
      tags$head(
        tags$script(
          "setInterval(function() {
        if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show();
        $('div.notbusy').hide();
        } else {
        $('div.busy').hide();
        $('div.notbusy').show();
        }
        },100)")
      ),
      tags$a(href = href,
             div(class = "busy",  
                 img(src = loadingsrc, height = height, width = width, alt = alt)),
             div(class = 'notbusy',
                 img(src = src, height = height, width = width, alt = alt))
      )
    )
  }


# Load data ---------------------------------------------------------------
## THESE ALL NEED TO BE TURNED INTO QS BINARIES

title_text <- qread("data/title_text.qs")

did_you_know <- 
  read_csv("data/did_you_know.csv") %>% 
  mutate(right_variable = if_else(is.na(right_variable), " ", right_variable))

variable_explanations <- 
  fread("data/variable_explanations.csv")

# Load data for pedestrian realm 
load(file = "data/sidewalks_WSG.Rdata")
load(file = "data/census_circular.Rdata")
load(file = "data/original_VAS_plan.Rdata")
load(file = "data/revised_VAS_plan.Rdata")
load(file = "data/census_analysis_quantile.Rdata")
load(file = "data/census_analysis_ct.Rdata")

census_analysis_quantile_WSG <- census_analysis_quantile %>% 
  st_transform(4326)

#mode
cycling1 <- loadRData("data/car_1_finals.Rdata")
cycling2 <- loadRData("data/car_3_finals.Rdata")
cycling_network <- loadRData("data/reseau_cyclable.Rdata")
car_share <- loadRData("data/Car_Share.Rdata")
cycling_access <- loadRData("data/Cycling_Access.Rdata")
trip_distance <- loadRData("data/Trip_Distance.Rdata")

load("data/cycling_total_final.Rdata")

dropshadow1 <- normalizePath(file.path("www/dropshadow1.png"))
dropshadow2 <- normalizePath(file.path("www/dropshadow2.png"))

uni_legend <- normalizePath(file.path("www/Univariate_left.png"))
uni_legend_right <- normalizePath(file.path("www/Univariate_right.png"))


# Other prep --------------------------------------------------------------

js_ped_1 <- "$(document).ready(function(){
  $('#plotContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js_ped <- "$(document).ready(function(){
  $('#plotContainer_ped').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js_ped_control <- "$(document).ready(function(){
  $('#plotContainer_ped_control').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js <- "
  $(document).ready(function(){
  $('#plotContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js2 <- "
$(document).ready(function(){
  $('#menuContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
       $(this).css('opacity', 1).animate({opacity: 0}, {duration: 1000});
    })
  });
});
"


js3 <- "
$(document).ready(function(){
  $('#plotContainer2').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
       $(this).css('opacity', 1).animate({opacity: 0}, {duration: 1000});
    })
  });
});
"

styler <- '
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
                                '


# Establish reactiveValues ------------------------------------------------

#qz <- reactiveValues(zoom_level = 'OUT')

rz_pedestrian <- reactiveValues(zoom = 'OUT',
                                poly_selected = NA)

rz <- reactiveValues(zoom = 'OUT',
                     poly_selected = NA)



# Commute mode change globals ---------------------------------------------

# Set access token  
set_token('pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ')

scenario1 <- tibble(c("Criteria: Cycling Distance (km)",
                      "Potential Cyclable Trips (per day)", 
                      "VMT Savings (per day)"), 
                    c(4.4, 60460, 102862))

scenario2 <- tibble(c("Criteria: Cycling Distance (km)",
                      "Criteria: Elevation Gain (m)", 
                      "Criteria: Time Ratio","Potential Cyclable Trips (per day)", 
                      "VMT Savings (per day)"), 
                    c(4.4, 45, 2.4, 44205, 72992))

# Legend
df_pal1 <- data.frame(
  color = c(1,2,3,4,5),
  color_value = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
  stringsAsFactors = F
)

cycling_access <- left_join(cycling_access, df_pal1, by = "color")

legend_po1 <- legend_element(
  variables = c("0 - 0.87","0.88 - 1.91","1.92 - 3.08","3.09 - 4.61","4.62 - 16.8"),
  colours = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
  colour_type = "fill",
  variable_type = "category",
  title = "Access to Cycling Infrastructure (km/sq.km)"
)
legend1 <- mapdeck_legend(legend_po1)

df_pal2 <- data.frame(
  color = c(1,2,3,4,5),
  color_value = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
  stringsAsFactors = F
)

car_share <- left_join(car_share, df_pal2, by = "color")

legend_po2 <- legend_element(
  variables = c("4% - 21%","22% - 33%","34% - 47%","48% - 61%","62% - 91%"),
  colours = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
  colour_type = "fill",
  variable_type = "category",
  title = "Share of Car Trips by Origin (%)"
)
legend2 <- mapdeck_legend(legend_po2)

df_pal3 <- data.frame(
  color = c(1,2,3,4,5),
  color_value = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
  stringsAsFactors = F
)

trip_distance <- left_join(trip_distance, df_pal3, by = "color")

legend_po3 <- legend_element(
  variables = c("2.3 - 6.4","6.5 - 7.8","7.9 - 8.9","9.0 - 10.4","10.5 - 22.6"),
  colours = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
  colour_type = "fill",
  variable_type = "category",
  title = "Average Commuting Distance (km)"
)

legend3 <- mapdeck_legend(legend_po3)


spinner <- tagList(
  spin_chasing_dots(),
  span("Sus is currently down for maintenance", style="color:white;")
)
