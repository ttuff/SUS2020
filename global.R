#### Packages and functions ####################################################


# Shiny options -----------------------------------------------------------

shinyOptions(cache = diskCache("./app-cache"))


# Packages ----------------------------------------------------------------

library(mapdeck)
library(shiny)
library(shinydashboard)
library(jsonify)
library(sf)
library(geojsonsf)
library(tidyverse)
library(raster)
library(mapboxapi)
library(shinybusy)
library(markdown)
library(shinyjqui)
library(plotly)
library(png)
library(shinyWidgets)
library(cowplot)
library(RColorBrewer)
library(classInt)
library(scales)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(extrafont)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(DT)
library(gghighlight)
library(qs)
library(glue)
library(shinipsum)
# library(fakir)
library(shiny.i18n)
library(googleLanguageR)
library(shinyanimate)
library(aniview)

# library(shinycssloaders)
# 
# # Options for Spinner
# options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


#i18n <- Translator$new(translation_json_path = "www/translation.json")
i18n <- Translator$new(translation_csvs_path = "translations/")
# print(i18n$t("Hello Shiny!"))
#i18n <- Translator$new(automatic = TRUE)
#i18n <- init_i18n(translation_json_path = "www/translation.json")
# change this to the target language
i18n$set_translation_language("fr")
# i18n$set_translation_language("en")
print(i18n$t("Learn more"))
#HTML(as.character(usei18n(i18n)))


# load translations
translation_fr <- read_csv("translations/translation_fr.csv")

# creation o r to store all our reactive values like active_language which will
# be called with r$active_language()
r <- reactiveValues() # r to store all our reactive values


#create_translation_file("/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/www/translation.json", type = "json", handle = "i18n", output = "/Users/Ty/Dropbox/Dendritic connectivity/SUS2020/www/translation.json")

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
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      legend.position = "none",
      # borders and margins
      plot.margin = unit(c(0, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 22, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

## save colors
bivariate_color_scale <- tibble(
  "3 - 3" = "#2A5A5B",
  "2 - 3" = "#567994",
  "1 - 3" = "#6C83B5",
  "3 - 2" = "#5A9178",
  "2 - 2" = "#90B2B3",
  "1 - 2" = "#B5C0DA",
  "3 - 1" = "#73AE80",
  "2 - 1" = "#B8D6BE",
  "1 - 1" = "#E8E8E8") %>%
  gather("group", "fill")

color_scale <- tibble(
  "6" = "#73AE80",
  "5" = "#B8D6BE", # medium inequality, medium income
  "4" = "#E8E8E8",
  "3" = "#6C83B5", # high inequality, low income
  "2" = "#B5C0DA",
  "1" = "#E8E8E8" # low inequality, low income
) %>%
  gather("group", "fill") 

color_scale_2 <- 
  tibble(
    "3" = "#73AE80",
    "2" = "#B8D6BE",
    "1" = "#E8E8E8"
  ) %>%
  gather("group", "fill")

colors <- as.character(color_scale$fill)

default_background_color <- "transparent"
default_font_color <- "black"
default_font_family <- "Helvetica"

#### Animation function

animateCSS <- function(effect, delay = 0, duration = 500, then = NULL){
  effect <- match.arg(effect, c(
    "bounce",
    "flash",
    "pulse",
    "rubberBand",
    "shakeX",
    "shakeY",
    "headShake",
    "swing",
    "tada",
    "wobble",
    "jello",
    "heartBeat",
    "backInDown",
    "backInLeft",
    "backInRight",
    "backInUp",
    "backOutDown",
    "backOutLeft",
    "backOutRight",
    "backOutUp",
    "bounceIn",
    "bounceInDown",
    "bounceInLeft",
    "bounceInRight",
    "bounceInUp",
    "bounceOut",
    "bounceOutDown",
    "bounceOutLeft",
    "bounceOutRight",
    "bounceOutUp",
    "fadeIn",
    "fadeInDown",
    "fadeInDownBig",
    "fadeInLeft",
    "fadeInLeftBig",
    "fadeInRight",
    "fadeInRightBig",
    "fadeInUp",
    "fadeInUpBig",
    "fadeInTopLeft",
    "fadeInTopRight",
    "fadeInBottomLeft",
    "fadeInBottomRight",
    "fadeOut",
    "fadeOutDown",
    "fadeOutDownBig",
    "fadeOutLeft",
    "fadeOutLeftBig",
    "fadeOutRight",
    "fadeOutRightBig",
    "fadeOutUp",
    "fadeOutUpBig",
    "fadeOutTopLeft",
    "fadeOutTopRight",
    "fadeOutBottomRight",
    "fadeOutBottomLeft",
    "flip",
    "flipInX",
    "flipInY",
    "flipOutX",
    "flipOutY",
    "lightSpeedInRight",
    "lightSpeedInLeft",
    "lightSpeedOutRight",
    "lightSpeedOutLeft",
    "rotateIn",
    "rotateInDownLeft",
    "rotateInDownRight",
    "rotateInUpLeft",
    "rotateInUpRight",
    "rotateOut",
    "rotateOutDownLeft",
    "rotateOutDownRight",
    "rotateOutUpLeft",
    "rotateOutUpRight",
    "hinge",
    "jackInTheBox",
    "rollIn",
    "rollOut",
    "zoomIn",
    "zoomInDown",
    "zoomInLeft",
    "zoomInRight",
    "zoomInUp",
    "zoomOut",
    "zoomOutDown",
    "zoomOutLeft",
    "zoomOutRight",
    "zoomOutUp",
    "slideInDown",
    "slideInLeft",
    "slideInRight",
    "slideInUp",
    "slideOutDown",
    "slideOutLeft",
    "slideOutRight",
    "slideOutUp"
  ))
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
  function(condition, ..., onShow = NULL, fadeIn = 600, onHide = NULL, fadeOut = 400){
    id <- paste0("animateCSS-", stringi::stri_rand_strings(1, 15))
    jsShow <- ifelse(!is.null(onShow), sprintf(onShowJS(onShow, fadeIn), id), "")
    jsHide <- ifelse(!is.null(onHide), sprintf(onHideJS(onHide, fadeOut), id), "")
    script <- tags$script(HTML(paste(jsShow,jsHide,sep="\n")))
    condPanel <- conditionalPanel(condition, ...)
    tags$div(id=id, tagList(condPanel, script))
  }


# Drop down list for variable selection -----------------------------------

var_list <- 
  list("----" = " ", 
       "Housing" = list("Tenant-occupied (%)" = "tenant_prop",
                        "Average rent" = "avg_rent",
                        "Average property value" = "avg_property_value",
                        "Unaffordable housing (%)" = "unaffordable_prop",
                        "Unsuitable housing (%)" = "unsuitable_prop"),
       "Income" = list("Median household income" = "median_income",
                       "Income under $50k (%)" = "income_50_prop",
                       "Income between $50k-$100k (%)" = "income_100_prop",
                       "Income above $100k (%)" = "income_high_prop"),
       "Immigration" = list("Immigrants (%)" =  "immigrant_prop",
                            "New immigrants (%)" = "immigrant_new_prop"),
       "Transportation" = list("Drive to work (%)" = "car_prop",
                               "Walk or cycle to work (%)" = "walk_or_bike_prop",
                               "Public transit to work (%)" = "transit_prop",
                               "15 minutes to work (%)" = "time_15_prop",
                               "15-30 minutes to work (%)" = "time_30_prop",
                               "30-45 minutes to work (%)" = "time_45_prop",
                               "45-60 minutes to work (%)" = "time_60_prop"))

loadingLogo <- 
  function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
    tagList(
      tags$head(
        tags$script(
          "setInterval(function(){
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

# Load bivariate census data
qload("data/new_bivariate.qsm")

did_you_know <- 
  read_csv("data/did_you_know.csv") %>% 
  mutate(right_variable = if_else(is.na(right_variable), " ", right_variable))

variable_explanations <- 
  read_csv("data/variable_explanations.csv")

# Load data for pedestrian realm 
load(file = "data/sidewalks_WSG.Rdata")
load(file = "data/census_circular.Rdata")
load(file = "data/original_VAS_plan.Rdata")
load(file = "data/revised_VAS_plan.Rdata")
load(file = "data/census_analysis_quantile.Rdata")
load(file = "data/census_analysis_ct.Rdata")
cycling1 <- loadRData("data/car_1_finals.Rdata")
cycling2 <- loadRData("data/car_3_finals.Rdata")
cycling_network <- loadRData("data/reseau_cyclable.Rdata")
car_share <- loadRData("data/Car_Share.Rdata")
cycling_access <- loadRData("data/Cycling_Access.Rdata")
trip_distance <- loadRData("data/Trip_Distance.Rdata")

load("data/cycling_total_final.Rdata")

census_analysis_quantile_WSG <- census_analysis_quantile %>% 
  st_transform(4326)

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



