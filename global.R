#### Packages and functions ####################################################

library(mapdeck)
library(shiny)
library(shinydashboard)
library(jsonify)
library(sf)
library(geojsonsf)
library(tidyverse)
library(raster)
library(mapboxapi)
library(mapdeck)
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
  "3 - 3" = "#3F2949",
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1",
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A",
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E",
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0") %>%
  gather("group", "fill")

color_scale <- tibble(
  "6" = "#AE3A4E",
  "5" = "#BC7C8F", # medium inequality, medium income
  "4" = "#CABED0",
  "3" = "#4885C1", # high inequality, low income
  "2" = "#89A1C8",
  "1" = "#CABED0" # low inequality, low income
) %>%
  gather("group", "fill") 

color_scale_2 <- 
  tibble(
    "3" = "#AE3A4E",
    "2" = "#BC7C8F",
    "1" = "#CABED0"
  ) %>%
  gather("group", "fill")

colors <- as.character(color_scale$fill)

default_background_color <- "transparent"
default_font_color <- "black"
default_font_family <- "Helvetica"



var_list_2 <- list(
  choices = list(
    "Tenant housing" = "TenantH_quant3",
    "Sublet rental" = "Subs_quant3",
    "Over 30 yrs old" = "Plus30_quant3",
    "Median Rent" = "MedRent_quant3",
    "Average Rent" = "AvRent_quant3",
    "Median morgage price" = "MedMort_quant3",
    "Average morgage price" = "AvMort_quant3",
    "Median property value" = "MedVal_quant3",
    "Average property value" = "AvVal_quant3",
    "Number of owners" = "Owner_quant3",
    "Owners with morgages" = "Wmortg_quant3",
    "Over 30yr old that own home" = "Plus30Own_quant3",
    "CTIR" = "CTIR_quant3",
    "Less than 30" = "Less30_quant3",
    "More than 30" = "More30_quant3"))



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
load(file = "data/data_for_plot.Rdata")

# Load data for pedestrian realm 
load(file = "data/census_analysis.Rdata")
load(file = "data/census_circular.Rdata")
load(file = "data/data_for_app.Rdata")
load(file = "data/sample_points_for_app_WSG.Rdata")
load(file = "data/census_analysis_WSG.Rdata")
load(file = "data/data_for_app_WSG.Rdata")
load(file = "data/centroids.Rdata")
load(file = "data/original_VAS_plan.Rdata")
load(file = "data/revised_VAS_plan.Rdata")
load(file = "data/sample_points_for_app.Rdata")
load(file = "data/census_analysis_quantile.Rdata")
cycling1 <- loadRData("data/car_1_finals.Rdata")
cycling2 <- loadRData("data/car_3_finals.Rdata")
cycling_network <- loadRData("data/reseau_cyclable.Rdata")
car_share <- loadRData("data/Car_Share.Rdata")
cycling_access <- loadRData("data/Cycling_Access.Rdata")
trip_distance <- loadRData("data/Trip_Distance.Rdata")

census_analysis_quantile_WSG <- census_analysis_quantile %>% 
  st_transform(4326)


dropshadow1 <- normalizePath(file.path("www/dropshadow1.png"))
dropshadow2 <- normalizePath(file.path("www/dropshadow2.png"))


# Other prep --------------------------------------------------------------

js_ped <- "$(document).ready(function(){
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



### Establish reactiveValues

qz <- reactiveValues(zoom_level = 'ISO')

rz_pedestrian <- reactiveValues(zoom = 'OUT')

rz <- reactiveValues(zoom = 'IN')


# Set access token  
set_token('pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ')




scenario1 <- data.frame(c("Criteria: Cycling Distance (km)","Potential Cyclable Trips (per day)", "VMT Savings (per day)"), c(4.4, 60460, 102862))
scenario2 <- data.frame(c("Criteria: Cycling Distance (km)","Criteria: Elevation Gain (m)", "Criteria: Time Ratio","Potential Cyclable Trips (per day)", "VMT Savings (per day)"), c(4.4,45,2.4, 44205, 72992))


###########legend#####
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


