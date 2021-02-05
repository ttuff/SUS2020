### CANALE MODULE GLOBALS ######################################################

# Data and helper functions -----------------------------------------------

# Load bivariate census data
qload("data/data_canale.qsm")

# Initialize reactive values
rv_canale <- reactiveValues(zoom = "OUT", poly_selected = NA)
data_canale <- reactive(
  data_canale_borough %>%
    dplyr::select(ID, name, name_2, population, left_variable_full = ale_index,
                  left_variable = ale_index_quant3, ale_class, width, group, 
                  fill, elevation, fill_opacity))

# Dropdown menu
var_right_list <- 
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
