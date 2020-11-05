#### Logic/prep for bivariate comparison #######################################



# Drop down list for variable selection -----------------------------------

var_list <- 
  list("Housing" = list("Tenant-occupied (%)" = "tenant_prop",
                        "Median rent" = "median_rent",
                        "Median mortgage" = "median_mortgage",
                        "Median property value" = "median_property_value",
                        "Suitable housing (%)" = "suitable_prop"),
       "Income" = list("Median household income" = "median_income",
                       "Income under $50k (%)" = "income_50_prop",
                       "Income between $50k-$100k (%)" = "income_100_prop",
                       "Income above $100k (%)" = "income_high_prop"),
       "Immigration" = list("Immigrants (%)" =  "immigrant_prop",
                            "New immigrants (%)" = "immigrant_new_prop"),
       "Transportation" = list("Drive to work (%)" = "car_prop",
                               "Public transit to work (%)" = "transit_prop",
                               "Walk or cycle to work (%)" = "walk_or_bike_proportion",
                               "15 minutes to work (%)" = "time_30_prop",
                               "15-30 minutes to work (%)" = "time_45_prop",
                               "30-45 minutes to work (%)" = "time_60_prop",
                               "45-60 minutes to work (%)" = "time_60_prop"))

