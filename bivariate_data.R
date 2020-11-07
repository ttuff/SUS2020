#### Bivariate data setup ######################################################

# Get CSDs ----------------------------------------------------------------

CSDs <-
  cancensus::get_census("CA16", list(CMA = "24462"), "CSD",
                        geo_format = "sf") %>%
  filter(name != "Montréal (V)") %>%
  mutate(type = "City") %>%
  select(name, type, geometry) %>%
  mutate(name = str_remove(name, " \\(.*\\)"))


# Get CTs -----------------------------------------------------------------

CTs <-
  cancensus::get_census("CA16", list(CMA = "24462"), "CT", geo_format = "sf") %>%
  as_tibble() %>%
  st_as_sf() %>%
  select(CTUID = GeoUID)


# Get boroughs ------------------------------------------------------------

CMA <- cancensus::get_census("CA16", list(CMA = "24462"), geo_format = "sf")

boroughs <-
  read_sf("data/montreal_boroughs_2019.shp") %>%
  st_intersection(CMA) %>%
  select(name = NOM, type = TYPE, geometry) %>%
  mutate(type = if_else(type == "Arrondissement", "Borough", "City")) %>%
  rbind(CSDs) %>%
  st_cast("MULTIPOLYGON")


# Get data ----------------------------------------------------------------

load("data/data_for_plot.Rdata")

data_circle <-
  data_for_plot %>%
  st_union()

data <-
  data_for_plot %>%
  as_tibble() %>%
  st_as_sf() %>%
  select(-c(PRUID:CSDUID), -c(CSDTYPE:CMATYPE), -c(ADAUID:z_poi), -c(DA_1:UID),
         -c(transit:ale_transi)) %>%
  select(-ends_with("_quant3"), -ends_with("_proportion")) %>%
  relocate(DA_12, .after = DAUID) %>%
  rename(households = Households,
         households_affordable = CTIR,
         households_suitable = Househol_1,
         pop_immigrants = Pop,
         mode = Mode) %>%
  relocate(households, households_affordable, households_suitable,
           pop_immigrants, mode, .after = ale_class) %>%
  rename(tenant = TenantH,
         avg_rent = AvRent,
         avg_property_value = AvVal,
         unaffordable = More30,
         unsuitable = NonSuit) %>%
  relocate(tenant, avg_rent, avg_property_value, unaffordable, unsuitable,
           .after = mode) %>%
  select(-c(Subs:Suitable)) %>%
  rename(median_income = MHI) %>%
  mutate(income_50 = Under_5k + IN5k_10k + IN10k_15k + IN15k + IN20k_25k +
           IN25k_30k + IN30k_35k + IN35k_40k + IN40k_45k + IN45k_50k,
         income_100 = IN50k_60k + IN60k_70k + IN70k_80k + IN80k_90k +
           IN90k,
         income_high = INOver100k,
         .after = median_income) %>%
  select(-c(Under_5k:Over200k)) %>%
  rename(immigrant = Imm, immigrant_new = Imm_5year) %>%
  relocate(immigrant, immigrant_new, .after = income_high) %>%
  select(-Non_Im) %>%
  mutate(car = driver + passenger,
         walk_or_bike = Walked + Bicycle,
         transit = Pubtrans) %>%
  rename(time_15 = T_15,
         time_30 = B15_29,
         time_45 = B30_44,
         time_60 = B_45_59) %>%
  relocate(car, walk_or_bike, transit, time_15, time_30, time_45, time_60,
           .after = immigrant_new) %>%
  select(-c(driver:sum_under_and_over_40))

data_DA <-
  data %>%
  mutate(tenant_prop = tenant / households_suitable,
         across(c(income_50, income_100, income_high),
                ~{.x / households}, .names = "{.col}_prop"),
         unaffordable_prop = unaffordable / households_affordable,
         unsuitable_prop = unsuitable / households_suitable,
         immigrant_prop = immigrant / pop_immigrants,
         immigrant_new_prop = immigrant_new / pop_immigrants,
         across(c(car:time_60), ~{.x / mode}, .names = "{.col}_prop")) %>%
  mutate(across(c(ale_index, tenant_prop, avg_rent, avg_property_value,
                  unaffordable_prop, unsuitable_prop, median_income,
                  income_50_prop, income_100_prop, income_high_prop,
                  immigrant_prop, immigrant_new_prop, car_prop:time_60_prop),
                ntile, 3, .names = "{.col}_quant3")) %>%
  mutate(width = 2) %>%
  st_intersection(data_circle) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  rename(ID = DAUID, name = DA_12, city_name = CSDNAME) %>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.x), 0, as.numeric(.x))))

data_CT <-
  data %>%
  st_drop_geometry() %>%
  mutate(CTUID = as.character(CTUID)) %>%
  group_by(CSDNAME, CTUID, CTNAME) %>%
  summarize(across(c(ale_index, ale_class, avg_rent, avg_property_value,
                     median_income),
                   ~{sum(.x * households, na.rm = TRUE) /
                       sum(households, na.rm = TRUE)}),
            across(c(households:tenant, unaffordable:unsuitable,
                     income_50:time_60), sum, na.rm = TRUE),
            .groups = "drop") %>%
  inner_join(CTs, .) %>%
  relocate(avg_rent, avg_property_value, .after = tenant) %>%
  relocate(median_income, .after = unsuitable) %>%
  mutate(tenant_prop = tenant / households_suitable,
         across(c(income_50, income_100, income_high),
                ~{.x / households}, .names = "{.col}_prop"),
         unaffordable_prop = unaffordable / households_affordable,
         unsuitable_prop = unsuitable / households_suitable,
         immigrant_prop = immigrant / pop_immigrants,
         immigrant_new_prop = immigrant_new / pop_immigrants,
         across(c(car:time_60), ~{.x / mode}, .names = "{.col}_prop")) %>%
  mutate(across(c(ale_index, tenant_prop, avg_rent, avg_property_value,
                  unaffordable_prop, unsuitable_prop, median_income,
                  income_50_prop, income_100_prop, income_high_prop,
                  immigrant_prop, immigrant_new_prop, car_prop:time_60_prop),
                ntile, 3, .names = "{.col}_quant3")) %>%
  mutate(width = 10) %>%
  st_transform(3347) %>%
  st_intersection(data_circle) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  rename(ID = CTUID, name = CTNAME, city_name = CSDNAME) %>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.x), 0, as.numeric(.x))))

borough_join <-
  data %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  st_join(boroughs, left = FALSE) %>%
  select(DAUID, name, type) %>%
  st_drop_geometry() %>%
  group_by(DAUID) %>%
  slice(1) %>%
  ungroup()

data_borough <-
  data %>%
  st_drop_geometry() %>%
  left_join(borough_join) %>%
  group_by(name, type) %>%
  summarize(across(c(ale_index, ale_class, avg_rent, avg_property_value,
                     median_income),
                   ~{sum(.x * households, na.rm = TRUE) /
                       sum(households, na.rm = TRUE)}),
            across(c(households:tenant, unaffordable:unsuitable,
                     income_50:time_60), sum, na.rm = TRUE),
            .groups = "drop") %>%
  relocate(avg_rent, avg_property_value, .after = tenant) %>%
  relocate(median_income, .after = unsuitable) %>%
  mutate(tenant_prop = tenant / households_suitable,
         across(c(income_50, income_100, income_high),
                ~{.x / households}, .names = "{.col}_prop"),
         unaffordable_prop = unaffordable / households_affordable,
         unsuitable_prop = unsuitable / households_suitable,
         immigrant_prop = immigrant / pop_immigrants,
         immigrant_new_prop = immigrant_new / pop_immigrants,
         across(c(car:time_60), ~{.x / mode}, .names = "{.col}_prop")) %>%
  mutate(across(c(ale_index, tenant_prop, avg_rent, avg_property_value,
                  unaffordable_prop, unsuitable_prop, median_income,
                  income_50_prop, income_100_prop, income_high_prop,
                  immigrant_prop, immigrant_new_prop, car_prop:time_60_prop),
                ntile, 3, .names = "{.col}_quant3")) %>%
  mutate(width = 100) %>%
  inner_join(boroughs, .) %>%
  st_transform(3347) %>%
  st_intersection(data_circle) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(ID = 1:n(), .before = name) %>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.x), 0, as.numeric(.x))))


# Build did-you-know table ------------------------------------------------

did_you_know <-
  tibble(
    "left_variable" = "ale_index",
    "right_variable" = c(" ", " ", " ", " ", " ", " ", "immigrant_prop", "walk_or_bike_prop", "walk_or_bike_prop", "median_income"),
    "text" = c("21.3% of people walk or cycle to work in areas in the highest class of active living potential. In areas with the lowest active living potential, only 2.5% do, compared to a regional average of 6%.\n",
               "Two thirds of new immigrants (67.4%) live in areas with the best access to active living potential (ALE class 5).\n",
               "11% of new immigrants live in areas with the worst access to active living potential (ALE class 1).\n",
               "While, overall, there is a trend towards lower income DAs being in high ALE class, roughly 340,000 people are in CanALE class 3 or lower AND have median household income less than $50,000.\n",
               "Areas in class 4 of active living potential have the highest average dwelling values, but nearly 10% less are renters than areas with active living class 5.\n",
               "Roughly 90% of the region’s population live within a 1km walk of a transit station. The vast majority of areas which access are located off the island of Montreal.\n",
               "Immigrant factoid.\n",
               "The Plateau-Mont-Royal has the highest active mode share to work (36% walking or biking), followed by Ville Marie with 32.5%.\n",
               "Pierrefonds-Roxboro and L'Île-Bizard-Sainte-Geneviève are the two boroughs with the lowest active mode share to work (less than 3% walk or bike).\n",
               "Median household income factoid.\n")
  )


# Build title text --------------------------------------------------------

title_text <- 
  tibble(
    tab = c("active", "active", "commute", "commute", "pedestrian_ct", "pedestrian_ct", "pedestrian_da", "pedestrian_da", "pedestrian_sidewalk", "pedestrian_sidewalk"),
    type = rep(c("main", "extra"), 5),
    text = c(paste0("The CanALE dataset (developed by Prof. Nancy Ross ",
                    "and her team) captures four key elements related ",
                    "to active living environments: population density, ", 
                    "points of interest, street grid, and proximity of ",
                    "transit service."),
             paste0("A safe and inviting pedestrian ",
                    "realm is not distributed equally across ",
                    "socio-demographic factors. The risks of ", 
                    "pedestrian injuries and fatalities are higher in ",
                    "low-income and racialized communities where ", 
                    "residents often rely on walking as a daily mode ",
                    "of transport but where the local environment is ",
                    "not necessarily inviting and safe. In addition to ",
                    "evidence pointing towards large discrepancies in ",
                    "the provision of walkable urban space across ",
                    "income and racial lines, concern has been ",
                    "raised with regard to the possible ",
                    "gentrification and displacement impacts of ",
                    "improved pedestrian infrastructure. In other ",
                    "words, who can afford to live in walkable ",
                    "neighbourhoods?"),
             "", "", paste0("The capacity for pedestrian social ",
                            "distancing is a capacity measurement that determines ",
                            "the percentage of a neighbourhood’s population that ",
                            "can make local trips on foot at the same time while respecting ",
                            "‘social distancing’ regulations."), 
             paste0("Neighbourhoods where less than ",
                            "100% of the local population can make trips on foot at the same time ",
                            "are above capacity and more at risk of overcrowding from local ",
                            "pedestrian trips. While it is still important to take into account ",
                            "pedestrian flows coming from external neighbourhoods (some data on pedestrian ",
                            "flows obtained from 2016 TrajetMtl data is presented in this research), the measurement’s focus on local pedestrian ",
                            "capacity is especially relevant during a pandemic situation where shelter ",
                            "in place and travel restrictions have generally led to a rise in local trips ",
                            "and a decline in trips from other neighbourhoods. Using open data ",
                            "from Montreal's open data portal as well as OpenStreetMap, it was possible to ",
                            "calculate the total surface area of sidewalks, neighbourhood parks, and pre-Covid ",
                            "pedestrian streets. Summing these surface areas ",
                            "gets us the neighbourhood's total walkable surface area. It is ",
                            "then possible to calculate how many residents can ‘fit’ into the pedestrian realm ",
                            "while respecting ‘social distancing’ regulations of 2 meters (total walkable surface ",
                            "area divided by the surface area of a circle with a 2-meter radius, that is 12.54 ",
                            "square meters). Finally, we normalize the value by representing it as a percentage of the residential population."),
                            "", "", "", "")
  )

save(data_DA, data_CT, data_borough, did_you_know, title_text,
     file = "data/new_bivariate.Rdata")
