#### Bivariate data setup ######################################################

# Get CSDs ----------------------------------------------------------------

CSDs <-
  cancensus::get_census("CA16", list(CMA = "24462"), "CSD",
                        geo_format = "sf") %>%
  filter(name != "Montréal (V)") %>%
  mutate(type = "City") %>%
  select(name, type, geometry) %>%
  mutate(name = str_replace_all(name, "\\(PE\\)", "--parish municipality")) %>% 
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
  mutate(type = if_else(type == "Arrondissement", "Borough", "City"))

CSDs <- 
  CSDs %>% 
  st_centroid() %>% 
  st_filter(boroughs) %>% pull(name) %>% 
  {filter(CSDs, !name %in% .)}

boroughs <- 
  boroughs %>% 
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
         population = Pop,
         mode = Mode) %>%
  relocate(households, households_affordable, households_suitable,
           population, mode, .after = ale_class) %>%
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
         immigrant_prop = immigrant / population,
         immigrant_new_prop = immigrant_new / population,
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
  rename(ID = DAUID, name = DA_12, name_2 = CSDNAME) %>%
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
         immigrant_prop = immigrant / population,
         immigrant_new_prop = immigrant_new / population,
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
  rename(ID = CTUID, name = CTNAME, name_2 = CSDNAME) %>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.x), 0, as.numeric(.x)))) %>% 
  relocate(name_2, .after = name)

borough_join <-
  data %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  st_join(boroughs, left = FALSE) %>%
  select(DAUID, name) %>%
  st_drop_geometry() %>%
  group_by(DAUID) %>%
  slice(1) %>%
  ungroup()

leftovers <- 
  data %>% 
  filter(!DAUID %in% borough_join$DAUID) %>% 
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

borough_join <- 
  leftovers %>% 
  filter(CSDNAME %in% borough_join$name) %>% 
  st_drop_geometry() %>% 
  select(DAUID, name = CSDNAME) %>% 
  bind_rows(borough_join)

leftovers <- 
  data %>% 
  filter(!DAUID %in% borough_join$DAUID) %>% 
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

borough_join <- 
  leftovers %>% 
  st_drop_geometry() %>% 
  select(DAUID) %>% 
  mutate(name = "Ahuntsic-Cartierville") %>% 
  bind_rows(borough_join)

borough_join <- 
  borough_join %>% 
  left_join(st_drop_geometry(boroughs))

data_borough <-
  data %>%
  st_drop_geometry() %>%
  inner_join(borough_join) %>%
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
         immigrant_prop = immigrant / population,
         immigrant_new_prop = immigrant_new / population,
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
  mutate(across(where(is.numeric), ~if_else(is.nan(.x), 0, as.numeric(.x)))) %>% 
  rename(name_2 = type)


# Build title text --------------------------------------------------------

title_text <- 
  tibble(
    tab = c("active", "active", "commute", "commute", "pedestrian_ct", 
            "pedestrian_ct", "pedestrian_da", "pedestrian_da", 
            "pedestrian_sidewalk", "pedestrian_sidewalk"),
    type = rep(c("main", "extra"), 5),
    text = c(paste0("The CanALE dataset (developed by Prof. Nancy Ross ",
                    "and her team) captures four key elements related ",
                    "to active living environments: population density, ", 
                    "points of interest, street grid, and proximity of ",
                    "transit service."),
             paste0("<p>A safe and inviting pedestrian ",
                    "realm is not distributed equally across ",
                    "socio-demographic factors. The risks of ", 
                    "pedestrian injuries and fatalities are higher in ",
                    "low-income and racialized communities where ", 
                    "residents often rely on walking as a daily mode ",
                    "of transport but where the local environment is ",
                    "not necessarily inviting and safe.",
                    "<p>In addition to ",
                    "evidence pointing towards large discrepancies in ",
                    "the provision of walkable urban space across ",
                    "income and racial lines, concern has been ",
                    "raised with regard to the possible ",
                    "gentrification and displacement impacts of ",
                    "improved pedestrian infrastructure. In other ",
                    "words, who can afford to live in walkable ",
                    "neighbourhoods?",
                    "<br>",
                    "<p>Further resources:",
                    "<ul><li><a href= ''>Thomas Herrmann, William Gleckner, ", 
                    "Rania A. Wasfi, Benoît Thierry, Yan Kestens ", 
                    "and Nancy A. Ross. 2019. 'A pan-Canadian measure of ",
                    "active living environments using open data. ",
                    "Statistics Canada Health Reports, 82-003-X.</a>",
                    "<li><a href = ''>Kevin Manaugh, Linnea Soli, Samuel Kohn, ",
                    "Robin Basalaev-Binder, Ty Tuff, David Wachsmuth. 2020. ",
                    "'Montreal’s response to COVID-19: An equity analysis of ",
                    "new active transport infrastructure.' ",
                    "Transportation Research Board working paper.</a> ",
                    "<b>(MSSI research)</b></ul>",
                    "<br>",
                    "<p><i>Module lead authors: Robin Basalaev-Binder, ", 
                    "Ty Tuff, David Wachsmuth</i>"
                    ),
             paste0("Based on 2016 commuting data, we quantify possible ",
                    "reductions in VMT and GHG by identifying car trips that ",
                    "could be shifted to cycling based on distance, ",
                    "elevation change and other factors."), 
             paste0("<p>The transportation sector is a major contributor ",
                    "to Canada's greenhouse gas (GHG) emissions which are ", 
                    "linked to global climate change. Reducing vehicle miles ",
                    "traveled (VMT) over the long term is increasingly ", 
                    "recognized as the key to reduce GHG emissions from the ",
                    "transportation sector but has not received as much ", 
                    "attention as needed.",
                    "<p>The primary objective of this study is to ",
                    "investigate the potential for reducing VMT and GHG ",
                    "emissions by shifting short car trips to cycling ",
                    "in Montreal. Based on commuting data from the 2016 ",
                    "Canadian Census, commuting patterns were explored. ",
                    "Two scenarios were introduced to model environmental ",
                    "effects of a modal shift towards cycling based on ",
                    "characteristics of current bicycle trips.", 
                    "<p>The results showed that enhanced cycling commuting ",
                    "can reduce VMT and GHG emissions from car travel. ", 
                    "Other mitigation measures are necessary for achieving ",
                    "GHG emissions reduction targets.",
                    "<br>",
                    "<i>Module lead authors: Qiao Zhao, Kevin Manaugh</i>"), 
             paste0("The capacity for pedestrian social ",
                    "distancing is a capacity measurement that determines ",
                    "the percentage of a neighbourhood’s population that ",
                    "can make local trips on foot at the same time while respecting ",
                    "‘social distancing’ regulations."), 
             paste0("Using open data from Montreal's open data portal as well as OpenStreetMap, it was possible to ",
                    "calculate the total surface area of sidewalks, neighbourhood parks, and pre-Covid ",
                    "pedestrian streets. Summing these surface areas ",
                    "gets us the neighbourhood's total walkable surface area. It is ",
                    "then possible to calculate how many residents can ‘fit’ into the pedestrian realm ",
                    "while respecting ‘social distancing’ regulations of 2 meters (total walkable surface ",
                    "area divided by the surface area of a circle with a 2-meter radius, that is 12.54 ",
                    "square meters). Finally, we normalize the value by representing it as a percentage of the residential population. ",
                    "Neighbourhoods where less than 100% of the local population can make trips on foot at the same time ",
                    "are above capacity and more at risk of overcrowding from local ",
                    "pedestrian trips. While it is still important to take into account ",
                    "pedestrian flows coming from external neighbourhoods (some data on pedestrian ",
                    "flows obtained from 2016 TrajetMtl data is presented in this research), the measurement’s focus on local pedestrian ",
                    "capacity is especially relevant during a pandemic situation where shelter ",
                    "in place and travel restrictions have generally led to a rise in local trips ",
                    "and a decline in trips from other neighbourhoods. "),
             paste0("Compare the pedestrian capacity for social distancing metric across a variety of other variables, such as walkable access ",
             "to amenities, income level, immigration, visible minorities, population density, etc. "),
             paste0("The data shows us that the ability to safely navigate pedestrian space tends to be much lower in DAs with lower incomes as ",
                    "well as DAs with high proportions of visible minorities and immigrants compared to majority white regions of Montreal. While ",
                    "the City’s plans to increase walkable urban space made some improvements to these discrepancies, there is room for improvement. ",
                    "The interactive data within this platform has the potential to support policy-makers towards making strategic decisions with more ",
                    "equitable outcomes. Below is a policy analysis exemplar using two variables: capacity for pedestrian social distancing and ",
                    "walkable access to key amenities."),
             "",
             paste0("In order to calculate the width of sidewalks in Montreal, we used a spatial dataset published by Montréal Open Data Portal ",
             "that includes polygons of all sidewalks within the Montreal agglomeration. Using the the object-oriented programming language R, ",
             "we developed a function which first creates negative buffers inside each sidewalk segment, and then iteratively adjusts the distance ",
             "of that buffer until the maximum distance is achieved which still produces valid buffer geometry (if the buffer boundaries overlap, ",
             "the geometry becomes invalid). The outcome is the equivalent of a centreline inside each sidewalk polygon. The last step to determine ",
             "sidewalk width is to sum the distances between the centreline and ",
             "both edges of a given sidewalk polygon segment. This process is illustrated below."))
  )


# New large data files ----------------------------------------------------

opacity <- "EE"

data_borough_large <- 
  data_borough %>% 
  mutate(across(tenant_prop_quant3:time_60_prop_quant3, list(
    group = ~paste(ale_index_quant3, "-", .x),
    elevation = ~{(ale_index_quant3 * .x) ^ 2 * 50}))) %>% 
  mutate(across(ends_with("_group"), ~{
    tibble(.x) %>% 
      left_join(bivariate_color_scale, by = c(".x" = "group")) %>% 
      mutate(fill = if_else(str_detect(.x, "NA"), "#B3B3BB", fill)) %>% 
      pull(fill) %>% 
      paste0("FF")}, .names = "{str_remove(.col, '_group')}_fill")) %>% 
  mutate(across(tenant_prop_quant3_fill:time_60_prop_quant3_fill,
                ~paste0(substr(.x, 1, 7), opacity),
                .names = "{.col}_opacity")) %>% 
  mutate(group = paste(ale_index_quant3, "- 1")) %>%
  left_join(bivariate_color_scale, by = "group") %>% 
  mutate(fill = if_else(str_detect(group, "NA"), "#B3B3BB", fill)) %>% 
  mutate(elevation = (ale_index_quant3 * 1) ^ 2 * 50) %>% 
  mutate(fill_opacity = paste0(fill, opacity),
         fill = paste0(fill, "FF"))

opacity <- "CC"

data_CT_large <- 
  data_CT %>% 
  mutate(across(tenant_prop_quant3:time_60_prop_quant3, list(
    group = ~paste(ale_index_quant3, "-", .x),
    elevation = ~{(ale_index_quant3 * .x) ^ 2 * 50}))) %>% 
  mutate(across(ends_with("_group"), ~{
    tibble(.x) %>% 
      left_join(bivariate_color_scale, by = c(".x" = "group")) %>% 
      mutate(fill = if_else(str_detect(.x, "NA"), "#B3B3BB", fill)) %>% 
      pull(fill) %>% 
      paste0("FF")}, .names = "{str_remove(.col, '_group')}_fill")) %>% 
  mutate(across(tenant_prop_quant3_fill:time_60_prop_quant3_fill,
                ~paste0(substr(.x, 1, 7), opacity),
                .names = "{.col}_opacity")) %>% 
  mutate(group = paste(ale_index_quant3, "- 1")) %>%
  left_join(bivariate_color_scale, by = "group") %>% 
  mutate(fill = if_else(str_detect(group, "NA"), "#B3B3BB", fill)) %>% 
  mutate(elevation = (ale_index_quant3 * 1) ^ 2 * 50) %>% 
  mutate(fill_opacity = paste0(fill, opacity),
         fill = paste0(fill, "FF"))

opacity <- "AA"

data_DA_1_large <- 
  data_DA %>% 
  mutate(across(tenant_prop_quant3:time_60_prop_quant3, list(
    group = ~paste(ale_index_quant3, "-", .x),
    elevation = ~{(ale_index_quant3 * .x) ^ 2 * 50}))) %>% 
  mutate(across(ends_with("_group"), ~{
    tibble(.x) %>% 
      left_join(bivariate_color_scale, by = c(".x" = "group")) %>% 
      mutate(fill = if_else(str_detect(.x, "NA"), "#B3B3BB", fill)) %>% 
      pull(fill) %>% 
      paste0("FF")}, .names = "{str_remove(.col, '_group')}_fill")) %>% 
  mutate(across(tenant_prop_quant3_fill:time_60_prop_quant3_fill,
                ~paste0(substr(.x, 1, 7), opacity),
                .names = "{.col}_opacity")) %>% 
  mutate(group = paste(ale_index_quant3, "- 1")) %>%
  left_join(bivariate_color_scale, by = "group") %>% 
  mutate(fill = if_else(str_detect(group, "NA"), "#B3B3BB", fill)) %>% 
  mutate(elevation = (ale_index_quant3 * 1) ^ 2 * 50) %>% 
  mutate(fill_opacity = paste0(fill, opacity),
         fill = paste0(fill, "FF")) %>% 
  mutate(width = 5)

opacity <- "80"

data_DA_2_large <- 
  data_DA %>% 
  mutate(across(tenant_prop_quant3:time_60_prop_quant3, list(
    group = ~paste(ale_index_quant3, "-", .x),
    elevation = ~{(ale_index_quant3 * .x) ^ 2 * 50}))) %>% 
  mutate(across(ends_with("_group"), ~{
    tibble(.x) %>% 
      left_join(bivariate_color_scale, by = c(".x" = "group")) %>% 
      mutate(fill = if_else(str_detect(.x, "NA"), "#B3B3BB", fill)) %>% 
      pull(fill) %>% 
      paste0("FF")}, .names = "{str_remove(.col, '_group')}_fill")) %>% 
  mutate(across(tenant_prop_quant3_fill:time_60_prop_quant3_fill,
                ~paste0(substr(.x, 1, 7), opacity),
                .names = "{.col}_opacity")) %>% 
  mutate(group = paste(ale_index_quant3, "- 1")) %>%
  left_join(bivariate_color_scale, by = "group") %>% 
  mutate(fill = if_else(str_detect(group, "NA"), "#B3B3BB", fill)) %>% 
  mutate(elevation = (ale_index_quant3 * 1) ^ 2 * 50) %>% 
  mutate(fill_opacity = paste0(fill, opacity),
         fill = paste0(fill, "FF"))

qsavem(data_DA, data_CT, data_borough, title_text, 
       data_borough_large, data_CT_large, data_DA_1_large, data_DA_2_large,
       file = "data/new_bivariate.qsm")
