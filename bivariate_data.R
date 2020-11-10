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


# Save data files ---------------------------------------------------------

qsavem(data_DA, data_CT, data_borough, title_text, 
       data_borough_large, data_CT_large, data_DA_1_large, data_DA_2_large,
       file = "data/new_bivariate.qsm")


# Produce static images ---------------------------------------------------

var_names <- c("tenant_prop", "avg_rent", "avg_property_value", 
               "unaffordable_prop", "unsuitable_prop", "median_income", 
               "income_50_prop", "income_100_prop", "income_high_prop",
               "immigrant_prop", "immigrant_new_prop", "car_prop", 
               "walk_or_bike_prop", "transit_prop", "time_15_prop", 
               "time_30_prop", "time_45_prop", "time_60_prop")

walk(list(data_borough_large, data_CT_large, data_DA_1_large), function(df) {
  
  x_name <- case_when(
    nrow(df) == 71 ~ "borough",
    nrow(df) == 830 ~ "CT",
    nrow(df) == 5404 ~ "DA"
  )
  
  walk(var_names, ~{
    
    df <- 
      df %>% 
      select(right_variable = paste0(.x, "_quant3"))
    
    p <- 
      df %>% 
      ggplot() +
      geom_sf(aes(fill = as.factor(right_variable)), color = "white", 
              size = 0.01) +
      scale_fill_manual(values = rev(colors[c(4:6)])) +
      theme_map()
    
    ggdraw() + 
      draw_image("www/dropshadow1.png", scale = 1.49, vjust = -0.003, hjust = -0.003) +
      draw_plot(p) +
      draw_image("www/Univariate_right.png", scale = .5, vjust = 0.25, hjust = -0.25)
    
    ggsave(paste0("www/sidebar_maps/", x_name, "_", .x, ".png"), width = 1, 
           height = 1, units = "in", dpi = 250)
    
  })
  
  
})

