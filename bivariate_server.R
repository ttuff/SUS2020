#### Logic/prep for bivariate comparison server ################################

load(file = "data/data_for_plot.Rdata")

# Mutate/rename variables
data_for_plot <-
  data_for_plot %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  mutate(tenant_prop = TenantH / Households,
         suitable_prop = Suitable / Households,
         income_50_prop = Under_5k_proportion + IN5k_10k_proportion +
           IN10k_15k_proportion + IN15k_proportion + IN20k_25k_proportion +
           IN25k_30k_proportion + IN30k_35k_proportion + IN35k_40k_proportion +
           IN40k_45k_proportion + IN45k_50k_proportion,
         income_100_prop = IN50k_60k_proportion + IN60k_70k_proportion +
           IN70k_80k_proportion + IN80k_90k_proportion + IN90k_proportion,
         driver_proportion = if_else(
           is.nan(driver_proportion) | is.infinite(driver_proportion),
           NA_real_, driver_proportion),
         walk_or_bike_prop = Walked_proportion + Bicycle_proportion,
         car_prop = driver_proportion + passenger_proportion) %>% 
  # select(DAUID, ale_index, ale_class,
  #        households_housing = Households,
  #        households_income = Househol_1,
  #        population = Pop,
  #        tenant_prop,
  #        median_rent = MedRent,
  #        median_mortgage = MedMort,
  #        median_property_value = MedVal,
  #        suitable_prop,
  #        median_income = MHI,
  #        income_50_prop,
  #        income_100_prop,
  #        income_high_prop = INOver100k_proportion,
  #        immigrant_prop = Imm_proportion,
  #        immigrant_new_prop = Imm_5year_proportion,
  #        car_prop,
  #        walk_or_bike_prop,
  #        transit_prop = Pubtrans_proportion,
  #        time_15_prop = T_15_proportion,
  #        time_30_prop = B15_29_proportion,
  #        time_45_prop = B30_44_proportion,
  #        time_60_prop = B_45_59_proportion,
  #        ale_tranis_quant3,
  #        Bicycle_proportion_quant3) %>% 
  rename(households_housing = Households,
         households_income = Househol_1,
         population = Pop,
         median_rent = MedRent,
         median_mortgage = MedMort,
         median_property_value = MedVal,
         median_income = MHI,
         income_high_prop = INOver100k_proportion,
         immigrant_prop = Imm_proportion,
         immigrant_new_prop = Imm_5year_proportion,
         transit_prop = Pubtrans_proportion,
         time_15_prop = T_15_proportion,
         time_30_prop = B15_29_proportion,
         time_45_prop = B30_44_proportion,
         time_60_prop = B_45_59_proportion)

# Fix geometry
# data_for_plot <- 
#   data_for_plot %>% 
#   st_transform(4326) %>% 
#   st_cast("MULTIPOLYGON")
