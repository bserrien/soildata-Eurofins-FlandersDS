# script for importing and cleaning the Eurofins dataset (Wallonia, CRA-W)

# Eurofins-Wallonia data
data_craw_wal <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Belgium_Wallonia_dataset",
       "Database_Belgium_Wallonia_EUROFINS_600_samples.RDS")
)



# data cleaning and formatting
data_craw_wal_clean <- data_craw_wal %>%
  filter(!is.na(Identification)) %>%
  rename(
    identification = Identification,
    soc            = SOC_percent,
    n_tot          = N_tot_g_kg,
    clay           = Clay_2_um_g_kg,
    silt           = Silt_2_50_um_g_kg_USDA_limits,
    sand           = Sand_50_2000_um_g_kg_USDA_limits,
    ph_water       = pH_water,
    ph_kcl         = pH_KCl
  ) %>%
  mutate(
    lab = "CRA-W",
    country = "Wallonia",
    across(
      c(soc, n_tot, clay, silt, sand, contains("ph_")), 
      ~ as.numeric(.x)
    ),
    # g/kg to %
    across(c(n_tot, clay, silt, sand), ~ .x/10)
  ) %>%
  # add lab methods as variable labels
  tinylabels::label_variables(
    soc      = "ISO 14235 / ISO 10694",
    n_tot    = "ISO 13878",
    clay     = "NF X31-107",
    silt     = "NF X31-107",
    sand     = "NF X31-107",
    ph_water = "ISO 10390 1:5 1N H2O",
    ph_kcl   = "ISO 10390 1:5 1N KCl"
  ) %>%
  # define units
  mutate(
    soc   = set_units(soc, "%"),
    n_tot = set_units(n_tot, "%"),
    clay  = set_units(clay, "%"),
    silt  = set_units(silt, "%"),
    sand  = set_units(sand, "%")
  ) %>%
  select(identification, country, lab, 
         soc, n_tot, 
         clay, silt, sand,
         contains("ph_"))

rm(data_craw_wal)
