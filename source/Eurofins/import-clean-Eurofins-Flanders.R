# script for importing and cleaning the Eurofins dataset (Flanders, INBO)

# Eurofins-Flanders data
data_inbo_fl <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Belgium_Flanders_dataset",
       "Database_Cmon_EUROFINS_600_samples.RDS")
)

# data cleaning and formatting
data_inbo_fl_clean <- data_inbo_fl %>%
  rename(
    identification = LUCAS_ID,
    soc            = SOC_ISO_10694._percent,
    sic            = SIC_ISO_10694._percent,
    c_tot          = C.tot_ISO_10694._percent,
    n_tot          = N_tot_ISO_13878._g_kg,
    clay           = Clay_2_um_ISO_13320_percent,
    silt           = Silt_2_50_um_ISO_13320_percent,
    sand           = Sand_50_2000_um_ISO_13320_percent,
    ph_water       = pH_water_ISO_10390_1N_KCl_1_5_ratio, # water or KCl?
    ph_cacl2       = pH_CaCl2_ISO_10390_1N_KCl_1_5_ratio, # CaCl2 or KCl?
    ph_kcl         = pH_KCl_ISO_10390_1N_KCl_1_5_ratio
  ) %>%
  mutate(
    lab = "Flanders",
    country = "Flanders",
    identification = as.character(identification),
    across(
      c(soc, sic, c_tot, n_tot, clay, silt, sand, contains("ph_")), 
      ~ as.numeric(.x)
    )
  ) %>%
  # add lab methods as variable labels
  tinylabels::label_variables(
    soc      = "ISO 10694",
    sic      = "ISO 10694",
    c_tot    = "ISO 10694",
    n_tot    = "ISO 13878",
    clay     = "ISO 13320",
    silt     = "ISO 13320",
    sand     = "ISO 13320",
    ph_water = "ISO 10390 1:5 1N H2O",
    ph_cacl2 = "ISO 10390 1:5 1N CaCl2",
    ph_kcl   = "ISO 10390 1:5 1N KCl"
  ) %>%
  # define units
  mutate(
    soc   = set_units(soc, "%"),
    sic   = set_units(sic, "%"),
    c_tot = set_units(c_tot, "%"),
    n_tot = set_units(n_tot, "g/kg"),
    clay  = set_units(clay, "%"),
    silt  = set_units(silt, "%"),
    sand  = set_units(sand, "%")
  ) %>%
  select(identification, country, lab, 
         soc, sic, c_tot, n_tot, 
         clay, silt, sand,
         contains("ph_"))


rm(data_inbo_fl)
