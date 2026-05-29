# script for importing and cleaning the Eurofins dataset

# Eurofins data
data_eurofins <- read_excel(
  here("data", "Eurofins_comparison", 
       "Results_and_Standards_EUROFINS_N_SIMS.xlsx"),
  sheet = "Results_EUROFINS",
  col_types = "text"
)

# data cleaning and formatting
data_eurofins_clean <- data_eurofins %>%
  rename(
    identification = Identification,
    country        = Country,
    soc            = `SOC, %`,
    sic            = `SIC, %`,
    c_tot          = `C tot, %`,
    n_tot          = `N tot, gkg`,
    clay           = `Clay, < 2 µm %`,
    silt           = `Silt, 2- 50 µm, %`,
    sand           = `Sand, >50 - 2000 µm, %`,
    ph_cacl2       = `pH-CaCl₂`
  ) %>%
  mutate(
    lab = "Eurofins",
    across(
      c(soc, sic, c_tot, n_tot, clay, silt, sand, ph_cacl2), 
      ~ as.numeric(.x)
    ),
    # convert g/kg to %
    across(
      c(n_tot), ~ .x/10
    )
  ) %>%
  # add lab methods as variable labels
  tinylabels::label_variables(
    soc      = "ISO 10694 NEN 15936",
    sic      = "ISO 10693",
    c_tot    = "ISO 10694",
    n_tot    = "ISO 13878",
    clay     = "NEN 5753",
    silt     = "NEN 5753",
    sand     = "NEN 5753",
    ph_cacl2 = "ISO 10390 1:10 0.01M CaCl2"
  ) %>%
  # define units
  mutate(
    soc   = set_units(soc, "%"),
    sic   = set_units(sic, "%"),
    c_tot = set_units(c_tot, "%"),
    n_tot = set_units(n_tot, "%"),
    clay  = set_units(clay, "%"),
    silt  = set_units(silt, "%"),
    sand  = set_units(sand, "%")
  ) %>%
  select(identification, country, lab, 
         soc, sic, c_tot, n_tot, 
         clay, silt, sand,
         ph_cacl2)

rm(data_eurofins)
