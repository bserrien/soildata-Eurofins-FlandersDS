# dataset for variable: ECEC

#' Eurofins method:   ISO 23470
#' country methods:
#' Austria            ÖNORM L 1086-1
#' Belgium Flanders   -
#' Belgium Wallonia   ISO 23470
#' Czech Republic     -
#' Estonia            -
#' France             ISO 23470
#' Hungary            modified Mechlich (pH 8,1; 0,1M BaCl2; soil:solution 1:25; 
#'                    4 hours) 
#' Italy              DM 13/09/1999 met XIII.2 (barium chloride solution buffered 
#'                    at pH 8.2 - similar to ISO13536)
#' Portugal           Potential Cation Exchange Capacity - BaCl2 at pH 8.1, 
#'                    ISO 13536, Atomic Absorption Spectrophotometry (AAS)


# Eurofins data
data_eurofins <- read_excel(
  here("data", "Eurofins_comparison", 
       "Results_and_Standards_EUROFINS_N_SIMS.xlsx"),
  sheet = "Results_EUROFINS",
  col_types = "text"
)

data_eurofins_ecec <- data_eurofins %>%
  filter(!is.na(`ECEC, mmol+kg`)) %>%
  mutate(ecec = as.numeric(`ECEC, mmol+kg`),
         ecec = set_units(ecec, "mmol/kg"),
         lab = "Eurofins",
         method = "ISO-23470") %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, ecec)
  
hist(data_eurofins_ecec$ecec)



# country data

# Austria
data_austria_ecec <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Austria_dataset",
       "Database_Austria_EUROFINS_600_samples.RDS")
) %>%
  filter(!is.na(ECEC_cmol_kg)) %>%
  mutate(ecec = 10 * ECEC_cmol_kg,
         ecec = set_units(ecec, "mmol/kg"),
         lab = "NSIMS",
         method = "ÖNORM-L-1086-1") %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, ecec)

hist(data_austria_ecec$ecec)

left_join(data_austria_ecec, data_eurofins_ecec,
          by = c("identification")) %>%
  ggplot(aes(ecec.x, ecec.y)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)



# Wallonia
data_wallonia_ecec <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Belgium_Wallonia_dataset",
       "Database_Belgium_Wallonia_EUROFINS_600_samples.RDS")
) %>%
  filter(!is.na(CEC_cmol_kg)) %>%
  mutate(ecec = 10 * CEC_cmol_kg,
         ecec = set_units(ecec, "mmol/kg"),
         lab = "NSIMS",
         method = "ISO-23470") %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, ecec)

left_join(data_wallonia_ecec, data_eurofins_ecec,
          by = c("identification")) %>%
  ggplot(aes(ecec.x, ecec.y)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)



# France
data_france_ecec <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "France_dataset",
       "Database_RMQS_EUROFINS_600_samples.RDS")
) %>%
  filter(
    Soil_monitoring_network == "RMQS",
    !is.na(ECEC_ISO_23470_cmol_kg)
  ) %>%
  mutate(ecec = 10 * ECEC_ISO_23470_cmol_kg,
         ecec = set_units(ecec, "mmol/kg"),
         lab = "NSIMS",
         method = "ISO-23470",
         identification = paste0(LUCAS_ID, " ", LAS_reference)) %>%
  rename(country = Country) %>%
  select(identification, country, lab, method, ecec)

left_join(data_france_ecec, data_eurofins_ecec,
          by = c("identification")) %>%
  ggplot(aes(ecec.x, ecec.y)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)



# Hungary
# data_hungary_ecec <- read_rds(
#   here("data", "Eurofins_comparison", 
#        "All_countries_datasets_XLSX_CSV_RDS_format",
#        "Hungary_dataset",
#        "Database_Hungary_EUROFINS_600_samples.RDS")
# )
# listed to be measured with "modified Mechlich" but no ECEC data found ??



# Italy
data_italy_ecec <- read_rds(
  here("data", "Eurofins_comparison",
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Italy_dataset",
       "Database_Italy_EUROFINS_600_samples.RDS")
) %>%
  filter(!is.na(Potential_CEC_cmol_kg)) %>%
  mutate(ecec = 10 * Potential_CEC_cmol_kg,
         ecec = set_units(ecec, "mmol/kg"),
         lab = "NSIMS",
         method = "quasi-ISO-13536") %>%
  rename(identification = Identification,
         country = Country) %>%
  select(identification, country, lab, method, ecec)

left_join(data_italy_ecec, data_eurofins_ecec,
          by = c("identification")) %>%
  ggplot(aes(ecec.x, ecec.y)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)



# Portugal
data_portugal_ecec <- read_rds(
  here("data", "Eurofins_comparison",
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Portugal_dataset",
       "Database_Portugal_EUROFINS_600_samples.RDS")
) %>%
  filter(!is.na(Potential_CEC_cmol_kg_ISO_13536)) %>%
  mutate(ecec = 10 * Potential_CEC_cmol_kg_ISO_13536,
         ecec = set_units(ecec, "mmol/kg"),
         lab = "NSIMS",
         method = "ISO-13536",
         Identification = as.character(Identification)) %>%
  rename(identification = Identification,
         country = Country) %>%
  select(identification, country, lab, method, ecec)

left_join(data_portugal_ecec, data_eurofins_ecec,
          by = c("identification")) %>%
  ggplot(aes(ecec.x, ecec.y)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)



# all together now

data_ecec <- data_eurofins_ecec %>%
  rename(method_eurofins = method,
         ecec_eurofins   = ecec) %>%
  select(-lab) %>%
  inner_join(
    rbind(
      data_austria_ecec, data_wallonia_ecec, data_france_ecec, 
      data_italy_ecec, data_portugal_ecec
    ) %>%
      select(-lab) %>%
      rename(method_nsims = method,
             ecec_nsims = ecec),
    by = c("identification","country")
  )

ggplot(data_ecec, aes(ecec_nsims, ecec_eurofins, color = country)) +
  geom_point()


save(data_ecec, file = "data/data_ecec.rda")


