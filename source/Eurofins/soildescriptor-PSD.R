# dataset for variable: PSD (particle size distribution: clay + silt + sand)

# EUROFINS	        sieve - pipet (%)
#                   NIRS (first " burning of organic matter and washing of salt" (Stokes Law)
#                   NEN 5753
# Austria	          ISO 11464 
# Belgium Flanders	ISO 13320
# Belgium Wallonia	​NF X31-107
# Czech Republic	  NA
# Estonia	          ISO 11277
# France	          NF X 31-107
# Hungary	          MSZ-08-0205:1978
# Italy	            DM 13/09/1999 met II.5 (pipette with OM destruction as ISO 11277)
# Portugal	        ISO 11277 clay,  < 2 µm
# Spain	            pipette method, < 2 µm


library(tidyverse)
library(readxl)
library(here)

# Eurofins data
data_eurofins <- read_excel(
  here("data", "Eurofins_comparison", 
       "Results_and_Standards_EUROFINS_N_SIMS.xlsx"),
  sheet = "Results_EUROFINS",
  col_types = "text"
)

data_eurofins_psd <- data_eurofins %>%
  filter(
    !is.na(`Clay, < 2 µm %`),
    !is.na(`Silt, 2- 50 µm, %`),
    !is.na(`Sand, >50 - 2000 µm, %`)
  ) %>%
  mutate(
    clay = as.numeric(`Clay, < 2 µm %`) / 100,
    silt = as.numeric(`Silt, 2- 50 µm, %`) / 100,
    sand = as.numeric(`Sand, >50 - 2000 µm, %`) / 100,
    total = clay + silt + sand,
    lab = "Eurofins",
    method = "NEN-5753"
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, clay, silt, sand, total)

summary(data_eurofins_psd$total)



# Austria
# data_austria_psd <- read_rds(
#   here("data", "Eurofins_comparison", 
#        "All_countries_datasets_XLSX_CSV_RDS_format",
#        "Austria_dataset",
#        "Database_Austria_EUROFINS_600_samples.RDS")
# )

# only clay available? the metadata shows a method for all 3


# Flanders
data_flanders <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Belgium_Flanders_dataset",
       "Database_Cmon_EUROFINS_600_samples.RDS")
)

data_flanders_psd <- data_flanders %>%
  filter(
    !is.na(Clay_2_um_ISO_13320_percent),
    !is.na(Silt_2_50_um_ISO_13320_percent),
    !is.na(Sand_50_2000_um_ISO_13320_percent)
  ) %>%
  mutate(
    clay = as.numeric(Clay_2_um_ISO_13320_percent) / 100,
    silt = as.numeric(Silt_2_50_um_ISO_13320_percent) / 100,
    sand = as.numeric(Sand_50_2000_um_ISO_13320_percent) / 100,
    total = clay + silt + sand,
    lab = "NSIMS",
    method = "ISO-13320",
    identification = as.character(LUCAS_ID),
    country = "Belgium Flanders"
  ) %>%
  select(identification, country, lab, method, clay, silt, sand, total)

summary(data_flanders_psd$total)

left_join(data_flanders_psd, data_eurofins_psd,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(clay.x, clay.y), color = "red") + 
  geom_point(aes(silt.x, silt.y), color = "blue") + 
  geom_point(aes(sand.x, sand.y), color = "green") + 
  geom_abline(intercept = 0, slope = 1, lty = 2)



# Wallonia
data_wallonia <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Belgium_Wallonia_dataset",
       "Database_Belgium_Wallonia_EUROFINS_600_samples.RDS")
)

data_wallonia_psd <- data_wallonia %>%
  filter(
    !is.na(Clay_2_um_g_kg),
    !is.na(Silt_2_50_um_g_kg_USDA_limits),
    !is.na(Sand_50_2000_um_g_kg_USDA_limits)
  ) %>%
  mutate(
    clay = as.numeric(Clay_2_um_g_kg) / 1000,
    silt = as.numeric(Silt_2_50_um_g_kg_USDA_limits) / 1000,
    sand = as.numeric(Sand_50_2000_um_g_kg_USDA_limits) / 1000,
    total = clay + silt + sand,
    lab = "NSIMS",
    method = "NF X31-107"
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, clay, silt, sand, total)

summary(data_wallonia_psd$total)

left_join(data_wallonia_psd, data_eurofins_psd,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(clay.x, clay.y), color = "red") + 
  geom_point(aes(silt.x, silt.y), color = "blue") + 
  geom_point(aes(sand.x, sand.y), color = "green") + 
  geom_abline(intercept = 0, slope = 1, lty = 2)



# Czech republic
# data_czechia <- read_rds(
#   here("data", "Eurofins_comparison", 
#        "All_countries_datasets_XLSX_CSV_RDS_format",
#        "Czech_Republic_dataset",
#        "Database_Czech_Republic_EUROFINS_600_samples.RDS")
# )

# PSD not measured



# Estonia
# data_estonia <- read_rds(
#   here("data", "Eurofins_comparison", 
#        "All_countries_datasets_XLSX_CSV_RDS_format",
#        "Estonia_dataset",
#        "Database_Estonia_EUROFINS_600_samples.RDS")
# )

# PSD not in the dataset, although the meta-data says it's measured with ISO 11277



# France
data_france <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "France_dataset",
       "Database_RMQS_EUROFINS_600_samples.RDS")
)

data_france_psd <- data_france %>%
  filter(
    Soil_monitoring_network == "RMQS",
    !is.na(Clay_._2_µm_NF_X_31_107_percent),
    !is.na(Loam_._2_µm_NF_X_31_107_percent),
    !is.na(Sand_50_2000_µm_NF_X_31_107_percent)
  ) %>%
  mutate(
    clay = as.numeric(Clay_._2_µm_NF_X_31_107_percent) / 100,
    silt = as.numeric(Loam_._2_µm_NF_X_31_107_percent) / 100,
    sand = as.numeric(Sand_50_2000_µm_NF_X_31_107_percent) / 100,
    total = clay + silt + sand,
    lab = "NSIMS",
    method = "NF X31-107",
    identification = paste0(LUCAS_ID, " ", LAS_reference)
  ) %>%
  rename(country = Country) %>%
  select(identification, country, lab, method, clay, silt, sand, total)

summary(data_france_psd$total)

left_join(data_france_psd, data_eurofins_psd,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(clay.x, clay.y), color = "red") + 
  geom_point(aes(silt.x, silt.y), color = "blue") + 
  geom_point(aes(sand.x, sand.y), color = "green") + 
  geom_abline(intercept = 0, slope = 1, lty = 2)




# Hungary
# data_hungary <- read_rds(
#   here("data", "Eurofins_comparison", 
#        "All_countries_datasets_XLSX_CSV_RDS_format",
#        "Hungary_dataset",
#        "Database_Hungary_EUROFINS_600_samples.RDS")
# )

# no PSD variables found, 
# despite meta-data indicates a method MSZ-08-0205:1978



# Italy
data_italy <- read_rds(
  here("data", "Eurofins_comparison",
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Italy_dataset",
       "Database_Italy_EUROFINS_600_samples.RDS")
)

data_italy_psd <- data_italy %>%
  filter(
    !is.na(Clay_2_um_g_kg),
    !is.na(Silt_2_50_um_g_kg_USDA_limits),
    !is.na(Sand_50_2000_um_g_kg_USDA_limits)
  ) %>%
  mutate(
    clay = as.numeric(Clay_2_um_g_kg) / 1000,
    silt = as.numeric(Silt_2_50_um_g_kg_USDA_limits) / 1000,
    sand = as.numeric(Sand_50_2000_um_g_kg_USDA_limits) / 1000,
    total = clay + silt + sand,
    lab = "NSIMS",
    method = "quasi ISO-11277"
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, clay, silt, sand, total)

summary(data_italy_psd$total)

left_join(data_italy_psd, data_eurofins_psd,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(clay.x, clay.y), color = "red") + 
  geom_point(aes(silt.x, silt.y), color = "blue") + 
  geom_point(aes(sand.x, sand.y), color = "green") + 
  geom_abline(intercept = 0, slope = 1, lty = 2)



# Portugal
data_portugal <- read_rds(
  here("data", "Eurofins_comparison",
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Portugal_dataset",
       "Database_Portugal_EUROFINS_600_samples.RDS")
)

data_portugal_psd <- data_portugal %>%
  filter(
    !is.na(Clay_2_um_g_kg),
    !is.na(Silt_2_20_um_g_kg),
    !is.na(Sand_20_2000_um_g_kg)
  ) %>%
  mutate(
    clay = as.numeric(Clay_2_um_g_kg) / 100,
    silt = as.numeric(Silt_2_20_um_g_kg) / 100,
    sand = as.numeric(Sand_20_2000_um_g_kg) / 100,
    total = clay + silt + sand,
    lab = "NSIMS",
    method = "ISO-11277",
    identification = as.character(Identification)
  ) %>%
  rename(country = Country) %>%
  select(identification, country, lab, method, clay, silt, sand, total)

summary(data_portugal_psd$total)

left_join(data_portugal_psd, data_eurofins_psd,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(clay.x, clay.y), color = "red") + 
  geom_point(aes(silt.x, silt.y), color = "blue") + 
  geom_point(aes(sand.x, sand.y), color = "green") + 
  geom_abline(intercept = 0, slope = 1, lty = 2)



# combine data
data_psd <- data_eurofins_psd %>%
  rename(method_eurofins = method,
         clay_eurofins   = clay,
         silt_eurofins   = silt,
         sand_eurofins   = sand,
         total_eurofins  = total) %>%
  select(-lab) %>%
  inner_join(
    rbind(
      data_wallonia_psd, data_france_psd, data_flanders_psd,
      data_italy_psd, data_portugal_psd
    ) %>%
      select(-lab) %>%
      rename(method_nsims = method,
             clay_nsims   = clay,
             silt_nsims   = silt,
             sand_nsims   = sand,
             total_nsims  = total),
    by = c("identification","country")
  )

# visualize in simplex
library(ggtern)

ggtern(data_psd, aes(silt_nsims, clay_nsims, sand_nsims)) +
  geom_point()

ggtern(data_psd, aes(silt_eurofins, clay_eurofins, sand_eurofins)) +
  geom_point()

ggtern(data_psd, aes(silt_nsims, clay_nsims, sand_nsims)) +
  geom_point() +
  facet_wrap(~ country)

spc <- data.frame(
  .name = c("silt_nsims", "clay_nsims", "sand_nsims",
            "silt_eurofins", "clay_eurofins", "sand_eurofins"),
  .value = rep(c("silt", "clay", "sand"), 2)
) %>%
  mutate(lab = case_when(grepl("nsims", .name) ~ "NSIMS", TRUE ~ "Eurofins"))
data_psd %>%
  pivot_longer_spec(spc) %>%
  ggtern(aes(silt, clay, sand, group = identification, color = lab)) +
  geom_point() +
  geom_line(aes(color = NULL)) +
  facet_wrap(~ country)




save(data_psd, file = "data/data_psd.rda")




