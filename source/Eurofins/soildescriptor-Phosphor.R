# dataset for variable: Phosphor

# EUROFINS	        1:10 0.01 M CaCl2 (mg/kg)
#                   DA (CaCl₂)
#                   ISO 15923-1
# Austria	          ÖNORM L 1987 ("available P")
# Belgium Flanders	NA
# Belgium Wallonia	Ammonium acetate + EDTA / ISO 11885
# Czech Republic	  Mehlich 3, ICP-OES (mg.kg-1)
# Estonia	          NA
# France	          ISO 11263​ (Olsen); NF X 31-161 (Joret-Hébert)
# Hungary	          NA
# Italy	            ISO 11263 (Olsen)
# Portugal	        ISO 11263:1994​ (Olsen)
# Spain	            ISO 11263​ (Olsen)



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

data_eurofins_phosphor <- data_eurofins %>%
  filter(
    !is.na(P) | !is.na(`P-CaCl2`)
  ) %>%
  mutate(
    phosphor = 10*as.numeric(`P-CaCl2`),
    detlim = P,
    lab = "Eurofins",
    method = "ISO 15923-1"
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, detlim, phosphor)

summary(data_eurofins_phosphor$phosphor)
hist(data_eurofins_phosphor$phosphor)
table(data_eurofins_phosphor$detlim, useNA = "always")


# Austria
data_austria <- read_rds(
  here("data", "Eurofins_comparison",
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Austria_dataset",
       "Database_Austria_EUROFINS_600_samples.RDS")
)

data_austria_phosphor <- data_austria %>%
  # there are 22 NA's in P_mg_kg but not indicated whether they were below a threshold or just missing values?
  # filter(  
  #   !is.na(P_mg_kg)
  # ) %>%
  mutate(
    detlim = NA_character_,
    phosphor = P_mg_kg,
    lab = "NSIMS",
    method = "ÖNORM L 1987"
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, detlim, phosphor)
  
summary(data_austria_phosphor$phosphor)
hist(data_austria_phosphor$phosphor)
table(data_austria_phosphor$detlim, useNA = "always")

left_join(data_austria_phosphor, data_eurofins_phosphor,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(phosphor.x, phosphor.y)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2)



# Wallonia
data_wallonia <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Belgium_Wallonia_dataset",
       "Database_Belgium_Wallonia_EUROFINS_600_samples.RDS")
)

data_wallonia_phosphor <- data_wallonia %>%
  filter(!is.na(Identification)) %>%
  mutate(
    detlim = NA_character_,
    phosphor = P_mg_kg,
    lab = "NSIMS",
    method = "ISO 11885"
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, detlim, phosphor)

summary(data_wallonia_phosphor$phosphor)
hist(data_wallonia_phosphor$phosphor)
table(data_wallonia_phosphor$detlim, useNA = "always")

left_join(data_wallonia_phosphor, data_eurofins_phosphor,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(phosphor.x, phosphor.y)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2)



# Czechia
data_czechia <- read_rds(
  here("data", "Eurofins_comparison",
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Czech_Republic_dataset",
       "Database_Czech_Republic_EUROFINS_600_samples.RDS")
)

data_czechia_phosphor <- data_czechia %>%
  mutate(
    detlim = NA_character_,
    phosphor = P_mg_kg,
    lab = "NSIMS",
    method = "ISO 11885"
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, detlim, phosphor)

summary(data_czechia_phosphor$phosphor)
hist(data_czechia_phosphor$phosphor)
table(data_czechia_phosphor$detlim, useNA = "always")

left_join(data_czechia_phosphor, data_eurofins_phosphor,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(phosphor.x, phosphor.y)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2)



# France (2 methods)
data_france <- read_rds(
  here("data", "Eurofins_comparison", 
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "France_dataset",
       "Database_RMQS_EUROFINS_600_samples.RDS")
)

data_france_phosphor <- data_france %>%
  filter(Soil_monitoring_network == "RMQS") %>%
  mutate(identification = paste0(LUCAS_ID, " ", LAS_reference)) %>%
  select(identification, Country,
         P_NF_X_31_161_Joret_Hebert_g_kg, P_ISO_11263_Olsen_g_kg) %>%
  pivot_longer(contains("P_"), names_to = "method", values_to = "phosphor") %>%
  mutate(
    detlim = NA_character_,
    phosphor = phosphor * 1000,
    lab = "NSIMS",
    method = case_when(
      method == "P_NF_X_31_161_Joret_Hebert_g_kg" ~ "NF X31-161",
      method == "P_ISO_11263_Olsen_g_kg" ~ "ISO 11263"
    )
  ) %>%
  rename(country = Country) %>%
  select(identification, country, lab, method, detlim, phosphor)

with(data_france_phosphor, boxplot(phosphor ~ method))

left_join(data_france_phosphor, data_eurofins_phosphor,
          by = c("identification")) %>%
  ggplot() +
  facet_wrap(~ method.x) +
  geom_point(aes(phosphor.x, phosphor.y)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2)



# Italy
data_italy <- read_rds(
  here("data", "Eurofins_comparison",
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Italy_dataset",
       "Database_Italy_EUROFINS_600_samples.RDS")
)

str(data_italy$P_Olsen_g_kg)

data_italy_phosphor <- data_italy %>%
  filter(Identification != "") %>%
  mutate(
    detlim = case_when(
      grepl("<", P_Olsen_g_kg) ~ "<",
      TRUE ~ NA_character_
    ),
    P_Olsen_g_kg = gsub("<", "", P_Olsen_g_kg),
    phosphor = as.numeric(gsub(",", ".", P_Olsen_g_kg)) * 1000,
    lab = "NSIMS",
    method = "ISO 11263"
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, detlim, phosphor)

summary(data_italy_phosphor$phosphor)
hist(data_italy_phosphor$phosphor)
table(data_italy_phosphor$detlim, useNA = "always")

left_join(data_italy_phosphor, data_eurofins_phosphor,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(phosphor.x, phosphor.y)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2)




# Portugal
data_portugal <- read_rds(
  here("data", "Eurofins_comparison",
       "All_countries_datasets_XLSX_CSV_RDS_format",
       "Portugal_dataset",
       "Database_Portugal_EUROFINS_600_samples.RDS")
)

data_portugal_phosphor <- data_portugal %>%
  mutate(
    detlim = NA_character_,
    phosphor = P_Olsen_g_kg,
    lab = "NSIMS",
    method = "ISO 11263",
    Identification = as.character(Identification)
  ) %>%
  rename(identification = Identification, 
         country = Country) %>%
  select(identification, country, lab, method, detlim, phosphor)

summary(data_portugal_phosphor$phosphor)
hist(data_portugal_phosphor$phosphor)
table(data_portugal_phosphor$detlim, useNA = "always")

left_join(data_portugal_phosphor, data_eurofins_phosphor,
          by = c("identification")) %>%
  ggplot() +
  geom_point(aes(phosphor.x, phosphor.y)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2)





# all countries together
data_phosphor <- data_eurofins_phosphor %>%
  rename(method_eurofins   = method,
         phosphor_eurofins = phosphor,
         detlim_eurofins   = detlim) %>%
  select(-lab) %>%
  inner_join(
    rbind(
      data_austria_phosphor, data_czechia_phosphor, data_france_phosphor,
      data_italy_phosphor, data_portugal_phosphor, data_wallonia_phosphor
    ) %>%
      select(-lab) %>%
      rename(method_nsims   = method,
             phosphor_nsims = phosphor,
             detlim_nsims   = detlim),
    by = c("identification","country")
  )

data_phosphor %>%
  ggplot(aes(phosphor_eurofins, phosphor_nsims)) +
  facet_wrap(~ country + method_nsims) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)


save(data_phosphor, file = "data/data_phosphor.rda")






