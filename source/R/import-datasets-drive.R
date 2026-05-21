## Importing datasets from INBO drive PRJ_SoilHarmony


# manual step: download datasets from drive, save and unzip them in data/


library(tidyverse)
library(here)
library(sf)
library(inbospatial)
library(units)



# -------------------------------------------------------------------------
# Flemish double-sampling study -------------------------------------------

folder_flds <- here("data", "Flanders_Double_Sampling_Data")

# sampling points
#' kopie: needed to change , as decimal to . (Elevation)
sampling_points <- read_delim(
  here(folder_flds,
       "EJPsoil_LUCAS22_Double_sampling_BE_Flanders_Sampling_Points_list - kopie.csv"),
  delim = ";"
)
glimpse(sampling_points)

sampling_points_clean <- sampling_points %>%
  st_as_sf(coords = c("WGS84Longitude", "WGS84Latitude"),
           crs = "WGS84") %>%
  select(LUCAS_PlotID, Town, Elevation, contains("LU"), RSG, Bio_Point)


ogc_vrbg <- "https://geo.api.vlaanderen.be/VRBG2025/ogc/features/v1/"

provinces <- get_feature_ogc(
  url = ogc_vrbg,
  collection = "Refprv",
  crs = "EPSG:31370"
) %>% st_transform(crs = "WGS84")

ggplot() +
  geom_sf(data = provinces) +
  geom_sf(data = sampling_points_clean, aes(color = FieldObs_LU))
#' 5 + 1 sampling points outside Flanders:
#' Cerfontaine, Sombreffe, Seneffe, Genappe, Burdinne + Sint-Jans-Molenbeek


# remove points outside flanders (retain 160 points):
sampling_points_fl <- st_filter(
  sampling_points_clean,
  provinces
)
ggplot() +
  geom_sf(data = provinces) +
  geom_sf(data = sampling_points_fl, aes(color = FieldObs_LU))





# soildata
soildata <- read_csv2(
  here(folder_flds,
       "EJPsoil_LUCAS22_Double_sampling_BE_Flanders_Soildata.csv")
)
glimpse(soildata)

soildata_clean <- soildata %>%
  mutate(Depth = case_when(Depth == "okt/30" ~ "10-30", TRUE ~ Depth)) %>%
  # remove strooisel-laag
  filter(!grepl("strooisel", tolower(FieldSampleID)))

soildata_fl <- soildata_clean %>%
  filter(LUCAS_PlotID %in% sampling_points_fl$LUCAS_PlotID)

length(unique(soildata_fl$LUCAS_PlotID)) # 158 ipv 160

#' 2 points that don't appear in the soildata
setdiff(sampling_points_fl$LUCAS_PlotID, soildata_fl$LUCAS_PlotID)

#' plotid 40083088 has partial duplicates
#' retain the first set of observations for that plotid
soildata_fl %>%
  filter(.by = LUCAS_PlotID, n() != 3) %>%
  View()
soildata_fl_clean <- soildata_fl %>%
  distinct(LUCAS_PlotID, Depth, .keep_all = T)




# bulk density
bulkdens <- read_csv2(
  here(folder_flds,
       "EJPsoil_LUCAS22_Double_sampling_BE_Flanders_Bulk_density.csv")
)
glimpse(bulkdens)

bulkdens_clean <- bulkdens %>%
  mutate(
    across(c(contains("BD_"), contains("SWC_")), ~ as.numeric(.x))
  )
length(unique(bulkdens$LUCAS_PlotID)) # 158 ipv 160



# clean it up and combine together in 1 dataset
soildata_flds <- soildata_fl_clean %>%
  left_join(sampling_points_fl, by = "LUCAS_PlotID") %>%
  left_join(bulkdens_clean, by = c("LUCAS_PlotID","Depth")) %>%
  select(
    LUCAS_PlotID, Town, Elevation, geometry, Depth, LUCAS_LU, FieldObs_LU, RSG, 
    FieldSampleID, LabSampleID, SampScheme, Matrix,       
    Thickness, Sand, Silt, Clay, TC, TIC, TOC, TN, 
    contains("pH_"), contains("EC_"),
    LayerID, BD_mean, BD_sd, BD_CV, SWC_volp_mean, SWC_volp_sd
  ) %>%
  mutate(
    Thickness = set_units(Thickness, "cm"),
    Elevation = set_units(Elevation, "m"),
    Sand = set_units(Sand, "%"),
    Silt = set_units(Silt, "%"),
    Clay = set_units(Clay, "%"),
    TC = set_units(TC, "g/kg"),
    TIC = set_units(TIC, "g/kg"),
    TOC = set_units(TOC, "g/kg"),
    TN = set_units(TN, "g/kg"),
    EC_m_v = set_units(EC_m_v, "µS/cm"),
    EC_v_v = set_units(EC_v_v, "µS/cm"),
    BD_mean = set_units(BD_mean, "g/cm³"),
    BD_sd = set_units(BD_sd, "g/cm³"),
    BD_CV = set_units(BD_CV, "g/cm³"),
    SWC_volp_mean = set_units(SWC_volp_mean, "%"),
    SWC_volp_sd = set_units(SWC_volp_sd, "%")
  )




# -------------------------------------------------------------------------
# Eurofins data -----------------------------------------------------------

folder_eurofin <- here("data", "Eurofins_comparison",
                       "All_countries_datasets_XLSX_CSV_RDS_format")

# subfolder per country
folders <- list.files(folder_eurofin)

import_eurofins <- function(x) {
  rds <- list.files(here(folder_eurofin, x), pattern = ".RDS")
  read_rds(here(folder_eurofin, x, rds))
}

x <- lapply(folders, import_eurofins)



