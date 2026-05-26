---
title: "Exploratory analysis Flanders double-sampling study"
format: 
  html:
    keep-md: true
---

## 

This short report contains my exploratory work with the Flanders double-sampling study and contains some questions on how the data should be used.


::: {.cell messages='false'}

:::


## Data import and inspection

The dataset contains three csv files.

### (1) Sampling points


::: {.cell}

```{.r .cell-code}
folder_flds <- here("data", "Flanders_Double_Sampling_Data")

# sampling points
sampling_points <- read_delim(
  here(folder_flds,
       "EJPsoil_LUCAS22_Double_sampling_BE_Flanders_Sampling_Points_list.csv"),
  delim = ";",
  col_types = cols(
    # import as characters to fix issues with different notation:
    # both . and , are used as decimal character ???
    WGS84Latitude = col_character(),
    WGS84Longitude = col_character(),
    Elevation = col_character()
  )
)
glimpse(sampling_points)
```

::: {.cell-output .cell-output-stdout}

```
Rows: 166
Columns: 11
$ Nb             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
$ LUCAS_PlotID   <dbl> 38723090, 38883112, 38923094, 38983120, 39283116, 39323…
$ Town           <chr> "Ronse", "Oosterzele", "Geraardsbergen", "Berlare", "Ka…
$ WGS84Latitude  <chr> "50.74562", "50.95486", "50.79672", "51.03398", "51.019…
$ WGS84Longitude <chr> "3.629459", "3.829376", "3.90732", "3.961944", "4.39292…
$ Elevation      <chr> "53,3", "32,6", "15,3", "2,8", "6,7", "4,6", "4,4", "3,…
$ FieldObs_LU    <chr> "Woodland", "Woodland", "Woodland", "Woodland", "Woodla…
$ LUCAS_LU       <chr> "Woodland", "Woodland", "Woodland", "Woodland", "Woodla…
$ RSG            <chr> "Retisols", "Stagnosols", "Cambisols", "Phaeozems", "Ph…
$ Bio_Point      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ LUCAS2018      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0…
```


:::
:::


Little data cleaning and formatting as `sf` object:


::: {.cell}

```{.r .cell-code}
sampling_points_clean <- sampling_points %>%
  mutate(
    across(c(WGS84Latitude, WGS84Longitude, Elevation), 
           ~ as.numeric(gsub(",", ".", .x)))
  ) %>%
  st_as_sf(coords = c("WGS84Longitude", "WGS84Latitude"),
           crs = "WGS84") %>%
  select(LUCAS_PlotID, Town, Elevation, contains("LU"), RSG, Bio_Point)

glimpse(sampling_points_clean)
```

::: {.cell-output .cell-output-stdout}

```
Rows: 166
Columns: 9
$ LUCAS_PlotID <dbl> 38723090, 38883112, 38923094, 38983120, 39283116, 3932311…
$ Town         <chr> "Ronse", "Oosterzele", "Geraardsbergen", "Berlare", "Kape…
$ Elevation    <dbl> 53.3, 32.6, 15.3, 2.8, 6.7, 4.6, 4.4, 3.9, 31.0, 13.9, 32…
$ FieldObs_LU  <chr> "Woodland", "Woodland", "Woodland", "Woodland", "Woodland…
$ LUCAS_LU     <chr> "Woodland", "Woodland", "Woodland", "Woodland", "Woodland…
$ LUCAS2018    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, …
$ RSG          <chr> "Retisols", "Stagnosols", "Cambisols", "Phaeozems", "Phae…
$ Bio_Point    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
$ geometry     <POINT [°]> POINT (3.629459 50.74562), POINT (3.829376 50.95486…
```


:::
:::


Five data points lie in Wallonia and 1 in Brussels. Question: Do we need to retain them for the comparison of sampling schemes (LUCAS vs. Cmon)?


::: {.cell}

```{.r .cell-code}
# import provinces of Flanders
ogc_vrbg <- "https://geo.api.vlaanderen.be/VRBG2025/ogc/features/v1/"
provinces <- get_feature_ogc(
  url = ogc_vrbg,
  collection = "Refprv",
  crs = "EPSG:31370"
) %>% st_transform(crs = "WGS84")

# print points outside Flanders
outside_idx <- st_disjoint(
  sampling_points_clean, 
  st_union(provinces), 
  sparse = FALSE
)
sampling_points_clean[outside_idx, ] %>%
  st_drop_geometry() %>%
  select(Town) %>%
  print()
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 1
  Town               
  <chr>              
1 Sint-Jans-Molenbeek
2 Cerfontaine        
3 Sombreffe          
4 Seneffe            
5 Genappe            
6 Burdinne           
```


:::

```{.r .cell-code}
# filter on points inside Flanders (N = 160)
sampling_points_fl <- st_filter(
  sampling_points_clean,
  provinces
)
ggplot() +
  geom_sf(data = provinces) +
  geom_sf(data = sampling_points_fl)
```

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/data-cleaning2-1.png){width=672}
:::
:::


### (2) Soildata


::: {.cell}

```{.r .cell-code}
soildata <- read_csv2(
  here(folder_flds,
       "EJPsoil_LUCAS22_Double_sampling_BE_Flanders_Soildata.csv")
)
glimpse(soildata)
```

::: {.cell-output .cell-output-stdout}

```
Rows: 559
Columns: 22
$ LUCAS_PlotID  <dbl> 38043130, 38043130, 38043130, 38083120, 38083120, 380831…
$ FieldSampleID <chr> "38043130_0-10", "38043130_10-30", "38043130_LUCAS", "38…
$ LabSampleID   <chr> "23-003424", "23-003425", "23-003426", "23-003430", "23-…
$ SampScheme    <chr> "Cmon", "Cmon", "LUCAS", "Cmon", "Cmon", "LUCAS", "Cmon"…
$ Matrix        <chr> "Mineral", "Mineral", "Mineral", "Mineral", "Mineral", "…
$ Depth         <chr> "0-10", "okt/30", "0-30", "0-10", "okt/30", "0-30", "0-1…
$ Thickness     <dbl> 10, 20, 30, 10, 20, 30, 10, 20, 30, 10, 20, 30, 10, 20, …
$ Sand          <dbl> 38.20, 40.30, 43.30, 39.90, 22.80, 40.90, 34.40, 37.20, …
$ Silt          <dbl> 41.00, 42.70, 43.10, 46.70, 67.90, 46.70, 50.10, 49.70, …
$ Clay          <dbl> 20.80, 17.00, 13.60, 13.30, 9.30, 12.40, 15.60, 13.20, 1…
$ TC            <dbl> 18.7, 19.3, 19.6, 13.7, 13.2, 14.7, 29.9, 12.8, 21.2, 28…
$ TIC           <dbl> 7.21, 7.24, 7.25, NA, NA, NA, NA, NA, NA, 9.19, 8.27, 14…
$ TOC           <dbl> 11.46, 12.03, 12.36, 13.68, 13.16, 14.70, 29.94, 12.80, …
$ TN            <dbl> 1.21, 1.25, 1.30, 1.49, 1.45, 1.61, 3.04, 1.34, 2.16, 1.…
$ pH_KCl_v_v    <dbl> 7.61, 7.54, 7.69, 5.84, 5.13, 6.14, 4.80, 4.60, 4.29, 7.…
$ EC_m_v        <dbl> 116.67, 126.09, 111.66, NA, NA, NA, NA, NA, NA, 149.51, …
$ EC_v_v        <dbl> 96.95, 104.83, 98.67, NA, NA, NA, NA, NA, NA, 136.87, 15…
$ pH_H20_m_v    <dbl> 8.39, 8.36, 8.38, NA, NA, NA, NA, NA, NA, 8.34, 8.38, 8.…
$ pH_H20_v_v    <dbl> 8.32, 8.27, 8.26, NA, NA, NA, NA, NA, NA, 8.20, 8.38, 8.…
$ pH_KCl_m_v    <dbl> 7.51, 7.48, 7.59, NA, NA, NA, NA, NA, NA, 7.54, 7.59, 7.…
$ pH_CaCl2_m_v  <dbl> 7.600, 7.570, 7.410, NA, NA, NA, NA, NA, NA, 7.650, 7.69…
$ pH_CaCl2_v_v  <dbl> 7.49, 7.46, 7.48, NA, NA, NA, NA, NA, NA, 7.50, 7.68, 7.…
```


:::
:::


Little data cleaning step and filtering observations. Questions/remarks: Do we need to retain the data from the strooisel-laag? There is 1 plot with 6 observations, are these partial duplicates? There are two points that are listed in the previous csv file but have no corresponding records in the soildata csv.


::: {.cell}

```{.r .cell-code}
# Depth data were formatted as a date (probably a setting from Excel/GSheet?)
soildata_clean <- soildata %>%
  mutate(
    Depth = case_when(Depth == "okt/30" ~ "10-30", 
                      TRUE ~ Depth)
  )

# 136 plots have 3 observations (2*Cmon + 1*LUCAS), 29 plots have 5 observations (mineral + organic matrix) and 1 plot has 6 observations (partial duplicates?)
soildata_clean %>%
  summarise(.by = LUCAS_PlotID, N = n()) %>%
  summarise(.by = N, plots = n())
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 3 × 2
      N plots
  <int> <int>
1     3   136
2     5    29
3     6     1
```


:::

```{.r .cell-code}
# selecting soildata: remove strooisel-laag, keep only points in Flanders and remove partial duplicates
soildata_clean_fl <- soildata_clean %>%
  filter(
    !grepl("strooisel", tolower(FieldSampleID)),
    LUCAS_PlotID %in% sampling_points_fl$LUCAS_PlotID
  ) %>%
  distinct(LUCAS_PlotID, Depth, .keep_all = T)

# there are 2 points that appear in the sampling_points object but that don't appear in the soildata:
sampling_points_fl %>%
  filter(!(LUCAS_PlotID %in% soildata_clean_fl$LUCAS_PlotID)) %>%
  st_drop_geometry() %>%
  select(LUCAS_PlotID, Town) %>%
  print()
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 2
  LUCAS_PlotID Town      
         <dbl> <chr>     
1     38463096 Kortrijk  
2     39743098 Kortenaken
```


:::
:::


Question: Why are some TOC values sometimes larger than TC? (is TC not the sum of TOC and TIC?; is there a rounding issue?) Missing values in TIC when TC and TOC are both observed, can you just use TC - TOC as imputed value? 


::: {.cell}

```{.r .cell-code}
soildata_clean_fl %>%
  mutate(
    TC  = janitor::round_half_up(TC, 1),
    TOC = janitor::round_half_up(TOC, 1),
    TIC = janitor::round_half_up(TIC, 1)
  ) %>%
  filter(TOC > TC) %>%
  select(LUCAS_PlotID, TC, TOC, TIC)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 16 × 4
   LUCAS_PlotID    TC   TOC   TIC
          <dbl> <dbl> <dbl> <dbl>
 1     38363144  25.8  25.9    NA
 2     38363148  21.6  21.7    NA
 3     38703142  24.9  25      NA
 4     39323116  31.9  32      NA
 5     39423090   7     7.1    NA
 6     39463128  13.5  13.6    NA
 7     39503150  18.3  18.4    NA
 8     39823134  31.2  31.3    NA
 9     39823144  17    17.1    NA
10     39963138  20.1  20.2    NA
11     40043090  28    28.1    NA
12     40043090  15.9  16      NA
13     40043108  12.1  12.2    NA
14     40043130  56    56.1    NA
15     40163108  19.1  19.2    NA
16     40203120  53.3  53.4    NA
```


:::
:::


Question: To compare LUCAS vs. Cmon, should we combine the two Cmon measurements? Can you take the weighted average of the 0-10 cm sample and the 10-30 cm sample (with respective weights 1/3 and 2/3) to get an average for the 0-30 cm depth?  


::: {.cell}

```{.r .cell-code}
# weighted average for Cmon sampling scheme:
soildata_clean_fl_avg <- soildata_clean_fl %>%
  mutate(
    weights = case_when(Depth == "0-10"  ~ 1/3,
                        Depth == "10-30" ~ 2/3,
                        Depth == "0-30"  ~ 1),
    .after = Depth
  ) %>%
  summarise(
    .by = c(LUCAS_PlotID, SampScheme, Matrix),
    Depth = "0-30",
    Thickness = sum(Thickness),
    across(c(Sand, Silt, Clay, TC, TIC, TOC, TN, 
             contains("pH"), contains("EC")), 
           ~ weighted.mean(.x, w = weights, na.rm = T))
  ) %>%
  mutate(
    across(c(Sand, Silt, Clay, TC, TIC, TOC, TN, 
             contains("pH"), contains("EC")),
           ~ replace_when(.x, is.nan(.x) ~ NA_real_))
  )
```
:::


### (3) Bulk density


::: {.cell}

```{.r .cell-code}
bulkdens <- read_csv2(
  here(folder_flds,
       "EJPsoil_LUCAS22_Double_sampling_BE_Flanders_Bulk_density.csv")
)
glimpse(bulkdens)
```

::: {.cell-output .cell-output-stdout}

```
Rows: 331
Columns: 8
$ LUCAS_PlotID  <dbl> 29723114, 29723114, 38043130, 38043130, 38083120, 380831…
$ LayerID       <chr> "29723114_0-10 cm", "29723114_10-30 cm", "38043130_0-10 …
$ Depth         <chr> "0-10", "10-30", "0-10", "10-30", "0-10", "10-30", "0-10…
$ BD_mean       <chr> "1.55", "1.63", "1.51", "1.50", "1.44", "1.50", "1.35", …
$ BD_sd         <chr> "0.087", "0.075", "0.014", "0.053", "0.035", "0.013", "0…
$ BD_CV         <chr> "5.65", "4.62", "0.94", "3.50", "2.43", "0.84", "6.45", …
$ SWC_volp_mean <dbl> 1373, 1325, 4698, 4778, 4765, 4605, 4243, 3978, 5175, 51…
$ SWC_volp_sd   <chr> "2.31", "1.59", "1.29", "2.10", "0.50", "0.89", "3.59", …
```


:::
:::


Little data cleaning step and weighted-average:


::: {.cell}

```{.r .cell-code}
bulkdens_clean <- bulkdens %>%
  mutate(
    across(c(contains("BD_"), contains("SWC_")), ~ as.numeric(.x))
  ) %>%
  mutate(
    weights = case_when(Depth == "0-10"  ~ 1/3,
                        Depth == "10-30" ~ 2/3,
                        Depth == "0-30"  ~ 1),
    .after = Depth
  ) %>%
  summarise(
    .by = LUCAS_PlotID,
    Depth = "0-30",
    SampScheme = "Cmon",
    across(c(contains("BD_"), contains("SWC_")), 
           ~ weighted.mean(.x, w = weights, na.rm = T))
  )
```
:::


### Combine datasets and some further cleaning


::: {.cell}

```{.r .cell-code}
soildata_flds <- soildata_clean_fl_avg %>%
  left_join(sampling_points_fl, by = "LUCAS_PlotID") %>%
  left_join(bulkdens_clean, by = c("LUCAS_PlotID","Depth","SampScheme")) %>%
  select(
    LUCAS_PlotID, Town, Elevation, geometry, LUCAS_LU, FieldObs_LU, RSG, 
    #FieldSampleID, LabSampleID, 
    SampScheme, Depth, Matrix,       
    Thickness, Sand, Silt, Clay, TC, TIC, TOC, TN, 
    contains("pH_"), contains("EC_"),
    #LayerID, 
    BD_mean, BD_sd, BD_CV, SWC_volp_mean, SWC_volp_sd
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

summary(soildata_flds)
```

::: {.cell-output .cell-output-stdout}

```
  LUCAS_PlotID             Town       Elevation               geometry  
 Min.   :38043130   Length   :316   Min.   :  0.90   POINT        :316  
 1st Qu.:38683138   N.unique :123   1st Qu.: 10.60   epsg:4326    :  0  
 Median :39373116   N.blank  :  0   Median : 24.35   +proj=long...:  0  
 Mean   :39229448   Min.nchar:  3   Mean   : 31.07                      
 3rd Qu.:39743148   Max.nchar: 21   3rd Qu.: 44.20                      
 Max.   :40223060                   Max.   :123.00                      
                                                                        
      LUCAS_LU      FieldObs_LU         RSG          SampScheme 
 Length   :316   Length   :316   Length   :316   Length   :316  
 N.unique :  5   N.unique :  5   N.unique : 13   N.unique :  2  
 N.blank  :  0   N.blank  :  0   N.blank  :  0   N.blank  :  0  
 Min.nchar:  7   Min.nchar:  7   Min.nchar:  7   Min.nchar:  4  
 Max.nchar: 10   Max.nchar:  9   Max.nchar: 10   Max.nchar:  5  
                                                                
                                                                
       Depth           Matrix      Thickness       Sand            Silt      
 Length   :316   Length   :316   Min.   :30   Min.   : 3.60   Min.   : 1.70  
 N.unique :  1   N.unique :  1   1st Qu.:30   1st Qu.:19.65   1st Qu.:15.53  
 N.blank  :  0   N.blank  :  0   Median :30   Median :49.78   Median :33.38  
 Min.nchar:  4   Min.nchar:  7   Mean   :30   Mean   :49.53   Mean   :36.32  
 Max.nchar:  4   Max.nchar:  7   3rd Qu.:30   3rd Qu.:77.55   3rd Qu.:56.82  
                                 Max.   :30   Max.   :95.50   Max.   :78.90  
                                                                             
      Clay              TC              TIC               TOC       
 Min.   : 1.967   Min.   :  2.40   Min.   : 0.1000   Min.   : 2.37  
 1st Qu.: 6.638   1st Qu.: 13.62   1st Qu.: 0.1567   1st Qu.:13.46  
 Median :13.483   Median : 19.10   Median : 0.5500   Median :18.94  
 Mean   :14.141   Mean   : 23.30   Mean   : 3.1703   Mean   :22.56  
 3rd Qu.:18.775   3rd Qu.: 26.65   3rd Qu.: 2.5600   3rd Qu.:26.24  
 Max.   :49.200   Max.   :134.50   Max.   :48.8300   Max.   :88.68  
                                   NAs    :241                      
       TN          pH_KCl_v_v      pH_H20_m_v      pH_H20_v_v   
 Min.   :0.140   Min.   :2.750   Min.   :3.830   Min.   :3.800  
 1st Qu.:1.133   1st Qu.:4.310   1st Qu.:4.860   1st Qu.:4.910  
 Median :1.467   Median :5.083   Median :6.055   Median :6.080  
 Mean   :1.801   Mean   :5.208   Mean   :6.031   Mean   :5.999  
 3rd Qu.:2.133   3rd Qu.:6.140   3rd Qu.:7.114   3rd Qu.:7.120  
 Max.   :8.370   Max.   :7.940   Max.   :8.440   Max.   :8.360  
                                 NAs    :174     NAs    :173    
   pH_KCl_m_v     pH_CaCl2_m_v    pH_CaCl2_v_v       EC_m_v       
 Min.   :2.690   Min.   :3.185   Min.   :2.900   Min.   :  23.98  
 1st Qu.:3.840   1st Qu.:4.044   1st Qu.:4.065   1st Qu.:  52.95  
 Median :5.030   Median :5.280   Median :5.330   Median :  83.56  
 Mean   :5.088   Mean   :5.296   Mean   :5.294   Mean   : 110.85  
 3rd Qu.:6.282   3rd Qu.:6.383   3rd Qu.:6.450   3rd Qu.: 117.51  
 Max.   :7.770   Max.   :7.820   Max.   :7.790   Max.   :1026.34  
 NAs    :180     NAs    :174     NAs    :173     NAs    :173      
     EC_v_v           BD_mean          BD_sd             BD_CV        
 Min.   :  19.01   Min.   :0.490   Min.   :0.01200   Min.   : 0.7933  
 1st Qu.:  42.39   1st Qu.:1.283   1st Qu.:0.05967   1st Qu.: 4.2300  
 Median :  63.62   Median :1.407   Median :0.08667   Median : 6.4467  
 Mean   :  93.91   Mean   :1.359   Mean   :0.10018   Mean   : 8.2955  
 3rd Qu.:  95.72   3rd Qu.:1.483   3rd Qu.:0.12333   3rd Qu.: 9.7933  
 Max.   :1093.39   Max.   :1.703   Max.   :0.44667   Max.   :77.5433  
 NAs    :174       NAs    :163     NAs    :163       NAs    :163      
 SWC_volp_mean    SWC_volp_sd    
 Min.   :-2495   Min.   : 0.500  
 1st Qu.: 1681   1st Qu.: 1.647  
 Median : 2773   Median : 2.710  
 Mean   : 2819   Mean   : 4.125  
 3rd Qu.: 3793   3rd Qu.: 4.477  
 Max.   : 7194   Max.   :83.997  
 NAs    :163     NAs    :163     
```


:::
:::


## Data exploration

### Landuse & soil types

Subgroup analyses within land-use and soil types will need to be restricted to groups with enough observations. 


::: {.cell}

```{.r .cell-code}
soildata_flds %>%
  summarise(.by = FieldObs_LU, N = n_distinct(LUCAS_PlotID)) %>%
  arrange(desc(N)) %>%
  print(n = 5)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 5 × 2
  FieldObs_LU     N
  <chr>       <int>
1 Cropland       61
2 Grassland      51
3 Woodland       41
4 Wetland         4
5 Orchard         1
```


:::

```{.r .cell-code}
soildata_flds %>%
  summarise(.by = RSG, N = n_distinct(LUCAS_PlotID)) %>%
  arrange(desc(N)) %>%
  print(n = 15)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 13 × 2
   RSG            N
   <chr>      <int>
 1 Cambisols     41
 2 Luvisols      23
 3 Retisols      19
 4 Anthrosols    19
 5 Podzols       17
 6 Umbrisols      8
 7 Phaeozems      7
 8 Stagnosols     6
 9 Arenosols      6
10 Gleysols       5
11 Technosols     3
12 Planosols      2
13 Regosols       2
```


:::
:::


### Comparing LUCAS & Cmon sampling schemes

The graphs below show a scatter plot and loess smoother for the parameter measured with LUCAS vs. Cmon (weighted average). The bisector is added for reference. Detection of bivariate outliers is performed with the robust method Minimum Covariance Determinant (`performance` package). Not all the detected outliers will be influential for model fitting but may signal interesting differences between LUCAS and Cmon. Alternative: outlier detection with Bland-Altman analysis.


::: {.cell}

```{.r .cell-code}
# plot + outlier detection with Minimum Covariance Determinant
compare_schemes <- function(vrb) {
  data_wide <- soildata_flds %>%
    select(LUCAS_PlotID, SampScheme, all_of(vrb)) %>%
    pivot_wider(names_from = SampScheme, values_from = all_of(vrb),
                names_prefix = paste0(vrb, "_")) %>%
    filter(
      !is.na(.data[[paste0(vrb,"_Cmon")]]),
      !is.na(.data[[paste0(vrb,"_LUCAS")]])
    ) 
  outl <- performance::check_outliers(
    data_wide %>% 
      select(-LUCAS_PlotID), 
    method = "mcd", percentage_central = .75,
    verbose = TRUE
  )
  data_wide$outlier <- FALSE
  data_wide$outlier[which(outl)] <- TRUE
  
  data_wide %>%
    mutate(
      lab_outl = case_when(outlier ~ LUCAS_PlotID, TRUE ~ NA_real_)
    ) %>%
    ggplot(aes(.data[[paste0(vrb,"_Cmon")]], 
               .data[[paste0(vrb,"_LUCAS")]], 
               color = outlier)) +
    geom_point() +
    geom_smooth(
      inherit.aes = F,
      aes(.data[[paste0(vrb,"_Cmon")]], 
               .data[[paste0(vrb,"_LUCAS")]]),
      method = "loess", color = "grey"
    ) +
    ggrepel::geom_text_repel(aes(label = lab_outl), show.legend = F,
                             size = 2) +
    geom_abline(slope = 1, intercept = 0, lty = 2)
}

# apply function
sapply(
  FUN = compare_schemes,
  X = c("Sand","Silt","Clay","TC","TOC","TIC","TN",
        "pH_KCl_v_v","pH_KCl_m_v",
        "pH_H20_m_v","pH_H20_v_v",
        "pH_CaCl2_m_v","pH_CaCl2_v_v",
        "EC_m_v","EC_v_v")
)
```

::: {.cell-output .cell-output-stdout}

```
$Sand
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$Silt
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-2.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$Clay
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-3.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$TC
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-4.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$TOC
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-5.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$TIC
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-6.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$TN
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-7.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$pH_KCl_v_v
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-8.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$pH_KCl_m_v
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-9.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$pH_H20_m_v
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-10.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$pH_H20_v_v
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-11.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$pH_CaCl2_m_v
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-12.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$pH_CaCl2_v_v
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-13.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$EC_m_v
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-14.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```

$EC_v_v
```


:::

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/comparing-lucas-cmon-15.png){width=672}
:::
:::


Question: For the comparison between Cmon and LUCAS; should a TF be developed to translate (predict) values between them or are descriptive statistics enough? Eg. Bland-Altman, Wilcox-test.


::: {.cell}

```{.r .cell-code}
# to be decided: statistical comparison with a Wilcox test (location-shift)
# => need to apply a multiple-comparison correction (15 variables tested)
compare_schemes_wilcox <- function(vrb) {
  data_wide <- soildata_flds %>%
    select(LUCAS_PlotID, SampScheme, all_of(vrb)) %>%
    mutate(across(all_of(vrb), ~ as.numeric(.x))) %>%
    pivot_wider(names_from = SampScheme, values_from = all_of(vrb),
                names_prefix = paste0(vrb, "_")) %>%
    filter(
      !is.na(.data[[paste0(vrb,"_Cmon")]]),
      !is.na(.data[[paste0(vrb,"_LUCAS")]])
    )
  wilcox.test(
    data_wide[[paste0(vrb,"_Cmon")]],
    data_wide[[paste0(vrb,"_LUCAS")]],
    paired = T, conf.int = T, correct = T,
  ) %>% broom::tidy()
}
  
# apply function
sapply(
  FUN = compare_schemes_wilcox,
  X = c("Sand","Silt","Clay","TC","TOC","TIC","TN",
        "pH_KCl_v_v","pH_KCl_m_v",
        "pH_H20_m_v","pH_H20_v_v",
        "pH_CaCl2_m_v","pH_CaCl2_v_v",
        "EC_m_v","EC_v_v")
) %>% t() %>%
  as.data.frame() %>%
  select(estimate, p.value, conf.low, conf.high) %>%
  print(digits = 2)
```

::: {.cell-output .cell-output-stdout}

```
             estimate p.value conf.low conf.high
Sand              0.2    0.33    -0.22      0.67
Silt            -0.42   0.045    -0.87  -5.6e-05
Clay             0.15    0.24     -0.1      0.38
TC               -2.2 2.4e-13     -3.1      -1.5
TOC              -2.2 5.6e-13     -3.1      -1.5
TIC              0.02    0.82    -0.18      0.21
TN              -0.18 1.8e-14    -0.25     -0.13
pH_KCl_v_v      0.057 0.00029    0.028      0.09
pH_KCl_m_v       0.07    0.16   -0.016      0.19
pH_H20_m_v      0.075   0.045   0.0025      0.16
pH_H20_v_v       0.08   0.059  -0.0017      0.18
pH_CaCl2_m_v     0.13  0.0025    0.048      0.23
pH_CaCl2_v_v    0.055    0.27   -0.027      0.21
EC_m_v            4.4     0.5     -8.6        17
EC_v_v            2.3     0.5     -5.3        11
```


:::
:::


### Transfer functions pH

For this analysis we don't need the averaged Cmon data because we need to compare methods that are measured at each depth, but depth could play the role of a covariate if necessary. Strong correlations (>= .99) are observed between all pairs of pH measurement methods (>= .98 when using Spearman instead of Pearson).  


::: {.cell}

```{.r .cell-code}
soildata_clean_fl %>%
  select(contains("pH")) %>%
  GGally::ggscatmat(corMethod = c("pearson"))
```

::: {.cell-output-display}
![](FlandersDS-report_files/figure-html/scatmat-1.png){width=672}
:::
:::


## How to use these data in the simulation experiment?


