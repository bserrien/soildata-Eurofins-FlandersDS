# script for importing and cleaning the Eurofins dataset

library(tidyverse)

# Eurofins data
data_eurofins <- readxl::read_excel(
  here::here("data", "Eurofins_comparison", 
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
    clay           = `Clay, < 2 µm %`,
    silt           = `Silt, 2- 50 µm, %`,
    sand           = `Sand, >50 - 2000 µm, %`,
    medium_sand    = `Medium sand fraction, M50`,
    som            = `SOM, %`,
    caco3          = `CaCO₃, %`,
    n_tot          = `N tot, gkg`,
    s_tot          = `S-total, g kg`,
    ecec           = `ECEC, mmol+kg`,
    ph_cacl2       = `pH-CaCl₂`,
    soil_bd        = `Soil bulk density, pedo transfer Hollis et al., 2012`,
    s_detlim       = S,
    s_cacl2        = `S-CaCl2`,
    p_detlim       = P,
    p_cacl2        = `P-CaCl2`,
    k_detlim       = K,
    k_cacl2        = `K-CaCl2`,
    ca_detlim      = Ca,
    ca_h2o         = `Ca-water (mmol/l)`,
    mg_detlim      = Mg,
    mg_cacl2       = `Mg-CaCl2`,
    na_detlim      = Na,
    na_cacl2       = `Na-CaCl2`,
    fe_detlim      = Fe,
    fe_cacl2       = `Fe-CaCl2`,
    zn_detlim      = Zn,
    zn_cacl2       = `Zn-CaCl2`,
    ni_detlim      = Ni,
    ni_cacl2       = `Ni-CaCl2`,
    mn_detlim      = Mn,
    mn_cacl2       = `Mn-CaCl2`,
    cu_detlim      = Cu,
    cu_cacl2       = `Cu-CaCl2`,
    b_detlim       = B,
    b_cacl2        = `B-CaCl2`,
    mo_detlim      = Mo,
    mo_cacl2       = `Mo--CaCl2`,
    si_detlim      = Si,
    si_cacl2       = `Si-CaCl2`,
    co_detlim      = Co,
    co_cacl2       = `Co-CaCl2`,
    se_detlim      = Se,
    se_cacl2       = `Se-CaCl2`,
    al_detlim      = Al,
    al_cacl2       = `Al-CaCl2`,
    ti_detlim      = Ti,
    ti_cacl2       = `Ti-CaCl2`,
    v_detlim       = V,
    v_cacl2        = `V-CaCl2`,
    as_detlim      = As,
    as_cacl2       = `As-CaCl2`,
    cd_detlim      = Cd,
    cd_cacl2       = `Cd-CaCl2`,
    cr_detlim      = Cr,
    cr_cacl2       = `Cr-CaCl2`,
    pb_detlim      = Pb,
    pb_cacl2       = `Pb-CaCl2`,
    sn_detlim      = Sn,
    sn_cacl2       = `Sn-CaCl2`,
    pot_min_n      = `Pot. Mineralizable N`,
    oxex_al        = `oxalate extracted Al`,
    oxex_fe        = `oxalate extracted Fe`,
    total_fungi    = `Total Fungi (PLFA)`
  ) %>%
  mutate(
    lab = "Eurofins",
    across(
      c(soc, sic, c_tot, clay, silt, sand, medium_sand, som, caco3,
        n_tot, s_tot, ecec, ph_cacl2, soil_bd,
        s_cacl2, p_cacl2, k_cacl2, ca_h2o, mg_cacl2, na_cacl2, fe_cacl2,
        zn_cacl2, ni_cacl2, mn_cacl2, cu_cacl2, b_cacl2, mo_cacl2, si_cacl2,
        co_cacl2, se_cacl2, al_cacl2, ti_cacl2, v_cacl2, as_cacl2, cd_cacl2,
        cr_cacl2, pb_cacl2, sn_cacl2, pot_min_n, oxex_al, oxex_fe, total_fungi), 
      ~ as.numeric(.x)
    )
  )

rm(data_eurofins)

str(data_eurofins_clean)




# -------------------------------------------------------------------------
# trying to figure out which Hollis et al. (2012) PTF was used by  --------

# Eurofins metadata does not specify which PTF was used, very annoying!
# probably different PTF's (Netherlands has outlying values not fitting)

data_eurofins_clean <- data_eurofins_clean %>%
  mutate(
    #soil_bd = soil_bd / 1000,
    soil_bd2 = 1.780 - 0.379 * sqrt(soc) + 0.00123 * 15,
    soil_bd3 = 1.4903 - 0.33293 * log(soc),
    soil_bd4 = 0.80806 + 0.823844 * exp(-0.27993 * soc) + 
      0.0014065 * sand - 0.0010299 * clay,
    soil_bd5 = 0.69794 + 0.750636 * exp(-0.230355 * soc) + 
      0.0008687 * sand - 0.0005164 * clay,
    soil_bd6 = 1.482 - 0.6786 * log10(som)
  )

data_eurofins_clean %>%
  ggplot(aes(soil_bd, soil_bd2, color = country)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)



data_eurofins_clean <- data_eurofins_clean %>%
  filter(!is.na(soc)) %>%
  mutate(
    soil_bd_random = rnorm(n(), 
                           mean = 1.780 - 0.379 * sqrt(soc) + 0.00123 * 15,
                           sd = .2)
  )

ggplot(data_eurofins_clean, aes(soil_bd/1000, soil_bd_random)) +
  geom_point(aes(color = country))


ggplot(data_eurofins_clean, aes(soil_bd, fill = country)) +
  geom_density()


ggplot(data_eurofins_clean, aes(soil_bd_random)) +
  geom_density()

summary(data_eurofins_clean$soil_bd_random)



ggplot(data_eurofins_clean, aes(soc, soil_bd_random)) +
  geom_point() +
  geom_function(fun = function(x) 1.780 - 0.379 * sqrt(x) + 0.00123 * 15,
                color = "red")





