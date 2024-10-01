
## AAS summative code: data pre-processing =====================================

## preliminaries ---------------------------------------------------------------

## empty global env
rm(list = ls())

## installing packages
pacman::p_load (tidyverse, vdemdata, countrycode, naniar, mice)

## Chapter 1: Refugee data -----------------------------------------------------

## getting data
refugees_raw = read_csv (file.path ("/Users", 
                                    "teddyyankov", 
                                    "Library", 
                                    "CloudStorage", 
                                    "OneDrive-Nexus365", 
                                    "AAS", 
                                    "Summative", 
                                    "population.csv"))

## cleaning data
refugees = refugees_raw %>%
  
  # removing unnecessary variables
  dplyr::select (-c('Country of origin',
                    'Country of asylum',
                    'IDPs of concern to UNHCR',
                    'Other people in need of international protection',
                    'Stateless persons',
                    'Host Community',
                    'Others of concern')) %>%
  
  # renaming cols
  rename (orig = `Country of origin (ISO)`,
          dest = `Country of asylum (ISO)`,
          year = Year) %>%
  
  # aggregating to obtain forced_mig feature
  mutate (forced_mig = `Refugees under UNHCR's mandate` + `Asylum-seekers`) %>%
  select (-c(`Refugees under UNHCR's mandate`, `Asylum-seekers`)) %>%
  
  # converting to numeric
  mutate (forced_mig = as.numeric(forced_mig)) %>%
  
  # removing NAs
  drop_na (forced_mig) %>%
  
  # adding dyad and triad identifiers
  unite ("orig_dest_year", c(orig, dest, year), sep = "_", remove = F, na.rm = F) %>%
  unite ("orig_dest", c(orig, dest), sep = "_", remove = F, na.rm = F) %>%
  
  # remove rows where orig and dest are the same
  filter (!(orig == dest)) %>%
  
  # filtering out PSE due to duplicate issues in the data
  filter (!orig == "PSE") %>%
  filter (!dest == "PSE")

## obtaining expanded df with all possible combinations
all_combinations <- expand.grid (orig = unique (refugees$orig),
                                 dest = unique (refugees$dest),
                                 year = unique (refugees$year)) %>%
  
  # adding dyad and triad identifiers
  unite ("orig_dest_year", c(orig, dest, year), sep = "_", remove = F, na.rm = F) %>%
  unite ("orig_dest", c(orig, dest), sep = "_", remove = F, na.rm = F)

## merging the original dataframe with the expanded combinations
refugees_exp <- all_combinations %>% left_join (refugees, by = c("orig", "dest", "year", "orig_dest_year", "orig_dest"))

## replacing missing values with 0 for the refugee flows
#refugees_exp[is.na(refugees_exp)] <- 0
  
## filtering to only min 10 flows
refugees_exp_filt = refugees_exp %>%
  
  # grouping
  dplyr::group_by (orig_dest) %>%
  
  # filtering for min 10 flows
  filter (sum (!is.na(forced_mig)) >= 15) %>% ungroup () %>%
  
  # filtering out PSE due to duplicate issues in the data
  filter (!orig == "PSE") %>%
  filter (!dest == "PSE")
  

## Chapter 2: VDEM -------------------------------------------------------------

## getting data 
vdem_raw = vdemdata::vdem

## variables to keep
vars <- c(
  #"e_wbgi_pve", # political stability (likelihood estimator)
  "v2cltort", # freedom from torture 0-4
  #"v2clkill", # freedom from political killings 0-4
  #"v2clacjust", # Social class equality in respect for civil liberty 0-4
  "v2clsocgrp", # Social group equality in respect for civil liberties 0-4
  #"v2csreprss", # civil society repression 0 (a lot of repression) - 4 (no repression)
  #"v2csrlgrep", # religious organisation repression 0-4 
  "v2clrelig", # freedom of religion
  #"v2xcl_rol", # equality before the law
  "e_miinteco", # armed conflict international
  "e_miinterc", # armed conflict internal
  "v2xeg_eqprotec" # Equal protection index ordinal
  ) 

id_vars <- c(
  'country_name', # name of the country
  'COWcode',
  'year'
)

## cleaning V-DEM data
vdem = vdem_raw %>%
  
  # filtering vars to keep
  dplyr::select (all_of (id_vars), all_of (vars)) %>%
  
  # years of interest
  filter (year %in% c(1977:2023)) %>%
  
  # reformatting country var
  mutate (ccode = countrycode (country_name, "country.name", "iso3c")) %>%
  
  # removing countries that did not get match to ISO3 codes
  filter (!(is.na (ccode))) %>%
  
  # keeping only ccode, year and the vars of interest
  dplyr::select (ccode, year, all_of(vars)) %>% 
  
  # renaming vars
  rename_with (
    ~ case_when (
      . == "e_wbgi_pve" ~ "political_stability",
      . == "v2cltort" ~ "freedom_torture",
      . == "v2clkill" ~ "freedom_killings",
      . == "v2clacjust" ~ "class_equality_lib",
      . == "v2clsocgrp" ~ "group_equality_lib",
      . == "v2csreprss" ~ "civilsoc_repression",
      . == "v2csrlgrep" ~ "relig_org_repression",
      . == "v2clrelig" ~ "freedom_religion",
      . == "v2xcl_rol" ~ "equality_law",
      . == "e_miinteco" ~ "conflict_international",
      . == "e_miinterc" ~ "conflict_internal",
      . == "v2xeg_eqprotec" ~ "equal_protection",
      TRUE ~ .
    )
  ) %>%
  
  # arranging alphabetically by ccode
  arrange (ccode)


## Chapter 3: CEPII ------------------------------------------------------------

## getting data
cepii_raw = read_csv (file.path ("/Users", 
                                 "teddyyankov", 
                                 "Library", 
                                 "CloudStorage", 
                                 "OneDrive-Nexus365", 
                                 "AAS", 
                                 "Summative", 
                                 "Gravity_V202211.csv"))

## cleaning
cepii = cepii_raw %>%
  
  # isolating vars
  dplyr::select (year, iso3_o, iso3_d, 
                 pop_o, pop_d, 
                 contig, distw_harmonic, 
                 gdp_o, gdp_d, gdpcap_o, gdpcap_d, 
                 comlang_off, comlang_ethno, comcol, comrelig, col_dep, col_dep_ever, sibling, sibling_ever, scaled_sci_2021) %>%
  
  #renaming vars
  rename_with (
    ~ case_when (
      . == "iso3_d" ~ "dest",
      . == "iso3_o" ~ "orig",
      . == "distw_harmonic" ~ "dist",
      TRUE ~ .
    )
  ) %>%
  
  # adding triad identifiers
  unite ("orig_dest_year", c(orig, dest, year), sep = "_", remove = F, na.rm = F) %>%
  
  # remove rows where orig and dest are the same
  filter (!(orig == dest)) %>%
  
  # filtering to include only dyads for which we have data 
  filter (orig_dest_year %in% unique (refugees$orig_dest_year))

## Chapter 3: ILO --------------------------------------------------------------

## getting data (https://ilostat.ilo.org/data/#)
ilo_raw = read_csv (file.path ("/Users", 
                               "teddyyankov", 
                               "Library", 
                               "CloudStorage", 
                               "OneDrive-Nexus365", 
                               "AAS", 
                               "Summative", 
                               "EAP_DWAP_SEX_AGE_RT_A.csv"))

## cleaning data
ilo = ilo_raw %>%
  
  # keeping only relevant variable
  dplyr::select (ref_area, sex, classif1, time, obs_value) %>%
  
  # renaming vars
  rename_with (
    ~ case_when (
      . == "ref_area" ~ "ccode",
      . == "classif1" ~ "age",
      . == "time" ~ "year",
      . == "obs_value" ~ "labour_part_rate",
      TRUE ~ .
    )
  ) %>%
  
  # filtering age to show only 15-64
  filter (age == "AGE_YTHADULT_Y15-64") %>%
  
  # re-coding sex
  mutate (sex = ifelse (sex == "SEX_M", "male", 
                        ifelse (sex == "SEX_F", "female", 
                                ifelse (sex == "SEX_T", "total", NA 
                                )))) %>%
  
  
  # widening
  pivot_wider (names_from = "sex", values_from = "labour_part_rate", names_prefix = "labour_part_rate_") %>%
  
  # removing no longer relevant vars
  dplyr::select (-c(age, labour_part_rate_NA))
  
## Chapter 4: merging ----------------------------------------------------------

## prerequisite: creating orig and dest versions of VDEM
vdem_orig = vdem %>% rename (orig = ccode) %>% filter (!orig == "PSE")
vdem_dest = vdem %>% rename (dest = ccode) %>% filter (!dest == "PSE")

## prerequisite: creating orig and dest versions of ILO
ilo_orig = ilo %>% rename (orig = ccode)
ilo_dest = ilo %>% rename (dest = ccode)

## creating merged df
merged_dat = refugees %>%
  
  # joining VDEM vars
  left_join (vdem_orig, by = c("orig", "year")) %>%
  left_join (vdem_dest, by = c("dest", "year"), suffix = c("_orig", "_dest")) %>%
  
  # joining CEPII vars
  left_join (cepii, by = c("orig", "dest", "year", "orig_dest_year")) %>%
  
  # joining ILO vars
  left_join (ilo_orig, by = c("orig", "year")) %>%
  left_join (ilo_dest, by = c("dest", "year"), suffix = c("_orig", "_dest")) %>%
  
  # keeping orig_dest pairs only with at least 10 non-NA values in each variable
  dplyr::group_by (orig_dest) %>%
  filter (sum (across (7:40, ~ !is.na(.))) >= 10) %>%
  ungroup()
  
## Chapter 5: Exporting --------------------------------------------------------

write.csv (merged_dat, "merged_dat.csv", row.names = F)

## making sure correct library is enabled
library (naniar)

## NAs matrix
vis_miss (merged_dat, warn_large_data = FALSE) + 
  theme (axis.text.x.top = element_text (angle = 90, hjust = 1, vjust = 0.5))

## Chapter 6: imputing missing data --------------------------------------------
#imputed_merged_dat = complete (mice (merged_dat, m = 5, method = "cart"))

## for conflict vars, replace missing values with zero


## Chapter X: troubleshooting --------------------------------------------------

## examining duplicates 
filter_duplicates <- function (input_df, columns_to_check) {
  
  input_df %>%
    dplyr::group_by (across (all_of (columns_to_check))) %>%
    filter (n() > 1) %>%
    ungroup()
  
}

## applying function to see duplicates in VDEM
result_df <- filter_duplicates (vdem_orig, c('orig', 'year'))

## applying function to see duplicates in CEPII
duplicates_cepii <- filter_duplicates (cepii, c('orig_dest_year'))
cepii2 = cepii %>% filter (!(orig_dest_year %in% unique (duplicates_cepii$orig_dest_year)))
rows_to_keep <- seq (2, nrow(duplicates_cepii), by = 2)
duplicates_cepii = duplicates_cepii %>% arrange (orig_dest_year)
duplicates_cepii_filtered = duplicates_cepii[rows_to_keep, ]
cepii3 = rbind (cepii2, duplicates_cepii_filtered) %>% arrange (orig_dest_year)







  
  
  
  
  
