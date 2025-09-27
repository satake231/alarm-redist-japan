###############################################################################
# Download and prepare data for `01_hokkaido` HoR analysis
# © ALARM Project, May 2023
# Modified for House of Representatives data
###############################################################################

# Set up packages
library(redist)
library(geomander)
library(sf)
library(tidyverse)
library(here)

# Pull functions
setwd(here("function"))
files.sources <- list.files()
sapply(files.sources, source)
rm(files.sources)
setwd(here())

# Define parameters for simulation
sim_type <- "smc"
nsims <- 20000
pref_code <- 01
pref_name <- "hokkaido"
lakes_removed <- c()
ndists_new <- 12
ndists_old <- 12
pop_tol <- 0.33

# 2017 redistricting parameters
lh_2017_max_to_min <- 1.871
lh_2017_mun_split <- 2
lh_2017_gun_split <- 0
lh_2017_koiki_split <- 2

# 2022 redistricting parameters
lh_2022_max_to_min <- 1.858
lh_2022_mun_split <- 3
lh_2022_gun_split <- 1
lh_2022_koiki_split <- 2

# Split municipalities under 2017 plan
split_code_2017 <- c(01102, 01107)  # 札幌市北区、札幌市西区

# Split municipalities under 2022 plan
split_code_2022 <- c(01102, 01107, 01104)  # 札幌市北区、札幌市西区、札幌市白石区

# 振興局 that are split
gun_exception <- c("ishikari")  # 石狩振興局

# Change time limit
options(timeout = 300)

# Download Census shapefile (2020)
pref_shp_2020 <- download_shp(pref_code)

# Clean Census shapefile
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf()

# Download 2020 Census population data
pref_pop_2020 <- download_pop_2020(pref_code)

# Download House of Representatives election data (Proportional Representation)
pref_2017_HoR_PR <- download_2017_HoR_PR(pref_code)

pref_2021_HoR_PR <- download_2021_HoR_PR(pref_code)

# Clean 2020 Census data at the 小地域-level
pref_pop_cleaned <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE) %>%
  rename(code = mun_code)

# Clean House of Representatives election data
pref_2017_HoR_PR_cleaned <- clean_pref_HoR_PR(pref_2017_HoR_PR, year = 2017)

pref_2021_HoR_PR_cleaned <- clean_pref_HoR_PR(pref_2021_HoR_PR, year = 2021)

# Calculate baseline votes
pref_HoR_PR <- calculate_baseline_HoR_PR(pref_2017_HoR_PR_cleaned, 
                                        pref_2021_HoR_PR_cleaned)

# Match shapefile with population data
pref_join <- pref_shp_cleaned %>%
  dplyr::mutate(sub_code = as.numeric(KIHON1)) %>%
  dplyr::left_join(pref_pop_cleaned, by = c("code", "sub_code")) %>%
  dplyr::select(code, mun_name, sub_code, sub_name, pop, geometry)

# Freeze municipalities and calculate baseline votes
pref_mun <- dplyr::bind_rows(
  # Municipalities without splits
  pref_join %>%
    dplyr::filter(!(code %in% split_code_2022)) %>%
    dplyr::group_by(code, mun_name) %>%
    dplyr::summarise(
      sub_code = first(sub_code),
      sub_name = "-",
      pop = sum(pop),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    dplyr::left_join(pref_HoR_PR, by = "mun_name"),
  
  # Municipalities with splits
  pref_join %>%
    dplyr::filter(code %in% split_code_2022) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(pop_ratio = pop / sum(pop)) %>%
    dplyr::left_join(pref_HoR_PR, by = "mun_name") %>%
    dplyr::mutate(
      across(starts_with("nv_"), ~ .x * pop_ratio)
    ) %>%
    dplyr::select(-pop_ratio)
)

# Verification checks
message("Population total: ", format(sum(pref_mun$pop), big.mark = ","))
message("LDP votes total: ", format(sum(pref_mun$nv_ldp, na.rm = TRUE), big.mark = ","))
message("Total votes: ", format(sum(pref_mun$nv_total, na.rm = TRUE), big.mark = ","))

# Check for any NA values in vote data
na_summary <- pref_mun %>%
  summarise(across(starts_with("nv_"), ~sum(is.na(.))))

if(any(na_summary > 0)){
  warning("Found NA values in vote data:")
  print(na_summary[, na_summary > 0])
}

message("Data preparation complete!")