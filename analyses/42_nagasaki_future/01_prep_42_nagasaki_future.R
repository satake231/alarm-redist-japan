###############################################################################
# Download and prepare data for `42_nagasaki_future` analysis
# © ALARM Project, May 2023
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

# TODO: Define parameters for simulation
year <- 2050

sim_type <- "smc"
nsims <- 5000  # Set so that the number of valid plans > 5,000
pref_code <- 42
pref_name <- "nagasaki"
lakes_removed <- c()
ndists_new <- 3  # 2050年の定数は3のまま
ndists_old <- 3
pop_tol <- 0.25
lh_old_max_to_min <- 1.418
lh_old_mun_split <- 2
lh_old_gun_split <- 1
lh_old_koiki_split <- 2
lh_2022_max_to_min <- 1.173
lh_2022_mun_split <- 0
lh_2022_gun_split <- 0
lh_2022_koiki_split <- 1

# Split the municipalities that are split under the status quo
split_code <- c(42201, 42202)
# 長崎市、佐世保市

# Code of 郡 that are split under the status quo
gun_exception <- c(42380)

# Change time limit
options(timeout = 300)

# Download Census shapefile
pref_shp_2020 <- download_shp(pref_code)

# Clean Census shapefile
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf()

# Download 2020 Census data at 小地域-level (size of Japanese population)
pref_pop_2020 <- download_pop_2020(pref_code)

# Download predicted population data at municipal-level
future_pop <- download_future_pop()

# Download 2019 House of Councillors election data (Proportional Representation)
pref_2019_HoC_PR <- download_2019_HoC_PR(pref_code)

# Download 2022 House of Councillors election data (Proportional Representation)
pref_2022_HoC_PR <- download_2022_HoC_PR(pref_code)

####1. Rural Prefectures########

# Clean 2020 Census data
pref_pop_cleaned <- clean_pref_pop_2020(pref_pop_2020)

# Download and clean 2020 census data at municipality/old-munipality-level
census_mun_old_2020 <- clean_2020_census(pref_code)

# Clean future population data at municipal-level
future_pop_cleaned <- clean_future_pop(future_pop) %>%
  mutate(
    across(starts_with("pop_"), ~ as.numeric(as.character(.x))),
    code = as.numeric(code)
  ) %>%
  filter(!is.na(code)) %>%
  filter(code >= 42000 & code < 43000)

# Clean 2019 House of Councillors election data
pref_2019_HoC_PR_cleaned <- clean_pref_2019_HoC_PR(pref_2019_HoC_PR)

# Clean 2022 House of Councillors election data
pref_2022_HoC_PR_cleaned <- clean_pref_2022_HoC_PR(pref_2022_HoC_PR)

# Estimate baseline votes
pref_HoC_PR <- clean_pref_HoC_PR(pref_2019_HoC_PR_cleaned, pref_2022_HoC_PR_cleaned)

# Download data from old boundaries (pre-平成の大合併)
old_mun <- download_old_shp(pref_code)

# custom data for the analysis
pop <- pref_pop_cleaned %>%
  dplyr::rename(code = mun_code)

geom <- pref_shp_cleaned %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::select(code, geometry)

# Combine data frames
pref_mun <- full_join(pop, geom, by = "code") %>%
  full_join(pref_HoC_PR, by = "mun_name")
pref_mun <- sf::st_as_sf(pref_mun)

# Confirm that the population figure matches that of the redistricting committee
sum(pref_mun$pop)