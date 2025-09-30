###############################################################################
# Download and prepare data for `40_fukuoka_future` analysis
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
nsims <- 15000 # Set so that the number of valid plans > 5,000
pref_code <- 40
pref_name <- "fukuoka"
lakes_removed <- c()
ndists_new <- 12  # 2050年の予測される定数は12（現在の11から増加予定）
ndists_old <- 11
pop_tol <- 0.33
lh_old_max_to_min <- 1.871
lh_old_mun_split <- 2
lh_old_gun_split <- 0
lh_old_koiki_split <- 1
lh_2022_max_to_min <- 1.836
lh_2022_mun_split <- 3
lh_2022_gun_split <- 0
lh_2022_koiki_split <- 1

# Split the municipalities that are split under the status quo
split_code <- c(40134, 40136)
# 福岡市南区、城南区

# Municipalities that are split under the newly enacted plan
split_code_lh_2022 <- c(40134, 40136, 40131)
# 福岡市南区、城南区、東区

# Code of 郡 that are split under the status quo
gun_exception <- c()

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

####2. Urban Prefectures########

# Clean 2020 Census data at the 小地域-level
pref_pop_cleaned <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE) %>%
  rename(code = mun_code)

# Clean predicted population data at municipal-level
future_pop_cleaned <- clean_future_pop(future_pop) %>%
  mutate(
    across(starts_with("pop_"), ~ as.numeric(as.character(.x))),
    code = as.numeric(code)
  ) %>%
  filter(!is.na(code)) %>%
  # Filter for Fukuoka prefecture (code 40xxx)
  filter(code >= 40000 & code < 41000)

# Clean 2019 House of Councillors election data
pref_2019_HoC_PR_cleaned <- clean_pref_2019_HoC_PR(pref_2019_HoC_PR)

# Clean 2022 House of Councillors election data
pref_2022_HoC_PR_cleaned <- clean_pref_2022_HoC_PR(pref_2022_HoC_PR)

# Estimate baseline votes
pref_HoC_PR <- clean_pref_HoC_PR(pref_2019_HoC_PR_cleaned, pref_2022_HoC_PR_cleaned)

# Match `pref_shp_cleaned` with `pref_pop_cleaned`
pref_join <- pref_shp_cleaned %>%
  dplyr::mutate(sub_code = as.numeric(KIHON1)) %>%
  dplyr::left_join(pref_pop_cleaned, by = c("code", "sub_code")) %>%
  dplyr::select(code, mun_name, sub_code, sub_name, pop, geometry)

# Freeze municipalities except for `split_code` and `split_code_lh_2022`
# Calculate the baseline votes per municipality
pref_mun <- dplyr::bind_rows(
  # Municipalities without splits
  pref_join %>%
    dplyr::filter(code %in% c(split_code, split_code_lh_2022) == FALSE) %>%
    dplyr::group_by(code, mun_name) %>%
    dplyr::summarise(sub_code = first(sub_code),
                    sub_name = "-",
                    pop = sum(pop),
                    geometry = sf::st_union(geometry)) %>%
    dplyr::left_join(pref_HoC_PR, by = "mun_name"),
  # Municipalities with splits
  pref_join %>%
    dplyr::filter(code %in% c(split_code, split_code_lh_2022)) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(pop_ratio = pop / sum(pop)) %>%
    dplyr::left_join(pref_HoC_PR, by = "mun_name") %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("nv"), ~ .x * pop_ratio)) %>%
    dplyr::select(-pop_ratio)
)

# Add future population data with proper handling for split municipalities
detail_data <- pref_mun %>%
  filter(sub_name != "-") %>%
  group_by(code) %>%
  mutate(pop_ratio = pop / sum(pop)) %>%
  ungroup() %>%
  left_join(future_pop_cleaned, by = "code") %>%
  mutate(across(starts_with("pop_"), ~ .x * pop_ratio, .names = "adjusted_{.col}")) %>%
  select(-starts_with("pop_"), -pop_ratio)

agg_data <- pref_mun %>%
  filter(sub_name == "-") %>%
  left_join(future_pop_cleaned, by = "code") %>%
  rename_with(~ paste0("adjusted_", .), starts_with("pop_"))

pref_mun <- bind_rows(agg_data, detail_data) %>%
  arrange(code, sub_code) %>%
  rename_with(~ gsub("adjusted_", "", .x), starts_with("adjusted_")) %>%
  mutate(
    across(starts_with("pop_") & !matches("^pop$"), ~ case_when(
      !is.na(.x) ~ .x,
      TRUE ~ as.numeric(pop) * 0.9  # Default assumption for missing data (Fukuoka has slower decline)
    ))
  ) %>%
  mutate(across(starts_with("pop_") & !matches("^pop$"), ~ pmax(as.integer(round(.x)), 1)))

# Check for missing values
cat("Missing values in pop_2050:", sum(is.na(pref_mun$pop_2050)), "\n")
cat("Total 2050 population:", sum(pref_mun$pop_2050, na.rm = TRUE), "\n")

# Confirm that the population figure matches that of the redistricting committee
sum(pref_mun$pop, na.rm = TRUE)
sum(pref_mun$nv_ldp, na.rm = TRUE)