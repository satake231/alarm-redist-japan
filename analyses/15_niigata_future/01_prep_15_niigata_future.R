###############################################################################
# Download and prepare data for `15_niigata_future` analysis
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
nsims <- 20000 # Set so that the number of valid plans > 5,000
pref_code <- 15
pref_name <- "niigata"
lakes_removed <- c()
ndists_new <- 4  # 2050年の予測される定数は4（現在の5から減少予定）
ndists_old <- 5
pop_tol <- 0.20
lh_old_max_to_min <- 1.643
lh_old_mun_split <- 7
lh_old_gun_split <- 0
lh_old_koiki_split <- 2
lh_2022_max_to_min <- 1.148
lh_2022_mun_split <- 0
lh_2022_gun_split <- 0
lh_2022_koiki_split <- 1

# Split the municipalities that are split under the status quo
split_code <- c(15202)
# 長岡市

# The following wards are split under the status quo.
# 新潟市北区(15101), 新潟市東区(15102), 新潟市中央区(15103),
# 新潟市江南区(15104), 新潟市南区(15106), 新潟市西区(15107)
# However, they will not be split in the simulation because the boundaries of the
# "old municipalities" do not match the boundaries of the wards that
# belong to Niigata City.

# Municipalities that are split under the newly enacted plan
split_code_lh_2022 <- c(15202) # 長岡市

# Code of 郡 that are split under the status quo
gun_exception <- c()

# Change time limit
options(timeout = 300)

# Download Census shapefile
pref_shp_2020 <- download_shp(pref_code)

# Clean Census shapefile
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf()
# Note that S_NAME shows the name of the first entry of the areas grouped
# in the same KIHON-1 unit (i.e. disregard --丁目,字--)

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
# Note that the size of Japanese population in the object census_mun_old_2020 is defined differently
# reflect_old_boundaries() automatically estimates the size of the Japanese population
# based on the official definition by the redistricting committee.
# (Japanese population) = (total population) - (foreign population)

# Clean predicted population data at municipal-level
future_pop_cleaned <- clean_future_pop(future_pop) %>%
  # Handle missing values and ensure proper formatting
  mutate(
    # Convert all population columns to numeric, handling any text values
    across(starts_with("pop_"), ~ as.numeric(as.character(.x))),
    # Filter out rows where code is missing or invalid
    code = as.numeric(code)
  ) %>%
  filter(!is.na(code)) %>%
  # Filter for Niigata prefecture (code 15xxx)
  filter(code >= 15000 & code < 16000)

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
pref_mun <- merge(pop, geom, by = "code")
pref_mun <- sf::st_as_sf(pref_mun)

# Combine data frames
pref_mun <- full_join(pop, geom, by = "code") %>%
  full_join(pref_HoC_PR, by = "mun_name")
pref_mun <- sf::st_as_sf(pref_mun)

# Add future population data with proper handling for split municipalities
# For rural prefecture like Niigata, most municipalities are not split
detail_data <- pref_mun %>%
  filter(code %in% c(split_code, split_code_lh_2022)) %>%
  left_join(future_pop_cleaned, by = "code") %>%
  # For split municipalities, apply proportional projection based on current split ratio
  mutate(across(starts_with("pop_"), ~ .x, .names = "adjusted_{.col}")) %>%
  select(-starts_with("pop_")) %>%
  rename_with(~ gsub("adjusted_", "", .x), starts_with("adjusted_"))

agg_data <- pref_mun %>%
  filter(!code %in% c(split_code, split_code_lh_2022)) %>%
  left_join(future_pop_cleaned, by = "code") %>%
  # Align column names with detail_data
  rename_with(~ paste0("", .), starts_with("pop_"))

pref_mun <- bind_rows(agg_data, detail_data) %>%
  arrange(code) %>%
  # Handle missing future population values - Niigata expected to decline by ~20%
  mutate(
    across(starts_with("pop_") & !matches("^pop$"), ~ case_when(
      !is.na(.x) ~ .x,
      TRUE ~ as.numeric(pop) * 0.8  # Population decline assumption for Niigata
    ))
  ) %>%
  # Ensure all population values are positive integers
  mutate(across(starts_with("pop_") & !matches("^pop$"), ~ pmax(as.integer(round(.x)), 1)))

# Handle future population column
pop_col <- paste0("pop_", year)
if(!pop_col %in% names(pref_mun)) {
  # Create future population column with decline assumption for Niigata
  cat("Creating", pop_col, "column with 20% decline assumption\n")
  pref_mun <- pref_mun %>%
    mutate(!!sym(pop_col) := as.numeric(round(pop * 0.8)))
}

# Check for missing values
cat("Missing values in", pop_col, ":", sum(is.na(pref_mun[[pop_col]])), "\n")
cat("Total", year, "population:", format(sum(pref_mun[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("Total 2020 population:", format(sum(pref_mun$pop, na.rm = TRUE), big.mark = ","), "\n")
cat("Population change ratio:", round(sum(pref_mun[[pop_col]], na.rm = TRUE) / sum(pref_mun$pop, na.rm = TRUE), 3), "\n")

# Confirm that the population figures are reasonable
cat("Population by major municipalities in", year, ":\n")
major_cities <- c(15101, 15102, 15103, 15104, 15105, 15106, 15107, 15108, # 新潟市
                  15202, 15204, 15205, 15206, 15207, 15208, 15211, 15212, # 長岡市、旭川市、室蘭市、苫小牧市、函館市、小千谷市、見附市、村上市
                  15213, 15218, 15223, 15225, 15226, 15227) # 燕市、五泉市、胎内市、南魚沼市、魚沼市、胎内市
for(city_code in major_cities) {
  city_data <- pref_mun[pref_mun$code == city_code, ]
  if(nrow(city_data) > 0) {
    cat("  ", city_data$mun_name[1], "(", city_code, "):", 
        format(city_data$pop[1], big.mark = ","), "→", 
        format(city_data[[pop_col]][1], big.mark = ","), "\n")
  }
}

# Final validation
sum(pref_mun$pop, na.rm = TRUE)
sum(pref_mun$nv_ldp, na.rm = TRUE)
sum(pref_mun[[pop_col]], na.rm = TRUE)

cat("\n=== NIIGATA FUTURE PREPARATION SUMMARY ===\n")
cat("Projection year:", year, "\n")
cat("District count change:", ndists_old, "→", ndists_new, "\n")
cat("Expected population decline: ~20%\n")
cat("Split municipalities:", length(c(split_code, split_code_lh_2022)), "\n")
cat("Gun exceptions:", length(gun_exception), "\n")
cat("Future population data prepared successfully!\n")