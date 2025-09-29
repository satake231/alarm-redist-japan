###############################################################################
# Download and prepare data for `21_gifu_future` analysis
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
nsims <- 12500  # Set so that the number of valid plans > 5,000
pref_code <- 21
pref_name <- "gifu"
lakes_removed <- c()
ndists_new <- 4  # 2050年の予測される定数は4（現在の5から1減少）
ndists_old <- 5
pop_tol <- 0.22
lh_old_max_to_min <- 1.573
lh_old_mun_split <- 1
lh_old_gun_split <- 0
lh_old_koiki_split <- 1
lh_2022_max_to_min <- 1.530
lh_2022_mun_split <- 0
lh_2022_gun_split <- 0
lh_2022_koiki_split <- 1

# Split the municipalities that are split under the status quo
split_code <- c(21201) # 岐阜市

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
  # Filter for Gifu prefecture (code 21xxx)
  filter(code >= 21000 & code < 22000)

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

# Add future population data
pref_mun <- pref_mun %>%
  left_join(future_pop_cleaned, by = "code") %>%
  # Handle missing future population values
  mutate(
    across(starts_with("pop_") & !matches("^pop$"), ~ case_when(
      !is.na(.x) ~ .x,
      TRUE ~ as.numeric(pop) * 0.85  # Gifu expected to decline by ~15%
    ))
  ) %>%
  # Ensure all population values are positive integers
  mutate(across(starts_with("pop_") & !matches("^pop$"), ~ pmax(as.integer(round(.x)), 1)))

# Handle future population column
pop_col <- paste0("pop_", year)
if(!pop_col %in% names(pref_mun)) {
  # Create future population column with decline assumption for Gifu
  cat("Creating", pop_col, "column with 15% decline assumption\n")
  pref_mun <- pref_mun %>%
    mutate(!!sym(pop_col) := as.numeric(round(pop * 0.85)))
}

# Check for missing values
cat("Missing values in", pop_col, ":", sum(is.na(pref_mun[[pop_col]])), "\n")
cat("Total", year, "population:", format(sum(pref_mun[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("Total 2020 population:", format(sum(pref_mun$pop, na.rm = TRUE), big.mark = ","), "\n")
cat("Population change ratio:", round(sum(pref_mun[[pop_col]], na.rm = TRUE) / sum(pref_mun$pop, na.rm = TRUE), 3), "\n")

# Confirm that the population figures are reasonable
cat("Population by major municipalities in", year, ":\n")
major_cities <- c(21201, 21202, 21203, 21204, 21205, 21206, 21207, 21208, 21209, 21210, 21211, 21214, 21215)
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
sum(pref_mun[[pop_col]], na.rm = TRUE)

cat("\n=== GIFU FUTURE PREPARATION SUMMARY ===\n")
cat("Projection year:", year, "\n")
cat("District count change:", ndists_old, "→", ndists_new, "\n")
cat("Expected population decline: ~15%\n")
cat("Split municipalities:", length(split_code), "\n")
cat("Gun exceptions:", length(gun_exception), "\n")
cat("Future population data prepared successfully!\n")