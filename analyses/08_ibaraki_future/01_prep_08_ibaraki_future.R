###############################################################################
# Download and prepare data for `08_ibaraki_future` analysis
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
pref_code <- 08
pref_name <- "ibaraki"
lakes_removed <- c("霞ヶ浦", "北浦", "涸沼")
ndists_new <- 6  # 2050年の予測される定数は6（現在の7から減少予定）
ndists_old <- 7
pop_tol <- 0.25
lh_old_max_to_min <- 1.938
lh_old_mun_split <- 5
lh_old_gun_split <- 1
lh_old_koiki_split <- 1
lh_2022_max_to_min <- 1.910
lh_2022_mun_split <- 0
lh_2022_gun_split <- 1
lh_2022_koiki_split <- 1

# Split the municipalities that are split under the status quo
split_code <- c(8201, 8210, 8216, 8225, 8236)
# 水戸市、下妻市、笠間市、常陸大宮市、小美玉市

# Code of 郡 that are split under the status quo
gun_exception <- c(8300) #東茨城郡

# Change time limit
options(timeout = 300)

# Download Census shapefile
pref_shp_2020 <- download_shp(pref_code)

# Clean Census shapefile
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf()
# remove lakes
pref_shp_cleaned <- remove_lake(pref_shp_cleaned, lakes_removed[1])
pref_shp_cleaned <- remove_lake(pref_shp_cleaned, lakes_removed[2])
pref_shp_cleaned <- remove_lake(pref_shp_cleaned, lakes_removed[3])
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

# Download and clean 2020 census data at municipality/old-municipality-level
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
  # Filter for Ibaraki prefecture (code 8xxx)
  filter(code >= 8000 & code < 9000)

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
  full_join(pref_HoC_PR, by = "mun_name") %>%
  # Add future population data
  left_join(future_pop_cleaned, by = "code")
pref_mun <- sf::st_as_sf(pref_mun)

# Handle missing future population values for 2050
# Apply a conservative projection factor for missing data
pop_col <- paste0("pop_", year)
if(pop_col %in% names(pref_mun)) {
  # Calculate a default projection ratio based on available data
  available_ratio <- pref_mun %>%
    filter(!is.na(.data[[pop_col]]) & !is.na(pop)) %>%
    summarise(avg_ratio = mean(.data[[pop_col]] / pop, na.rm = TRUE)) %>%
    pull(avg_ratio)
  
  # If no ratio can be calculated, use a conservative 0.85 factor (15% decline)
  if(is.na(available_ratio) || available_ratio <= 0) {
    available_ratio <- 0.85
    cat("Warning: Using default projection ratio of 0.85 for missing future population data\n")
  } else {
    cat("Using calculated projection ratio of", round(available_ratio, 3), "for missing data\n")
  }
  
  # Fill missing values
  pref_mun <- pref_mun %>%
    mutate(
      !!sym(pop_col) := case_when(
        !is.na(.data[[pop_col]]) ~ .data[[pop_col]],
        !is.na(pop) ~ as.numeric(round(pop * available_ratio)),
        TRUE ~ NA_real_
      )
    )
} else {
  # Create future population column if it doesn't exist
  cat("Creating", pop_col, "column with 15% decline assumption\n")
  pref_mun <- pref_mun %>%
    mutate(!!sym(pop_col) := as.numeric(round(pop * 0.85)))
}

# Ensure all population values are positive integers
pref_mun <- pref_mun %>%
  mutate(across(starts_with("pop_") & !matches("^pop$"), ~ pmax(as.integer(round(.x)), 1)))

# Check for missing values
cat("Missing values in", pop_col, ":", sum(is.na(pref_mun[[pop_col]])), "\n")
cat("Total", year, "population:", format(sum(pref_mun[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("Total 2020 population:", format(sum(pref_mun$pop, na.rm = TRUE), big.mark = ","), "\n")
cat("Population change ratio:", round(sum(pref_mun[[pop_col]], na.rm = TRUE) / sum(pref_mun$pop, na.rm = TRUE), 3), "\n")

# Confirm that the population figures are reasonable
cat("Population by major municipalities in", year, ":\n")
major_cities <- c(8201, 8202, 8203, 8205, 8207, 8208, 8210, 8212, 8214, 8220)
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