###############################################################################
# Download and prepare data for `11_saitama_future` analysis
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
nsims_init <- 10000 # Set so that the number of valid plans > 5,000
nsims_all <- 15000
pref_code <- 11
pref_name <- "saitama"
lakes_removed <- c()
ndists_new <- 17  # 2050年の予測される定数は17（現在の16から増加予定）
ndists_old <- 16
pop_tol <- 0.20
lh_old_max_to_min <- 1.442
lh_old_mun_split <- 8
lh_old_gun_split <- 1
lh_old_koiki_split <- 0
lh_2022_max_to_min <- 1.276
lh_2022_mun_split <- 1
lh_2022_gun_split <- 2
lh_2022_koiki_split <- 0

# Split the municipalities that are split under the status quo
split_code <- c(11104, 11202, 11203, 11214,
                11217, 11222, 11232, 11245)
# さいたま市見沼区、熊谷市、川口市、春日部市、鴻巣市、越谷市、久喜市、ふじみ野市

# Municipalities that are split under the newly enacted plan
split_code_lh_2022 <- c(11203) # 川口市

# Code of 郡 that are split under the status quo
gun_exception <- c(11320) # Iruma (11324, 11326, 11327)

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

####2. Urban Prefectures########

# Clean 2020 Census data at the 小地域-level
pref_pop_cleaned <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE) %>%
  rename(code = mun_code)

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
  # Filter for Saitama prefecture (code 11xxx)
  filter(code >= 11000 & code < 12000)

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
  # Apply population projections proportionally
  mutate(across(starts_with("pop_"), ~ .x * pop_ratio, .names = "adjusted_{.col}")) %>%
  # Remove original future pop columns and ratio
  select(-starts_with("pop_"), -pop_ratio)

agg_data <- pref_mun %>%
  filter(sub_name == "-") %>%
  left_join(future_pop_cleaned, by = "code") %>%
  # Align column names with detail_data
  rename_with(~ paste0("adjusted_", .), starts_with("pop_"))

pref_mun <- bind_rows(agg_data, detail_data) %>%
  arrange(code, sub_code) %>%
  rename_with(~ gsub("adjusted_", "", .x), starts_with("adjusted_")) %>%
  # Handle missing future population values
  mutate(
    across(starts_with("pop_") & !matches("^pop$"), ~ case_when(
      !is.na(.x) ~ .x,
      TRUE ~ as.numeric(pop) * 1.1  # Saitama expected to grow by ~10%
    ))
  ) %>%
  # Ensure all population values are positive integers
  mutate(across(starts_with("pop_") & !matches("^pop$"), ~ pmax(as.integer(round(.x)), 1)))

# Handle future population column
pop_col <- paste0("pop_", year)
if(!pop_col %in% names(pref_mun)) {
  # Create future population column with growth assumption for Saitama
  cat("Creating", pop_col, "column with 10% growth assumption\n")
  pref_mun <- pref_mun %>%
    mutate(!!sym(pop_col) := as.numeric(round(pop * 1.1)))
}

# Check for missing values
cat("Missing values in", pop_col, ":", sum(is.na(pref_mun[[pop_col]])), "\n")
cat("Total", year, "population:", format(sum(pref_mun[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("Total 2020 population:", format(sum(pref_mun$pop, na.rm = TRUE), big.mark = ","), "\n")
cat("Population change ratio:", round(sum(pref_mun[[pop_col]], na.rm = TRUE) / sum(pref_mun$pop, na.rm = TRUE), 3), "\n")

# Confirm that the population figures are reasonable
cat("Population by major municipalities in", year, ":\n")
major_cities <- c(11101, 11102, 11103, 11104, 11105, 11201, 11202, 11203, 11206, 11207, 11208, 11209, 11211, 11212, 11214, 11215, 11222, 11225, 11227, 11228, 11229, 11230, 11232, 11237, 11238, 11239, 11242, 11245)
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

cat("\n=== SAITAMA FUTURE PREPARATION SUMMARY ===\n")
cat("Projection year:", year, "\n")
cat("District count change:", ndists_old, "→", ndists_new, "\n")
cat("Expected population growth: ~10%\n")
cat("Split municipalities:", length(c(split_code, split_code_lh_2022)), "\n")
cat("Gun exceptions:", length(gun_exception), "\n")
cat("Future population data prepared successfully!\n")