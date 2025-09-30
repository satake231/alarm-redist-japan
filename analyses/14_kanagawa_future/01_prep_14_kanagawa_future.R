###############################################################################
# Download and prepare data for `14_kanagawa_future` analysis
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
nsims <- 19000  # Set so that the number of valid plans > 5,000
pref_code <- 14
pref_name <- "kanagawa"
lakes_removed <- c()
ndists_new <- 21  # 2050年の予測される定数は21（現在の20から増加予定）
ndists_old <- 20
pop_tol <- 0.25
lh_old_max_to_min <- 1.466
lh_old_mun_split <- 6
lh_old_gun_split <- 0
lh_old_koiki_split <- 0
lh_2022_max_to_min <- 1.493
lh_2022_mun_split <- 0
lh_2022_gun_split <- 1 #中郡
lh_2022_koiki_split <- 0

# Split the municipalities that are split under the status quo
split_code <- c(14118, 14133, 14136, 14151, 14153, 14216)
# 横浜市都筑区, 川崎市中原区, 宮前区, 相模原市緑区, 南区, 座間市

# Municipalities that are split under the newly enacted plan
split_code_lh_2022 <- c()

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
  # Handle missing values and ensure proper formatting
  mutate(
    # Convert all population columns to numeric, handling any text values
    across(starts_with("pop_"), ~ as.numeric(as.character(.x))),
    # Filter out rows where code is missing or invalid
    code = as.numeric(code)
  ) %>%
  filter(!is.na(code)) %>%
  # Filter for Kanagawa prefecture (code 14xxx)
  filter(code >= 14000 & code < 15000)

# Clean 2019 House of Councillors election data
pref_2019_HoC_PR_cleaned <- clean_pref_2019_HoC_PR(pref_2019_HoC_PR)
# Fix municipality name
pref_2019_HoC_PR_cleaned$mun_name[2] <- "横浜市神奈川区"

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
      TRUE ~ as.numeric(pop) * 1.05  # Kanagawa expected to grow by ~5%
    ))
  ) %>%
  # Ensure all population values are positive integers
  mutate(across(starts_with("pop_") & !matches("^pop$"), ~ pmax(as.integer(round(.x)), 1)))

# Check for missing values
cat("Missing values in pop_2050:", sum(is.na(pref_mun$pop_2050)), "\n")
cat("Total 2050 population:", format(sum(pref_mun$pop_2050, na.rm = TRUE), big.mark = ","), "\n")
cat("Total 2020 population:", format(sum(pref_mun$pop, na.rm = TRUE), big.mark = ","), "\n")
cat("Population change ratio:", round(sum(pref_mun$pop_2050, na.rm = TRUE) / sum(pref_mun$pop, na.rm = TRUE), 3), "\n")

# Confirm that the population figures are reasonable
cat("\n=== Population by major municipalities in 2050 ===\n")
major_cities <- c(14100:14118, 14130:14137, 14150:14153, 14201:14217)
for(city_code in major_cities) {
  city_data <- pref_mun[pref_mun$code == city_code, ]
  if(nrow(city_data) > 0) {
    city_name <- city_data$mun_name[1]
    pop_2020 <- sum(city_data$pop[city_data$code == city_code])
    pop_2050 <- sum(city_data$pop_2050[city_data$code == city_code])
    cat("  ", city_name, "(", city_code, "):", 
        format(pop_2020, big.mark = ","), "→", 
        format(pop_2050, big.mark = ","), "\n")
  }
}

# Final validation
sum(pref_mun$pop)
sum(pref_mun$nv_ldp)
sum(pref_mun$pop_2050, na.rm = TRUE)

cat("\n=== KANAGAWA FUTURE PREPARATION SUMMARY ===\n")
cat("Projection year:", year, "\n")
cat("District count change:", ndists_old, "→", ndists_new, "\n")
cat("Expected population growth: ~5%\n")
cat("Split municipalities:", length(c(split_code, split_code_lh_2022)), "\n")
cat("Future population data prepared successfully!\n")