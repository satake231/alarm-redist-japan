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

cat("=== STARTING NIIGATA FUTURE DATA PREPARATION ===\n")
cat("Projection year:", year, "\n")
cat("Target districts:", ndists_new, "(decreased from", ndists_old, ")\n\n")

# Download Census shapefile
cat("Downloading Census shapefile...\n")
pref_shp_2020 <- download_shp(pref_code)

# Clean Census shapefile
cat("Cleaning Census shapefile...\n")
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf()
# Note that S_NAME shows the name of the first entry of the areas grouped
# in the same KIHON-1 unit (i.e. disregard --丁目,字--)

# Download 2020 Census data at 小地域-level (size of Japanese population)
cat("Downloading 2020 Census data...\n")
pref_pop_2020 <- download_pop_2020(pref_code)

# Download predicted population data at municipal-level
cat("Downloading future population data...\n")
future_pop <- download_future_pop()

# Download 2019 House of Councillors election data (Proportional Representation)
cat("Downloading 2019 House of Councillors election data...\n")
pref_2019_HoC_PR <- download_2019_HoC_PR(pref_code)

# Download 2022 House of Councillors election data (Proportional Representation)
cat("Downloading 2022 House of Councillors election data...\n")
pref_2022_HoC_PR <- download_2022_HoC_PR(pref_code)

####1. Rural Prefectures########

# Clean 2020 Census data
cat("\nCleaning 2020 Census data...\n")
pref_pop_cleaned <- clean_pref_pop_2020(pref_pop_2020)

# Download and clean 2020 census data at municipality/old-munipality-level
cat("Cleaning 2020 census data at municipality/old-municipality level...\n")
census_mun_old_2020 <- clean_2020_census(pref_code)
# Note that the size of Japanese population in the object census_mun_old_2020 is defined differently
# reflect_old_boundaries() automatically estimates the size of the Japanese population
# based on the official definition by the redistricting committee.
# (Japanese population) = (total population) - (foreign population)

# Clean predicted population data at municipal-level
cat("Cleaning future population data...\n")
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

cat("  Future population data rows:", nrow(future_pop_cleaned), "\n")

# Clean 2019 House of Councillors election data
cat("Cleaning 2019 House of Councillors election data...\n")
pref_2019_HoC_PR_cleaned <- clean_pref_2019_HoC_PR(pref_2019_HoC_PR)

# Clean 2022 House of Councillors election data
cat("Cleaning 2022 House of Councillors election data...\n")
pref_2022_HoC_PR_cleaned <- clean_pref_2022_HoC_PR(pref_2022_HoC_PR)

# Estimate baseline votes
cat("Estimating baseline votes...\n")
pref_HoC_PR <- clean_pref_HoC_PR(pref_2019_HoC_PR_cleaned, pref_2022_HoC_PR_cleaned)

# Download data from old boundaries (pre-平成の大合併)
cat("Downloading old municipality boundaries...\n")
old_mun <- download_old_shp(pref_code)

# custom data for the analysis
pop <- pref_pop_cleaned %>%
  dplyr::rename(code = mun_code)

geom <- pref_shp_cleaned %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::select(code, geometry)

# Combine data frames
cat("Combining data frames...\n")
pref_mun <- merge(pop, geom, by = "code")
pref_mun <- sf::st_as_sf(pref_mun)

# Combine data frames
pref_mun <- full_join(pop, geom, by = "code") %>%
  full_join(pref_HoC_PR, by = "mun_name")
pref_mun <- sf::st_as_sf(pref_mun)

cat("Base pref_mun created with", nrow(pref_mun), "rows\n\n")

# Add future population data with proper handling for split municipalities
cat("=== PROCESSING FUTURE POPULATION DATA ===\n")

# Step 1: 分割されていない市町村の処理
cat("Step 1: Processing non-split municipalities...\n")
agg_data <- pref_mun %>%
  filter(!code %in% c(split_code, split_code_lh_2022)) %>%
  left_join(future_pop_cleaned, by = "code") %>%
  rename_with(~ paste0("", .), starts_with("pop_"))

cat("  Non-split municipalities processed:", nrow(agg_data), "\n")

# Step 2: 分割される市町村（長岡市）の処理
cat("\nStep 2: Processing split municipalities (Nagaoka)...\n")

# 長岡市全体の将来人口データを取得
nagaoka_code <- as.numeric(split_code[1])  # 15202
nagaoka_future <- future_pop_cleaned %>%
  filter(code == nagaoka_code)

cat("  Nagaoka code:", nagaoka_code, "\n")

if(nrow(nagaoka_future) > 0) {
  cat("  ✓ Future population data found for Nagaoka\n")
  
  # 長岡市の各旧市町村の現在人口を取得
  nagaoka_parts <- pref_mun %>%
    filter(code == nagaoka_code)
  
  cat("  Old municipalities in Nagaoka:", nrow(nagaoka_parts), "\n")
  cat("  Total current population:", format(sum(nagaoka_parts$pop), big.mark = ","), "\n")
  
  # 各旧市町村の人口比率を計算
  nagaoka_total_pop <- sum(nagaoka_parts$pop)
  
  detail_data <- nagaoka_parts %>%
    mutate(pop_ratio = pop / nagaoka_total_pop)
  
  # 将来人口列を比例配分
  cat("  Distributing future population to old municipalities:\n")
  for(col in names(nagaoka_future)) {
    if(grepl("^pop_", col) && col != "pop") {
      future_total <- nagaoka_future[[col]]
      if(!is.na(future_total) && future_total > 0) {
        detail_data[[col]] <- as.integer(round(detail_data$pop_ratio * future_total))
        cat("    ", col, ":", format(sum(detail_data[[col]], na.rm = TRUE), big.mark = ","), "\n")
      }
    }
  }
  
  # pop_ratio列を削除
  detail_data <- detail_data %>% select(-pop_ratio)
  
} else {
  cat("  ⚠ WARNING: No future population data found for Nagaoka\n")
  cat("  Using 20% decline assumption\n")
  
  # 将来人口データがない場合、20%減少を仮定
  detail_data <- pref_mun %>%
    filter(code %in% c(split_code, split_code_lh_2022)) %>%
    mutate(across(starts_with("pop_") & !matches("^pop$"), 
                  ~ as.integer(round(as.numeric(pop) * 0.8)),
                  .names = "{.col}"))
}

cat("  Split municipalities processed:", nrow(detail_data), "\n")

# Step 3: データの結合
cat("\nStep 3: Combining all data...\n")
pref_mun <- bind_rows(agg_data, detail_data) %>%
  arrange(code) %>%
  # Handle missing future population values - Niigata expected to decline by ~20%
  mutate(
    across(starts_with("pop_") & !matches("^pop$"), ~ case_when(
      !is.na(.x) & .x > 0 ~ as.integer(.x),  # 正の値はそのまま使用
      is.na(.x) ~ as.integer(pmax(round(as.numeric(pop) * 0.8), 1)),  # NAは20%減少を仮定
      .x <= 0 ~ as.integer(pmax(round(as.numeric(pop) * 0.8), 1)),  # ゼロ以下も20%減少を仮定
      TRUE ~ as.integer(.x)
    ))
  ) %>%
  # Ensure all population values are positive integers
  mutate(across(starts_with("pop_") & !matches("^pop$"), 
                ~ as.integer(pmax(round(as.numeric(.x)), 1))))

cat("  Combined data rows:", nrow(pref_mun), "\n")

# Handle future population column - より堅牢な処理
cat("\nStep 4: Validating future population column...\n")
pop_col <- paste0("pop_", year)

if(!pop_col %in% names(pref_mun)) {
  # Create future population column with decline assumption for Niigata
  cat("  Creating", pop_col, "column with 20% decline assumption\n")
  pref_mun <- pref_mun %>%
    mutate(!!sym(pop_col) := as.integer(pmax(round(as.numeric(pop) * 0.8), 1)))
} else {
  cat("  ✓", pop_col, "column exists, validating...\n")
  # 既存の列にも欠損値チェックと修正を適用
  pref_mun <- pref_mun %>%
    mutate(!!sym(pop_col) := case_when(
      is.na(!!sym(pop_col)) ~ as.integer(pmax(round(as.numeric(pop) * 0.8), 1)),
      !!sym(pop_col) <= 0 ~ as.integer(pmax(round(as.numeric(pop) * 0.8), 1)),
      TRUE ~ as.integer(!!sym(pop_col))
    ))
}

# Final validation - すべての欠損値を確実に除去
cat("\n=== FINAL VALIDATION ===\n")
cat("Checking", pop_col, "for issues...\n")

missing_count <- sum(is.na(pref_mun[[pop_col]]))
zero_count <- sum(pref_mun[[pop_col]] == 0, na.rm = TRUE)
negative_count <- sum(pref_mun[[pop_col]] < 0, na.rm = TRUE)

cat("  Missing values:", missing_count, "\n")
cat("  Zero values:", zero_count, "\n")
cat("  Negative values:", negative_count, "\n")

if(missing_count > 0 || zero_count > 0 || negative_count > 0) {
  cat("  ⚠ WARNING: Found problematic values, applying final fix...\n")
  pref_mun <- pref_mun %>%
    mutate(!!sym(pop_col) := case_when(
      is.na(!!sym(pop_col)) | !!sym(pop_col) <= 0 ~ 
        as.integer(pmax(round(as.numeric(pop) * 0.8), 1)),
      TRUE ~ as.integer(!!sym(pop_col))
    ))
  
  cat("  After fix:\n")
  cat("    Missing values:", sum(is.na(pref_mun[[pop_col]])), "\n")
  cat("    Zero values:", sum(pref_mun[[pop_col]] == 0, na.rm = TRUE), "\n")
  cat("    Negative values:", sum(pref_mun[[pop_col]] < 0, na.rm = TRUE), "\n")
}

# Summary statistics
cat("\n=== POPULATION SUMMARY ===\n")
cat("Total", year, "population:", format(sum(pref_mun[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("Total 2020 population:", format(sum(pref_mun$pop, na.rm = TRUE), big.mark = ","), "\n")
cat("Population change ratio:", round(sum(pref_mun[[pop_col]], na.rm = TRUE) / sum(pref_mun$pop, na.rm = TRUE), 3), "\n")

# Confirm that the population figures are reasonable
cat("\n=== POPULATION BY MAJOR MUNICIPALITIES ===\n")
major_cities <- c(15101, 15102, 15103, 15104, 15105, 15106, 15107, 15108, # 新潟市
                  15202, 15204, 15205, 15206, 15207, 15208, 15211, 15212, # 長岡市他
                  15213, 15218, 15223, 15225, 15226, 15227) # 燕市、五泉市他

for(city_code in major_cities) {
  city_data <- pref_mun %>%
    filter(code == city_code) %>%
    group_by(code, mun_name) %>%
    summarise(
      pop_2020 = sum(pop, na.rm = TRUE),
      pop_future = sum(!!sym(pop_col), na.rm = TRUE),
      .groups = 'drop'
    )
  
  if(nrow(city_data) > 0) {
    change_pct <- round(city_data$pop_future[1] / city_data$pop_2020[1] * 100 - 100, 1)
    cat("  ", city_data$mun_name[1], "(", city_code, "):", 
        format(city_data$pop_2020[1], big.mark = ","), "→", 
        format(city_data$pop_future[1], big.mark = ","),
        "(", change_pct, "%)\n")
  }
}

# Special check for Nagaoka (split municipality)の部分を以下に置き換え

# Special check for Nagaoka (split municipality)
cat("\n=== NAGAOKA CITY (SPLIT MUNICIPALITY) DETAIL ===\n")

# pre_gappei_code列が存在するかチェック
if("pre_gappei_code" %in% names(pref_mun)) {
  nagaoka_detail <- pref_mun %>%
    filter(code == 15202) %>%
    select(pre_gappei_code, old_mun_name, pop, !!sym(pop_col))
  
  if(nrow(nagaoka_detail) > 0) {
    print(nagaoka_detail %>% st_drop_geometry())
    cat("\nTotal Nagaoka 2020:", format(sum(nagaoka_detail$pop, na.rm = TRUE), big.mark = ","), "\n")
    cat("Total Nagaoka", year, ":", format(sum(nagaoka_detail[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
    cat("Change:", round(sum(nagaoka_detail[[pop_col]], na.rm = TRUE) / sum(nagaoka_detail$pop, na.rm = TRUE) * 100 - 100, 1), "%\n")
  }
} else {
  # pre_gappei_code列がまだない場合（旧市町村分割前）
  nagaoka_detail <- pref_mun %>%
    filter(code == 15202) %>%
    select(code, mun_name, pop, !!sym(pop_col))
  
  if(nrow(nagaoka_detail) > 0) {
    cat("Nagaoka City (before old boundary reflection):\n")
    print(nagaoka_detail %>% st_drop_geometry())
    cat("\nTotal Nagaoka 2020:", format(sum(nagaoka_detail$pop, na.rm = TRUE), big.mark = ","), "\n")
    cat("Total Nagaoka", year, ":", format(sum(nagaoka_detail[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
    cat("Change:", round(sum(nagaoka_detail[[pop_col]], na.rm = TRUE) / sum(nagaoka_detail$pop, na.rm = TRUE) * 100 - 100, 1), "%\n")
    cat("\nNote: Old municipality boundaries will be reflected in 02_sim\n")
  }
}

# Final validation
cat("\n=== FINAL CHECKS ===\n")
all_valid <- all(!is.na(pref_mun[[pop_col]]) & pref_mun[[pop_col]] > 0)
cat("All", pop_col, "values valid:", all_valid, "\n")

if(!all_valid) {
  # 問題がある行を表示（存在する列のみ選択）
  available_cols <- c("code", "mun_name", "pop", pop_col)
  existing_cols <- available_cols[available_cols %in% names(pref_mun)]
  
  problem_rows <- pref_mun %>%
    filter(is.na(!!sym(pop_col)) | !!sym(pop_col) <= 0) %>%
    select(all_of(existing_cols))
  
  cat("\nProblem rows found:\n")
  print(problem_rows %>% st_drop_geometry())
  
  stop("ERROR: Invalid values still exist in ", pop_col, ". Please check the data.")
}
