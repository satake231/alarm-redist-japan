###############################################################################
# Download and prepare data for `01_hokkaido` analysis
# Modified for House of Representatives (衆議院) data
# © ALARM Project, December 2024
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

# Parameters for simulation (unchanged)
sim_type <- "smc"
nsims <- 20000
pref_code <- 01
pref_name <- "hokkaido"
lakes_removed <- c()
ndists_new <- 12
ndists_old <- 12
pop_tol <- 0.33
lh_old_max_to_min <- 1.871
lh_old_mun_split <- 2
lh_old_gun_split <- 0
lh_old_koiki_split <- 2
lh_2022_max_to_min <- 1.858
lh_2022_mun_split <- 3
lh_2022_gun_split <- 1
lh_2022_koiki_split <- 2

# Split municipalities (unchanged)
split_code <- c(01102, 01107)  # 札幌市北区、札幌市西区
split_code_lh_2022 <- c(01102, 01107, 01104)  # ＋札幌市白石区
gun_exception <- c("ishikari")  # 石狩振興局

# Change time limit
options(timeout = 300)

# Download Census shapefile (unchanged)
pref_shp_2020 <- download_shp(pref_code)
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf()

# Download 2020 Census data (unchanged)
pref_pop_2020 <- download_pop_2020(pref_code)
pref_pop_cleaned <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE) %>%
  rename(code = mun_code)

################
# Modified section: Use HoR election data instead of HoC
################

# Option 1: Use 2024 HoR data only (simpler approach)
# Download 2024 House of Representatives election data
pref_2024_HoR_PR <- download_2024_HoR_PR(pref_code)

# Clean 2024 HoR election data
pref_2024_HoR_PR_cleaned <- clean_pref_2024_HoR_PR(pref_2024_HoR_PR)

# Aggregate split wards to match census municipality units
pref_2024_HoR_PR_aggregated <- aggregate_split_wards(pref_2024_HoR_PR_cleaned)

# Harmonize to standard format
pref_HoR_PR <- harmonize_HoR_to_HoC_format(pref_2024_HoR_PR_aggregated)

# Option 2: Use both 2021 and 2024 HoR data (for averaging like HoC approach)
# Uncomment below if 2021 data is available
# pref_2021_HoR_PR <- download_2021_HoR_PR(pref_code)
# pref_2021_HoR_PR_cleaned <- clean_pref_2021_HoR_PR(pref_2021_HoR_PR)
# pref_2021_HoR_PR_aggregated <- aggregate_split_wards(pref_2021_HoR_PR_cleaned)
# 
# # Average 2021 and 2024 data
# pref_HoR_PR <- pref_2021_HoR_PR_aggregated %>%
#   full_join(pref_2024_HoR_PR_aggregated, by = "mun_name", suffix = c("_2021", "_2024")) %>%
#   mutate(
#     across(starts_with("nv_"), 
#            ~(get(paste0(cur_column(), "_2021")) + get(paste0(cur_column(), "_2024"))) / 2,
#            .names = "{.col}")
#   ) %>%
#   select(mun_name, starts_with("nv_"))

################
# Continue with original code structure
################

# Match `pref_shp_cleaned` with `pref_pop_cleaned`
pref_join <- pref_shp_cleaned %>%
  dplyr::mutate(sub_code = as.numeric(KIHON1)) %>%
  dplyr::left_join(pref_pop_cleaned, by = c("code", "sub_code")) %>%
  dplyr::select(code, mun_name, sub_code, sub_name, pop, geometry)

# Freeze municipalities and calculate baseline votes
pref_mun <- dplyr::bind_rows(
  # Municipalities without splits
  pref_join %>%
    dplyr::filter(code %in% c(split_code, split_code_lh_2022) == FALSE) %>%
    dplyr::group_by(code, mun_name) %>%
    dplyr::summarise(sub_code = first(sub_code),
                    sub_name = "-",
                    pop = sum(pop),
                    geometry = sf::st_union(geometry)) %>%
    dplyr::left_join(pref_HoR_PR, by = "mun_name"),
  
  # Municipalities with splits
  pref_join %>%
    dplyr::filter(code %in% c(split_code, split_code_lh_2022)) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(pop_ratio = pop / sum(pop)) %>%
    dplyr::left_join(pref_HoR_PR, by = "mun_name") %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("nv_"), ~ .x * pop_ratio)) %>%
    dplyr::select(-pop_ratio)
)

# Add logging for aggregation information
if (any(pref_HoR_PR$was_aggregated)) {
  cat("The following municipalities were aggregated from split wards:\n")
  aggregated_info <- pref_HoR_PR %>%
    filter(was_aggregated) %>%
    select(mun_name, original_wards)
  print(aggregated_info)
}

# Confirm population and vote totals
cat("\n=== Data Summary ===\n")
cat("Total population:", sum(pref_mun$pop), "\n")
cat("Total LDP votes:", sum(pref_mun$nv_ldp, na.rm = TRUE), "\n")
cat("Total CDP votes:", sum(pref_mun$nv_cdp, na.rm = TRUE), "\n")
cat("Total Komei votes:", sum(pref_mun$nv_komei, na.rm = TRUE), "\n")

# Save intermediate data for verification
write_csv(pref_HoR_PR, here(paste("temp/",
                                  as.character(pref_code),
                                  "_",
                                  as.character(pref_name),
                                  "_hor_2024_votes.csv",
                                  sep = "")))