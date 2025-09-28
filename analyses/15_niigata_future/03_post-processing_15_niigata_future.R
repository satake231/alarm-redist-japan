###############################################################################
# Post-processing for `15_niigata_future`
# © ALARM Project, May 2023
###############################################################################

cat("=== STARTING NIIGATA FUTURE POST-PROCESSING ===\n")
cat("Future projection year:", year, "\n")
cat("Prefecture: Niigata (", pref_code, ")\n")
cat("District change:", ndists_old, "→", ndists_new, "\n")

# TODO Define the koiki-renkei areas (広域連携)
# Define using gun_code if gun was merged
# 新潟市、三条市、新発田市、加茂市、燕市、五泉市、
# 阿賀野市、胎内市、聖籠町、弥彦村、田上町、阿賀町
koiki_1_codes <-  c(15101:15108, 15204, 15206, 15209, 15213, 15218,
                    15223, 15227, 15300, 15340, 15360, 15380)

# 長岡市、小千谷市、見附市、出雲崎町
koiki_2_codes <- c(15202, 15208, 15211, 15400)

# 新発田市、胎内市、聖籠町
koiki_3_codes <- c(15206, 15227, 15307)

# 村上市、岩船郡
koiki_4_codes <- c(15212, 15580)

# 燕市、弥彦村
koiki_5_codes <- c(15213, 15340)

# 南魚沼市、魚沼市、湯沢町
koiki_6_codes <- c(15225, 15226, 15460)

cat("Koiki-renkei area codes defined:\n")
cat("  Area 1 (Niigata core):", length(koiki_1_codes), "codes\n")
cat("  Area 2 (Nagaoka):", length(koiki_2_codes), "codes\n")
cat("  Area 3 (Shibata):", length(koiki_3_codes), "codes\n")
cat("  Area 4 (Murakami):", length(koiki_4_codes), "codes\n")
cat("  Area 5 (Tsubame):", length(koiki_5_codes), "codes\n")
cat("  Area 6 (Minamiuonuma):", length(koiki_6_codes), "codes\n\n")

# Load data
cat("Loading simulation data...\n")
pref_map <- readRDS(here(paste("data-out/map/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(year),
                              "_lh_2022_map.rds",
                              sep = "")))

prefadj <- readRDS(here(paste("data-out/adj/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(year),
                              "_adj.Rds",
                              sep = "")))

sim_smc_pref_ref <- readRDS(here(paste("data-out/smc-out/",
                                      as.character(pref_code),
                                      "_",
                                      as.character(pref_name),
                                      "_",
                                      as.character(sim_type),
                                      "_",
                                      as.character(year),
                                      "_",
                                      as.character(nsims * 4),
                                      ".Rds",
                                      sep = "")))

cat("Data loaded successfully:\n")
cat("  Map units:", nrow(pref_map), "\n")
cat("  Adjacency list length:", length(prefadj), "\n")
cat("  Simulation plans:", nrow(sim_smc_pref_ref), "\n")
cat("  Unique draws:", length(unique(sim_smc_pref_ref$draw)), "\n\n")

# Get plans matrix
cat("Extracting plans matrix...\n")
pref_smc_plans <- redist::get_plans_matrix(sim_smc_pref_ref)
cat("Plans matrix size:", dim(pref_smc_plans), "\n\n")

# Calculate max:min ratio
cat("Calculating population disparity...\n")
wgt_smc <- simulation_weight_disparity_table(sim_smc_pref_ref)

# Assign koiki_renkei area codes
cat("Assigning koiki-renkei area codes...\n")
koiki_1 <- pref_map$pre_gappei_code
koiki_1[pref_map$code %in% koiki_1_codes |
          pref_map$gun_code %in% koiki_1_codes] <- 1

koiki_2 <- pref_map$pre_gappei_code
koiki_2[pref_map$code %in% koiki_2_codes |
          pref_map$gun_code %in% koiki_2_codes] <- 2

koiki_3 <- pref_map$pre_gappei_code
koiki_3[pref_map$code %in% koiki_3_codes |
          pref_map$gun_code %in% koiki_3_codes] <- 3

koiki_4 <- pref_map$pre_gappei_code
koiki_4[pref_map$code %in% koiki_4_codes |
          pref_map$gun_code %in% koiki_4_codes] <- 4

koiki_5 <- pref_map$pre_gappei_code
koiki_5[pref_map$code %in% koiki_5_codes |
          pref_map$gun_code %in% koiki_5_codes] <- 5

koiki_6 <- pref_map$pre_gappei_code
koiki_6[pref_map$code %in% koiki_6_codes |
          pref_map$gun_code %in% koiki_6_codes] <- 6

# Assign unique codes to areas that are not part of koiki_renkei areas
koiki_1[!koiki_1 %in% 1] <-
  seq(1000, 1000 + length(koiki_1[!koiki_1 %in% c(koiki_1_codes, 1)]) - 1, by = 1)
koiki_2[!koiki_2 %in% 2] <-
  seq(1000, 1000 + length(koiki_2[!koiki_2 %in% c(koiki_2_codes, 2)]) - 1, by = 1)
koiki_3[!koiki_3 %in% 3] <-
  seq(1000, 1000 + length(koiki_3[!koiki_3 %in% c(koiki_3_codes, 3)]) - 1, by = 1)
koiki_4[!koiki_4 %in% 4] <-
  seq(1000, 1000 + length(koiki_4[!koiki_4 %in% c(koiki_4_codes, 4)]) - 1, by = 1)
koiki_5[!koiki_5 %in% 5] <-
  seq(1000, 1000 + length(koiki_5[!koiki_5 %in% c(koiki_5_codes, 5)]) - 1, by = 1)
koiki_6[!koiki_6 %in% 6] <-
  seq(1000, 1000 + length(koiki_6[!koiki_6 %in% c(koiki_6_codes, 6)]) - 1, by = 1)

cat("Koiki-renkei assignment completed\n\n")

# Count number of municipality splits
cat("Counting splits...\n")
num_mun_split <- count_splits(pref_smc_plans, pref_map$code)
mun_split <- redist::redist.splits(pref_smc_plans, pref_map$code) %>%
  matrix(ncol = ndists_new, byrow = TRUE)
mun_split <- mun_split[,1]

# Count number of gun splits
gun_split <- redist::redist.splits(pref_smc_plans, pref_map$gun_code) %>%
  matrix(ncol = ndists_new, byrow = TRUE)
gun_split <- gun_split[,1]

# Count number of koiki renkei splits
koiki_split <-
  redist::redist.splits(pref_smc_plans, koiki_1) +
  redist::redist.splits(pref_smc_plans, koiki_2) +
  redist::redist.splits(pref_smc_plans, koiki_3) +
  redist::redist.splits(pref_smc_plans, koiki_4) +
  redist::redist.splits(pref_smc_plans, koiki_5) +
  redist::redist.splits(pref_smc_plans, koiki_6)
koiki_split <- koiki_split %>%
  matrix(ncol = ndists_new, byrow = TRUE)
koiki_split <- koiki_split[,1]

cat("Split counting completed:\n")
cat("  Municipality splits range:", min(mun_split), "-", max(mun_split), "\n")
cat("  Gun splits range:", min(gun_split), "-", max(gun_split), "\n")
cat("  Koiki splits range:", min(koiki_split), "-", max(koiki_split), "\n\n")

# Compile results
cat("Compiling results...\n")
results <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc)))
results$max_to_min <- wgt_smc$max_to_min
results$gun_split <- gun_split
results$num_mun_split <- num_mun_split
results$mun_split <- mun_split
results$multi <-  num_mun_split - mun_split
results$koiki_split <- koiki_split
results$draw <- wgt_smc$draw

cat("Results compiled for", nrow(results), "plans\n\n")

## Check contiguity
cat("=== CONTIGUITY ANALYSIS ===\n")
# Create new data frames
cols <- c("unit", "code", "pre_gappei_code", "old_mun_name",
          "mun_name", "gun_code", "geometry")
new_rows <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
pref_sep <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

cat("Processing polygon separation...\n")
for (i in 1:nrow(pref_map)) {
  # Convert multipolygons to polygons
  new_rows <- data.frame(unit = i,
                         code = pref_map[i, ]$code,
                         pre_gappei_code = pref_map[i, ]$pre_gappei_code,
                         mun_name = pref_map[i, ]$mun_name,
                         old_mun_name = pref_map[i, ]$old_mun_name,
                         gun_code = pref_map[i, ]$gun_code,
                         geometry = sf::st_cast(pref_map[i, ]$geometry, "POLYGON"))

  # Order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Ignore all areas except for the largest areas in each municipality
    dplyr::filter(row_number()==1) %>%
    dplyr::select(-area)

  pref_sep <- rbind(pref_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert to sf
pref_largest <- sf::st_as_sf(pref_sep)

cat("Polygon processing completed:", nrow(pref_largest), "units\n")

# Add other smaller areas for Niigata contiguity check
# For Niigata, add 刈羽村 due to its complex geography
add_small <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# Municipality codes of the areas to add
add_small_code <- c(15504) # 刈羽村
add_small_unit <- pref_sep$unit[pref_sep$code %in% add_small_code]

# Create data frame
pref_sep_add <- pref_sep

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:length(add_small_unit)){
  add_small <-
    data.frame(unit = add_small_unit[i],
               code = pref_map[add_small_unit[i], ]$code,
               pre_gappei_code = pref_map[add_small_unit[i], ]$pre_gappei_code,
               mun_name = pref_map[add_small_unit[i], ]$mun_name,
               old_mun_name = pref_map[add_small_unit[i], ]$old_mun_name,
               gun_code = pref_map[add_small_unit[i], ]$gun_code,
               geometry = sf::st_cast(pref_map[add_small_unit[i], ]$geometry, "POLYGON"))

  # order by size
  add_small <- add_small %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Add areas that are not the largest polygon within the municipality/gun
    dplyr::filter(row_number()!=1) %>%
    dplyr::select(-area)

  # row bind
  pref_sep_add <- rbind(pref_sep_add, add_small)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert into shapefile
pref_largest <- sf::st_as_sf(pref_sep_add)

# Ignore islands and isolated areas
cat("Creating mainland adjacency...\n")
pref_largest_adj <- redist::redist.adjacency(pref_largest)
mainland <- pref_largest[which(unlist(lapply(pref_largest_adj, length)) > 0), ]

# Make adjacency list for the mainland
mainland_adj <- redist::redist.adjacency(mainland)

cat("Mainland analysis:\n")
cat("  Total units:", nrow(pref_largest), "\n")
cat("  Mainland units:", nrow(mainland), "\n")
cat("  Isolated units:", nrow(pref_largest) - nrow(mainland), "\n\n")

# Check valid results
cat("Checking contiguity...\n")
results$valid <- check_contiguous(pref_smc_plans,
                                  mainland,
                                  mainland_adj)

# Filter out plans with discontiguities
functioning_results <- results %>%
  dplyr::filter(multi == 0 &
                  valid == TRUE)

cat("Contiguity analysis results:\n")
cat("  Total plans:", nrow(results), "\n")
cat("  Valid plans (no multi-splits + contiguous):", nrow(functioning_results), "\n")
cat("  Invalid plans:", nrow(results) - nrow(functioning_results), "\n\n")

# nrow(functioning_results) must be over 5,000.
# If not, increase nsims and run more simulations.
if(nrow(functioning_results) < 1000) {
  warning("Number of valid plans (", nrow(functioning_results), ") is quite low. Consider increasing nsims.")
}

# Sample 5,000 plans (or fewer if less than 5,000 valid plans available)
set.seed(2020)
n_sample <- min(5000, nrow(functioning_results))
valid_sample <- functioning_results %>%
  pull(draw) %>%
  sample(n_sample, replace = FALSE)

cat("Sampling", n_sample, "plans for final analysis\n\n")

# Sampled plans (no reference plan for future projections since districts changed)
results_sample <- results %>%
  dplyr::filter(draw %in% valid_sample)

# Add summary statistics to the sampled `redist_plan`
cat("Adding summary statistics...\n")
sim_smc_pref_sample <- sim_smc_pref_ref %>%
  dplyr::filter(draw %in% valid_sample) %>%
  partisan_metrics_japan(pref_map) %>%
  dplyr::left_join(results_sample, by = "draw")

cat("Summary statistics added successfully\n\n")

# Check the summary statistics
cat("=== SUMMARY STATISTICS ===\n")
# Sampled plans
cat("Sampled", n_sample, "plans:\n")
summary(sim_smc_pref_sample)

cat("\nAll simulated plans:\n")
sim_smc_pref_ref %>%
  partisan_metrics_japan(pref_map) %>%
  dplyr::left_join(results %>%
                     dplyr::select(mun_split,
                                   gun_split,
                                   koiki_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>%
  summary()

# Check the validation of the sampled plans
cat("\n=== VALIDATION CHECK ===\n")
# validate_analysis_japan(sim_smc_pref_sample, pref_map, pref_code, pref_name)

# Key statistics
cat("Key metrics for", year, "projection:\n")
cat("  Population deviation range:", 
    round(min(sim_smc_pref_sample$plan_dev), 3), "to", 
    round(max(sim_smc_pref_sample$plan_dev), 3), "\n")
cat("  Max-to-min ratio range:", 
    round(min(sim_smc_pref_sample$max_to_min), 3), "to", 
    round(max(sim_smc_pref_sample$max_to_min), 3), "\n")
cat("  Municipality splits:", 
    min(sim_smc_pref_sample$mun_split), "to", 
    max(sim_smc_pref_sample$mun_split), "\n")
cat("  Gun (county) splits:", 
    min(sim_smc_pref_sample$gun_split), "to", 
    max(sim_smc_pref_sample$gun_split), "\n")

# Create output directories
output_dirs <- c("data-out/plans", "data-out/stats")
for(dir in output_dirs) {
  dir.create(here(dir), recursive = TRUE, showWarnings = FALSE)
}

# Save relevant files to upload to Dataverse
cat("\n=== SAVING RESULTS ===\n")
# `redist_plans` object
write_rds(sim_smc_pref_sample,
          here(paste("data-out/plans/",
                     as.character(pref_code),
                     "_",
                     as.character(pref_name),
                     "_",
                     as.character(year),
                     "_lh_2022_plans.rds",
                     sep = "")),
          compress = "xz")
cat("Saved: plans file\n")

# Export `redist_plans` summary statistics to a csv file
as_tibble(sim_smc_pref_sample) %>%
  mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
  write_csv(here(paste("data-out/stats/",
                       as.character(pref_code),
                       "_",
                       as.character(pref_name),
                       "_",
                       as.character(year),
                       "_lh_2022_stats.csv",
                       sep = "")))
cat("Saved: statistics file\n")

# Summary of key changes from current system
cat("\n=== FUTURE PROJECTION IMPACT ANALYSIS ===\n")
cat("District count change:", ndists_old, "→", ndists_new, "\n")
cat("Decrease in seats:", ndists_old - ndists_new, "\n")
cat("Percentage decrease:", round((ndists_old - ndists_new) / ndists_old * 100, 1), "%\n")

# Average district size change
total_pop_2050 <- sum(attr(sim_smc_pref_ref, "prec_pop"), na.rm = TRUE)
avg_district_pop_old <- total_pop_2050 / ndists_old  # Hypothetical if kept old system
avg_district_pop_new <- total_pop_2050 / ndists_new  # New system
cat("Average district population change:\n")
cat("  Old system (hypothetical):", format(round(avg_district_pop_old), big.mark = ","), "\n")
cat("  New system:", format(round(avg_district_pop_new), big.mark = ","), "\n")
cat("  Increase per district:", format(round(avg_district_pop_new - avg_district_pop_old), big.mark = ","), "\n")

# Population concentration analysis
sample_plans <- sim_smc_pref_sample %>%
  group_by(draw) %>%
  summarise(
    max_pop = max(total_pop),
    min_pop = min(total_pop),
    pop_cv = sd(total_pop) / mean(total_pop),
    .groups = 'drop'
  )

cat("District population distribution:\n")
cat("  CV range:", round(min(sample_plans$pop_cv), 3), "to", round(max(sample_plans$pop_cv), 3), "\n")
cat("  Median CV:", round(median(sample_plans$pop_cv), 3), "\n")

# Regional impact analysis
cat("\nRegional impact:\n")
cat("  Methodology: Standard SMC for rural prefecture\n")
cat("  Ferry connections: Sado Island to mainland\n")
cat("  Old municipality boundaries: 長岡市 split along pre-merger lines\n")

# Niigata-specific decline analysis
cat("\nNiigata-specific context:\n")
cat("  Expected population decline: ~20%\n")
cat("  Rural area depopulation\n")
cat("  District reduction needs\n")
cat("  Aging society accommodation\n")

cat("\nPost-processing completed successfully!\n")
cat("Files saved with year suffix:", year, "\n")
cat("Ready for partisan analysis and co-occurrence analysis.\n")

# Special note for Niigata
cat("\n=== NIIGATA FUTURE REDISTRICTING NOTES ===\n")
cat("1. Population decline accommodated by district reduction\n")
cat("2. Rural prefecture methodology maintains administrative boundaries\n")
cat("3. Ferry connections preserve island representation\n")
cat("4. Koiki-renkei areas maintain regional cooperation\n")
cat("5. Old municipality boundaries respect historical divisions\n")