###############################################################################
# Post-processing for `08_ibaraki_future`
# © ALARM Project, May 2023
###############################################################################

cat("=== STARTING IBARAKI FUTURE POST-PROCESSING ===\n")
cat("Future projection year:", year, "\n")
cat("Prefecture: Ibaraki (", pref_code, ")\n")
cat("District change:", ndists_old, "→", ndists_new, "\n\n")

# TODO Define the koiki-renkei areas (広域連携)
# Define using gun_code if gun was merged
koiki_1_codes <- c(8201, 8216, 8221, 8226, 8236,
                   8302, 8309, 8310, 8340)

cat("Koiki-renkei area codes defined:\n")
cat("  Area 1 municipalities:", length(koiki_1_codes), "\n\n")

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
# Assign unique codes to areas that are not part of koiki_renkei areas
koiki_1[!koiki_1 %in% 1] <-
  seq(1000, 1000 + length(koiki_1[!koiki_1 %in% c(koiki_1_codes, 1)]) - 1, by = 1)

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
  redist::redist.splits(pref_smc_plans, koiki_1)
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

# TODO (Optional)
# Add other smaller areas to `pref_largest` as necessary
# For Ibaraki, typically no additional small areas need to be added
# due to relatively simple geography without major isolated islands

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

# TODO: Repair adjacency list if necessary
# For Ibaraki, typically no adjacency repairs are needed

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

# Sample plans (or fewer if less than 5,000 valid plans available)
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
  dplyr::left_join(results_sample %>%
                     dplyr::select(mun_split,
                                   gun_split,
                                   koiki_split,
                                   max_to_min,
                                   draw),
                   by = "draw")

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
cat("Reduction in seats:", ndists_old - ndists_new, "\n")
cat("Percentage reduction:", round((ndists_old - ndists_new) / ndists_old * 100, 1), "%\n")

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

cat("\nPost-processing completed successfully!\n")
cat("Files saved with year suffix:", year, "\n")
cat("Ready for partisan analysis and co-occurrence analysis.\n")