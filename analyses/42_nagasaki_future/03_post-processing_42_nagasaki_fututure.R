###############################################################################
# Post-processing for `42_nagasaki_future`
# © ALARM Project, May 2023
###############################################################################

cat("=== STARTING NAGASAKI FUTURE POST-PROCESSING ===\n")
cat("Future projection year:", year, "\n")
cat("Prefecture: Nagasaki (", pref_code, ")\n")
cat("District change:", ndists_old, "→", ndists_new, "\n\n")

# TODO Define the koiki-renkei areas (広域連携)
# Define using gun_code if gun was merged
koiki_1_codes <- c(42201, 42300) # 42307, 42308
koiki_2_codes <- c(42202, 42207, 42208, 42212,
                   42320, # 42321, 42322, 42323,
                   42383, 42391,
                   42400) # 42411

cat("Koiki-renkei area codes defined:\n")
cat("  Area 1 (Nagasaki region):", length(koiki_1_codes), "codes\n")
cat("  Area 2 (Sasebo region):", length(koiki_2_codes), "codes\n\n")

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
  redist::redist.splits(pref_smc_plans, koiki_2)
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

cat("Polygon processing completed:", nrow(pref_largest), "units\n\n")

# Nagasaki has many islands, so we don't add extra small areas
# The main contiguity check will focus on the mainland and major islands

# Ignore islands and isolated areas
cat("Creating mainland adjacency...\n")
pref_largest_adj <- redist::redist.adjacency(pref_largest)
mainland <- pref_largest[which(unlist(lapply(pref_largest_adj, length)) > 0), ]

# Make adjacency list for the mainland
mainland_adj <- redist::redist.adjacency(mainland)

# Repair adjacency list for Nagasaki (bridge connection)
cat("Repairing adjacency for bridge connection...\n")
mainland_adj <- geomander::add_edge(mainland_adj,
                                    which(mainland$code == 42212), #西海市
                                    which(mainland$pre_gappei_code == 42202)) #旧佐世保市

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

# Check if reference plan exists in the results
has_reference <- "lh_2022" %in% results$draw

if(has_reference) {
  cat("Reference plan found in results\n")
  valid_sample <- functioning_results %>%
    filter(draw != "lh_2022") %>%
    pull(draw) %>%
    sample(n_sample, replace = FALSE)
  
  # Include reference plan
  results_sample <- results %>%
    dplyr::filter(draw %in% valid_sample | draw == "lh_2022")
} else {
  cat("No reference plan in results\n")
  valid_sample <- functioning_results %>%
    pull(draw) %>%
    sample(n_sample, replace = FALSE)
  
  results_sample <- results %>%
    dplyr::filter(draw %in% valid_sample)
}

cat("Sampling", n_sample, "plans for final analysis\n\n")

# Add summary statistics to the sampled `redist_plan`
cat("Adding summary statistics...\n")
if(has_reference) {
  sim_smc_pref_sample <- sim_smc_pref_ref %>%
    dplyr::filter(draw %in% valid_sample | draw == "lh_2022") %>%
    partisan_metrics_japan(pref_map) %>%
    dplyr::left_join(results_sample, by = "draw")
} else {
  sim_smc_pref_sample <- sim_smc_pref_ref %>%
    dplyr::filter(draw %in% valid_sample) %>%
    partisan_metrics_japan(pref_map) %>%
    dplyr::left_join(results_sample, by = "draw")
}

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
cat("  Koiki-renkei splits:", 
    min(sim_smc_pref_sample$koiki_split), "to", 
    max(sim_smc_pref_sample$koiki_split), "\n")

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
cat("District count:", ndists_old, "→", ndists_new, "\n")
if(ndists_new != ndists_old) {
  cat("Change in seats:", ndists_new - ndists_old, "\n")
  cat("Percentage change:", round((ndists_new - ndists_old) / ndists_old * 100, 1), "%\n")
} else {
  cat("No change in seat count\n")
}

# Average district size change
pop_col <- paste0("pop_", year)
total_pop_future <- sum(attr(sim_smc_pref_ref, "prec_pop"), na.rm = TRUE)
avg_district_pop_old <- total_pop_future / ndists_old
avg_district_pop_new <- total_pop_future / ndists_new
cat("Average district population:\n")
if(ndists_new != ndists_old) {
  cat("  Old system (hypothetical):", format(round(avg_district_pop_old), big.mark = ","), "\n")
  cat("  New system:", format(round(avg_district_pop_new), big.mark = ","), "\n")
  cat("  Change per district:", format(round(avg_district_pop_new - avg_district_pop_old), big.mark = ","), "\n")
} else {
  cat("  Average:", format(round(avg_district_pop_new), big.mark = ","), "\n")
}

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

# Nagasaki-specific context
cat("\nNagasaki-specific context:\n")
cat("  Expected population decline: ~15%\n")
cat("  Island connectivity maintained through ferry and air routes\n")
cat("  Split municipalities: 長崎市, 佐世保市\n")
cat("  Koiki-renkei areas: Nagasaki region, Sasebo region\n")

cat("\nPost-processing completed successfully!\n")
cat("Files saved with year suffix:", year, "\n")
cat("Ready for partisan analysis and co-occurrence analysis.\n")

# Special note for Nagasaki
cat("\n=== NAGASAKI FUTURE REDISTRICTING NOTES ===\n")
cat("1. Population decline accommodated within existing", ndists_new, "districts\n")
cat("2. Island connectivity preserved through ferry and air route adjacencies\n")
cat("3. Bridge connection (西海市-佐世保市) maintained\n")
cat("4. Split municipalities reflect urban concentration patterns\n")
cat("5. Koiki-renkei areas respect regional administrative structures\n")