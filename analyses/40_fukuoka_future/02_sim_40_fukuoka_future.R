###############################################################################
# Simulations for `40_fukuoka_future`
# © ALARM Project, May 2023
###############################################################################

cat("=== STARTING FUKUOKA FUTURE SIMULATION ===\n")
cat("Future projection year:", year, "\n")
cat("Target districts:", ndists_new, "(increased from", ndists_old, ")\n")
cat("Population projection:", paste0("pop_", year), "\n\n")

####-------------- 2. Method for Urban Prefectures-------------------------####
# Re-order and add 郡 codes
pref <- pref_mun %>%
  arrange(code, sub_code) %>%
  merge_gun()

# Determine which population column to use for future projection
pop_col <- paste0("pop_", year)

# Validate population column exists
if (!pop_col %in% names(pref)) {
  stop(paste("ERROR: Population column", pop_col, "not found in data"))
}

cat("Population data validation:\n")
cat("  Using column:", pop_col, "\n")
cat("  Total future population:", format(sum(pref[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("  Missing values:", sum(is.na(pref[[pop_col]])), "\n")
cat("  Average per district:", format(round(sum(pref[[pop_col]], na.rm = TRUE) / ndists_new), big.mark = ","), "\n\n")

# Make adjacency list
cat("Creating base adjacency matrix...\n")
prefadj <- redist::redist.adjacency(pref)
cat("Base adjacency created with", length(prefadj), "units\n")

# Add Fukuoka-specific manual adjacency corrections
cat("Adding Fukuoka-specific adjacency corrections...\n")

# 福岡市東区香椎照葉 (sub_code: 690) and 福岡市東区大字奈多 (sub_code: 540)
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 40131 & pref$sub_code == 690),
                               which(pref$code == 40131 & pref$sub_code == 540),
                               zero = TRUE)

# 福岡市東区香椎照葉 (sub_code: 690) and 福岡市東区香椎浜ふ頭 (sub_code: 700)
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 40131 & pref$sub_code == 690),
                               which(pref$code == 40131 & pref$sub_code == 700),
                               zero = TRUE)

cat("Manual adjacency corrections completed\n\n")

# Create redist.map object using future population
cat("Creating redistricting map object...\n")
pref_map <- redist::redist_map(pref,
                              ndists = ndists_new,
                              pop_tol = pop_tol,
                              total_pop = !!sym(pop_col),
                              adj = prefadj,
                              planarize = 4612)

cat("Redistricting map created:\n")
cat("  Units:", nrow(pref_map), "\n")
cat("  Districts:", ndists_new, "\n")
cat("  Population tolerance:", pop_tol * 100, "%\n\n")

# Merge gun
cat("Merging gun (郡) units...\n")
pref_map_merged <- pref_map %>%
  mutate(code = as.character(code),
         sub_code = as.character(sub_code),
         gun_code = as.character(gun_code)) %>%
  mutate(freeze_code = if_else(gun_code %in% gun_exception,
                              code,
                              gun_code)) %>%
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  select(-freeze_code)

cat("Unit merging completed:\n")
cat("  Original units:", nrow(pref_map), "\n")
cat("  Merged units:", nrow(pref_map_merged), "\n")
cat("  Splittable municipalities:", paste(split_code_lh_2022, collapse = ", "), "\n\n")

# Set up redistricting constraints
cat("Setting up redistricting constraints...\n")
constr_pref <- redist::redist_constr(pref_map_merged)
constr_pref <- redist::add_constr_splits(constr_pref, 
                                        strength = 1, 
                                        admin = pref_map_merged$code)
constr_pref <- redist::add_constr_multisplits(constr_pref, 
                                             strength = 1, 
                                             admin = pref_map_merged$code)

cat("Constraints added:\n")
cat("  Split penalty: strength = 1\n")
cat("  Multi-split penalty: strength = 1\n\n")

# Run Sequential Monte Carlo simulation
cat("=== STARTING SMC SIMULATION ===\n")
cat("Configuration:\n")
cat("  Samples per run:", nsims, "\n")
cat("  Number of runs: 4\n")
cat("  Total samples:", nsims * 4, "\n")
cat("  Population temperance: 0\n")
cat("  Sequence alpha: 0.5\n")
cat("  This may take 30-60 minutes for future projections...\n\n")

set.seed(2020)
start_time <- Sys.time()

sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 4L,
  counties = pref_map_merged$code,
  constraints = constr_pref,
  pop_temper = 0,
  seq_alpha = 0.5)

end_time <- Sys.time()
cat("SMC simulation completed in:", round(as.numeric(end_time - start_time), 1), "minutes\n\n")

# Check simulation results
cat("=== SIMULATION DIAGNOSTICS ===\n")
summary(sim_smc_pref)

# Check plan diversity
cat("\nPlan diversity analysis:\n")
diversity_scores <- plans_diversity(sim_smc_pref)
cat("  Diversity mean:", round(mean(diversity_scores), 3), "\n")
cat("  Diversity median:", round(median(diversity_scores), 3), "\n")
cat("  Diversity range:", round(min(diversity_scores), 3), "-", round(max(diversity_scores), 3), "\n")

# Create diversity histogram
png(here(paste0("temp/diversity_fukuoka_", year, ".png")), width = 800, height = 600)
hist(diversity_scores, main = paste("Plan Diversity - Fukuoka", year, "Projection"), 
     xlab = "Diversity Score", breaks = 30)
dev.off()
cat("  Diversity histogram saved to temp/\n\n")

# Pull back plans to unmerged units
cat("Pulling back plans to original units...\n")
sim_smc_pref_pullback <- pullback(sim_smc_pref)
cat("Pullback completed\n\n")

# Handle reference plan (no reference plan for future with different district count)
cat("=== REFERENCE PLAN HANDLING ===\n")
cat("District count changed (", ndists_old, "→", ndists_new, ") - no reference plan\n")
sim_smc_pref_ref <- sim_smc_pref_pullback

# Set precinct population attribute
attr(sim_smc_pref_ref, "prec_pop") <- pref_map[[pop_col]]
cat("Precinct population attribute set\n\n")

# Save simulation results
cat("=== SAVING RESULTS ===\n")

# Create output directories
output_dirs <- c("data-out/shapefile", "data-out/adj", "data-out/map", "data-out/smc-out")
for(dir in output_dirs) {
  dir.create(here(dir), recursive = TRUE, showWarnings = FALSE)
}

# Save files with year suffix
files_to_save <- list(
  list(obj = pref, 
       path = paste("data-out/shapefile/", pref_code, "_", pref_name, "_", year, ".Rds", sep = "")),
  list(obj = prefadj, 
       path = paste("data-out/adj/", pref_code, "_", pref_name, "_", year, "_adj.Rds", sep = "")),
  list(obj = pref_map, 
       path = paste("data-out/map/", pref_code, "_", pref_name, "_", year, "_lh_2022_map.rds", sep = "")),
  list(obj = sim_smc_pref_ref, 
       path = paste("data-out/smc-out/", pref_code, "_", pref_name, "_", sim_type, "_", year, "_", nsims * 4, ".Rds", sep = ""))
)

for(file_info in files_to_save) {
  if(grepl("\\.rds$", file_info$path, ignore.case = TRUE)) {
    write_rds(file_info$obj, here(file_info$path), compress = "xz")
  } else {
    saveRDS(file_info$obj, here(file_info$path))
  }
  cat("Saved:", basename(file_info$path), "\n")
}

# Final summary
cat("\n=== SIMULATION SUMMARY ===\n")
cat("Projection year:", year, "\n")
cat("Districts:", ndists_old, "→", ndists_new, "\n")
cat("Population:", format(sum(pref[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("Simulated plans:", nsims * 4, "\n")
cat("Diversity mean:", round(mean(diversity_scores), 3), "\n")
cat("Processing time:", round(as.numeric(end_time - start_time), 1), "minutes\n")

# Municipality split information
cat("\nSplittable municipalities:\n")
for(code in split_code_lh_2022) {
  mun_name <- pref$mun_name[pref$code == code][1]
  if(!is.na(mun_name)) {
    cat("  ", code, ":", mun_name, "\n")
  }
}

cat("\nSimulation completed successfully!\n")
cat("Ready for post-processing analysis.\n")
cat("Files saved with", year, "suffix for future projection analysis.\n")