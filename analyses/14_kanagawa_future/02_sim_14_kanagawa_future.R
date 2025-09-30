###############################################################################
# Simulations for `14_kanagawa_future`
# © ALARM Project, May 2023
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####

cat("=== STARTING KANAGAWA FUTURE SIMULATION ===\n")
cat("Future projection year:", year, "\n")
cat("Target districts:", ndists_new, "(increased from", ndists_old, ")\n")
cat("Population projection:", paste0("pop_", year), "\n\n")

# Determine which population column to use for future projection
pop_col <- paste0("pop_", year)

# Validate population column exists
if (!pop_col %in% names(pref_mun)) {
  stop(paste("ERROR: Population column", pop_col, "not found in data"))
}

cat("Population data validation:\n")
cat("  Using column:", pop_col, "\n")
cat("  Total future population:", format(sum(pref_mun[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("  Missing values:", sum(is.na(pref_mun[[pop_col]])), "\n")
cat("  Average per district:", format(round(sum(pref_mun[[pop_col]], na.rm = TRUE) / ndists_new), big.mark = ","), "\n\n")

# Re-order and add 郡 codes using future population
cat("Processing municipality data with future population...\n")
pref <- pref_mun %>%
  arrange(code, sub_code) %>%
  merge_gun()

cat("Gun codes merged successfully\n")
cat("  Total units:", nrow(pref), "\n")
cat("  Unique gun codes:", length(unique(pref$gun_code)), "\n\n")

# Make adjacency list
cat("Creating adjacency matrix...\n")
prefadj <- redist::redist.adjacency(pref)
cat("Adjacency matrix created with", length(prefadj), "units\n\n")

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

# Merge gun (county) units
cat("Merging gun (county) units...\n")
pref_map_merged <- pref_map %>%
  # Convert codes to character for consistent handling
  mutate(code = as.character(code),
         sub_code = as.character(sub_code),
         gun_code = as.character(gun_code)) %>%
  # Determine which units to freeze together
  mutate(freeze_code = if_else(gun_code %in% gun_exception,
                               code,  # Keep individual municipalities
                               gun_code)) %>%  # Group by gun (county)
  # Group and merge by the determined freeze code
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Clean up temporary column
  select(-freeze_code)

cat("Unit merging completed:\n")
cat("  Original units:", nrow(pref_map), "\n")
cat("  Merged units:", nrow(pref_map_merged), "\n\n")

# Set up constraints
cat("Setting up simulation constraints...\n")
constr_pref = redist::redist_constr(pref_map_merged)
constr_pref = redist::add_constr_splits(constr_pref,
                                        strength = 1,
                                        admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref,
                                             strength = 0.4,
                                             admin = pref_map_merged$code)

# Run simulation
cat("=== STARTING SMC SIMULATION ===\n")
cat("Configuration:\n")
cat("  Samples per run:", nsims, "\n")
cat("  Number of runs: 8\n")
cat("  Total samples:", nsims * 8, "\n")
cat("  Population temperance: 0.02\n")
cat("  Sequence alpha: 0.9\n")
cat("  This may take 30-60 minutes for future projections...\n\n")

set.seed(2020)
start_time <- Sys.time()

sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 8L,
  counties = pref_map_merged$code,
  constraints = constr_pref,
  pop_temper = 0.02,
  seq_alpha = 0.9
)

end_time <- Sys.time()
cat("SMC simulation completed in:", round(as.numeric(end_time - start_time), 1), "minutes\n\n")

# Check simulation results
cat("=== SIMULATION DIAGNOSTICS ===\n")
summary(sim_smc_pref)

# Check plan diversity
diversity_scores <- plans_diversity(sim_smc_pref)
cat("\nPlan diversity analysis:\n")
cat("  Diversity mean:", round(mean(diversity_scores), 3), "\n")
cat("  Diversity median:", round(median(diversity_scores), 3), "\n")
cat("  Diversity range:", round(min(diversity_scores), 3), "-", round(max(diversity_scores), 3), "\n")

# Create diversity histogram
png(here(paste0("temp/diversity_", year, "_kanagawa.png")), width = 800, height = 600)
hist(diversity_scores, main = paste("Plan Diversity - Kanagawa", year, "Projection"), 
     xlab = "Diversity Score", breaks = 30)
dev.off()
cat("  Diversity histogram saved to temp/\n\n")

# Pull back plans to unmerged units
cat("Pulling back plans to original units...\n")
sim_smc_pref_pullback <- pullback(sim_smc_pref)
cat("Pullback completed\n\n")

# Handle reference plan (only if district count unchanged)
cat("=== REFERENCE PLAN HANDLING ===\n")
if (ndists_new == ndists_old) {
  cat("District count unchanged - adding reference plan\n")
  
  # Export current data for reference
  pref %>%
    as.data.frame() %>%
    select("code", "gun_code", "mun_name", "sub_name",
           pop = all_of(pop_col)) %>%
    write_excel_csv(here(paste("temp/",
                              pref_code, "_", pref_name, "_", year, "_export.csv",
                              sep = "")))
  
  # Try to read existing reference plan
  ref_file <- here(paste("data-raw/lh_2022/",
                        pref_code, "_", pref_name, "_lh_2022.csv",
                        sep = ""))
  
  if(file.exists(ref_file)) {
    dist_lh_2022 <- read_csv(ref_file, show_col_types = FALSE)
    
    # Add reference plan
    pref_map$lh_2022 <- dist_lh_2022$lh_2022
    sim_smc_pref_ref <- add_reference(plans = sim_smc_pref_pullback,
                                      ref_plan = as.numeric(dist_lh_2022$lh_2022),
                                      name = "lh_2022")
    
    # Add total_pop for reference plan
    for(i in 1:ndists_new){
      ref_pop <- sum(dist_lh_2022$pop[which(dist_lh_2022$lh_2022 == i)])
      sim_smc_pref_ref$total_pop[which(sim_smc_pref_ref$draw == "lh_2022" &
                                        sim_smc_pref_ref$district == i)] <- ref_pop
    }
    
    cat("Reference plan (lh_2022) added successfully\n")
  } else {
    cat("Reference file not found:", ref_file, "\n")
    sim_smc_pref_ref <- sim_smc_pref_pullback
  }
  
} else {
  cat("District count changed (", ndists_old, "→", ndists_new, ") - no reference plan\n")
  sim_smc_pref_ref <- sim_smc_pref_pullback
}

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
       path = paste("data-out/smc-out/", pref_code, "_", pref_name, "_", sim_type, "_", year, "_", nsims * 8, ".Rds", sep = ""))
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
cat("Simulated plans:", nsims * 8, "\n")
cat("Diversity mean:", round(mean(diversity_scores), 3), "\n")
cat("Processing time:", round(as.numeric(end_time - start_time), 1), "minutes\n")

# Municipality split information
cat("\nSplittable municipalities:\n")
split_names <- c("横浜市都筑区", "川崎市中原区", "川崎市宮前区", "相模原市緑区", "相模原市南区", "座間市")
for(i in 1:length(split_code)) {
  cat("  ", split_code[i], ":", split_names[i], "\n")
}

cat("\nSimulation completed successfully!\n")
cat("Ready for post-processing analysis.\n")
cat("Files saved with", year, "suffix for future projection analysis.\n")