###############################################################################
# Simulations for `08_ibaraki_future`
# © ALARM Project, May 2023
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####

cat("=== STARTING IBARAKI FUTURE SIMULATION ===\n")
cat("Future projection year:", year, "\n")
cat("Target districts:", ndists_new, "(reduced from", ndists_old, ")\n")
cat("Population projection:", paste0("pop_", year), "\n\n")

# Split the municipalities that are split under the status quo
split_code <- as.character(split_code)

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

# Prepare data for reflect_old_boundaries by using future population as the main population
# Store original pop column and replace with future population
pref_mun_for_processing <- pref_mun %>%
  rename(pop_original = pop) %>%
  rename(pop = !!sym(pop_col))

# Note that the size of Japanese population in the object census_mun_old_2020 is defined differently
# reflect_old_boundaries() automatically estimates the size of the Japanese population
# based on the official definition (total population - foreign population)
cat("Processing old municipality boundaries...\n")
pref_mun_old <- reflect_old_boundaries(pref_mun_for_processing, old_mun, census_mun_old_2020, split_code[1])
pref_mun_old <- reflect_old_boundaries(pref_mun_old, old_mun, census_mun_old_2020, split_code[2])
pref_mun_old <- reflect_old_boundaries(pref_mun_old, old_mun, census_mun_old_2020, split_code[3])
pref_mun_old <- reflect_old_boundaries(pref_mun_old, old_mun, census_mun_old_2020, split_code[4])
pref_mun_old <- reflect_old_boundaries(pref_mun_old, old_mun, census_mun_old_2020, split_code[5])

# Replace NA values in `old_mun_name`
pref_mun_old$old_mun_name <- replace_na(pref_mun_old$old_mun_name, "-")

# Restore the proper column names for clarity
pref_mun_old <- pref_mun_old %>%
  rename(!!sym(pop_col) := pop) %>%
  rename(pop = pop_original)

cat("Old municipality boundary processing completed\n")
cat("  Split municipalities processed:", length(split_code), "\n")
cat("  Total units after processing:", nrow(pref_mun_old), "\n\n")

# Re-order and add 郡 codes
cat("Adding gun (county) codes...\n")
pref <- pref_mun_old %>%
  arrange(code, pre_gappei_code) %>%
  merge_gun()

cat("Gun codes added:\n")
cat("  Total units:", nrow(pref), "\n")
cat("  Unique gun codes:", length(unique(pref$gun_code)), "\n")
cat("  Gun exception (splittable):", paste(gun_exception, collapse = ", "), "\n\n")

# Make adjacency list
cat("Creating adjacency matrix...\n")
prefadj <- redist::redist.adjacency(pref)
cat("Adjacency matrix created with", length(prefadj), "units\n\n")

# TODO Repair adjacencies if necessary, and document these changes.
# For Ibaraki, typically no special adjacency repairs are needed due to its geography

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
  mutate(pre_gappei_code = as.character(pre_gappei_code),
         code = as.character(code),
         gun_code = as.character(gun_code)) %>%
  # Determine which units to freeze together
  # If a gun is in `gun_exception`, don't freeze it (allow splits)
  mutate(freeze_code = if_else(gun_code %in% gun_exception,
                              pre_gappei_code,  # Keep individual old municipalities
                              gun_code)) %>%  # Group by gun (county)
  # Group and merge by the determined freeze code
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Clean up temporary column
  select(-freeze_code)

cat("Unit merging completed:\n")
cat("  Original units:", nrow(pref_map), "\n")
cat("  Merged units:", nrow(pref_map_merged), "\n")
cat("  Splittable gun (county):", paste(gun_exception, collapse = ", "), "\n")
cat("  Splittable municipalities:", paste(split_code, collapse = ", "), "\n\n")

# Set up redistricting constraints
cat("Setting up redistricting constraints...\n")
constr_pref <- redist::redist_constr(pref_map_merged)
constr_pref <- redist::add_constr_splits(constr_pref,
                                        strength = 1,
                                        admin = pref_map_merged$code)
constr_pref <- redist::add_constr_multisplits(constr_pref,
                                             strength = 1, # set strength of constraint
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
cat("  Population temperance: 0.05\n")
cat("  This may take 15-30 minutes for future projections...\n\n")

set.seed(2020)
start_time <- Sys.time()

sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 4L,
  # Vector of municipality codes
  counties = pref_map_merged$code, # Comment out if you are not adding any constraints
  constraints = constr_pref, # Comment out if you are not adding any constraints
  pop_temper = 0.05
)

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
png(here(paste0("temp/diversity_", year, "_ibaraki.png")), width = 800, height = 600)
hist(diversity_scores, main = paste("Plan Diversity - Ibaraki", year, "Projection"), 
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
    select("pre_gappei_code", "old_mun_name", "code", "gun_code", "mun_name",
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
for(code in split_code) {
  mun_name <- pref$mun_name[pref$code == as.numeric(code)][1]
  if(!is.na(mun_name)) {
    cat("  ", code, ":", mun_name, "\n")
  }
}

# Gun information  
cat("\nGun (county) exception (splittable):\n")
for(gun in gun_exception) {
  gun_name <- case_when(
    as.character(gun) == "8300" ~ "東茨城郡",
    TRUE ~ as.character(gun)
  )
  cat("  ", gun, ":", gun_name, "\n")
}

# Lake removal information
cat("\nLakes removed:\n")
for(lake in lakes_removed) {
  cat("  ", lake, "\n")
}

cat("\nSimulation completed successfully!\n")
cat("Ready for post-processing analysis.\n")
cat("Files saved with", year, "suffix for future projection analysis.\n")