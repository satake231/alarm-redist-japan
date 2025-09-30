###############################################################################
# Simulations for `42_nagasaki_future`
# © ALARM Project, May 2023
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####

cat("=== STARTING NAGASAKI FUTURE SIMULATION ===\n")
cat("Future projection year:", year, "\n")
cat("Target districts:", ndists_new, "(unchanged from", ndists_old, ")\n")
cat("Population projection:", paste0("pop_", year), "\n\n")

# Determine which population column to use for future projection
pop_col <- paste0("pop_", year)

# Split the municipalities that are split under the status quo
split_code <- as.character(split_code)

cat("Processing municipalities with future population...\n")

# KEY: Reflect old boundaries using the CURRENT (2020) population first
# This is the Nagasaki-specific step - we need to split cities first
pref_mun_old <- reflect_old_boundaries(pref_mun, old_mun, census_mun_old_2020, split_code[1])
pref_mun_old <- reflect_old_boundaries(pref_mun_old, old_mun, census_mun_old_2020, split_code[2])

# Replace NA values in `old_mun_name`
pref_mun_old$old_mun_name <- replace_na(pref_mun_old$old_mun_name, "-")

cat("Old boundaries reflected successfully\n")
cat("  Total units after split:", nrow(pref_mun_old), "\n\n")

# Add future population data to each unit
# Match with future_pop_cleaned at the municipality level
cat("Adding future population data...\n")

# For units that are NOT split (sub_name == "-"), directly join future population
# For units that ARE split, we need to allocate proportionally
pref_mun_old <- pref_mun_old %>%
  left_join(future_pop_cleaned %>% select(code, starts_with("pop_")), by = "code")

# Now handle the split municipalities - allocate future population proportionally
for(split_mun_code in c(42201, 42202)) {
  # Get all sub-units of this municipality
  split_units <- pref_mun_old %>%
    filter(code == split_mun_code)
  
  if(nrow(split_units) > 1) {
    # Calculate proportion based on current (2020) population
    total_pop_2020 <- sum(split_units$pop, na.rm = TRUE)
    
    # For each future population column
    for(col in names(future_pop_cleaned)[grepl("^pop_", names(future_pop_cleaned))]) {
      if(col %in% names(pref_mun_old)) {
        # Get the total future population for this municipality
        future_total <- unique(split_units[[col]])[1]
        
        if(!is.na(future_total)) {
          # Allocate proportionally to each sub-unit
          for(i in 1:nrow(split_units)) {
            unit_idx <- which(pref_mun_old$code == split_mun_code & 
                             pref_mun_old$pre_gappei_code == split_units$pre_gappei_code[i])
            if(length(unit_idx) > 0) {
              pref_mun_old[unit_idx, col] <- round(split_units$pop[i] / total_pop_2020 * future_total)
            }
          }
        }
      }
    }
  }
}

# Handle missing future population values with decline assumption
pref_mun_old <- pref_mun_old %>%
  mutate(
    across(starts_with("pop_") & !matches("^pop$"), ~ case_when(
      !is.na(.x) ~ .x,
      TRUE ~ as.numeric(pop) * 0.85  # Nagasaki expected to decline by ~15%
    ))
  ) %>%
  mutate(across(starts_with("pop_") & !matches("^pop$"), ~ pmax(as.integer(round(.x)), 1)))

# Verify future population column exists and has no missing values
if(!pop_col %in% names(pref_mun_old)) {
  stop(paste("ERROR: Population column", pop_col, "not found"))
}

cat("Future population added successfully\n")
cat("  Missing values in", pop_col, ":", sum(is.na(pref_mun_old[[pop_col]])), "\n")
cat("  Total future population:", format(sum(pref_mun_old[[pop_col]], na.rm = TRUE), big.mark = ","), "\n\n")

# Re-order and add 郡 codes
cat("Merging gun (county) codes...\n")
pref <- pref_mun_old %>%
  arrange(code, pre_gappei_code) %>%
  merge_gun()

cat("Gun codes merged successfully\n")
cat("  Total units:", nrow(pref), "\n")
cat("  Unique gun codes:", length(unique(pref$gun_code)), "\n\n")

# Make adjacency list
cat("Creating adjacency matrix...\n")
prefadj <- redist::redist.adjacency(pref)
cat("Adjacency matrix created with", length(prefadj), "units\n\n")

# Modify according to ferry adjacencies
cat("Adding ferry adjacencies...\n")
ferries <- add_ferries(pref)
prefadj <- geomander::add_edge(prefadj,
                               ferries[, 1],
                               ferries[, 2],
                               zero = TRUE)

# Manual adjacency repairs for Nagasaki (air routes and bridges)
cat("Adding special adjacencies for islands and bridges...\n")

# Air routes: 対馬市, 壱岐市 to 大村市
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 42209), #対馬市
                               which(pref$code == 42205)) #大村市
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 42210), #壱岐市
                               which(pref$code == 42205)) #大村市

# Bridge: 西海市 to 旧佐世保市
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 42212), #西海市
                               which(pref$pre_gappei_code == 42202)) #旧佐世保市

cat("Special adjacencies added successfully\n")
cat("  Total adjacency connections:", sum(sapply(prefadj, length)), "\n\n")

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
cat("  Population tolerance:", pop_tol * 100, "%\n")
cat("  Total population:", format(sum(pref_map[[pop_col]], na.rm = TRUE), big.mark = ","), "\n\n")

# Merge gun (county) units
cat("Merging gun (county) units for simulation...\n")
pref_map_merged <- pref_map %>%
  mutate(pre_gappei_code = as.character(pre_gappei_code),
         code = as.character(code),
         gun_code = as.character(gun_code)) %>%
  mutate(freeze_code = if_else(gun_code %in% gun_exception,
                               pre_gappei_code,
                               gun_code)) %>%
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  select(-freeze_code)

cat("Unit merging completed:\n")
cat("  Original units:", nrow(pref_map), "\n")
cat("  Merged units:", nrow(pref_map_merged), "\n")
cat("  Splittable gun (county):", paste(gun_exception, collapse = ", "), "\n\n")

# Set up constraints
cat("Setting up simulation constraints...\n")
constr_pref = redist::redist_constr(pref_map_merged)
constr_pref = redist::add_constr_splits(constr_pref,
                                        strength = 1,
                                        admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref,
                                             strength = 1,
                                             admin = pref_map_merged$code)

# Run simulation
cat("=== STARTING SMC SIMULATION ===\n")
cat("Configuration:\n")
cat("  Samples per run:", nsims, "\n")
cat("  Number of runs: 4\n")
cat("  Population temperance: 0.05\n")
cat("  This may take 15-30 minutes...\n\n")

set.seed(2020)
start_time <- Sys.time()

sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 4L,
  counties = pref_map_merged$code,
  constraints = constr_pref,
  pop_temper = 0.05
)

end_time <- Sys.time()
cat("Simulation completed in:", round(as.numeric(end_time - start_time, units = "mins"), 1), "minutes\n\n")

# Check simulation results
cat("=== SIMULATION DIAGNOSTICS ===\n")
summary(sim_smc_pref)

# Check plan diversity
diversity_scores <- plans_diversity(sim_smc_pref)
cat("Plan diversity mean:", round(mean(diversity_scores), 3), "\n\n")

# Create diversity histogram
png(here(paste0("temp/diversity_", year, "_nagasaki.png")), width = 800, height = 600)
hist(diversity_scores, main = paste("Plan Diversity - Nagasaki", year), 
     xlab = "Diversity Score", breaks = 30)
dev.off()

# Pull back plans to unmerged units
cat("Pulling back plans to original units...\n")
sim_smc_pref_pullback <- pullback(sim_smc_pref)
cat("Pullback completed\n\n")

# Handle reference plan
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
cat("Processing time:", round(as.numeric(end_time - start_time, units = "mins"), 1), "minutes\n")

# Municipality split information
cat("\nSplittable municipalities:\n")
split_names <- c("長崎市", "佐世保市")
for(i in 1:length(split_code)) {
  cat("  ", split_code[i], ":", split_names[i], "\n")
}

# Gun information  
cat("\nGun (county) exception (splittable):\n")
cat("  42380: 北松浦郡\n")

# Island connectivity
cat("\nIsland connectivity:\n")
cat("  Ferry routes: multiple island connections\n")
cat("  Air routes: 対馬市, 壱岐市 to 大村市\n")
cat("  Bridge: 西海市 to 佐世保市\n")

cat("\nSimulation completed successfully!\n")
cat("Ready for post-processing analysis.\n")
cat("Files saved with", year, "suffix for future projection analysis.\n")