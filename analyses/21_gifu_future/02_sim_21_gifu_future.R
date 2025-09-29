###############################################################################
# Simulations for `21_gifu_future`
# © ALARM Project, May 2023
###############################################################################


###############################################################################
# Simulations for `21_gifu_future`
# © ALARM Project, May 2023
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####

cat("=== STARTING GIFU FUTURE SIMULATION ===\n")
cat("Future projection year:", year, "\n")
cat("Target districts:", ndists_new, "(decreased from", ndists_old, ")\n")
cat("Population projection:", paste0("pop_", year), "\n\n")

# Determine which population column to use for future projection
pop_col <- paste0("pop_", year)

# Validate population column exists
if (!pop_col %in% names(pref_mun)) {
  stop(paste("ERROR: Population column", pop_col, "not found in data"))
}

cat("=== INITIAL DATA VALIDATION ===\n")
cat("Population data validation:\n")
cat("  Using column:", pop_col, "\n")
cat("  Total future population:", format(sum(pref_mun[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("  Missing values:", sum(is.na(pref_mun[[pop_col]])), "\n")
cat("  Zero values:", sum(pref_mun[[pop_col]] == 0, na.rm = TRUE), "\n")
cat("  Average per district:", format(round(sum(pref_mun[[pop_col]], na.rm = TRUE) / ndists_new), big.mark = ","), "\n\n")

# Split the municipalities that are split under the status quo
cat("=== PROCESSING MUNICIPALITY SPLITS ===\n")
cat("Split municipalities with future population...\n")
split_code <- as.character(split_code)

# reflect_old_boundaries() will split Gifu City into old municipalities
cat("Reflecting old municipality boundaries for split municipalities...\n")
pref_mun_old <- reflect_old_boundaries(pref_mun, old_mun, census_mun_old_2020, split_code)

# Replace NA values in `old_mun_name`
pref_mun_old$old_mun_name <- replace_na(pref_mun_old$old_mun_name, "-")

cat("Municipality split processing completed\n")
cat("  Split municipalities:", length(split_code), "\n")
cat("  Total units after split:", nrow(pref_mun_old), "\n\n")

# 岐阜市の旧市町村に将来人口を比例配分
cat("=== DISTRIBUTING FUTURE POPULATION TO OLD MUNICIPALITIES ===\n")
if(pop_col %in% names(pref_mun_old)) {
  # 岐阜市の詳細確認
  gifu_detail <- pref_mun_old %>%
    filter(code == 21201) %>%
    st_drop_geometry() %>%
    select(pre_gappei_code, old_mun_name, pop, all_of(pop_col))
  
  if(nrow(gifu_detail) > 0) {
    cat("Old municipalities in Gifu City:\n")
    print(gifu_detail)
    cat("\nTotal Gifu 2020:", format(sum(gifu_detail$pop, na.rm = TRUE), big.mark = ","), "\n")
    cat("Total Gifu", year, "(before distribution):", format(sum(gifu_detail[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
    
    # 欠損値チェック
    missing_in_gifu <- sum(is.na(gifu_detail[[pop_col]]))
    if(missing_in_gifu > 0) {
      cat("\n⚠ Found", missing_in_gifu, "missing values in Gifu City parts\n")
      cat("Applying proportional distribution based on current population...\n")
      
      # 岐阜市全体の将来人口を取得（NAではない値）
      gifu_future_total <- sum(pref_mun_old[pref_mun_old$code == 21201, ][[pop_col]], na.rm = TRUE)
      
      # もしgifu_future_totalが0またはNAの場合は、現在人口×0.85で推計
      if(is.na(gifu_future_total) || gifu_future_total == 0) {
        gifu_current_total <- sum(gifu_detail$pop, na.rm = TRUE)
        gifu_future_total <- round(gifu_current_total * 0.85)
        cat("  Using 15% decline assumption for Gifu City\n")
        cat("  Estimated total:", format(gifu_future_total, big.mark = ","), "\n")
      }
      
      gifu_current_total <- sum(gifu_detail$pop, na.rm = TRUE)
      
      # 各旧市町村に比例配分
      pref_mun_old <- pref_mun_old %>%
        mutate(!!sym(pop_col) := if_else(
          code == 21201,
          as.integer(round(pop * gifu_future_total / gifu_current_total)),
          !!sym(pop_col)
        ))
      
      cat("After distribution - Gifu City", year, ":", 
          format(sum(pref_mun_old[pref_mun_old$code == 21201, ][[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
    } else {
      cat("✓ No missing values in Gifu City parts\n")
    }
  }
} else {
  cat("⚠ WARNING:", pop_col, "column not found after split\n")
  cat("Creating column with proportional distribution...\n")
  
  # カラムがない場合は新規作成
  gifu_current <- pref_mun_old %>%
    filter(code == 21201)
  
  if(nrow(gifu_current) > 0) {
    # 岐阜市全体の将来人口（分割前のデータから取得）
    gifu_future_from_pref_mun <- pref_mun %>%
      filter(code == 21201) %>%
      pull(!!sym(pop_col))
    
    if(length(gifu_future_from_pref_mun) > 0 && !is.na(gifu_future_from_pref_mun[1])) {
      gifu_future_total <- gifu_future_from_pref_mun[1]
    } else {
      gifu_future_total <- round(sum(gifu_current$pop) * 0.85)
    }
    
    gifu_current_total <- sum(gifu_current$pop)
    
    # 新しい列を作成して比例配分
    pref_mun_old <- pref_mun_old %>%
      mutate(!!sym(pop_col) := if_else(
        code == 21201,
        as.integer(round(pop * gifu_future_total / gifu_current_total)),
        as.integer(round(pop * 0.85))  # その他の市町村は15%減少
      ))
    
    cat("✓ Created", pop_col, "column with proportional distribution\n")
    cat("  Gifu City", year, ":", 
        format(sum(pref_mun_old[pref_mun_old$code == 21201, ][[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
  }
}

cat("\n")

# Re-order and add 郡 codes using future population
cat("Processing prefecture data with future population...\n")
pref <- pref_mun_old %>%
  # Temporarily replace pop with future population for processing
  mutate(pop_original = pop) %>%
  mutate(pop = !!sym(pop_col)) %>%
  arrange(code, pre_gappei_code) %>%
  merge_gun() %>%
  # Restore original population column for reference
  mutate(!!sym(pop_col) := pop) %>%
  mutate(pop = pop_original) %>%
  select(-pop_original)

cat("Gun codes merged successfully\n")
cat("  Total units:", nrow(pref), "\n")
cat("  Unique gun codes:", length(unique(pref$gun_code)), "\n\n")

# Make adjacency list
cat("Creating adjacency matrix...\n")
prefadj <- redist::redist.adjacency(pref)
cat("Adjacency matrix created with", length(prefadj), "units\n\n")

# Modify according to ferry adjacencies
# There are no ferry routes in Gifu so no ferry-related edges are added.

# Suggest connection between disconnected groups
# No additional edges are added for Gifu

# TODO Repair adjacencies if necessary, and document these changes.
# For Gifu, typically no special adjacency repairs are needed

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
cat("Merging gun (county) units...\n")
pref_map_merged <- pref_map %>%
  # Convert codes to character
  mutate(pre_gappei_code = as.character(pre_gappei_code),
         code = as.character(code),
         gun_code = as.character(gun_code)) %>%
  # Only freeze the "gun" that are kept together in the same district under the old plan
  # Make a code to determine which gun to freeze
  # If a gun is one of the gun in `gun_exception`, don't freeze it
  mutate(freeze_code = if_else(gun_code %in% gun_exception,
                               pre_gappei_code,
                               gun_code)) %>%
  # Group by and merge by `gun_code`
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Drop column `freeze_code`
  select(-freeze_code)

cat("Unit merging completed:\n")
cat("  Original units:", nrow(pref_map), "\n")
cat("  Merged units:", nrow(pref_map_merged), "\n")
cat("  Gun exceptions:", ifelse(length(gun_exception) > 0, paste(gun_exception, collapse = ", "), "None"), "\n\n")

# Since 岐阜市 is made up of two "old municipalities",
# there is no risk that 岐阜市 will be split into more than 2 districts.
# Thus, a multi-split constraint will not be applied.
# We also do not apply a municipality split constraint in order to increase
# the diversity of the simulated plans.

cat("=== STARTING SIMULATION ===\n")
cat("Configuration:\n")
cat("  Samples:", nsims, "\n")
cat("  Number of runs: 4\n")
cat("  Population temperance: 0.05\n")
cat("  Constraints: None (to maximize plan diversity)\n")
cat("  This may take 15-30 minutes for future projections...\n\n")

# Run simulation
set.seed(2020)
start_time <- Sys.time()

sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 4L,
  # Vector of municipality codes
  # counties = pref_map_merged$code, # Comment out if you are not adding any constraints
  # constraints = constr_pref, # Comment out if you are not adding any constraints
  pop_temper = 0.05
)

end_time <- Sys.time()
cat("Simulation completed in:", round(as.numeric(difftime(end_time, start_time, units = "mins")), 1), "minutes\n\n")

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
cat("=== SIMULATION DIAGNOSTICS ===\n")
summary(sim_smc_pref)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
diversity_scores <- plans_diversity(sim_smc_pref)
cat("Plan diversity mean:", round(mean(diversity_scores), 3), "\n\n")

# Create diversity histogram
png(here(paste0("temp/diversity_", year, "_gifu.png")), width = 800, height = 600)
hist(diversity_scores, main = paste("Plan Diversity - Gifu", year), 
     xlab = "Diversity Score", breaks = 30)
dev.off()

cat("Diversity histogram saved\n\n")

# Pull back plans to unmerged units
cat("Pulling back plans to original units...\n")
sim_smc_pref_pullback <- pullback(sim_smc_pref)
cat("Pullback completed\n\n")

# Handle reference plan (only if district count unchanged)
cat("=== REFERENCE PLAN HANDLING ===\n")
if (ndists_new == ndists_old) {
  cat("District count unchanged - adding reference plan\n")
  
  # Write csv file
  pref %>%
    as.data.frame() %>%
    select("pre_gappei_code",
           "old_mun_name",
           "code",
           "gun_code",
           pop = all_of(pop_col),
           "mun_name") %>%
    write_excel_csv(here(paste("temp/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               year,
                               "_export.csv",
                               sep = "")))
  
  # Read back the CSV to environment
  ref_file <- here(paste("data-raw/lh_2022/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_lh_2022.csv",
                        sep = ""))
  
  if(file.exists(ref_file)) {
    dist_lh_2022 <- read_csv(ref_file, show_col_types = FALSE)
    
    # Add reference plan
    pref_map$lh_2022 <- dist_lh_2022$lh_2022
    sim_smc_pref_ref <- add_reference(plans = sim_smc_pref_pullback,
                                      ref_plan = as.numeric(dist_lh_2022$lh_2022),
                                      name = "lh_2022")
    
    # Add `total_pop`
    for(i in 1:ndists_new){
      sim_smc_pref_ref$total_pop[which(sim_smc_pref_ref$draw == "lh_2022" &
                                         sim_smc_pref_ref$district == i)] <-
        # Population in District i
        sum(dist_lh_2022$pop[which(dist_lh_2022$lh_2022 == i)])
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

# Add precinct population
attr(sim_smc_pref_ref, "prec_pop") <- pref_map[[pop_col]]
cat("Precinct population attribute set\n\n")

# Save simulation results
cat("=== SAVING RESULTS ===\n")

# Create output directories
output_dirs <- c("data-out/shapefile", "data-out/adj", "data-out/map", "data-out/smc-out")
for(dir in output_dirs) {
  dir.create(here(dir), recursive = TRUE, showWarnings = FALSE)
}

# Save pref object, pref_map object, adjacency list, and simulation data
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
cat("Processing time:", round(as.numeric(difftime(end_time, start_time, units = "mins")), 1), "minutes\n")

# Split municipality information
cat("\nSplittable municipalities:\n")
cat("  21201: 岐阜市 (split along old municipality boundaries)\n")

# Gun information  
if(length(gun_exception) > 0) {
  cat("\nGun (county) exceptions (splittable):\n")
  for(gun in gun_exception) {
    cat("  ", gun, "\n")
  }
} else {
  cat("\nNo gun (county) exceptions - all counties kept together\n")
}

cat("\nSimulation completed successfully!\n")
cat("Ready for post-processing analysis.\n")
cat("Files saved with", year, "suffix for future projection analysis.\n")