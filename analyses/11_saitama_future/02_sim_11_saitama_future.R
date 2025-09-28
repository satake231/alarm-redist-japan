###############################################################################
# Simulations for `11_saitama_future`
# © ALARM Project, May 2023
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####

cat("=== STARTING SAITAMA FUTURE SIMULATION ===\n")
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
  # Temporarily replace pop with future population for processing
  mutate(pop_original = pop) %>%
  mutate(pop = !!sym(pop_col)) %>%
  arrange(code, sub_code) %>%
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

# TODO Repair adjacencies if necessary, and document these changes.
# For Saitama, typically no special adjacency repairs are needed

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

# Define unique id (necessary for partial SMC)
pref_map <- pref_map %>%
  mutate(uid = row_number())

# Merge gun (county) units
cat("Merging gun (county) units...\n")
pref_map_merged <- pref_map %>%
  # Convert codes to character for consistent handling
  mutate(code = as.character(code),
         sub_code = as.character(sub_code),
         gun_code = as.character(gun_code)) %>%
  # Determine which units to freeze together
  # If a gun is in `gun_exception`, don't freeze it (allow splits)
  mutate(freeze_code = if_else(gun_code %in% c(gun_exception),
                              code,  # Keep individual municipalities
                              # For Saitama, merge 秩父市 and 秩父郡 due to discontiguous parts
                              if_else(gun_code %in% as.character(c(110322, 11360)),
                                      as.character(11360), # Assign code for 秩父郡
                                      gun_code))) %>%  # Group by gun (county)
  # Group and merge by the determined freeze code
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Clean up temporary column
  select(-freeze_code)

cat("Unit merging completed:\n")
cat("  Original units:", nrow(pref_map), "\n")
cat("  Merged units:", nrow(pref_map_merged), "\n")
cat("  Splittable gun (county):", paste(gun_exception, collapse = ", "), "\n")
cat("  Special handling: 秩父市 and 秩父郡 merged\n\n")

# Because the SMC algorithm does not converge when running it for the entire prefecture, 
# we separate Saitama into two regions using administrative boundaries (地域振興センター)
cat("=== PARTIAL SMC SETUP ===\n")
cat("Dividing Saitama into North and South regions based on 地域振興センター\n")

# Define regions based on 地域振興センター
# South: 南西部地域振興センター, 西部地域振興センター, and 川越比企地域振興センター
# North: Others (including 秩父郡東秩父村 within 秩父地域振興センター)
south <- c(11227, 11228, 11229, 11230, 11235, 11245, 11324,  # 南西部
          11208, 11209, 11215, 11225, 11242,                   # 西部
          11201, 11212, 11239, 11241, 11326, 11327,           # 川越比企
          "11341~11342~11343~11346~11347~11348~11349")        # 比企郡合併コード
north <- setdiff(unique(pref_map_merged$code), south)

# Calculate target population & number of districts for 2050
target_pop <- sum(pref_map[[pop_col]]) / ndists_new
cat("Target population per district:", format(round(target_pop), big.mark = ","), "\n")

# Calculate number of districts per region based on future population
ndists_new_south <- round(
  sum(pref_map_merged[[pop_col]][which(pref_map_merged$code %in% south)]) / target_pop
)
ndists_new_north <- ndists_new - ndists_new_south

cat("District allocation:\n")
cat("  South region:", ndists_new_south, "districts\n")
cat("  North region:", ndists_new_north, "districts\n")
cat("  Total:", ndists_new_south + ndists_new_north, "districts\n\n")

# Create Saitama-south Map
cat("Creating South region map...\n")
south_map <- pref_map_merged %>%
  filter(code %in% south) %>%
  `attr<-`("ndists", ndists_new_south) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

cat("South region map created:\n")
cat("  Units:", nrow(south_map), "\n")
cat("  Districts:", ndists_new_south, "\n")
cat("  Population:", format(sum(south_map[[pop_col]]), big.mark = ","), "\n\n")

####################
### Partial SMC  ###
####################
# Simulate South Region
cat("=== SIMULATING SOUTH REGION ===\n")
cat("Configuration:\n")
cat("  Samples per run:", nsims_init, "\n")
cat("  Number of runs: 4\n")
cat("  Population temperance: 0.05\n\n")

# Set up constraints for South region (commented out for faster simulation)
# constr_south = redist::redist_constr(south_map)
# constr_south = redist::add_constr_splits(constr_south,
#                                          strength = 1,
#                                          admin = south_map$code)
# constr_south = redist::add_constr_multisplits(constr_south,
#                                               strength = 1,
#                                               admin = south_map$code)

# Run simulation for South region
set.seed(2020)
start_time_south <- Sys.time()

sim_smc_south <- redist::redist_smc(
  map = south_map,
  nsims = nsims_init,
  runs = 4L,
  # Vector of municipality codes
  # counties = south_map$code,
  # constraints = constr_south,
  pop_temper = 0.05)

end_time_south <- Sys.time()
cat("South region simulation completed in:", round(as.numeric(end_time_south - start_time_south), 1), "minutes\n\n")

# Check South region simulation results
cat("=== SOUTH REGION DIAGNOSTICS ===\n")
summary(sim_smc_south)

# Check plan diversity for South region
diversity_scores_south <- plans_diversity(sim_smc_south)
cat("South region diversity mean:", round(mean(diversity_scores_south), 3), "\n\n")

# Create diversity histogram for South
png(here(paste0("temp/diversity_south_", year, "_saitama.png")), width = 800, height = 600)
hist(diversity_scores_south, main = paste("South Region Plan Diversity - Saitama", year), 
     xlab = "Diversity Score", breaks = 30)
dev.off()

######
# Filter out valid plans for Saitama-south
cat("Processing South region results...\n")

# Get plans matrix
south_smc_plans <- redist::get_plans_matrix(sim_smc_south)

# Calculate max:min ratio
wgt_smc_south <- simulation_weight_disparity_table(sim_smc_south)

# Count number of municipality splits
num_mun_split_south <- count_splits(south_smc_plans, south_map$code)
mun_split_south <- redist::redist.splits(south_smc_plans, south_map$code) %>%
  matrix(ncol = ndists_new_south, byrow = TRUE)
mun_split_south <- mun_split_south[,1]

# Count number of gun splits
gun_split_south <- redist::redist.splits(south_smc_plans, south_map$gun_code) %>%
  matrix(ncol = ndists_new_south, byrow = TRUE)
gun_split_south <- gun_split_south[,1]

# Compile results for South region
results_south <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_south)))
results_south$max_to_min <- wgt_smc_south$max_to_min
results_south$gun_split <- gun_split_south
results_south$num_mun_split <- num_mun_split_south
results_south$mun_split <- mun_split_south
results_south$multi <-  num_mun_split_south - mun_split_south
results_south$draw <- wgt_smc_south$draw

# Filter out plans with 0 multi-splits
no_multi_south <- results_south %>%
  filter(multi == 0) %>%
  pull(draw)

cat("South region filtering:\n")
cat("  Total plans:", nrow(results_south), "\n")
cat("  Plans with no multi-splits:", length(no_multi_south), "\n")

# Results for plans with 0 multi-splits
results_south_no_multi <- results_south %>%
  dplyr::filter(draw %in% no_multi_south)

# Plans with 0 multi-splits
sim_smc_south_no_multi <- sim_smc_south %>%
  dplyr::filter(draw %in% no_multi_south)

# Get plans matrix for plans with no multi-splits
south_smc_plans_no_multi <- redist::get_plans_matrix(sim_smc_south_no_multi)

## Check contiguity for South region
cat("Checking contiguity for South region...\n")

# Create new data frames for South region contiguity check
cols <- c("unit", "code", "sub_code", "sub_name", "mun_name", "gun_code", "geometry")
new_rows <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
south_sep <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:nrow(south_map)) {
  # Convert multipolygons to polygons
  new_rows <- data.frame(unit = i,
                        code = south_map[i, ]$code,
                        sub_code = south_map[i, ]$sub_code,
                        mun_name = south_map[i, ]$mun_name,
                        sub_name = south_map[i, ]$sub_name,
                        gun_code = south_map[i, ]$gun_code,
                        geometry = sf::st_cast(south_map[i, ]$geometry, "POLYGON"))

  # Order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Ignore all areas except for the largest areas in each municipality
    dplyr::filter(row_number()==1) %>%
    dplyr::select(-area)

  south_sep <- rbind(south_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert to sf
south_largest <- sf::st_as_sf(south_sep)

# Add smaller areas for South region contiguity check
add_small <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# Municipality codes of the areas to add for South region
add_small_code <- c(11245, #ふじみ野市
                    11340) #比企郡

add_small_unit <- south_sep$unit[south_sep$code %in% add_small_code | south_sep$gun_code %in% add_small_code]

# Create data frame
south_sep_add <- south_sep

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:length(add_small_unit)){
  add_small <-
    data.frame(unit = add_small_unit[i],
              code = south_map[add_small_unit[i], ]$code,
              sub_code = south_map[add_small_unit[i], ]$sub_code,
              mun_name = south_map[add_small_unit[i], ]$mun_name,
              sub_name = south_map[add_small_unit[i], ]$sub_name,
              gun_code = south_map[add_small_unit[i], ]$gun_code,
              geometry = sf::st_cast(south_map[add_small_unit[i], ]$geometry, "POLYGON"))

  # order by size
  add_small <- add_small %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Add areas that are not the largest polygon within the municipality/gun
    dplyr::filter(row_number()!=1) %>%
    dplyr::select(-area)

  # row bind
  south_sep_add <- rbind(south_sep_add, add_small)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert into shapefile
south_largest <- sf::st_as_sf(south_sep_add)

# Create adjacency for South region contiguity check
south_largest_adj <- redist::redist.adjacency(south_largest)
mainland_south <- south_largest[which(unlist(lapply(south_largest_adj, length)) > 0), ]

# Make adjacency list for the mainland South
mainland_south_adj <- redist::redist.adjacency(mainland_south)

# Check valid results for South region
results_south_no_multi$valid <- check_contiguous(south_smc_plans_no_multi,
                                                mainland_south,
                                                mainland_south_adj)

# Filter out plans with discontiguities in South region
functioning_results_south <- results_south_no_multi %>%
  dplyr::filter(valid == TRUE)

cat("South region contiguity results:\n")
cat("  Valid contiguous plans:", nrow(functioning_results_south), "\n\n")

# Sample plans for South region
set.seed(2020)
valid_sample_south <- functioning_results_south %>%
  pull(draw) %>%
  sample(nsims_all, replace = FALSE)

# Sampled plans for South region
results_sample_south <- results_south_no_multi %>%
  dplyr::filter(draw %in% valid_sample_south)

# Sample plans for South region
sim_smc_south_sample <- sim_smc_south %>%
  dplyr::filter(draw %in% valid_sample_south)

cat("South region sampling completed:\n")
cat("  Sampled plans:", length(valid_sample_south), "\n\n")

##### Pull everything together for full prefecture simulation #####
cat("=== PREPARING FULL PREFECTURE SIMULATION ===\n")

# Initialize particles for full simulation
init <- prep_particles(
  map = pref_map_merged,
  map_plan_list = list(
    south = list(map = south_map,
                plans = sim_smc_south_sample %>%
                  mutate(keep = district > 0))
  ),
  uid = uid,
  dist_keep = keep,
  nsims = nsims_all)

cat("Particles prepared for full simulation\n")

# Set up constraints for full prefecture
cat("Setting up constraints for full prefecture...\n")
constr_pref = redist::redist_constr(pref_map_merged)
constr_pref = redist::add_constr_splits(constr_pref, strength = 1, admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref, strength = 1, admin = pref_map_merged$code)

# Run full prefecture simulation
cat("=== STARTING FULL PREFECTURE SIMULATION ===\n")
cat("Configuration:\n")
cat("  Samples:", nsims_all, "\n")
cat("  Number of runs: 8\n")
cat("  Population temperance: 0.02\n")
cat("  Sequence alpha: 0.90\n")
cat("  This may take 45-90 minutes for future projections...\n\n")

set.seed(2020)
start_time_full <- Sys.time()

sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims_all,
  runs = 8L,
  # Vector of municipality codes
  counties = pref_map_merged$code,
  constraints = constr_pref,
  init_particles = init,
  pop_temper = 0.02,
  seq_alpha = 0.90)

end_time_full <- Sys.time()
cat("Full prefecture simulation completed in:", round(as.numeric(end_time_full - start_time_full), 1), "minutes\n\n")

# Check full simulation results
cat("=== FULL SIMULATION DIAGNOSTICS ===\n")
summary(sim_smc_pref)

# Check plan diversity
diversity_scores_full <- plans_diversity(sim_smc_pref)
cat("Full simulation diversity mean:", round(mean(diversity_scores_full), 3), "\n\n")

# Create diversity histogram for full simulation
png(here(paste0("temp/diversity_full_", year, "_saitama.png")), width = 800, height = 600)
hist(diversity_scores_full, main = paste("Full Prefecture Plan Diversity - Saitama", year), 
     xlab = "Diversity Score", breaks = 30)
dev.off()

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
       path = paste("data-out/smc-out/", pref_code, "_", pref_name, "_", sim_type, "_", year, "_", nsims_all * 8, ".Rds", sep = ""))
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
cat("Simulated plans:", nsims_all * 8, "\n")
cat("Diversity mean:", round(mean(diversity_scores_full), 3), "\n")
cat("South region processing time:", round(as.numeric(end_time_south - start_time_south), 1), "minutes\n")
cat("Full simulation processing time:", round(as.numeric(end_time_full - start_time_full), 1), "minutes\n")
cat("Total processing time:", round(as.numeric(end_time_full - start_time_south), 1), "minutes\n")

# Municipality split information
cat("\nSplittable municipalities:\n")
split_names <- c("さいたま市見沼区", "熊谷市", "川口市", "春日部市", "鴻巣市", "越谷市", "久喜市", "ふじみ野市")
for(i in 1:length(split_code)) {
  cat("  ", split_code[i], ":", split_names[i], "\n")
}

# Gun information  
cat("\nGun (county) exception (splittable):\n")
for(gun in gun_exception) {
  gun_name <- case_when(
    as.character(gun) == "11320" ~ "入間郡",
    TRUE ~ as.character(gun)
  )
  cat("  ", gun, ":", gun_name, "\n")
}

# Regional information
cat("\nRegional division:\n")
cat("  South region:", ndists_new_south, "districts\n")
cat("  North region:", ndists_new_north, "districts\n")
cat("  Special handling: 秩父市 and 秩父郡 merged\n")

cat("\nSimulation completed successfully!\n")
cat("Ready for post-processing analysis.\n")
cat("Files saved with", year, "suffix for future projection analysis.\n")