###############################################################################
# Co-occurrence analysis for `01_hokkaido_future`
# © ALARM Project, May 2023
###############################################################################

cat("Starting co-occurrence analysis for", year, "projection...\n")

# Load required libraries
library(ggplot2)
library(ggthemes)
library(cluster)
library(dplyr)

# Find Optimal Plan
# Note that `results_sample` includes the data for future projections (no lh_2022 for changed district count)
optimal <- as.numeric(results_sample$draw[which(results_sample$max_to_min ==
                                      min(results_sample$max_to_min))][1])
cat("Optimal plan found: draw", optimal, "with max_to_min ratio:", min(results_sample$max_to_min), "\n")

# Display optimal plan details
optimal_stats <- results_sample[which(results_sample$draw == optimal),]
print(optimal_stats)

# Calculate detailed population statistics for optimal plan
optimal_plan_data <- sim_smc_pref_ref %>% 
  filter(draw == optimal) %>%
  arrange(district)

cat("\n=== OPTIMAL PLAN POPULATION ANALYSIS ===\n")
cat("Draw:", optimal, "\n")
cat("Number of districts:", ndists_new, "\n")
cat("Population by district:\n")

pop_by_district <- optimal_plan_data$total_pop
names(pop_by_district) <- paste0("District ", optimal_plan_data$district)
print(pop_by_district)

cat("\nPopulation statistics:\n")
cat("Total population:", sum(pop_by_district), "\n")
cat("Average population per district:", round(mean(pop_by_district), 0), "\n")
cat("Maximum population:", max(pop_by_district), "\n")
cat("Minimum population:", min(pop_by_district), "\n")
cat("Max-to-Min ratio (1票の格差):", round(max(pop_by_district)/min(pop_by_district), 3), "\n")
cat("Population deviation range:", 
    round((min(pop_by_district) - mean(pop_by_district))/mean(pop_by_district) * 100, 2), "% to ",
    round((max(pop_by_district) - mean(pop_by_district))/mean(pop_by_district) * 100, 2), "%\n")

# Additional statistics
cat("\nDetailed district analysis:\n")
for(i in 1:ndists_new) {
  dist_pop <- pop_by_district[i]
  avg_pop <- mean(pop_by_district)
  deviation_pct <- round((dist_pop - avg_pop)/avg_pop * 100, 2)
  cat("District", i, ": population =", dist_pop, 
      ", deviation =", deviation_pct, "%\n")
}

# Optimal Plan
matrix_optimal <- redist::get_plans_matrix(sim_smc_pref_ref %>%
                                            filter(draw == optimal))
colnames(matrix_optimal) <- "district"

optimal_boundary <- cbind(pref_map, as_tibble(matrix_optimal))

# Gun/Municipality/Koiki-renkei boundaries - using original pref_shp_cleaned
cat("Creating boundary data...\n")
mun_boundary <- pref_shp_cleaned %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary <- pref %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun <- mun_boundary %>%
  summarise(geometry = sf::st_combine(geometry))
mun$type <- "Municipality Boundaries"
# Combine gun boundary data
gun <- gun_boundary %>%
  summarise(geometry = sf::st_combine(geometry))
gun$type <- "振興局 Boundaries"

# Municipality/Gun boundary
boundary <- rbind(mun, gun)
boundary$type <- factor(boundary$type, levels = boundary$type)

# Co-occurrence
# Filter out plans with top 10% max-min ratio
cat("Calculating co-occurrence matrix...\n")
good_num <- results_sample %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(min(length(results_sample$draw)*0.1, nrow(results_sample)))) %>%
  select(draw)
good_num <- as.vector(t(good_num))
sim_smc_pref_good <- sim_smc_pref_sample %>%
  filter(draw %in% good_num)

cat("Using", length(good_num), "plans for co-occurrence analysis\n")

# Obtain co-occurrence matrix
m_co = redist::prec_cooccurrence(sim_smc_pref_good, sampled_only=TRUE)

# Create clusters
cl_co = cluster::agnes(m_co)

# Analyze the dendrogram and pick an appropriate number of clusters
cat("Creating dendrogram...\n")
png(here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_dendrogram.png")), 
    width = 800, height = 600)
plot(as.dendrogram(cl_co), main = paste0("Co-occurrence Dendrogram - ", year, " Projection"))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue")
dev.off()

# Set the number of clusters (change k to an appropriate number)
k <- ndists_new # Default: ndists_new
cat("Using", k, "clusters for analysis\n")
prec_clusters = cutree(cl_co, k)
pref_membership <- as_tibble(as.data.frame(prec_clusters))
names(pref_membership) <- "membership"

# Obtain co-occurrence ratio
cooc_ratio <- vector(length = length(pref$code))

relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

cat("Calculating co-occurrence ratios...\n")
pop_col <- paste0("pop_", year)
for (i in 1:length(pref$code))
{
  cooc_ratio[i] <- 1 -
    sum(pref[[pop_col]][relcomp(prefadj[[i]]+1,
                        which(prec_clusters == prec_clusters[i]))] * m_co[i, relcomp(prefadj[[i]]+1,
                                                                                      which(prec_clusters == prec_clusters[i]))])/
    sum(pref[[pop_col]][prefadj[[i]]+1] * m_co[i, prefadj[[i]]+1])
}

# Co-occurrence Plot
# Find the coordinates of major cities in Hokkaido
cities <- data.frame(longitude = c(141.35438, 140.728948, 142.365055),
                    latitude = c(43.06206, 41.768663, 43.770687),
                    names = c("Sapporo", "Hakodate", "Asahikawa"))
cities <- sf::st_as_sf(cities, coords = c("longitude", "latitude"),
                      crs = 4612)

# Match membership data with map object
if(ndists_new > 6){
  pref_cooc <- cbind(pref_map, cooc_ratio, pref_membership) %>%
    mutate(color = redist:::color_graph(.$adj, as.integer(.$membership)))
}else{
  pref_cooc <- cbind(pref_map, cooc_ratio, pref_membership) %>%
    mutate(color = .$membership)
}

## Color Palette
PAL <- c('#6D9537', '#9A9BB9', '#DCAD35', '#7F4E28', '#2A4E45', '#364B7F')

# Create co-occurrence plot
cat("Creating co-occurrence plot...\n")
cooccurrence_plot <- ggplot() +
  geom_sf(data = pref_cooc, aes(fill = as.factor(color), alpha = cooc_ratio), show.legend = FALSE) +
  scale_fill_manual(values = PAL, guide = "none") +
  scale_alpha_continuous(range = c(min(cooc_ratio, na.rm = TRUE), max(cooc_ratio, na.rm = TRUE)), guide = "none") +

  geom_sf(data = boundary, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#373C38", "#606264")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.3, 0.6)) +

  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = c("black", "black", "black"),
              nudge_x = c(0, 0.2, 0), # adjust the position of the labels
              nudge_y = c(0.2, -0.1, 0.1), # adjust the position of the labels
              #"Sapporo", "Hakodate", "Asahikawa"
              family = "sans") +
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Co-occurrence Analysis - ", year, " Projection (", ndists_new, " districts)"))

print(cooccurrence_plot)

# Plot Optimal Plan Map
cat("Creating optimal plan map...\n")
if(ndists_new > 6){
  optimal_boundary_colored <- optimal_boundary %>%
    mutate(color = redist:::color_graph(prefadj, as.integer(district)))
} else {
  optimal_boundary_colored <- optimal_boundary %>%
    mutate(color = district)
}

# Create optimal plan plot - Full Hokkaido
optimal_max_to_min <- round(max(pop_by_district)/min(pop_by_district), 3)
total_population <- sum(pop_by_district)

optimal_plot <- ggplot() +
  geom_sf(data = optimal_boundary_colored, aes(fill = factor(color)), color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  geom_sf(data = boundary, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#373C38", "#606264")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.3, 0.6)) +
  
  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = c("black", "black", "black"),
              nudge_x = c(0, 0.2, 0), # adjust the position of the labels
              nudge_y = c(0.2, -0.1, 0.1), # adjust the position of the labels
              #"Sapporo", "Hakodate", "Asahikawa"
              family = "sans") +
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Optimal Plan (Minimum Population Deviation) - ", year, " Projection"),
          subtitle = paste0("1票の格差: ", optimal_max_to_min, 
                          " | Districts: ", ndists_old, "→", ndists_new, 
                          " | Total Pop: ", format(total_population, big.mark = ","), 
                          " | Draw: ", optimal))

print(optimal_plot)

# Create zoomed-in plot for Ishikari region (Sapporo area)
cat("Creating Ishikari (Sapporo) region zoomed plot...\n")

# Filter data for Ishikari region
ishikari_codes <- c(01101, 01102, 01103, 01104, 01105, 01106, 01107, 01108, 01109, 01110,
                   01217, 01224, 01231, 01234, 01235, 01303, 01304)



ishikari_boundary <- optimal_boundary_colored %>%
  filter(code %in% ishikari_codes) %>%
  sf::st_make_valid() %>%
  sf::st_buffer(dist = 0.001) %>%  # 微小なバッファで隙間を埋める
  group_by(district, color) %>%
  summarise(geometry = sf::st_union(geometry), .groups = 'drop')

sf_use_s2(FALSE)
ishikari_boundary <- ishikari_boundary %>%
  sf::st_make_valid() %>%
  sf::st_buffer(dist = 0)
sf_use_s2(TRUE)

# Filter boundaries for Ishikari region
ishikari_mun_boundary <- mun_boundary %>%
  filter(code %in% ishikari_codes)

ishikari_gun_boundary <- gun_boundary %>%
  filter(gun_code == "ishikari")

# Combine Ishikari boundaries
ishikari_mun <- ishikari_mun_boundary %>%
  summarise(geometry = sf::st_combine(geometry))
ishikari_mun$type <- "Municipality Boundaries"

ishikari_gun <- ishikari_gun_boundary %>%
  summarise(geometry = sf::st_combine(geometry))
ishikari_gun$type <- "振興局 Boundaries"

ishikari_boundary_combined <- rbind(ishikari_mun, ishikari_gun)
ishikari_boundary_combined$type <- factor(ishikari_boundary_combined$type, 
                                         levels = ishikari_boundary_combined$type)

# Filter cities for Ishikari (only Sapporo)
ishikari_cities <- cities %>%
  filter(names == "Sapporo")

# Get bounding box for Ishikari region
ishikari_bbox <- sf::st_bbox(ishikari_boundary)

# Create Ishikari-focused plot
optimal_plot_ishikari <- ggplot() +
  geom_sf(data = ishikari_boundary, aes(fill = factor(color)), color = "white", size = 0.5) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  geom_sf(data = ishikari_boundary_combined, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#373C38", "#606264")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.5, 0.8)) +
  
  geom_sf(data = ishikari_cities, size = 3, shape = 21, fill = "red") +
  geom_sf_text(data = ishikari_cities, aes(label = names), size = 4,
              color = "black",
              nudge_x = 0.02, nudge_y = 0.03,
              family = "sans", fontface = "bold") +
  
  coord_sf(xlim = c(ishikari_bbox["xmin"], ishikari_bbox["xmax"]),
           ylim = c(ishikari_bbox["ymin"], ishikari_bbox["ymax"])) +
  
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank(),
        axis.text = element_text(size = 8)) +
  ggtitle(paste0("Optimal Plan - Ishikari Region (Sapporo Area) - ", year),
          subtitle = paste0("Zoomed view | Districts with Sapporo splits | Draw: ", optimal))

print(optimal_plot_ishikari)

# Print district information for Ishikari region
cat("\n=== ISHIKARI REGION DISTRICT ANALYSIS ===\n")
ishikari_districts <- optimal_plan_data %>%
  filter(district %in% unique(ishikari_boundary$district)) %>%
  arrange(district)

cat("Districts covering Ishikari region:\n")
for(i in 1:nrow(ishikari_districts)) {
  dist_data <- ishikari_districts[i, ]
  cat("  District", dist_data$district, ": Population =", 
      format(dist_data$total_pop, big.mark = ","), 
      ", Ruling share =", round(dist_data$ruling_share, 3), "\n")
}

# Calculate Sapporo city split information
sapporo_codes <- c(01101, 01102, 01103, 01104, 01105, 01106, 01107, 01108, 01109, 01110)
sapporo_districts <- unique(ishikari_boundary$district[ishikari_boundary$code %in% sapporo_codes])

cat("\nSapporo city district coverage:\n")
cat("  Sapporo spans", length(sapporo_districts), "districts:", paste(sapporo_districts, collapse = ", "), "\n")

# Identify which wards are split
split_wards <- c(01102, 01107, 01104)  # 北区、西区、白石区
for(ward_code in split_wards) {
  ward_districts <- unique(ishikari_boundary$district[ishikari_boundary$code == ward_code])
  ward_name <- unique(ishikari_boundary$mun_name[ishikari_boundary$code == ward_code])
  if(length(ward_districts) > 1) {
    cat("  ", ward_name, "(", ward_code, ") is split across districts:", 
        paste(ward_districts, collapse = ", "), "\n")
  } else if(length(ward_districts) == 1) {
    cat("  ", ward_name, "(", ward_code, ") is in district:", ward_districts, "\n")
  }
}

# Print summary for easy reference
cat("\n=== OPTIMAL PLAN SUMMARY ===\n")
cat("Year:", year, "\n")
cat("Prefecture:", pref_name, "(", pref_code, ")\n")
cat("Number of districts (change):", ndists_old, "→", ndists_new, "\n")
cat("Draw number:", optimal, "\n")
cat("1票の格差 (Max-to-Min ratio):", optimal_max_to_min, "\n")
cat("Population range:", min(pop_by_district), "-", max(pop_by_district), "\n")
cat("Average district population:", round(mean(pop_by_district), 0), "\n")

# Hokkaido-specific analysis
cat("\n=== HOKKAIDO-SPECIFIC CONTEXT ===\n")
cat("振興局 (Shinkokyoku) system maintained in simulation\n")
if("gun_split" %in% names(results_sample)) {
  cat("振興局 splits in optimal plan:", results_sample$gun_split[results_sample$draw == optimal], "\n")
}
if("koiki_split" %in% names(results_sample)) {
  cat("広域連携 splits in optimal plan:", results_sample$koiki_split[results_sample$draw == optimal], "\n")
}

# Municipal split analysis
if("mun_split" %in% names(results_sample)) {
  cat("Municipality splits in optimal plan:", results_sample$mun_split[results_sample$draw == optimal], "\n")
  if(results_sample$mun_split[results_sample$draw == optimal] > 0) {
    cat("Split municipalities likely include Sapporo wards (北区, 西区, 白石区)\n")
  }
}

# Save plots
cat("Saving plots...\n")
dir.create(here("data-out/co-occurrence"), recursive = TRUE, showWarnings = FALSE)

ggsave(here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_cooccurrence.png")), 
      plot = cooccurrence_plot, width = 12, height = 10, dpi = 300)

ggsave(here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_optimal_plan.png")), 
      plot = optimal_plot, width = 12, height = 10, dpi = 300)

# Save Ishikari region plot
ggsave(here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_optimal_plan_ishikari.png")), 
      plot = optimal_plot_ishikari, width = 10, height = 8, dpi = 300)
cat("Saved: optimal_plan_ishikari.png (zoomed Sapporo area)\n")

# Compare with current system
cat("\n=== COMPARISON WITH CURRENT SYSTEM ===\n")
cat("Current system (2022):", ndists_old, "districts\n")
cat("Future projection (", year, "):", ndists_new, "districts\n")
cat("District reduction:", ndists_old - ndists_new, "seats\n")
cat("Population decline accommodation in redistricting\n")

# Save files
cat("Cleaning up workspace...\n")
# Remove the irrelevant objects
rm(cl_co,
  m_co,
  mun,
  gun,
  mun_boundary,
  gun_boundary,
  pref_pop_2020,
  pref_shp_2020,
  pref_pop_cleaned,
  pref_shp_cleaned,
  pref_mun,
  pref_sep,
  pref_sep_add,
  pref_add_edge,
  pref_largest,
  pref_largest_adj,
  mainland,
  mainland_adj,
  mainland_add_edge,
  add_small,
  new_rows,
  pref_smc_plans,
  sim_smc_pref_good,
  wgt_smc,
  num_mun_split,
  mun_split,
  gun_split,
  koiki_split,
  matrix_optimal,
  functioning_results,
  results,
  pref_2019_HoC_PR,
  pref_2019_HoC_PR_cleaned,
  pref_2022_HoC_PR,
  pref_2022_HoC_PR_cleaned,
  pref_HoC_PR,
  pref,
  pref_map,
  pref_map_merged,
  prefadj,
  sim_smc_pref_ref,
  sim_smc_pref_sample,
  PAL
)

# Save workspace
save.image(here(paste("data-out/environment/",
                      as.character(pref_code),
                      "_",
                      as.character(pref_name),
                      "_data",
                      "_",
                      as.character(year),
                      ".Rdata",
                      sep = "")),
          compress = "xz")

cat("Co-occurrence analysis completed successfully!\n")
cat("Results saved to data-out/co-occurrence/ and data-out/environment/\n")
cat("Analysis completed for future projection year:", year, "\n")
cat("District count change:", ndists_old, "→", ndists_new, "\n")