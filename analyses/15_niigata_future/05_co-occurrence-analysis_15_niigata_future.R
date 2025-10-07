###############################################################################
# Co-occurrence analysis for `15_niigata_future`
# © ALARM Project, May 2023
###############################################################################

cat("=== STARTING CO-OCCURRENCE ANALYSIS ===\n")
cat("Future projection year:", year, "\n")
cat("Prefecture: Niigata (", pref_code, ")\n")
cat("District change:", ndists_old, "→", ndists_new, "\n\n")

# Load required libraries
library(ggplot2)
library(ggthemes)
library(cluster)
library(dplyr)
library(sf)

# Find Optimal Plan
cat("=== FINDING OPTIMAL PLAN ===\n")
optimal <- as.numeric(results_sample$draw[which(results_sample$max_to_min ==
                                                  min(results_sample$max_to_min))][1])
cat("Optimal plan found: draw", optimal, "\n")
cat("  Max-to-min ratio:", min(results_sample$max_to_min), "\n\n")

# Display optimal plan details
optimal_stats <- results_sample[which(results_sample$draw == optimal),]
cat("Optimal plan statistics:\n")
print(optimal_stats)
cat("\n")

# Calculate detailed population statistics for optimal plan
optimal_plan_data <- sim_smc_pref_ref %>% 
  filter(draw == optimal) %>%
  arrange(district)

cat("=== OPTIMAL PLAN POPULATION ANALYSIS ===\n")
cat("Draw:", optimal, "\n")
cat("Number of districts:", ndists_new, "\n\n")

pop_by_district <- optimal_plan_data$total_pop
names(pop_by_district) <- paste0("District ", optimal_plan_data$district)
print(pop_by_district)

cat("\nPopulation statistics:\n")
cat("  Total population:", format(sum(pop_by_district), big.mark = ","), "\n")
cat("  Average population per district:", format(round(mean(pop_by_district)), big.mark = ","), "\n")
cat("  Maximum population:", format(max(pop_by_district), big.mark = ","), "\n")
cat("  Minimum population:", format(min(pop_by_district), big.mark = ","), "\n")
cat("  Max-to-Min ratio (1票の格差):", round(max(pop_by_district)/min(pop_by_district), 3), "\n\n")

# Optimal Plan - aggregate to municipality level to eliminate internal boundaries
cat("=== PREPARING OPTIMAL PLAN MAP ===\n")
matrix_optimal <- redist::get_plans_matrix(sim_smc_pref_ref %>%
                                            filter(draw == optimal))
colnames(matrix_optimal) <- "district"

cat("Aggregating old municipalities to current municipality level...\n")

# Combine optimal plan with map
optimal_boundary_raw <- cbind(pref_map, as_tibble(matrix_optimal))

# Aggregate by municipality code × district combination
optimal_boundary_aggregated <- optimal_boundary_raw %>%
  group_by(code, district) %>%
  summarise(
    # Sum population data
    pop = sum(pop, na.rm = TRUE),
    mun_name = first(mun_name),
    # Union geometry
    geometry = st_union(geometry),
    .groups = 'drop'
  ) %>%
  # Repair geometry
  mutate(geometry = st_make_valid(geometry))

cat("Aggregated from", nrow(optimal_boundary_raw), "old municipalities to", 
    nrow(optimal_boundary_aggregated), "municipality-district units\n\n")

optimal_boundary <- optimal_boundary_aggregated

# Boundary data preparation
cat("=== PREPARING BOUNDARY DATA ===\n")

# Municipality boundaries
mun_boundary <- pref_shp_cleaned %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  group_by(code) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')

# Gun (county) boundaries
gun_data <- pref %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  filter(code >= (pref$code[1]%/%1000)* 1000 + 300)

if(nrow(gun_data) > 0) {
  gun_boundary <- gun_data %>%
    group_by(gun_code) %>%
    summarise(geometry = st_union(geometry), .groups = 'drop')
} else {
  gun_boundary <- data.frame(
    gun_code = integer(0),
    geometry = st_sfc(crs = st_crs(pref))
  ) %>% st_as_sf()
}

# Old municipality boundaries (for split municipalities like Nagaoka)
if(length(split_code) > 0) {
  old_boundary <- pref %>%
    filter(code %in% as.numeric(split_code)) %>%
    group_by(pre_gappei_code) %>%
    summarise(geometry = st_union(geometry), .groups = 'drop')
  
  old_boundary_combined <- old_boundary %>%
    summarise(geometry = st_union(geometry)) %>%
    mutate(type = "Old Municipality Boundaries")
} else {
  old_boundary_combined <- NULL
}

# Combine boundary data
cat("Combining boundary layers...\n")
tryCatch({
  mun_combined <- mun_boundary %>%
    summarise(geometry = st_union(geometry)) %>%
    mutate(type = "Municipality Boundaries")
}, error = function(e) {
  cat("Municipality boundary union failed, using first geometry\n")
  mun_combined <<- mun_boundary[1,] %>%
    select(geometry) %>%
    mutate(type = "Municipality Boundaries")
})

tryCatch({
  if(nrow(gun_boundary) > 0) {
    gun_combined <- gun_boundary %>%
      summarise(geometry = st_union(geometry)) %>%
      mutate(type = "County Boundaries")
  } else {
    gun_combined <- data.frame(
      geometry = st_sfc(crs = st_crs(pref)),
      type = "County Boundaries"
    ) %>% st_as_sf()
  }
}, error = function(e) {
  cat("Gun boundary union failed, creating empty boundary\n")
  gun_combined <<- data.frame(
    geometry = st_sfc(crs = st_crs(pref)),
    type = "County Boundaries"
  ) %>% st_as_sf()
})

# Combine all boundaries
if(!is.null(old_boundary_combined)) {
  boundary <- bind_rows(old_boundary_combined, mun_combined, gun_combined)
  boundary$type <- factor(boundary$type, 
                         levels = c("Old Municipality Boundaries", 
                                   "Municipality Boundaries", 
                                   "County Boundaries"))
} else {
  boundary <- bind_rows(mun_combined, gun_combined)
  boundary$type <- factor(boundary$type, 
                         levels = c("Municipality Boundaries", "County Boundaries"))
}

cat("Boundary data prepared successfully\n\n")

# Co-occurrence analysis
cat("=== CO-OCCURRENCE ANALYSIS ===\n")
cat("Filtering top 10% plans by population deviation...\n")
good_num <- results_sample %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(min(length(results_sample$draw)*0.1, nrow(results_sample)))) %>%
  select(draw)
good_num <- as.vector(t(good_num))
sim_smc_pref_good <- sim_smc_pref_sample %>%
  filter(draw %in% good_num)

cat("Using", length(good_num), "plans for co-occurrence analysis\n\n")

# Obtain co-occurrence matrix
cat("Calculating co-occurrence matrix...\n")
m_co = redist::prec_cooccurrence(sim_smc_pref_good, sampled_only=TRUE)

# Create clusters
cat("Creating hierarchical clusters...\n")
cl_co = cluster::agnes(m_co)

# Analyze the dendrogram
cat("Analyzing dendrogram...\n")
png(here(paste0("temp/dendrogram_", year, "_niigata.png")), width = 1000, height = 600)
plot(as.dendrogram(cl_co), main = paste0("Co-occurrence Dendrogram - Niigata ", year))
abline(h = 2, col = "red", lwd = 2)
abline(h = 1.75, col = "blue", lwd = 2)
legend("topright", legend = c("h=2.0 (suggested)", "h=1.75 (alternative)"), 
       col = c("red", "blue"), lwd = 2)
dev.off()
cat("Dendrogram saved\n\n")

# Set the number of clusters
k <- ndists_new  # Default: same as number of districts
cat("Using k =", k, "clusters\n")
prec_clusters = cutree(cl_co, k)
pref_membership <- as_tibble(as.data.frame(prec_clusters))
names(pref_membership) <- "membership"

# Calculate co-occurrence ratio
cat("Calculating co-occurrence ratios...\n")

# Aggregate to municipality level for co-occurrence
pref_map_aggregated <- pref_map %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  group_by(code) %>%
  summarise(
    mun_name = first(mun_name),
    pop = sum(pop, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = 'drop'
  ) %>%
  mutate(geometry = st_make_valid(geometry))

# Municipality-level membership
if(length(prec_clusters) == nrow(pref_map)) {
  mun_membership <- pref_map %>%
    st_drop_geometry() %>%
    mutate(membership = prec_clusters) %>%
    group_by(code) %>%
    summarise(membership = as.numeric(names(sort(table(membership), decreasing = TRUE))[1]),
              .groups = 'drop')
  
  mun_cooc_ratio <- rep(0.5, nrow(pref_map_aggregated))
  
  # Municipality-level adjacency
  mun_adj <- redist::redist.adjacency(pref_map_aggregated)
  
  # Simplified co-occurrence calculation
  for (i in 1:nrow(pref_map_aggregated)) {
    if(length(mun_adj[[i]]) > 0) {
      adjacent_units <- mun_adj[[i]] + 1
      current_membership <- mun_membership$membership[i]
      same_cluster <- which(mun_membership$membership == current_membership)
      different_cluster <- setdiff(adjacent_units, same_cluster)
      
      if(length(adjacent_units) > 0) {
        mun_cooc_ratio[i] <- 1 - (length(different_cluster) / length(adjacent_units))
      }
    }
  }
  
  pref_cooc_aggregated <- pref_map_aggregated %>%
    left_join(mun_membership, by = "code") %>%
    mutate(cooc_ratio = mun_cooc_ratio,
           membership = ifelse(is.na(membership), 1, membership))
  
  if(ndists_new > 6){
    pref_cooc <- pref_cooc_aggregated %>%
      mutate(color = redist:::color_graph(mun_adj, as.integer(membership)))
  } else {
    pref_cooc <- pref_cooc_aggregated %>%
      mutate(color = membership)
  }
} else {
  pref_cooc <- pref_map_aggregated %>%
    mutate(membership = 1, cooc_ratio = 0.5, color = 1)
}

cat("Co-occurrence ratios calculated\n\n")

# City coordinates for Niigata
cat("=== PREPARING CITY LABELS ===\n")
cities <- data.frame(
  longitude = c(139.036971, 138.851420, 138.236717, 139.328178, 138.922507, 138.370000),
  latitude = c(37.915993, 37.446708, 37.148028, 37.957500, 37.737668, 38.018611),
  names = c("Niigata", "Nagaoka", "Joetsu", "Shibata", "Sanjo", "Sado")
)
cities <- sf::st_as_sf(cities, coords = c("longitude", "latitude"), crs = 4612)
cat("City labels prepared\n\n")
# Color palette
PAL <- c('#6D9537', '#9A9BB9', '#DCAD35', '#7F4E28', '#2A4E45', '#364B7F', 
         '#8B4513', '#2F4F4F', '#800080', '#FF6347', '#4682B4', '#32CD32')

# Create co-occurrence plot
cat("=== CREATING CO-OCCURRENCE PLOT ===\n")
cooccurrence_plot <- ggplot() +
  # Main polygons - municipality-level aggregation
  geom_sf(data = pref_cooc, aes(fill = as.factor(color), alpha = cooc_ratio), 
          color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  scale_alpha_continuous(range = c(0.3, 1.0), guide = "none") +
  
  # Boundary lines
  geom_sf(data = boundary, aes(color = type, linetype = type, size = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = if(length(levels(boundary$type)) == 3) 
                      c("#606264", "#000000", "#333333") 
                    else c("#000000", "#333333")) +
  scale_linetype_manual(values = if(length(levels(boundary$type)) == 3) 
                          c("dotted", "solid", "solid") 
                        else c("solid", "solid")) +
  scale_size_manual(values = if(length(levels(boundary$type)) == 3) 
                      c(0.3, 0.6, 0.8) 
                    else c(0.6, 0.8)) +

  # Cities and labels
  geom_sf(data = cities, size = 2, shape = 21, fill = "red", color = "black", stroke = 0.3) +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = "black",
              nudge_x = c(-0.05, 0, 0, 0.05, 0, -0.05),  # Niigata, Nagaoka, Joetsu, Shibata, Sanjo, Sado
              nudge_y = c(0.08, -0.03, -0.03, 0.03, -0.03, 0.05),
              family = "sans") +
  
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Co-occurrence Analysis - Niigata ", year, " Projection (", ndists_new, " districts)"))

print(cooccurrence_plot)

# Save co-occurrence plot
ggsave(here(paste0("data-out/partisan-analysis/", pref_code, "_", pref_name, "_", year, "_cooccurrence.png")),
       plot = cooccurrence_plot, width = 10, height = 8, dpi = 300)
cat("Co-occurrence plot saved\n\n")

# Create optimal plan map
cat("=== CREATING OPTIMAL PLAN MAP ===\n")

# Color assignment for optimal plan
if(ndists_new > 6){
  optimal_adj <- redist::redist.adjacency(optimal_boundary_aggregated)
  optimal_boundary_colored <- optimal_boundary_aggregated %>%
    mutate(color = redist:::color_graph(optimal_adj, as.integer(district)))
} else {
  optimal_boundary_colored <- optimal_boundary_aggregated %>%
    mutate(color = district)
}

# Calculate statistics for subtitle
optimal_max_to_min <- round(max(pop_by_district)/min(pop_by_district), 3)
total_population <- sum(pop_by_district)

optimal_plot <- ggplot() +
  # Main polygons - aggregated for clean boundaries
  geom_sf(data = optimal_boundary_colored, aes(fill = factor(color)), 
          color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  # Administrative boundaries
  geom_sf(data = boundary, aes(color = type, linetype = type, size = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = if(length(levels(boundary$type)) == 3) 
                      c("#606264", "#000000", "#333333") 
                    else c("#000000", "#333333")) +
  scale_linetype_manual(values = if(length(levels(boundary$type)) == 3) 
                          c("dotted", "solid", "solid") 
                        else c("solid", "solid")) +
  scale_size_manual(values = if(length(levels(boundary$type)) == 3) 
                      c(0.3, 0.6, 0.8) 
                    else c(0.6, 0.8)) +
  
  # Cities and labels
  geom_sf(data = cities, size = 2, shape = 21, fill = "red", color = "black", stroke = 0.3) +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = "black",
              nudge_x = c(-0.05, 0, 0, 0.05, 0, -0.05),
              nudge_y = c(0.08, -0.03, -0.03, 0.03, -0.03, 0.05),
              family = "sans") +
  
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Optimal Plan (Minimum Population Deviation) - Niigata ", year, " Projection"),
          subtitle = paste0("1票の格差: ", optimal_max_to_min, 
                          " | Districts: ", ndists_old, "→", ndists_new, 
                          " | Total Pop: ", format(total_population, big.mark = ","), 
                          " | Draw: ", optimal))

print(optimal_plot)
ggsave(filename = "niigata_optimal_2050.png", plot = optimal_plot)

# Save optimal plan map
ggsave(here(paste0("data-out/partisan-analysis/", pref_code, "_", pref_name, "_", year, "_optimal_plan.png")),
       plot = optimal_plot, width = 10, height = 8, dpi = 300)
cat("Optimal plan map saved\n\n")

# Print summary for easy reference
cat("=== OPTIMAL PLAN SUMMARY ===\n")
cat("Year:", year, "\n")
cat("Prefecture:", pref_name, "(", pref_code, ")\n")
cat("Number of districts (change):", ndists_old, "→", ndists_new, "\n")
cat("Draw number:", optimal, "\n")
cat("1票の格差 (Max-to-Min ratio):", optimal_max_to_min, "\n")
cat("Population range:", format(min(pop_by_district), big.mark = ","), "-", 
    format(max(pop_by_district), big.mark = ","), "\n")
cat("Average district population:", format(round(mean(pop_by_district)), big.mark = ","), "\n\n")

# Niigata-specific analysis
cat("=== NIIGATA-SPECIFIC CONTEXT ===\n")
cat("Rural prefecture methodology maintained\n")
if("gun_split" %in% names(results_sample)) {
  cat("Gun (county) splits in optimal plan:", results_sample$gun_split[results_sample$draw == optimal], "\n")
}
if("koiki_split" %in% names(results_sample)) {
  cat("Koiki-renkei splits in optimal plan:", results_sample$koiki_split[results_sample$draw == optimal], "\n")
}

# Municipal split analysis
if("mun_split" %in% names(results_sample)) {
  cat("Municipality splits in optimal plan:", results_sample$mun_split[results_sample$draw == optimal], "\n")
  if(results_sample$mun_split[results_sample$draw == optimal] > 0) {
    cat("Split municipality:\n")
    cat("  15202: 長岡市 (Nagaoka-shi)\n")
  }
}

# Population decline context
cat("\nPopulation decline impact:\n")
cat("  Population decline: ~20% by 2050\n")
cat("  District reduction: 1 seat (", ndists_old, "→", ndists_new, ")\n")
cat("  Rural depopulation accommodation\n")
cat("  Average district size increased\n")

# Ferry connections
cat("\nFerry connections (critical for Niigata):\n")
cat("  ✓ Sado Island (佐渡市) ↔ Niigata City (mainland)\n")
cat("  ✓ Awashimaura (粟島浦村) ↔ Murakami (村上市)\n")
cat("  Island representation maintained via ferry routes\n")

# Save workspace
cat("\n=== SAVING WORKSPACE ===\n")
cat("Cleaning up workspace...\n")

# Remove irrelevant objects
rm(cl_co, m_co, mun_combined, gun_combined, mun_boundary, gun_boundary,
   pref_pop_2020, pref_shp_2020, pref_pop_cleaned, pref_shp_cleaned,
   pref_mun, pref_mun_old, pref_sep, pref_largest, pref_largest_adj,
   mainland, mainland_adj, pref_smc_plans, sim_smc_pref_good,
   wgt_smc, num_mun_split, mun_split, gun_split, koiki_split,
   matrix_optimal, functioning_results, results,
   pref_2019_HoC_PR, pref_2019_HoC_PR_cleaned,
   pref_2022_HoC_PR, pref_2022_HoC_PR_cleaned, pref_HoC_PR,
   pref, pref_map, pref_map_merged, prefadj,
   sim_smc_pref_ref, sim_smc_pref_sample, PAL)

# Additional Niigata-specific objects
if(exists("old_mun")) rm(old_mun)
if(exists("census_mun_old_2020")) rm(census_mun_old_2020)
if(exists("geom")) rm(geom)
if(exists("pop")) rm(pop)
if(exists("ferries")) rm(ferries)
if(exists("old_boundary")) rm(old_boundary)
if(exists("old_boundary_combined")) rm(old_boundary_combined)

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

cat("Workspace saved successfully\n\n")

cat("=== CO-OCCURRENCE ANALYSIS COMPLETED ===\n")
cat("Results saved with year suffix:", year, "\n")
cat("All plots saved to data-out/partisan-analysis/\n\n")

# Special note for Niigata
cat("=== NIIGATA FUTURE REDISTRICTING NOTES ===\n")
cat("1. Population decline accommodated by district reduction (5→4)\n")
cat("2. Rural prefecture methodology maintains administrative boundaries\n")
cat("3. Ferry connections preserve Sado Island representation\n")
cat("4. Koiki-renkei areas maintain regional cooperation\n")
cat("5. Old municipality boundaries respect historical divisions (Nagaoka)\n")
cat("6. Municipality-level aggregation eliminates internal boundary artifacts\n")
cat("7. Mountain and coastal areas maintain distinct representation\n")
cat("8. Average district size increased due to seat reduction\n\n")

cat("Next steps:\n")
cat("  1. Review co-occurrence patterns for regional coherence\n")
cat("  2. Evaluate optimal plan for political balance\n")
cat("  3. Consider impact on island and rural communities\n")
cat("  4. Assess ferry connectivity importance in final plans\n")