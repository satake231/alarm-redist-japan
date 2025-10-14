###############################################################################
# Co-occurrence analysis for `01_hokkaido_future` (完全版)
# © ALARM Project, May 2023
###############################################################################

cat("Starting co-occurrence analysis for", year, "projection...\n")

# Load required libraries
library(ggplot2)
library(ggthemes)
library(cluster)
library(dplyr)
library(sf)
library(ggpattern)

# Find Optimal Plan
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

pop_by_district <- optimal_plan_data$total_pop
names(pop_by_district) <- paste0("District ", optimal_plan_data$district)
print(pop_by_district)

cat("\nPopulation statistics:\n")
cat("Total population:", sum(pop_by_district), "\n")
cat("Average population per district:", round(mean(pop_by_district), 0), "\n")
cat("Maximum population:", max(pop_by_district), "\n")
cat("Minimum population:", min(pop_by_district), "\n")
cat("Max-to-Min ratio (1票の格差):", round(max(pop_by_district)/min(pop_by_district), 3), "\n")

# Optimal Plan - 小地域レベルを市区町村レベルに集約して白い筋を除去
matrix_optimal <- redist::get_plans_matrix(sim_smc_pref_ref %>%
                                            filter(draw == optimal))
colnames(matrix_optimal) <- "district"

cat("Aggregating census tracts to municipality level to eliminate internal boundaries...\n")

# 小地域レベルのデータを市区町村×選挙区レベルに集約
optimal_boundary_raw <- cbind(pref_map, as_tibble(matrix_optimal))

# 市区町村×選挙区の組み合わせで集約
optimal_boundary_aggregated <- optimal_boundary_raw %>%
  group_by(code, district) %>%
  summarise(
    # 人口データなどを合計
    pop = sum(pop, na.rm = TRUE),
    mun_name = first(mun_name),
    # ジオメトリを結合
    geometry = st_union(geometry),
    .groups = 'drop'
  ) %>%
  # ジオメトリを修復
  mutate(geometry = st_make_valid(geometry))

cat("Aggregated from", nrow(optimal_boundary_raw), "census tracts to", nrow(optimal_boundary_aggregated), "municipality-district units\n")

optimal_boundary <- optimal_boundary_aggregated

# Boundary data preparation
cat("Creating boundary data safely...\n")

# Municipality boundaries - 市区町村レベルで集約
mun_boundary <- pref_shp_cleaned %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  group_by(code) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')

# 振興局 boundaries - 北海道特有
gun_data <- pref %>%
  mutate(geometry = st_make_valid(geometry))

if(nrow(gun_data) > 0) {
  gun_boundary <- gun_data %>%
    group_by(gun_code) %>%
    summarise(geometry = st_union(geometry), .groups = 'drop')
} else {
  gun_boundary <- data.frame(
    gun_code = character(0),
    geometry = st_sfc(crs = st_crs(pref))
  ) %>% st_as_sf()
}

# 境界データの結合
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
      mutate(type = "振興局 Boundaries")
  } else {
    gun_combined <- data.frame(
      geometry = st_sfc(crs = st_crs(pref)),
      type = "振興局 Boundaries"
    ) %>% st_as_sf()
  }
}, error = function(e) {
  cat("Gun boundary union failed, creating empty boundary\n")
  gun_combined <<- data.frame(
    geometry = st_sfc(crs = st_crs(pref)),
    type = "振興局 Boundaries"
  ) %>% st_as_sf()
})

boundary <- bind_rows(mun_combined, gun_combined)
boundary$type <- factor(boundary$type, levels = c("Municipality Boundaries", "振興局 Boundaries"))

# Co-occurrence analysis
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

# Set the number of clusters
k <- min(21, nrow(pref_map))
cat("Using", k, "clusters for analysis\n")
prec_clusters = cutree(cl_co, k)
pref_membership <- as_tibble(as.data.frame(prec_clusters))
names(pref_membership) <- "membership"

# Calculate co-occurrence ratio - 市区町村レベルで集約
cat("Calculating co-occurrence ratios...\n")

# 市区町村レベルでのco-occurrence計算
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

# 市区町村レベルでのmembership
if(length(prec_clusters) == nrow(pref_map)) {
  mun_membership <- pref_map %>%
    st_drop_geometry() %>%
    mutate(membership = prec_clusters) %>%
    group_by(code) %>%
    summarise(membership = as.numeric(names(sort(table(membership), decreasing = TRUE))[1]),
              .groups = 'drop')
  
  mun_cooc_ratio <- rep(0.5, nrow(pref_map_aggregated))
  
  # 市区町村レベルでの隣接関係
  mun_adj <- redist::redist.adjacency(pref_map_aggregated)
  
  # 簡略化されたco-occurrence計算
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

# City coordinates
cities <- data.frame(
  longitude = c(141.35438, 140.728948, 142.365055),
  latitude = c(43.06206, 41.768663, 43.770687),
  names = c("Sapporo", "Hakodate", "Asahikawa")
)
cities <- sf::st_as_sf(cities, coords = c("longitude", "latitude"), crs = 4612)

# Color palette - 十分な色数を確保（最大21色）
PAL <- c('#666666', '#999999', '#CCCCCC', '#E5E5E5', '#000000', '#333333')

# 既存のcooccurrence_plotを以下に置き換え
cooccurrence_plot <- ggplot() +
  # Main polygons - 市区町村レベルで集約済み
  geom_sf(data = pref_cooc, aes(fill = as.factor(color), alpha = cooc_ratio), 
          color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  scale_alpha_continuous(range = c(0.3, 1.0), guide = "none") +
  
  # Boundary lines
  geom_sf(data = boundary, aes(color = type, linetype = type, size = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#000000", "#333333")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_size_manual(values = c(0.4, 0.7)) +

  # Cities and labels
  geom_sf(data = cities, size = 3, shape = 21, fill = "red", color = "black", stroke = 0.4) +
  geom_sf_text(data = cities, aes(label = names), size = 4,
              color = "black",
              nudge_x = c(0, 0.2, 0),
              nudge_y = c(0.2, -0.1, 0.1),
              family = "sans", fontface = "bold") +
  
  theme_map() +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  ggtitle(paste0("Co-occurrence Analysis - Hokkaido ", year, " Projection"),
          subtitle = paste0(ndists_new, " districts | ", k, " clusters | Top 10% of ", 
                          length(results_sample$draw), " plans"))

# Color assignment for optimal plan - 集約済みデータを使用
if(ndists_new > 6){
  optimal_adj <- redist::redist.adjacency(optimal_boundary_aggregated)
  optimal_boundary_colored <- optimal_boundary_aggregated %>%
    mutate(color = redist:::color_graph(optimal_adj, as.integer(district)))
} else {
  optimal_boundary_colored <- optimal_boundary_aggregated %>%
    mutate(color = district)
}

# Create optimal plan plot with clean municipality-level boundaries (モノクロ対応)
cat("Creating optimal plan map with clean municipality-level boundaries...\n")
optimal_max_to_min <- round(max(pop_by_district)/min(pop_by_district), 3)
total_population <- sum(pop_by_district)


# 既存のoptimal_plotを以下に置き換え
optimal_plot <- ggplot() +
  geom_sf_pattern(data = optimal_boundary_colored, 
                  aes(fill = factor(color), 
                      pattern = factor(color),
                      pattern_type = factor(color)), 
                  color = "black", size = 0.3,
                  pattern_density = 0.1,
                  pattern_spacing = 0.01,
                  pattern_size = 0.1) +
  
  scale_fill_grey(start = 0.8, end = 0.95, guide = "none") +
  scale_pattern_manual(values = c("stripe", "circle", "crosshatch", 
                                   "none", "wave", "polygon_tiling",
                                   "stripe", "circle", "crosshatch",
                                   "none", "wave"), 
                       guide = "none") +
  scale_pattern_type_manual(values = c("vertical", "horizontal", "left45",
                                       "right45", "square", "triangle",
                                       "vertical", "horizontal", "left45",
                                       "right45"),
                            guide = "none") +

  scale_color_manual(values = c("#000000", "#333333")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.4, 0.7)) +
  
  geom_sf(data = cities, size = 3, shape = 21, fill = "black", color = "white", stroke = 0.6) +
  geom_sf_text(data = cities, aes(label = names), size = 4,
              color = "black",
              nudge_x = c(0, 0.2, 0),
              nudge_y = c(0.2, -0.1, 0.1),
              family = "sans", fontface = "bold") +
  
  theme_map() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  ggtitle(paste0("Optimal Plan (Minimum Population Deviation) - Hokkaido ", year, " Projection"),
          subtitle = paste0("1票の格差: ", optimal_max_to_min, 
                          " | ", ndists_old, "→", ndists_new, " districts",
                          " | Total Pop: ", format(total_population, big.mark = ","), 
                          " | Draw: ", optimal))

# Define Ishikari region codes
ishikari_codes <- c(01101, 01102, 01103, 01104, 01105, 01106, 01107, 01108, 01109, 01110,
                   01217, 01224, 01231, 01234, 01235, 01303, 01304)

ishikari_optimal <- optimal_boundary_colored %>%
  filter(code %in% ishikari_codes)

ishikari_bbox <- sf::st_bbox(ishikari_optimal)

# Filter boundary data for Ishikari region
ishikari_boundary_filter <- boundary %>%
  st_make_valid() %>%
  st_crop(st_buffer(st_as_sfc(ishikari_bbox), dist = 0.01))



# Ishikari zoom viewを以下に置き換え
optimal_plot_ishikari <- ggplot() +
  geom_sf_pattern(data = ishikari_optimal, 
                  aes(fill = factor(color), 
                      pattern = factor(color),
                      pattern_type = factor(color)), 
                  color = "black", size = 0.3,
                  pattern_density = 0.1,
                  pattern_spacing = 0.01,
                  pattern_size = 0.1) +
  
  scale_fill_grey(start = 0.8, end = 0.95, guide = "none") +
  scale_pattern_manual(values = c("stripe", "circle", "crosshatch", 
                                   "none", "wave", "polygon_tiling",
                                   "stripe", "circle", "crosshatch",
                                   "none", "wave"), 
                       guide = "none") +
  scale_pattern_type_manual(values = c("vertical", "horizontal", "left45",
                                       "right45", "square", "triangle",
                                       "vertical", "horizontal", "left45",
                                       "right45"),
                            guide = "none") +
  
  geom_sf(data = ishikari_boundary_filter, 
          aes(color = type, linetype = type, linewidth = type),
          show.legend = FALSE, fill = NA) +
  scale_color_manual(values = c("#000000", "#333333")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.7, 0.9)) +
  
  geom_sf(data = cities %>% filter(names == "Sapporo"), 
          size = 4, shape = 21, fill = "black", color = "white", stroke = 0.6) +
  geom_sf_text(data = cities %>% filter(names == "Sapporo"), 
              aes(label = names), size = 5,
              color = "black",
              nudge_x = 0.02, nudge_y = 0.03,
              family = "sans", fontface = "bold") +
  
  coord_sf(xlim = c(ishikari_bbox["xmin"], ishikari_bbox["xmax"]),
           ylim = c(ishikari_bbox["ymin"], ishikari_bbox["ymax"]),
           expand = FALSE) +
  
  theme_map() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  ggtitle(paste0("Optimal Plan - Ishikari Region (Sapporo Area) - ", year),
          subtitle = paste0("Municipality-level aggregation | Zoomed view | Draw: ", optimal))


print(optimal_plot)
print(optimal_plot_ishikari)

ggsave(filename = "hokkaido_optimal_2050_conditional.png", plot = optimal_plot, width = 10, height = 8, dpi = 300, bg = "white")
ggsave(filename = "hokkaido_ishikari_optimal_2050_conditional.png", plot = optimal_plot_ishikari, width = 10, height = 8, dpi = 300, bg = "white")
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

# Save files
cat("Cleaning up workspace...\n")
# Remove the irrelevant objects
rm(cl_co,
  m_co,
  mun_combined,
  gun_combined,
  mun_boundary,
  gun_boundary,
  pref_pop_2020,
  pref_shp_2020,
  pref_pop_cleaned,
  pref_shp_cleaned,
  pref_mun,
  pref_sep,
  pref_largest,
  pref_largest_adj,
  mainland,
  mainland_adj,
  mainland_add_edge,
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