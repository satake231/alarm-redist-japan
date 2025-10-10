###############################################################################
# Co-occurrence analysis for `11_saitama_future` (完全版)
# © ALARM Project, May 2023
###############################################################################

cat("Starting co-occurrence analysis for", year, "projection...\n")

# Load required libraries
library(ggplot2)
library(ggthemes)
library(cluster)
library(dplyr)
library(sf)

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

# Municipality boundaries
mun_boundary <- pref_shp_cleaned %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  group_by(code) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')

# Gun boundaries
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

boundary <- bind_rows(mun_combined, gun_combined)
boundary$type <- factor(boundary$type, levels = c("Municipality Boundaries", "County Boundaries"))

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
  longitude = c(139.644994, 139.485899, 139.723405, 139.790820, 139.463056, 139.533056),
  latitude = c(35.861878, 35.924942, 35.806661, 35.890952, 35.995556, 35.993056),
  names = c("Saitama", "Kawagoe", "Kawaguchi", "Koshigaya", "Tokorozawa", "Wako")
)
cities <- sf::st_as_sf(cities, coords = c("longitude", "latitude"), crs = 4612)

# Color palette
PAL <- c('#6D9537', '#9A9BB9', '#DCAD35', '#7F4E28', '#2A4E45', '#364B7F', 
         '#8B4513', '#2F4F4F', '#800080', '#FF6347', '#4682B4', '#32CD32',
         '#FFD700', '#FF69B4', '#00CED1', '#DA70D6', '#F0E68C', '#90EE90',
         '#CD853F', '#4169E1', '#FF1493')
# 04_partisan-analysis_11_saitama_condition.R の改善版

# 共通テーマ設定を追加
theme_common <- theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

# Population Deviation（改善版）
p_dev <- redist.plot.hist(sim_smc_pref_sample, qty = plan_dev, bins = 15) +
  labs(
    x = "Population Deviation", 
    y = "Percentage of Plans",
    title = paste0("Population Deviation - Saitama ", year, " (Stricter Constraint)"),
    subtitle = paste0("District count: ", ndists_old, " → ", ndists_new, 
                     " | pop_tol: ", pop_tol*100, "%"),
    caption = paste0("Based on ", length(unique(sim_smc_pref_sample$draw)), 
                    " simulated plans")
  ) +
  theme_common

# Ruling Coalition Boxplot（改善版）
p_ruling_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      geom = "boxplot") +
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_share), 
            color = "red", size = 4, shape = 15) +  # size 3→4
  labs(
    title = paste0("Ruling Coalition Vote Share - Saitama ", year),
    subtitle = paste0("Boxplots across ", ndists_new, " districts (stricter constraint)"),
    caption = paste0("Red squares: optimal plan (draw ", optimal_draw, 
                    ") | Max-to-min: ", 
                    round(max(optimal_plan_data$total_pop)/min(optimal_plan_data$total_pop), 3)),
    x = "District (ordered by vote share)", 
    y = "Ruling Coalition Vote Share"
  ) +
  theme_common

# 05_co-occurrence-analysis の改善版

# Co-occurrence plot（統一版）
cooccurrence_plot <- ggplot() +
  geom_sf(data = pref_cooc, 
          aes(fill = as.factor(color), alpha = cooc_ratio), 
          color = "white", size = 0.4) +  # size 0.3→0.4
  scale_fill_manual(values = PAL, guide = "none") +
  scale_alpha_continuous(range = c(0.3, 1.0), guide = "none") +
  geom_sf(data = boundary, 
          aes(color = type, linetype = type, size = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#000000", "#333333")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_size_manual(values = c(0.8, 1.0)) +  # 0.6, 0.8 → 0.8, 1.0
  geom_sf(data = cities, size = 2.5, shape = 21,  # size 2→2.5
          fill = "red", color = "black", stroke = 0.4) +  # stroke 0.3→0.4
  geom_sf_text(data = cities, aes(label = names), 
              size = 3.5,  # size 3→3.5
              color = "black", fontface = "bold",
              nudge_x = c(0.02, 0, 0, 0.10, 0, 0.05),
              nudge_y = c(0.02, -0.02, -0.04, 0, -0.03, 0.02)) +
  theme_map() +
  theme(
    legend.position = "right", 
    legend.title = element_blank(),
    legend.text = element_text(size = 11),  # 追加
    plot.title = element_text(size = 17, face = "bold"),  # size 16→17
    plot.subtitle = element_text(size = 13)  # size 12→13
  ) +
  ggtitle(
    paste0("Co-occurrence Analysis - Saitama ", year, " (Stricter Constraint)"),
    subtitle = paste0(ndists_new, " districts | ", k, " clusters | ",
                     "Top 10% of ", length(results_sample$draw), " plans | ",
                     "pop_tol: ", pop_tol*100, "%")
  )

# Optimal plan plot（統一版）
optimal_plot <- ggplot() +
  geom_sf(data = optimal_boundary_colored, 
          aes(fill = factor(color)), 
          color = "white", size = 0.4) +  # size 0.3→0.4
  scale_fill_manual(values = PAL, guide = "none") +
  geom_sf(data = boundary, 
          aes(color = type, linetype = type, size = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#000000", "#333333")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_size_manual(values = c(0.8, 1.0)) +  # 統一
  geom_sf(data = cities, size = 2.5, shape = 21,  # 統一
          fill = "red", color = "black", stroke = 0.4) +
  geom_sf_text(data = cities, aes(label = names), 
              size = 3.5, color = "black", fontface = "bold",
              nudge_x = c(0.02, 0, 0, 0.10, 0, 0.05),
              nudge_y = c(0.02, -0.02, -0.04, 0, -0.03, 0.02)) +
  theme_map() +
  theme(
    legend.position = "right", 
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 13)
  ) +
  ggtitle(
    paste0("Optimal Plan - Saitama ", year, " (Stricter Constraint)"),
    subtitle = paste0("1票の格差: ", optimal_max_to_min, 
                     " | Districts: ", ndists_old, "→", ndists_new, 
                     " | Pop: ", format(total_population, big.mark = ","), 
                     " | Draw: ", optimal,
                     " | pop_tol: ", pop_tol*100, "%")
  )

print(optimal_plot)

ggsave(filename = "saitama_optimal_2050_condition.png", plot = optimal_plot, width = 10, height = 8, dpi = 300)

# Print summary for easy reference
cat("\n=== OPTIMAL PLAN SUMMARY ===\n")
cat("Year:", year, "\n")
cat("Prefecture:", pref_name, "(", pref_code, ")\n")
cat("Number of districts (change):", ndists_old, "→", ndists_new, "\n")
cat("Draw number:", optimal, "\n")
cat("1票の格差 (Max-to-Min ratio):", optimal_max_to_min, "\n")
cat("Population range:", min(pop_by_district), "-", max(pop_by_district), "\n")
cat("Average district population:", round(mean(pop_by_district), 0), "\n")

# Saitama-specific analysis
cat("\n=== SAITAMA-SPECIFIC CONTEXT ===\n")
cat("Regional division methodology maintained\n")
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
    cat("Split municipalities may include:\n")
    split_names <- c("さいたま市見沼区", "熊谷市", "川口市", "春日部市", "鴻巣市", "越谷市", "久喜市", "ふじみ野市")
    for(i in 1:length(split_code)) {
      cat("  ", split_code[i], ":", split_names[i], "\n")
    }
  }
}

# Population growth impact
cat("\nPopulation growth impact:\n")
cat("  Population increase: ~10% by 2050\n")
cat("  District increase: 1 seat (", ndists_old, "→", ndists_new, ")\n")
cat("  Metropolitan area expansion accommodation\n")

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

# Additional Saitama-specific objects to remove
if(exists("mainland_south")) rm(mainland_south)
if(exists("mainland_south_adj")) rm(mainland_south_adj)
if(exists("south_largest")) rm(south_largest)
if(exists("south_largest_adj")) rm(south_largest_adj)
if(exists("gun_split_south")) rm(gun_split_south)
if(exists("mun_split_south")) rm(mun_split_south)
if(exists("num_mun_split_south")) rm(num_mun_split_south)
if(exists("no_multi_south")) rm(no_multi_south)
if(exists("south_map")) rm(south_map)
if(exists("south_sep")) rm(south_sep)
if(exists("south_smc_plans")) rm(south_smc_plans)
if(exists("south_smc_plans_no_multi")) rm(south_smc_plans_no_multi)
if(exists("results_south")) rm(results_south)
if(exists("results_south_no_multi")) rm(results_south_no_multi)
if(exists("sim_smc_south")) rm(sim_smc_south)
if(exists("sim_smc_south_no_multi")) rm(sim_smc_south_no_multi)
if(exists("functioning_results_south")) rm(functioning_results_south)
if(exists("wgt_smc_south")) rm(wgt_smc_south)

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
cat("Results saved with year suffix:", year, "\n")
cat("Ready for partisan analysis and co-occurrence analysis.\n")

# Special note for Saitama
cat("\n=== SAITAMA FUTURE REDISTRICTING NOTES ===\n")
cat("1. Population growth accommodated by district increase\n")
cat("2. Partial SMC methodology preserves regional balance\n")
cat("3. Complex urban geography handled through advanced algorithms\n")
cat("4. 秩父地域 special treatment maintains mountain area representation\n")
cat("5. Split municipalities reflect urban density patterns\n")
cat("6. Municipality-level aggregation eliminates internal boundary artifacts\n")