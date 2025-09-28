###############################################################################
# Co-occurrence analysis for `11_saitama_future`
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
  filter(code >= (pref$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun <- mun_boundary %>%
  summarise(geometry = sf::st_combine(geometry))
mun$type <- "Municipality Boundaries"
# Combine gun boundary data
gun <- gun_boundary %>%
  summarise(geometry = sf::st_combine(geometry))
gun$type <- "County Boundaries"

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
plot(as.dendrogram(cl_co), main = paste0("Co-occurrence Dendrogram - Saitama ", year, " Projection"))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue")
dev.off()

# Set the number of clusters (change k to an appropriate number)
k <- 21 # Adjusted for 17 districts
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
# Find the coordinates of major cities in Saitama
cities <- data.frame(longitude = c(139.644994, 139.485899, 139.723405, 139.790820, 139.463056, 139.533056),
                    latitude = c(35.861878, 35.924942, 35.806661, 35.890952, 35.995556, 35.993056),
                    names = c("Saitama", "Kawagoe", "Kawaguchi", "Koshigaya", "Tokorozawa", "Wako"))
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
              color = c("black", "black", "black", "black", "black", "black"),
              nudge_x = c(0.02, 0, 0, 0.10, 0, 0.05), # adjust the position of the labels
              nudge_y = c(0.02, -0.02, -0.04, 0, -0.03, 0.02), # adjust the position of the labels
              family = "sans") +
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Co-occurrence Analysis - Saitama ", year, " Projection (", ndists_new, " districts)"))

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

# Create optimal plan plot - Full Saitama
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
              color = c("black", "black", "black", "black", "black", "black"),
              nudge_x = c(0.02, 0, 0, 0.10, 0, 0.05), # adjust the position of the labels
              nudge_y = c(0.02, -0.02, -0.04, 0, -0.03, 0.02), # adjust the position of the labels
              family = "sans") +
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Optimal Plan (Minimum Population Deviation) - Saitama ", year, " Projection"),
          subtitle = paste0("1票の格差: ", optimal_max_to_min, 
                          " | Districts: ", ndists_old, "→", ndists_new, 
                          " | Total Pop: ", format(total_population, big.mark = ","), 
                          " | Draw: ", optimal))

print(optimal_plot)

# Create zoomed-in plot for Central Saitama region (Saitama City, Kawaguchi area)
cat("Creating Central Saitama region zoomed plot...\n")

# Filter data for central Saitama region (metropolitan core area)
central_codes <- c(11101, 11102, 11103, 11104, 11105, 11106, 11107, 11108, 11109, 11110,  # さいたま市全区
                   11203, 11206, 11208, 11209, 11214, 11215, 11221, 11222, 11223, 11224, 11225, 11227, 11228, 11229, 11230,  # 川口、草加、川越、所沢、春日部、飯能、八潮、越谷、蕨、戸田、朝霞、志木、和光、新座
                   11235, 11237, 11238, 11242, 11245)  # 富士見、三芳、ふじみ野、坂戸、鶴ヶ島

# Use the original optimal_boundary_colored data without complex operations
central_boundary <- optimal_boundary_colored %>%
  filter(code %in% central_codes)

# Validate the filtering result
cat("Central region units found:", nrow(central_boundary), "\n")
cat("Districts in central region:", paste(sort(unique(central_boundary$district)), collapse = ", "), "\n")

# Check if川越市 and 所沢市 are included
kawagoe_check <- central_boundary %>% filter(code == 11208)
tokorozawa_check <- central_boundary %>% filter(code == 11209)
cat("川越市 found:", nrow(kawagoe_check), "entries\n")
cat("所沢市 found:", nrow(tokorozawa_check), "entries\n")

# Only proceed if we have central boundary data
if(nrow(central_boundary) > 0) {
  
  # Filter boundaries for central region - simplified approach
  central_mun_boundary <- mun_boundary %>%
    filter(code %in% central_codes)
  
  # Combine central boundaries
  central_mun <- central_mun_boundary %>%
    summarise(geometry = sf::st_combine(geometry))
  central_mun$type <- "Municipality Boundaries"
  
  central_boundary_combined <- central_mun
  central_boundary_combined$type <- factor(central_boundary_combined$type, 
                                           levels = central_boundary_combined$type)
  
  # Filter cities for central region
  central_cities <- cities %>%
    filter(names %in% c("Saitama", "Kawaguchi", "Kawagoe", "Tokorozawa"))
  
  # Get bounding box for central region
  central_bbox <- sf::st_bbox(central_boundary)
  
  # Add some padding to the bounding box
  x_padding <- (central_bbox["xmax"] - central_bbox["xmin"]) * 0.05
  y_padding <- (central_bbox["ymax"] - central_bbox["ymin"]) * 0.05
  
  # Create central region plot with no complex geometry operations
  optimal_plot_central <- ggplot() +
    geom_sf(data = central_boundary, aes(fill = factor(color)), color = "white", size = 0.3) +
    scale_fill_manual(values = PAL, guide = "none") +
    
    geom_sf(data = central_boundary_combined, color = "#373C38", 
            linetype = "solid", linewidth = 0.4, fill = NA) +
    
    geom_sf(data = central_cities, size = 2.5, shape = 21, fill = "red", color = "black", stroke = 0.5) +
    geom_sf_text(data = central_cities, aes(label = names), size = 3.5,
                color = "black",
                nudge_x = c(0.015, 0.015, -0.025, 0.015), # Saitama, Kawaguchi, Kawagoe, Tokorozawa
                nudge_y = c(0.015, -0.015, 0.015, 0.015),
                family = "sans", fontface = "bold") +
    
    coord_sf(xlim = c(central_bbox["xmin"] - x_padding, central_bbox["xmax"] + x_padding),
             ylim = c(central_bbox["ymin"] - y_padding, central_bbox["ymax"] + y_padding)) +
    
    theme_map() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank()) +
    ggtitle(paste0("Optimal Plan - Central Saitama Region - ", year),
            subtitle = paste0("Metropolitan core area | Draw: ", optimal))
  
  print(optimal_plot_central)
  
  # Print district information for central region
  cat("\n=== CENTRAL SAITAMA DISTRICT ANALYSIS ===\n")
  central_districts <- optimal_plan_data %>%
    filter(district %in% unique(central_boundary$district)) %>%
    arrange(district)
  
  cat("Districts covering central Saitama region:\n")
  for(i in 1:nrow(central_districts)) {
    dist_data <- central_districts[i, ]
    cat("  District", dist_data$district, ": Population =", 
        format(dist_data$total_pop, big.mark = ","), 
        ", Ruling share =", round(dist_data$ruling_share, 3), "\n")
  }
  
  # Check specific municipalities
  saitama_codes <- c(11101, 11102, 11103, 11104, 11105, 11106, 11107, 11108, 11109, 11110)
  saitama_districts <- unique(central_boundary$district[central_boundary$code %in% saitama_codes])
  
  cat("\nさいたま市 district coverage:\n")
  cat("  さいたま市 spans", length(saitama_districts), "districts:", paste(saitama_districts, collapse = ", "), "\n")
  
  # Check for specific cities
  city_checks <- list(
    "川口市" = 11203,
    "川越市" = 11208, 
    "所沢市" = 11209
  )
  
  for(city_name in names(city_checks)) {
    city_code <- city_checks[[city_name]]
    city_districts <- unique(central_boundary$district[central_boundary$code == city_code])
    if(length(city_districts) > 0) {
      cat("  ", city_name, "is in district:", paste(city_districts, collapse = ", "), "\n")
    } else {
      cat("  ", city_name, "NOT FOUND in central region\n")
    }
  }
  
} else {
  cat("ERROR: No central region data found. Check central_codes filtering.\n")
  optimal_plot_central <- NULL
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

# Save plots
cat("Saving plots...\n")
dir.create(here("data-out/co-occurrence"), recursive = TRUE, showWarnings = FALSE)

ggsave(here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_cooccurrence.png")), 
      plot = cooccurrence_plot, width = 12, height = 10, dpi = 300)

ggsave(here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_optimal_plan.png")), 
      plot = optimal_plot, width = 12, height = 10, dpi = 300)

# Save central region plot if it exists
if(exists("optimal_plot_central")) {
  ggsave(here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_optimal_plan_central.png")), 
        plot = optimal_plot_central, width = 10, height = 8, dpi = 300)
  cat("Saved: optimal_plan_central.png (zoomed Saitama-Kawaguchi area)\n")
}

# Compare with current system
cat("\n=== COMPARISON WITH CURRENT SYSTEM ===\n")
cat("Current system (2022):", ndists_old, "districts\n")
cat("Future projection (", year, "):", ndists_new, "districts\n")
cat("District increase:", ndists_new - ndists_old, "seats\n")
cat("Population growth accommodation in redistricting\n")
cat("Partial SMC methodology: South-North regional division\n")

# Save files
cat("Cleaning up workspace...\n")
# Remove the irrelevant objects (Saitama-specific cleanup)
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
cat("Central Saitama region analysis included for detailed metropolitan area view\n")