###############################################################################
# Co-occurrence analysis for `08_ibaraki_future`
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

# Boundary for split municipality
old_boundary <- pref %>%
  filter(code %in% split_code) %>%
  summarise(geometry = sf::st_combine(geometry))
old_boundary$type <- "Old Municipality Boundaries"

# Municipality/Gun boundary
boundary <- rbind(old_boundary, mun, gun)
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
plot(as.dendrogram(cl_co), main = paste0("Co-occurrence Dendrogram - Ibaraki ", year, " Projection"))
abline(h = 2, col = "red") # explore different depths
abline(h = 1.75, col = "blue")
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
# Find the coordinates of major cities in Ibaraki
cities <- data.frame(longitude = c(140.471652, 140.103333, 140.006944, 140.256111, 140.096111),
                    latitude = c(36.365729, 36.189444, 35.975, 36.270833, 36.046667),
                    names = c("Mito", "Hitachi", "Tsukuba", "Hitachiota", "Tsuchiura"))
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
  scale_color_manual(values = c("#606264", "#373C38", "#606264")) +
  scale_linetype_manual(values = c("dotted", "solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.3, 0.3, 0.6)) +

  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = c("black", "black", "black", "black", "black"),
              nudge_x = c(0.05, 0.05, 0.05, 0.05, 0.05), # adjust the position of the labels
              nudge_y = c(-0.02, -0.02, -0.02, -0.02, -0.02), # adjust the position of the labels
              family = "sans") +
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Co-occurrence Analysis - Ibaraki ", year, " Projection (", ndists_new, " districts)"))

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

# Create optimal plan plot - Full Ibaraki
optimal_max_to_min <- round(max(pop_by_district)/min(pop_by_district), 3)
total_population <- sum(pop_by_district)

optimal_plot <- ggplot() +
  geom_sf(data = optimal_boundary_colored, aes(fill = factor(color)), color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  geom_sf(data = boundary, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#606264", "#373C38", "#606264")) +
  scale_linetype_manual(values = c("dotted", "solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.3, 0.3, 0.6)) +
  
  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = c("black", "black", "black", "black", "black"),
              nudge_x = c(0.05, 0.05, 0.05, 0.05, 0.05), # adjust the position of the labels
              nudge_y = c(-0.02, -0.02, -0.02, -0.02, -0.02), # adjust the position of the labels
              family = "sans") +
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Optimal Plan (Minimum Population Deviation) - Ibaraki ", year, " Projection"),
          subtitle = paste0("1票の格差: ", optimal_max_to_min, 
                          " | Districts: ", ndists_old, "→", ndists_new, 
                          " | Total Pop: ", format(total_population, big.mark = ","), 
                          " | Draw: ", optimal))

print(optimal_plot)

# Analysis covers the entire Ibaraki prefecture without regional zoom
cat("Analysis focused on entire Ibaraki prefecture\n")

# Print summary for easy reference
cat("\n=== OPTIMAL PLAN SUMMARY ===\n")
cat("Year:", year, "\n")
cat("Prefecture:", pref_name, "(", pref_code, ")\n")
cat("Number of districts (change):", ndists_old, "→", ndists_new, "\n")
cat("Draw number:", optimal, "\n")
cat("1票の格差 (Max-to-Min ratio):", optimal_max_to_min, "\n")
cat("Population range:", min(pop_by_district), "-", max(pop_by_district), "\n")
cat("Average district population:", round(mean(pop_by_district), 0), "\n")

# Ibaraki-specific analysis
cat("\n=== IBARAKI-SPECIFIC CONTEXT ===\n")
cat("Lakes removed: 霞ヶ浦, 北浦, 涸沼\n")
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
    split_names <- c("水戸市", "下妻市", "笠間市", "常陸大宮市", "小美玉市")
    for(i in 1:length(split_code)) {
      cat("  ", split_code[i], ":", split_names[i], "\n")
    }
  }
}

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
  cat("Saved: optimal_plan_central.png (zoomed Mito-Tsukuba area)\n")
}

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
  old_mun,
  mun_boundary,
  gun_boundary,
  old_boundary,
  census_mun_old_2020,
  pref_pop_2020,
  pref_shp_2020,
  pref_pop_cleaned,
  pref_shp_cleaned,
  pref_mun,
  pref_mun_old,
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