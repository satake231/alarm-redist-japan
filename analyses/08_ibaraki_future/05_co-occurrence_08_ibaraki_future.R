###############################################################################
# Co-occurrence analysis for `21_gifu_future`
# © ALARM Project, May 2023
###############################################################################

cat("=== STARTING GIFU FUTURE CO-OCCURRENCE ANALYSIS ===\n")
cat("Future projection year:", year, "\n")
cat("Prefecture: Gifu (", pref_code, ")\n")
cat("District change:", ndists_old, "→", ndists_new, "\n\n")

# Load required libraries
library(cluster)
library(ggplot2)
library(dplyr)
library(sf)
library(redist)
library(ggthemes)

# Load data
cat("=== LOADING DATA ===\n")
pref <- readRDS(here::here(paste("data-out/shapefile/",
                           as.character(pref_code),
                           "_",
                           as.character(pref_name),
                           "_",
                           as.character(year),
                           ".Rds",
                           sep = "")))

pref_map <- readRDS(here::here(paste("data-out/map/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(year),
                               "_lh_2022_map.rds",
                               sep = "")))

prefadj <- readRDS(here::here(paste("data-out/adj/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(year),
                              "_adj.Rds",
                              sep = "")))

sim_smc_pref_ref <- readRDS(here::here(paste("data-out/smc-out/",
                                       as.character(pref_code),
                                       "_",
                                       as.character(pref_name),
                                       "_",
                                       as.character(sim_type),
                                       "_",
                                       as.character(year),
                                       "_",
                                       as.character(nsims * 4),
                                       ".Rds",
                                       sep = "")))

sim_smc_pref_sample <- readRDS(here::here(paste("data-out/plans/",
                                          as.character(pref_code),
                                          "_",
                                          as.character(pref_name),
                                          "_",
                                          as.character(year),
                                          "_lh_2022_plans.rds",
                                          sep = "")))

# Load results_sample (from post-processing)
if(!exists("results_sample")) {
  cat("Creating results_sample from sim_smc_pref_sample...\n")
  results_sample <- sim_smc_pref_sample %>%
    group_by(draw) %>%
    summarise(
      max_to_min = max(total_pop) / min(total_pop),
      gun_split = first(gun_split),
      mun_split = first(mun_split),
      koiki_split = first(koiki_split),
      .groups = 'drop'
    )
}

cat("Data loaded successfully\n\n")

# Find Optimal Plan
cat("=== FINDING OPTIMAL PLAN ===\n")
optimal <- as.numeric(results_sample$draw[which(results_sample$max_to_min ==
                                                  min(results_sample$max_to_min))][1])
optimal_max_to_min <- round(results_sample$max_to_min[results_sample$draw == optimal], 3)
total_population <- sum(pref$pop)


cat("Optimal plan (lowest max-to-min ratio):\n")
cat("  Draw:", optimal, "\n")
cat("  Max-to-min ratio:", optimal_max_to_min, "\n")

optimal_stats <- results_sample[which(results_sample$draw == optimal),]
cat("  Municipality splits:", optimal_stats$mun_split, "\n")
cat("  Gun (county) splits:", optimal_stats$gun_split, "\n")
cat("  Koiki-renkei splits:", optimal_stats$koiki_split, "\n\n")

# Optimal Plan districts
cat("Creating optimal plan visualization data...\n")
matrix_optimal <- redist::get_plans_matrix(sim_smc_pref_ref %>%
                                             filter(draw == optimal))
colnames(matrix_optimal) <- "district"
optimal_boundary <- cbind(pref_map, as_tibble(matrix_optimal))

# Create boundary layers
cat("\n=== CREATING BOUNDARY LAYERS ===\n")

# Municipality boundaries
mun_boundary <- pref %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

# Gun boundaries (for prefectures with gun system)
gun_boundary <- pref %>%
  filter(code >= (pref$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun <- mun_boundary %>%
  summarise(geometry = sf::st_combine(geometry))
mun$type <- "市の境界線"

# Combine gun boundary data
gun <- gun_boundary %>%
  summarise(geometry = sf::st_combine(geometry))
gun$type <- "郡の境界線"

boundary <- rbind(mun, gun)
boundary$type <- factor(boundary$type, levels = c("郡の境界線", "市の境界線"))

cat("Boundary layers created:\n")
cat("  Municipality boundaries: ✓\n")
cat("  County boundaries: ✓\n\n")


# Co-occurrence Analysis
cat("=== CO-OCCURRENCE ANALYSIS ===\n")
cat("Filtering plans with top 10% max-min ratio...\n")

# Filter out plans with top 10% max-min ratio (best performing)
good_num <- results_sample %>%
  arrange(max_to_min) %>%
  slice(1:as.numeric(nrow(results_sample)*0.1)) %>%
  pull(draw)

cat("Selected", length(good_num), "plans for co-occurrence analysis\n")

sim_smc_pref_good <- sim_smc_pref_sample %>%
  filter(draw %in% good_num)

# Obtain co-occurrence matrix
cat("Calculating co-occurrence matrix...\n")
m_co <- redist::prec_cooccurrence(sim_smc_pref_good, sampled_only=TRUE)
cat("Co-occurrence matrix dimensions:", nrow(m_co), "x", ncol(m_co), "\n\n")

# Create clusters
cat("=== CLUSTERING ANALYSIS ===\n")
cat("Performing hierarchical clustering...\n")
cl_co <- cluster::agnes(m_co)

# Set the number of clusters
k <- ndists_new
cat("Number of clusters:", k, "\n\n")
prec_clusters <- cutree(cl_co, k)
pref_membership <- as_tibble(as.data.frame(prec_clusters))
names(pref_membership) <- "membership"
cat("Cluster assignment completed\n\n")

# Calculate co-occurrence ratio
cat("=== CALCULATING CO-OCCURRENCE RATIOS ===\n")
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

cat("Processing", length(pref$code), "units...\n")
for (i in 1:length(pref$code)) {
  if(i %% 50 == 0) cat("  Processed", i, "/", length(pref$code), "units\n")

  cooc_ratio[i] <- 1 -
    sum(pref$pop[relcomp(prefadj[[i]]+1,
                         which(prec_clusters == prec_clusters[i]))] * m_co[i, relcomp(prefadj[[i]]+1,
                          which(prec_clusters == prec_clusters[i]))])/
    sum(pref$pop[prefadj[[i]]+1] * m_co[i, prefadj[[i]]+1])
}
cat("Co-occurrence ratios calculated\n\n")

# City coordinates for labels
cat("=== CREATING VISUALIZATION ===\n")
cat("Setting up city labels...\n")
# Identify top 3 cities by population
top_cities <- pref %>%
  st_drop_geometry() %>%
  group_by(mun_name) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE)) %>%
  arrange(desc(total_pop)) %>%
  slice(1:3) %>%
  pull(mun_name)

# Get centroids for the top 3 cities
cities <- pref %>%
  filter(mun_name %in% top_cities) %>%
  group_by(mun_name) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid() %>%
  rename(names = mun_name)


# Match membership data with map object
if(ndists_new > 6){
  pref_cooc <- cbind(pref_map, cooc_ratio, pref_membership) %>%
    mutate(color = redist:::color_graph(.$adj, as.integer(.$membership)))
}else{
  pref_cooc <- cbind(pref_map, cooc_ratio, pref_membership) %>%
    mutate(color = .$membership)
}

# Color Palette
PAL <- c('#6D9537', '#9A9BB9', '#DCAD35', '#2A4E45', '#7F4E28', '#E85D75')

# Co-occurrence plot
cat("Creating co-occurrence plot...\n")
cooc_plot <- ggplot() +
  geom_sf(data = pref_cooc, aes(fill = as.factor(color), alpha = cooc_ratio), color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  scale_alpha_continuous(range = c(min(cooc_ratio), max(cooc_ratio)), guide = "none") +
  
  geom_sf(data = boundary, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("郡の境界線" = "#606264", "市の境界線" = "#373C38")) +
  scale_linetype_manual(values = c("郡の境界線" = "solid", "市の境界線" = "solid")) +
  scale_discrete_manual("linewidth", values = c("郡の境界線" = 0.6, "市の境界線" = 0.3)) +
  
  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = "black",
              nudge_x = 0.05,
              nudge_y = -0.02,
              family = "sans") +
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Co-occurrence Analysis - Gifu ", year, " Projection"),
          subtitle = paste0("Based on top 10% of plans (", length(good_num), " plans)"))

print(cooc_plot)

ggsave(here::here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_cooccurrence.png")),
       plot = cooc_plot, width = 12, height = 10, dpi = 300)

cat("Co-occurrence plot saved\n\n")

# Optimal Plan visualization
cat("=== CREATING OPTIMAL PLAN MAP ===\n")
if(ndists_new > 6){
  optimal_boundary_colored <- optimal_boundary %>%
    mutate(color = redist:::color_graph(pref_map$adj, as.integer(.$district)))
}else{
  optimal_boundary_colored <- optimal_boundary %>%
    mutate(color = district)
}

optimal_plot <- ggplot() +
  geom_sf(data = optimal_boundary_colored, aes(fill = factor(color)), color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  geom_sf(data = boundary, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("郡の境界線" = "#606264", "市の境界線" = "#373C38")) +
  scale_linetype_manual(values = c("郡の境界線" = "solid", "市の境界線" = "solid")) +
  scale_discrete_manual("linewidth", values = c("郡の境界線" = 0.6, "市の境界線" = 0.3)) +
  
  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = "black",
              nudge_x = 0.05,
              nudge_y = -0.02,
              family = "sans") +
  theme_map() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ggtitle(paste0("Optimal Plan (Minimum Population Deviation) - Gifu ", year, " Projection"),
          subtitle = paste0("1票の格差: ", optimal_max_to_min, 
                          " | Districts: ", ndists_old, "→", ndists_new, 
                          " | Total Pop: ", format(total_population, big.mark = ","), 
                          " | Draw: ", optimal))

print(optimal_plot)

ggsave(here::here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_optimal.png")),
       plot = optimal_plot, width = 12, height = 10, dpi = 300)

cat("Optimal plan map saved\n\n")

cat("=== SAVING SUMMARY DATA ===\n")

cooc_summary <- data.frame(
  unit = 1:length(cooc_ratio),
  cluster = prec_clusters,
  cooc_ratio = cooc_ratio,
  municipality = pref$mun_name,
  code = pref$code
)

write.csv(cooc_summary,
          here::here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_cooc_summary.csv")),
          row.names = FALSE)

cat("Co-occurrence summary saved\n")

cluster_stats <- cooc_summary %>%
  group_by(cluster) %>%
  summarise(
    n_units = n(),
    mean_cooc = mean(cooc_ratio),
    sd_cooc = sd(cooc_ratio),
    min_cooc = min(cooc_ratio),
    max_cooc = max(cooc_ratio),
    .groups = 'drop'
  )

write.csv(cluster_stats,
          here::here(paste0("data-out/co-occurrence/", pref_code, "_", pref_name, "_", year, "_cluster_stats.csv")),
          row.names = FALSE)

cat("Cluster statistics saved\n\n")

cat("=== CLEANING WORKSPACE ===\n")
rm(cl_co, m_co, mun, gun,
   mun_boundary, gun_boundary,
   matrix_optimal, optimal_boundary, optimal_boundary_colored,
   cities, PAL, cooc_ratio, prec_clusters, pref_membership,
   good_num, sim_smc_pref_good,
   pref_cooc, boundary, cooc_plot, optimal_plot, cooc_summary, cluster_stats,
   optimal_stats, total_population, optimal_max_to_min, relcomp,
   k, optimal, top_cities)
cat("Workspace cleaned\n\n")

cat("=== CO-OCCURRENCE ANALYSIS COMPLETED SUCCESSFULLY ===\n")