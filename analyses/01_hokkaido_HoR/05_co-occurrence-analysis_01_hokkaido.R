###############################################################################
# Co-occurrence analysis for `01_hokkaido`
# Modified for House of Representatives (衆議院) data
# © ALARM Project, December 2024
###############################################################################

# Load necessary data with HoR suffix
pref_map <- readRDS(here(paste("data-out/map/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_lh_2022_map_hor.rds",
                              sep = "")))

prefadj <- readRDS(here(paste("data-out/adj/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_adj_hor.Rds",
                              sep = "")))

sim_smc_pref_ref <- readRDS(here(paste("data-out/smc-out/",
                                      as.character(pref_code),
                                      "_",
                                      as.character(pref_name),
                                      "_",
                                      as.character(sim_type),
                                      "_",
                                      as.character(nsims * 4),
                                      "_hor.Rds",
                                      sep = "")))

sim_smc_pref_sample <- readRDS(here(paste("data-out/plans/",
                                          as.character(pref_code),
                                          "_",
                                          as.character(pref_name),
                                          "_lh_2022_plans_hor.rds",
                                          sep = "")))

# Load original pref object
pref <- readRDS(here(paste("data-out/shapefile/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_hor.Rds",
                          sep = "")))

# Load shapefile data for boundaries
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf()

# Get results_sample from post-processing
results_sample <- sim_smc_pref_sample %>%
  as_tibble() %>%
  select(draw, max_to_min, gun_split, mun_split, koiki_split) %>%
  distinct()

# Find Optimal Plan
optimal <- as.numeric(results_sample$draw[which(results_sample$max_to_min ==
                                                  min(results_sample$max_to_min))][1])-1
cat("Optimal plan statistics:\n")
print(results_sample[which(results_sample$draw == optimal),])

# Optimal Plan
matrix_optimal <- redist::get_plans_matrix(sim_smc_pref_ref %>%
                                            filter(draw == optimal))
colnames(matrix_optimal) <- "district"
optimal_boundary <- cbind(pref_map, as_tibble(matrix_optimal))

# Gun/Municipality/Koiki-renkei boundaries
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
# Filter out plans with top 10% max_min ratio
good_num <- results_sample %>%
  # Filter out `lh_2022`
  filter(draw != "lh_2022") %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_sample$draw)*0.1)) %>%
  select(draw)
good_num <- as.vector(t(good_num))
sim_smc_pref_good <- sim_smc_pref_sample %>%
  filter(draw %in% good_num)

cat("Number of plans in top 10% by max_to_min ratio:", length(good_num), "\n")

# Obtain co-occurrence matrix
m_co = redist::prec_cooccurrence(sim_smc_pref_good, sampled_only=TRUE)

# Create clusters
cl_co = cluster::agnes(m_co)

# Analyze the dendrogram
plot(as.dendrogram(cl_co), 
    main = "Co-occurrence Dendrogram (HoR 2024 Data)",
    xlab = "", sub = "")
abline(h = 2, col = "red")
abline(h = 3, col = "blue")

# Set the number of clusters
k <- 19 # Default: ndists_new
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

for (i in 1:length(pref$code))
{
  cooc_ratio[i] <- 1 -
    sum(pref$pop[relcomp(prefadj[[i]]+1,
                        which(prec_clusters == prec_clusters[i]))] * m_co[i, relcomp(prefadj[[i]]+1,
                                                                                      which(prec_clusters == prec_clusters[i]))])/
    sum(pref$pop[prefadj[[i]]+1] * m_co[i, prefadj[[i]]+1])
}

# Co-occurrence Plot
# Coordinates of major cities
cities <- data.frame(longitude = c(141.35438, 140.728948, 142.365055),
                    latitude = c(43.06206, 41.768663, 43.770687),
                    names = c("Sapporo", "Hakodate", "Asahikawa"))
cities <- sf::st_as_sf(cities, coords = c("longitude", "latitude"),
                      crs = 4612)

# Match membership data with map object
if(ndists_new > 6){
  pref_cooc <- cbind(pref_map, cooc_ratio, pref_membership) %>%
    mutate(color = redist:::color_graph(.$adj, as.integer(.$membership)))
} else{
  pref_cooc <- cbind(pref_map, cooc_ratio, pref_membership) %>%
    mutate(color = .$membership)
}

## Reorder Color Palette
PAL <- c('#6D9537', '#9A9BB9', '#DCAD35', '#7F4E28', '#2A4E45', '#7F4E28')

# Co-occurrence plot
p1 <- ggplot() +
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
              nudge_x = c(0, 0.2, 0),
              nudge_y = c(0.2, -0.1, 0.1),
              family = "HiraginoSans-W3") +
  labs(title = "Co-occurrence Analysis (HoR 2024 Data)") +
  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", legend.title = element_blank())

print(p1)

# Newly Enacted Plan
# Set Colors for Plot
if(ndists_new > 6){
  color_pref_map <- pref_map %>%
    mutate(color = redist:::color_graph(.$adj, as.integer(.$lh_2022)))
}else{
  color_pref_map <- pref_map %>%
    mutate(color = lh_2022)
}

# Plot Map
p2 <- ggplot() +
  geom_sf(data = color_pref_map, aes(fill = factor(color)), color = NA) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  geom_sf(data = boundary, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#373C38", "#606264")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.3, 0.6)) +
  
  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names), size = 3,
              color = c("black", "black", "black"),
              nudge_x = c(0, 0.2, 0),
              nudge_y = c(0.2, -0.1, 0.1),
              family = "HiraginoSans-W3") +
  labs(title = "2022 Enacted Plan (HoR 2024 Vote Data)") +
  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", legend.title = element_blank())

print(p2)

# Save plots
ggsave(here(paste("figures/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_cooccurrence_hor.pdf",
                  sep = "")),
      plot = p1, width = 10, height = 8)

ggsave(here(paste("figures/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_enacted_plan_hor.pdf",
                  sep = "")),
      plot = p2, width = 10, height = 8)

# Summary statistics
cat("\n=== Co-occurrence Analysis Summary (HoR 2024) ===\n")
cat("Number of clusters:", k, "\n")
cat("Co-occurrence ratio range:", range(cooc_ratio, na.rm = TRUE), "\n")
cat("Mean co-occurrence ratio:", mean(cooc_ratio, na.rm = TRUE), "\n")

# Save workspace
# Remove intermediate objects
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
  ferries,
  suggest,
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
  dist_lh_2022,
  # Remove HoC-specific objects that don't exist in HoR version
  # pref_2019_HoC_PR,
  # pref_2019_HoC_PR_cleaned,
  # pref_2022_HoC_PR,
  # pref_2022_HoC_PR_cleaned,
  # pref_HoC_PR,
  # Add HoR-specific objects
  pref_2024_HoR_PR,
  pref_2024_HoR_PR_cleaned,
  pref_2024_HoR_PR_aggregated,
  pref_HoR_PR,
  pref,
  pref_map,
  pref_map_merged,
  prefadj,
  pref_join,
  sim_smc_pref_ref,
  sim_smc_pref_sample,
  simulation_weight_disparity_table,
  PAL
)

save.image(here(paste("data-out/environment/",
                      as.character(pref_code),
                      "_",
                      as.character(pref_name),
                      "_data_hor",  # HoR version
                      ".Rdata",
                      sep = "")),
          compress = "xz")

cat("\n=== Analysis Complete ===\n")
cat("All HoR-based analysis files have been saved.\n")
cat("Output files use '_hor' suffix to distinguish from HoC versions.\n")