###############################################################################
# Co-occurrence analysis for `01_hokkaido`
# © ALARM Project, May 2023
###############################################################################

# Find Optimal Plan
# Note that `results_sample` includes the data for `lh_2022`
optimal <- as.numeric(results_sample$draw[which(results_sample$max_to_min ==
                                                  min(results_sample$max_to_min))][1])-1
results_sample[which(results_sample$draw == optimal),]

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

# Obtain co-occurrence matrix
m_co = redist::prec_cooccurrence(sim_smc_pref_good, sampled_only=TRUE)

# Create clusters
cl_co = cluster::agnes(m_co)

# Analyze the dendrogram and pick an appropriate number of clusters
plot(as.dendrogram(cl_co))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue")

# Set the number of clusters (change k to an appropriate number)
k <- 19 # Default: ndists_new
prec_clusters = cutree(cl_co, k)
pref_membership <- as_tibble(as.data.frame(prec_clusters))
names(pref_membership) <- "membership"

# Obtain co-occurrenc ratio
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
# TODO
## Find the coordinates of the 県庁,
## 政令指定都市の市庁舎 (if there is a 政令指定都市 different from 県庁所在地)
## and 中核市の市庁舎 (if there is a 中核市 different from 県庁所在地)
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
ggplot() +
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
              family = "HiraginoSans-W3") +
  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", legend.title = element_blank())


# Newly Enacted Plan
# Set Colors for Plot
if(ndists_new > 6){
  color_pref_map <- pref_map %>%
    mutate(color = redist:::color_graph(.$adj, as.integer(.$lh_2022)))
}else{
  color_pref_map <- pref_map %>%
    mutate(color = lh_2022)
}

# Plot Map with 1票の格差
enacted_map <- ggplot() +
  geom_sf(data = color_pref_map, aes(fill = factor(color)), color = NA) +
  scale_fill_manual(values = PAL, guide = "none") +

  geom_sf(data = boundary, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#373C38", "#606264")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.4, 0.7)) +

  geom_sf(data = cities, size = 3, shape = 21, fill = "red", color = "black", stroke = 0.4) +
  geom_sf_text(data = cities, aes(label = names), size = 4,
              color = "black",
              nudge_x = c(0, 0.2, 0),
              nudge_y = c(0.2, -0.1, 0.1),
              family = "HiraginoSans-W3", fontface = "bold") +

  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  ggtitle("2022 Enacted Plan - Hokkaido",
          subtitle = paste0("1票の格差: ", lh_2022_max_to_min, " | ", ndists_new, " districts"))

print(enacted_map)

ggsave(filename = "hokkaido_enacted_2022.png", plot = enacted_map, width = 12, height = 10, dpi = 300)

# Plot Optimal Plan Map
if(ndists_new > 6){
  optimal_boundary_colored <- optimal_boundary %>%
    mutate(color = redist:::color_graph(prefadj, as.integer(district)))
} else {
  optimal_boundary_colored <- optimal_boundary %>%
    mutate(color = district)
}

# Calculate optimal plan statistics BEFORE creating the plot
# Get population by district from pref_map and optimal plan assignment
optimal_pop_by_district <- optimal_boundary %>%
  st_drop_geometry() %>%
  group_by(district) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE), .groups = 'drop')

optimal_max_to_min <- round(max(optimal_pop_by_district$total_pop) / 
                            min(optimal_pop_by_district$total_pop), 3)
total_population <- sum(optimal_pop_by_district$total_pop)

# 色パレットを定義
PAL <- c('#6D9537', '#9A9BB9', '#DCAD35', '#7F4E28', '#2A4E45', '#364B7F')

# Plot Optimal Plan Map with detailed subtitle
optimal_map <- ggplot() +
  geom_sf(data = optimal_boundary_colored, aes(fill = factor(color)), color = NA) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  geom_sf(data = boundary, aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = if("old_boundary" %in% ls()) 
                      c("#606264", "#373C38", "#606264") 
                    else c("#373C38", "#606264")) +
  scale_linetype_manual(values = if("old_boundary" %in% ls()) 
                          c("dotted", "solid", "solid") 
                        else c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = if("old_boundary" %in% ls()) 
                          c(0.4, 0.4, 0.7) 
                        else c(0.4, 0.7)) +
  
  geom_sf(data = cities, size = 3, shape = 21, fill = "red", color = "black", stroke = 0.4) +
  geom_sf_text(data = cities, aes(label = names), size = 4,
              color = "black",
              nudge_x = c(0, 0.2, 0),
              nudge_y = c(0.2, -0.1, 0.1),
              family = "HiraginoSans-W3", fontface = "bold") +
  
  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  ggtitle("Optimal Plan (Minimum Population Deviation) - Hokkaido",
          subtitle = paste0("1票の格差: ", optimal_max_to_min, 
                          " | ", ndists_new, " districts",
                          " | Total Pop: ", format(total_population, big.mark = ","),
                          " | Draw: ", optimal))

print(optimal_map)

ggsave(filename = "hokkaido_optimal_2022.png", plot = optimal_map, width = 12, height = 10, dpi = 300)

###############################################################################
# Ishikari Region (Sapporo Area) Zoomed Plots - Enhanced
###############################################################################

# Define Ishikari region codes
ishikari_codes <- c(01101, 01102, 01103, 01104, 01105, 01106, 01107, 01108, 01109, 01110,
                   01217, 01224, 01231, 01234, 01235, 01303, 01304)

# Aggregate color_pref_map to municipality level to eliminate internal boundaries
color_pref_map_aggregated <- color_pref_map %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  group_by(code, color) %>%
  summarise(
    mun_name = first(mun_name),
    pop = sum(pop, na.rm = TRUE),
    lh_2022 = first(lh_2022),
    geometry = st_union(geometry),
    .groups = 'drop'
  ) %>%
  mutate(geometry = st_make_valid(geometry))

# 1. Enacted Plan - Ishikari Zoom
ishikari_enacted <- color_pref_map_aggregated %>%
  filter(code %in% ishikari_codes)

ishikari_bbox <- sf::st_bbox(ishikari_enacted)

# Filter boundary data for Ishikari region only
ishikari_boundary_filter <- boundary %>%
  st_make_valid() %>%
  st_crop(st_buffer(st_as_sfc(ishikari_bbox), dist = 0.01))

enacted_map_ishikari <- ggplot() +
  geom_sf(data = ishikari_enacted, aes(fill = factor(color)), 
          color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  geom_sf(data = ishikari_boundary_filter, 
          aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#000000", "#333333")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.7, 0.9)) +
  
  geom_sf(data = cities %>% filter(names == "Sapporo"), 
          size = 4, shape = 21, fill = "red", color = "black", stroke = 0.4) +
  geom_sf_text(data = cities %>% filter(names == "Sapporo"), 
              aes(label = names), size = 5,
              color = "black",
              nudge_x = 0.02, nudge_y = 0.03,
              family = "HiraginoSans-W3", fontface = "bold") +
  
  coord_sf(xlim = c(ishikari_bbox["xmin"], ishikari_bbox["xmax"]),
           ylim = c(ishikari_bbox["ymin"], ishikari_bbox["ymax"]),
           expand = FALSE) +
  
  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  ggtitle("2022 Enacted Plan - Ishikari Region (Sapporo Area)",
          subtitle = paste0("Municipality-level aggregation | Zoomed view | 1票の格差: ", lh_2022_max_to_min))

print(enacted_map_ishikari)

ggsave(filename = "hokkaido_enacted_2022_ishikari.png", 
       plot = enacted_map_ishikari, 
       width = 10, height = 8, dpi = 300)

# 2. Optimal Plan - Ishikari Zoom
# First aggregate optimal_boundary to municipality level
optimal_boundary_aggregated <- optimal_boundary %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  group_by(code, district) %>%
  summarise(
    mun_name = first(mun_name),
    pop = sum(pop, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = 'drop'
  ) %>%
  mutate(geometry = st_make_valid(geometry))

# Re-color after aggregation
if(ndists_new > 6){
  optimal_adj_agg <- redist::redist.adjacency(optimal_boundary_aggregated)
  optimal_boundary_aggregated <- optimal_boundary_aggregated %>%
    mutate(color = redist:::color_graph(optimal_adj_agg, as.integer(district)))
} else {
  optimal_boundary_aggregated <- optimal_boundary_aggregated %>%
    mutate(color = district)
}

ishikari_optimal <- optimal_boundary_aggregated %>%
  filter(code %in% ishikari_codes)

# Calculate 1票の格差 for zoom view subtitle
optimal_zoom_pop <- optimal_boundary_aggregated %>%
  st_drop_geometry() %>%
  group_by(district) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE), .groups = 'drop')

optimal_zoom_kakusa <- round(max(optimal_zoom_pop$total_pop) / 
                             min(optimal_zoom_pop$total_pop), 3)

optimal_map_ishikari <- ggplot() +
  geom_sf(data = ishikari_optimal, aes(fill = factor(color)), 
          color = "white", size = 0.3) +
  scale_fill_manual(values = PAL, guide = "none") +
  
  geom_sf(data = ishikari_boundary_filter, 
          aes(color = type, linetype = type, linewidth = type),
          show.legend = "line", fill = NA) +
  scale_color_manual(values = c("#000000", "#333333")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_discrete_manual("linewidth", values = c(0.7, 0.9)) +
  
  geom_sf(data = cities %>% filter(names == "Sapporo"), 
          size = 4, shape = 21, fill = "red", color = "black", stroke = 0.4) +
  geom_sf_text(data = cities %>% filter(names == "Sapporo"), 
              aes(label = names), size = 5,
              color = "black",
              nudge_x = 0.02, nudge_y = 0.03,
              family = "HiraginoSans-W3", fontface = "bold") +
  
  coord_sf(xlim = c(ishikari_bbox["xmin"], ishikari_bbox["xmax"]),
           ylim = c(ishikari_bbox["ymin"], ishikari_bbox["ymax"]),
           expand = FALSE) +
  
  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  ggtitle("Optimal Plan - Ishikari Region (Sapporo Area)",
          subtitle = paste0("Municipality-level aggregation | Zoomed view | 1票の格差: ", 
                          optimal_zoom_kakusa))

print(optimal_map_ishikari)

ggsave(filename = "hokkaido_optimal_2022_ishikari.png", 
       plot = optimal_map_ishikari, 
       width = 10, height = 8, dpi = 300)

cat("Ishikari region zoomed plots (enhanced) saved successfully!\n")
# Save files
# Remove the irrelevant objects
rm(cl_co,
  constr_pref,
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
  pref_2019_HoC_PR,
  pref_2019_HoC_PR_cleaned,
  pref_2022_HoC_PR,
  pref_2022_HoC_PR_cleaned,
  pref_HoC_PR,
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
                      "_data",
                      ".Rdata",
                      sep = "")),
          compress = "xz")

