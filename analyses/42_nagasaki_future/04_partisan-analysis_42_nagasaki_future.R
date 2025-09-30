###############################################################################
# Partisan Analysis for `42_nagasaki_future`
# © ALARM Project, May 2023
###############################################################################

cat("=== STARTING PARTISAN ANALYSIS ===\n")
cat("Future projection year:", year, "\n")
cat("Prefecture: Nagasaki (", pref_code, ")\n")
cat("District change:", ndists_old, "→", ndists_new, "\n\n")

# Load data
pref_map <- readRDS(here(paste("data-out/map/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(year),
                              "_lh_2022_map.rds",
                              sep = "")))

sim_smc_pref_sample <- readRDS(here(paste("data-out/plans/",
                                          as.character(pref_code),
                                          "_",
                                          as.character(pref_name),
                                          "_",
                                          as.character(year),
                                          "_lh_2022_plans.rds",
                                          sep = "")))

cat("Number of sampled plans:", length(unique(sim_smc_pref_sample$draw)), "\n")
cat("Analysis data loaded successfully\n\n")

# Load required libraries
library(ggplot2)
library(redist)
library(dplyr)

# Find optimal plan for highlighting
cat("=== IDENTIFYING OPTIMAL PLAN ===\n")
if(exists("results_sample")) {
  optimal_draw <- results_sample$draw[which(results_sample$max_to_min == min(results_sample$max_to_min))][1]
  cat("Optimal plan found from results_sample: draw", optimal_draw, "\n")
} else {
  # Calculate max_to_min ratio for all plans
  temp_results <- sim_smc_pref_sample %>%
    group_by(draw) %>%
    summarise(max_to_min = max(total_pop)/min(total_pop), .groups = 'drop')
  optimal_draw <- temp_results$draw[which(temp_results$max_to_min == min(temp_results$max_to_min))][1]
  cat("Optimal plan calculated: draw", optimal_draw, "\n")
}

# Create optimal plan data for highlighting
optimal_plan_data <- sim_smc_pref_sample %>%
  filter(draw == optimal_draw) %>%
  mutate(draw = as.factor(draw))

cat("Optimal plan 1票の格差:", round(max(optimal_plan_data$total_pop)/min(optimal_plan_data$total_pop), 3), "\n\n")

# 1. Population Deviation Analysis
cat("=== 1. POPULATION DEVIATION ANALYSIS ===\n")
p_dev <- redist.plot.hist(sim_smc_pref_sample, qty = plan_dev, bins = 15) +
  labs(x = "Population Deviation", 
       y = "Percentage of Plans",
       title = paste0("Population Deviation - Nagasaki ", year, " Projection"),
       subtitle = paste0("District count: ", ndists_new, " districts"),
       caption = paste0("Based on ", length(unique(sim_smc_pref_sample$draw)), " simulated plans")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

print(p_dev)

dev_stats <- summary(sim_smc_pref_sample$plan_dev)
cat("Population deviation range:", round(min(sim_smc_pref_sample$plan_dev), 3), 
    "to", round(max(sim_smc_pref_sample$plan_dev), 3), "\n")
cat("Population deviation median:", round(median(sim_smc_pref_sample$plan_dev), 3), "\n\n")

# 2. Compactness Analysis
cat("=== 2. COMPACTNESS ANALYSIS ===\n")
p_comp <- redist.plot.hist(sim_smc_pref_sample, qty = comp_edge, bins = 15) +
  labs(x = "Fraction of Edges Kept", 
       y = "Percentage of Plans",
       title = paste0("Compactness - Nagasaki ", year, " Projection"),
       subtitle = "Edge-based compactness measure") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p_comp)

comp_stats <- summary(sim_smc_pref_sample$comp_edge)
cat("Compactness range:", round(min(sim_smc_pref_sample$comp_edge), 3), 
    "to", round(max(sim_smc_pref_sample$comp_edge), 3), "\n")
cat("Compactness median:", round(median(sim_smc_pref_sample$comp_edge), 3), "\n\n")

# 3. Ruling Coalition Vote Share Analysis
cat("=== 3. RULING COALITION ANALYSIS ===\n")

# Jitter plot
p_ruling_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      color_thresh = 0.5) +
  labs(title = paste0("Ruling Coalition Vote Share - Nagasaki ", year),
       subtitle = paste0("LDP + Komeito across ", ndists_new, " districts"),
       x = "District", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_ruling_jitter)

# Boxplot with optimal plan highlighted
p_ruling_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      geom = "boxplot") +
  # Add optimal plan as large points
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_share), 
            color = "red", size = 3, shape = 15) +
  labs(title = paste0("Ruling Coalition Vote Share Distribution - Nagasaki ", year),
       subtitle = paste0("Boxplots across ", ndists_new, " districts"),
       caption = paste0("Red squares show optimal plan (draw ", optimal_draw, ")"),
       x = "District (ordered by vote share)", y = "Ruling Coalition Vote Share") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p_ruling_box)

ruling_stats <- summary(sim_smc_pref_sample$ruling_share)
cat("Ruling coalition vote share range:", round(min(sim_smc_pref_sample$ruling_share), 3), 
    "to", round(max(sim_smc_pref_sample$ruling_share), 3), "\n")
cat("Ruling coalition vote share median:", round(median(sim_smc_pref_sample$ruling_share), 3), "\n\n")

# 4. LDP vs Komei Analysis
cat("=== 4. LDP VS KOMEI ANALYSIS ===\n")
p_scatter1 <- sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_share)) %>%
  redist.plot.scatter(x = ldp_share, y = komei_share) +
  facet_wrap(~dist_by_ruling_share, ncol = 3) +
  labs(title = paste0("LDP vs Komei Vote Share - Nagasaki ", year),
       subtitle = "By district (ordered by total ruling vote share)",
       x = "LDP Vote Share", y = "Komei Vote Share") +
  theme_bw()

print(p_scatter1)

# Custom color dot plot (if function exists)
if(exists("redist.plot.distr.custom.color")) {
  cat("Creating LDP vs Komei gradient plot...\n")
  p_custom1 <- redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_share,
                                color_var = ldp_v_komei) +
    scale_colour_gradient(low = "#f55881", high = "#3CA324", name = "LDP-Komei\nBalance") +
    labs(title = paste0("Ruling Share by District - Nagasaki ", year),
         subtitle = "Color indicates LDP vs Komei balance",
         x = "District (ordered by ruling share)", y = "Ruling Coalition Vote Share") +
    theme_bw()
  print(p_custom1)
}

# 5. Opposition Analysis (excluding DPP)
cat("=== 5. OPPOSITION ANALYSIS (EXCLUDING DPP) ===\n")
p_opp4_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                      color_thresh = 0.5) +
  labs(title = paste0("Ruling vs Opposition (excl. DPP) - Nagasaki ", year),
       subtitle = "CDP + JCP + Reiwa + SDP coalition",
       x = "District", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_opp4_jitter)

p_opp4_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                      geom = "boxplot") +
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_v_opp_coalition_4), 
            color = "red", size = 3, shape = 15) +
  labs(title = paste0("Ruling vs Opposition (excl. DPP) - Nagasaki ", year),
       caption = paste0("Red squares show optimal plan (draw ", optimal_draw, ")"),
       x = "District (ordered by vote share)", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_opp4_box)

# 6. All Opposition Analysis
cat("=== 6. ALL OPPOSITION ANALYSIS ===\n")
p_opp_all_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                      color_thresh = 0.5) +
  labs(title = paste0("Ruling vs All Opposition - Nagasaki ", year),
       subtitle = "Including Ishin in opposition",
       x = "District", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_opp_all_jitter)

p_opp_all_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                      geom = "boxplot") +
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_v_opp_all), 
            color = "red", size = 3, shape = 15) +
  labs(title = paste0("Ruling vs All Opposition - Nagasaki ", year),
       caption = paste0("Red squares show optimal plan (draw ", optimal_draw, ")"),
       x = "District (ordered by vote share)", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_opp_all_box)

# 7. Summary Statistics
cat("=== 7. SUMMARY STATISTICS ===\n")
cat("Basic Statistics:\n")
cat("  Population deviation: ", round(min(sim_smc_pref_sample$plan_dev), 3), 
    " to ", round(max(sim_smc_pref_sample$plan_dev), 3), "\n")
cat("  Compactness (edge): ", round(min(sim_smc_pref_sample$comp_edge), 3), 
    " to ", round(max(sim_smc_pref_sample$comp_edge), 3), "\n")
cat("  Ruling coalition vote share: ", round(min(sim_smc_pref_sample$ruling_share), 3), 
    " to ", round(max(sim_smc_pref_sample$ruling_share), 3), "\n")

# Nagasaki-specific analysis
cat("\nNagasaki-specific Analysis:\n")
if("gun_split" %in% names(sim_smc_pref_sample)) {
  cat("  Gun (county) splits: ", min(sim_smc_pref_sample$gun_split), 
      " to ", max(sim_smc_pref_sample$gun_split), "\n")
  cat("  Mean gun splits: ", round(mean(sim_smc_pref_sample$gun_split), 2), "\n")
}
if("koiki_split" %in% names(sim_smc_pref_sample)) {
  cat("  Koiki-renkei splits: ", min(sim_smc_pref_sample$koiki_split), 
      " to ", max(sim_smc_pref_sample$koiki_split), "\n")
  cat("  Mean koiki-renkei splits: ", round(mean(sim_smc_pref_sample$koiki_split), 2), "\n")
}
if("mun_split" %in% names(sim_smc_pref_sample)) {
  cat("  Municipality splits: ", min(sim_smc_pref_sample$mun_split), 
      " to ", max(sim_smc_pref_sample$mun_split), "\n")
  cat("  Mean municipality splits: ", round(mean(sim_smc_pref_sample$mun_split), 2), "\n")
}

# District-level analysis for optimal plan
cat("\nOptimal Plan Analysis:\n")
optimal_districts <- optimal_plan_data %>%
  arrange(district) %>%
  select(district, total_pop, ruling_share, ldp_share, komei_share)

for(i in 1:ndists_new) {
  dist_data <- optimal_districts[optimal_districts$district == i, ]
  cat("  District", i, ": Pop =", format(dist_data$total_pop, big.mark = ","), 
      ", Ruling =", round(dist_data$ruling_share, 3), 
      " (LDP:", round(dist_data$ldp_share, 3), 
      ", Komei:", round(dist_data$komei_share, 3), ")\n")
}

# 8. Future Projection Impact Analysis
cat("\n=== 8. FUTURE PROJECTION IMPACT ===\n")
cat("District Configuration:\n")
cat("  Districts: ", ndists_new, "\n")
if(ndists_new != ndists_old) {
  cat("  Change: ", ndists_new - ndists_old, " seats\n")
  cat("  Percentage change: ", round((ndists_new - ndists_old) / ndists_old * 100, 1), "%\n")
} else {
  cat("  No change in seat count\n")
}

# Calculate average district population change
# Assume current average population around 1.3M / 3 = ~433k per district
current_avg_pop <- 1300000 / ndists_old 
future_avg_pop <- sum(optimal_plan_data$total_pop) / ndists_new
cat("  Average district size:\n")
if(ndists_new != ndists_old) {
  cat("    Old system (hypothetical):", format(round(current_avg_pop), big.mark = ","), "\n")
  cat("    New system:", format(round(future_avg_pop), big.mark = ","), "\n")
} else {
  cat("    Current system:", format(round(current_avg_pop), big.mark = ","), "\n")
  cat("    Future (", year, "):", format(round(future_avg_pop), big.mark = ","), "\n")
}

# Population concentration analysis
pop_cv <- sd(optimal_plan_data$total_pop) / mean(optimal_plan_data$total_pop)
cat("  Population distribution (CV): ", round(pop_cv, 3), "\n")

# Competitive district analysis
competitive_threshold <- 0.55
competitive_districts <- sum(optimal_plan_data$ruling_share > 0.45 & 
                           optimal_plan_data$ruling_share < competitive_threshold)
cat("  Competitive districts (45-55%): ", competitive_districts, "/", ndists_new, "\n")

# 9. Regional Analysis (Koiki-renkei)
cat("\n=== 9. REGIONAL ANALYSIS ===\n")
cat("Koiki-renkei Division Analysis:\n")
cat("  Region 1: Nagasaki region (長崎地域)\n")
cat("  Region 2: Sasebo region (佐世保地域)\n")
cat("  These divisions respect traditional administrative boundaries\n")

# Estimate regional balance based on optimal plan
# This is simplified - actual regional assignment would require detailed mapping
cat("\nRegional characteristics:\n")
cat("  Urban centers: Nagasaki City, Sasebo City\n")
cat("  Island areas: Tsushima, Iki, Goto\n")
cat("  Population distribution affected by depopulation in islands\n")

# 10. Save Key Plots
cat("\n=== 10. SAVING PLOTS ===\n")
dir.create(here("data-out/partisan-analysis"), recursive = TRUE, showWarnings = FALSE)

plot_files <- list(
  list(plot = p_dev, name = "population_deviation"),
  list(plot = p_comp, name = "compactness"),
  list(plot = p_ruling_box, name = "ruling_coalition"),
  list(plot = p_ruling_jitter, name = "ruling_coalition_jitter"),
  list(plot = p_opp4_box, name = "opposition_coalition"),
  list(plot = p_opp_all_box, name = "all_opposition")
)

for(plot_info in plot_files) {
  filename <- here(paste0("data-out/partisan-analysis/", 
                         pref_code, "_", pref_name, "_", year, "_", 
                         plot_info$name, ".png"))
  ggsave(filename, plot = plot_info$plot, width = 10, height = 6, dpi = 300)
  cat("Saved:", basename(filename), "\n")
}

# Save scatter plot with different dimensions (3 districts = 1 row x 3 columns)
ggsave(here(paste0("data-out/partisan-analysis/", pref_code, "_", pref_name, "_", year, "_ldp_komei_scatter.png")), 
       plot = p_scatter1, width = 12, height = 5, dpi = 300)
cat("Saved: ldp_komei_scatter.png\n")

# 11. Export Summary Statistics
cat("\n=== 11. SUMMARY EXPORT ===\n")

# Create comprehensive summary
summary_stats <- data.frame(
  metric = c("population_deviation_min", "population_deviation_max", "population_deviation_median",
             "compactness_min", "compactness_max", "compactness_median",
             "ruling_share_min", "ruling_share_max", "ruling_share_median",
             "optimal_plan_kakusa", "competitive_districts", "district_count_change"),
  value = c(min(sim_smc_pref_sample$plan_dev), max(sim_smc_pref_sample$plan_dev), median(sim_smc_pref_sample$plan_dev),
            min(sim_smc_pref_sample$comp_edge), max(sim_smc_pref_sample$comp_edge), median(sim_smc_pref_sample$comp_edge),
            min(sim_smc_pref_sample$ruling_share), max(sim_smc_pref_sample$ruling_share), median(sim_smc_pref_sample$ruling_share),
            max(optimal_plan_data$total_pop)/min(optimal_plan_data$total_pop), competitive_districts, ndists_new - ndists_old)
)

write.csv(summary_stats, 
          here(paste0("data-out/partisan-analysis/", pref_code, "_", pref_name, "_", year, "_summary_stats.csv")), 
          row.names = FALSE)
cat("Saved: summary_stats.csv\n")

cat("\n=== PARTISAN ANALYSIS COMPLETED ===\n")
cat("Analysis year:", year, "\n")
cat("District configuration:", ndists_new, "districts\n")
cat("Optimal plan 1票の格差:", round(max(optimal_plan_data$total_pop)/min(optimal_plan_data$total_pop), 3), "\n")
cat("All plots and summaries saved to data-out/partisan-analysis/\n")

# Final recommendations
cat("\n=== RECOMMENDATIONS FOR FUTURE ANALYSIS ===\n")
cat("1. Monitor population decline particularly in island areas\n")
cat("2. Evaluate impact of aging population on electoral patterns\n")
cat("3. Consider ferry and air route connectivity in district planning\n")
cat("4. Review split municipality patterns (Nagasaki City, Sasebo City)\n")
cat("5. Assess koiki-renkei balance across districts\n")
cat("6. Monitor urban-rural divide and its political implications\n")
cat("7. Consider shipbuilding industry concentration effects\n")

cat("\nPartisan analysis completed successfully!\n")

# Special note for Nagasaki decline context
cat("\n=== NAGASAKI DECLINE CONTEXT ===\n")
cat("Key factors for 2050 redistricting:\n")
cat("  - Population decline (~15%) affecting all areas\n")
cat("  - Accelerated depopulation in remote islands\n")
cat("  - Aging society challenges in rural areas\n")
cat("  - Maintaining representation for island communities\n")
cat("  - Balancing urban (Nagasaki, Sasebo) vs rural representation\n")