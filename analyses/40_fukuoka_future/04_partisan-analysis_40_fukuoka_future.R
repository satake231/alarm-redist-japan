###############################################################################
# Partisan Analysis for `40_fukuoka_future`
# © ALARM Project, May 2023
###############################################################################

cat("=== STARTING PARTISAN ANALYSIS ===\n")
cat("Future projection year:", year, "\n")
cat("Prefecture: Fukuoka (", pref_code, ")\n")
cat("District change:", ndists_old, "→", ndists_new, "\n")

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
  temp_results <- sim_smc_pref_sample %>%
    group_by(draw) %>%
    summarise(max_to_min = max(total_pop)/min(total_pop), .groups = 'drop')
  optimal_draw <- temp_results$draw[which(temp_results$max_to_min == min(temp_results$max_to_min))][1]
  cat("Optimal plan calculated: draw", optimal_draw, "\n")
}

optimal_plan_data <- sim_smc_pref_sample %>%
  filter(draw == optimal_draw) %>%
  mutate(draw = as.factor(draw))

cat("Optimal plan 1票の格差:", round(max(optimal_plan_data$total_pop)/min(optimal_plan_data$total_pop), 3), "\n\n")

# 1. Population Deviation Analysis
cat("=== 1. POPULATION DEVIATION ANALYSIS ===\n")
p_dev <- redist.plot.hist(sim_smc_pref_sample, qty = plan_dev, bins = 15) +
  labs(x = "Population Deviation", 
       y = "Percentage of Plans",
       title = paste0("Population Deviation - Fukuoka ", year, " Projection"),
       subtitle = paste0("District count: ", ndists_old, " → ", ndists_new, " districts"),
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
       title = paste0("Compactness - Fukuoka ", year, " Projection"),
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

p_ruling_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      color_thresh = 0.5) +
  labs(title = paste0("Ruling Coalition Vote Share - Fukuoka ", year),
       subtitle = paste0("LDP + Komeito across ", ndists_new, " districts"),
       x = "District", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_ruling_jitter)

p_ruling_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      geom = "boxplot") +
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_share), 
            color = "red", size = 3, shape = 15) +
  labs(title = paste0("Ruling Coalition Vote Share Distribution - Fukuoka ", year),
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
  facet_wrap(~dist_by_ruling_share, ncol = 4) +
  labs(title = paste0("LDP vs Komei Vote Share - Fukuoka ", year),
       subtitle = "By district (ordered by total ruling vote share)",
       x = "LDP Vote Share", y = "Komei Vote Share") +
  theme_bw()

print(p_scatter1)

if(exists("redist.plot.distr.custom.color")) {
  cat("Creating LDP vs Komei gradient plot...\n")
  p_custom1 <- redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_share,
                                color_var = ldp_v_komei) +
    scale_colour_gradient(low = "#f55881", high = "#3CA324", name = "LDP-Komei\nBalance") +
    labs(title = paste0("Ruling Share by District - Fukuoka ", year),
         subtitle = "Color indicates LDP vs Komei balance",
         x = "District (ordered by ruling share)", y = "Ruling Coalition Vote Share") +
    theme_bw()
  print(p_custom1)
}

# 5. Opposition Analysis (excluding DPP)
cat("=== 5. OPPOSITION ANALYSIS (EXCLUDING DPP) ===\n")
p_opp4_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                      color_thresh = 0.5) +
  labs(title = paste0("Ruling vs Opposition (excl. DPP) - Fukuoka ", year),
       subtitle = "CDP + JCP + Reiwa + SDP coalition",
       x = "District", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_opp4_jitter)

p_opp4_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                      geom = "boxplot") +
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_v_opp_coalition_4), 
            color = "red", size = 3, shape = 15) +
  labs(title = paste0("Ruling vs Opposition (excl. DPP) - Fukuoka ", year),
       caption = paste0("Red squares show optimal plan (draw ", optimal_draw, ")"),
       x = "District (ordered by vote share)", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_opp4_box)

# 6. All Opposition Analysis
cat("=== 6. ALL OPPOSITION ANALYSIS ===\n")
p_opp_all_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                      color_thresh = 0.5) +
  labs(title = paste0("Ruling vs All Opposition - Fukuoka ", year),
       subtitle = "Including Ishin in opposition",
       x = "District", y = "Ruling Coalition Vote Share") +
  theme_bw()

print(p_opp_all_jitter)

p_opp_all_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                      geom = "boxplot") +
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_v_opp_all), 
            color = "red", size = 3, shape = 15) +
  labs(title = paste0("Ruling vs All Opposition - Fukuoka ", year),
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

if("gun_split" %in% names(sim_smc_pref_sample)) {
  cat("  County (gun) splits: ", min(sim_smc_pref_sample$gun_split), 
      " to ", max(sim_smc_pref_sample$gun_split), "\n")
  cat("  Mean county splits: ", round(mean(sim_smc_pref_sample$gun_split), 2), "\n")
}
if("koiki_split" %in% names(sim_smc_pref_sample)) {
  cat("  広域連携 (koiki) splits: ", min(sim_smc_pref_sample$koiki_split), 
      " to ", max(sim_smc_pref_sample$koiki_split), "\n")
  cat("  Mean 広域連携 splits: ", round(mean(sim_smc_pref_sample$koiki_split), 2), "\n")
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
cat("District Increase Impact:\n")
cat("  Districts increased: ", ndists_new - ndists_old, " seat(s)\n")
cat("  Percentage increase: ", round((ndists_new - ndists_old) / ndists_old * 100, 1), "%\n")

current_avg_pop <- 5104921 # Approximate 2020 Fukuoka population
future_avg_pop <- sum(optimal_plan_data$total_pop) / ndists_new
cat("  Average district size change: ", 
    format(round(future_avg_pop - (current_avg_pop / ndists_old)), big.mark = ","), 
    " people per district\n")

pop_cv <- sd(optimal_plan_data$total_pop) / mean(optimal_plan_data$total_pop)
cat("  Population distribution (CV): ", round(pop_cv, 3), "\n")

competitive_threshold <- 0.55
competitive_districts <- sum(optimal_plan_data$ruling_share > 0.45 & 
                           optimal_plan_data$ruling_share < competitive_threshold)
cat("  Competitive districts (45-55%): ", competitive_districts, "/", ndists_new, "\n")

# 9. Save Key Plots
cat("\n=== 9. SAVING PLOTS ===\n")
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

ggsave(here(paste0("data-out/partisan-analysis/", pref_code, "_", pref_name, "_", year, "_ldp_komei_scatter.png")), 
       plot = p_scatter1, width = 12, height = 8, dpi = 300)
cat("Saved: ldp_komei_scatter.png\n")

# 10. Export Summary Statistics
cat("\n=== 10. SUMMARY EXPORT ===\n")

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
cat("District configuration:", ndists_old, "→", ndists_new, "districts\n")
cat("Optimal plan 1票の格差:", round(max(optimal_plan_data$total_pop)/min(optimal_plan_data$total_pop), 3), "\n")
cat("All plots and summaries saved to data-out/partisan-analysis/\n")

cat("\n=== RECOMMENDATIONS FOR FUTURE ANALYSIS ===\n")
cat("1. Monitor population projections for accuracy\n")
cat("2. Consider impact of district increase on representation\n")
cat("3. Evaluate 広域連携 boundary respect in final plans\n")
cat("4. Assess competitive balance across increased district count\n")
cat("5. Review Fukuoka City municipality splitting patterns\n")

cat("\nPartisan analysis completed successfully!\n")