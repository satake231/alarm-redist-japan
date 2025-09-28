###############################################################################
# Partisan Analysis for `09_tochigi_future`
# © ALARM Project, April 2023
###############################################################################

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

cat("Starting partisan analysis for", year, "projection...\n")
cat("Number of plans:", length(unique(sim_smc_pref_sample$draw)), "\n")
cat("Number of districts:", ndists_new, "\n")

# Load required libraries
library(ggplot2)
library(redist)

# Find optimal plan for highlighting
# First, we need to load the results to find the optimal plan
# Load from the saved results if available, or calculate here
if(exists("results_sample")) {
  optimal_draw <- results_sample$draw[which(results_sample$max_to_min == min(results_sample$max_to_min))][1]
} else {
  # Calculate max_to_min ratio for all plans
  temp_results <- sim_smc_pref_sample %>%
    group_by(draw) %>%
    summarise(max_to_min = max(total_pop)/min(total_pop), .groups = 'drop')
  optimal_draw <- temp_results$draw[which(temp_results$max_to_min == min(temp_results$max_to_min))][1]
}

cat("Optimal plan (minimum population deviation): draw", optimal_draw, "\n")

# Create optimal plan data for highlighting
optimal_plan_data <- sim_smc_pref_sample %>%
  filter(draw == optimal_draw) %>%
  mutate(draw = as.factor(draw))

# Population Deviation
cat("Creating population deviation plot...\n")
p_dev <- redist.plot.hist(sim_smc_pref_sample, qty = plan_dev, bins = 10) +
  labs(x = "Population Deviation", y = "Percentage of Plans") +
  theme_bw() +
  ggtitle(paste0("Population Deviation - ", year, " Projection"))
print(p_dev)

# Compactness
cat("Creating compactness plot...\n")
p_comp <- redist.plot.hist(sim_smc_pref_sample, qty = comp_edge, bins = 10) +
  labs(x = "Fraction of Edges Kept", y = "Percentage of Plans") +
  theme_bw() +
  ggtitle(paste0("Compactness (Fraction of Edges Kept) - ", year, " Projection"))
print(p_comp)

# Election results by district:
# Ruling coalition vote share
cat("Creating ruling coalition vote share plots...\n")
p_ruling_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      color_thresh = 0.5) +
  ggtitle(paste0("Ruling Coalition Vote Share - ", year, " Projection"))
print(p_ruling_jitter)

# Boxplot with optimal plan highlighted
p_ruling_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      geom = "boxplot") +
  # Add optimal plan as large points
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_share), 
            color = "red", size = 3, shape = 15) +
  ggtitle(paste0("Ruling Coalition Vote Share Distribution - ", year, " Projection")) +
  labs(caption = paste0("Red squares show optimal plan (draw ", optimal_draw, ")"))
print(p_ruling_box)

# LDP vote share vs Komei vote share
# Scatter Plot
cat("Creating LDP vs Komei scatter plot...\n")
p_scatter1 <- sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_share)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share) +
  ggtitle(paste0("LDP vs Komei Vote Share by District - ", year, " Projection"))
print(p_scatter1)

# Dot-plots by Ordered Districts
if(exists("redist.plot.distr.custom.color")) {
  cat("Creating custom color dot plot...\n")
  p_custom1 <- redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_share,
                                color_var = ldp_v_komei) +
    scale_colour_gradient(low = "#f55881", high = "#3CA324") +
    ggtitle(paste0("Ruling Share by District (LDP vs Komei gradient) - ", year, " Projection"))
  print(p_custom1)
}

# Election results by district:
# Ruling coalition vs opposition coalition that excludes the DPP
cat("Creating ruling vs opposition (excl. DPP) plots...\n")
p_opp4_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                      color_thresh = 0.5) +
  ggtitle(paste0("Ruling vs Opposition (excl. DPP) - ", year, " Projection"))
print(p_opp4_jitter)

# Boxplot for opposition analysis with optimal plan
p_opp4_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                      geom = "boxplot") +
  # Add optimal plan as large points
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_v_opp_coalition_4), 
            color = "red", size = 3, shape = 15) +
  ggtitle(paste0("Ruling vs Opposition (excl. DPP) Distribution - ", year, " Projection")) +
  labs(caption = paste0("Red squares show optimal plan (draw ", optimal_draw, ")"))
print(p_opp4_box)

# LDP vote share vs Komei vote share for opposition analysis
p_scatter2 <- sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_v_opp_coalition_4)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share) +
  ggtitle(paste0("LDP vs Komei by Opposition Contest District - ", year, " Projection"))
print(p_scatter2)

# Dot-plots by Ordered Districts for opposition analysis
if(exists("redist.plot.distr.custom.color")) {
  p_custom2 <- redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                                color_var = ldp_v_komei) +
    scale_colour_gradient(low = "#f55881", high = "#3CA324") +
    ggtitle(paste0("Ruling vs Opposition (excl. DPP) by District - ", year, " Projection"))
  print(p_custom2)
}

# Election results by district:
# Ruling coalition vs all major opposition parties
cat("Creating ruling vs all opposition plots...\n")
p_opp_all_jitter <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                      color_thresh = 0.5) +
  ggtitle(paste0("Ruling vs All Opposition Parties - ", year, " Projection"))
print(p_opp_all_jitter)

# Boxplot for all opposition analysis with optimal plan
p_opp_all_box <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                      geom = "boxplot") +
  # Add optimal plan as large points
  geom_point(data = optimal_plan_data, 
            aes(x = district, y = ruling_v_opp_all), 
            color = "red", size = 3, shape = 15) +
  ggtitle(paste0("Ruling vs All Opposition Distribution - ", year, " Projection")) +
  labs(caption = paste0("Red squares show optimal plan (draw ", optimal_draw, ")"))
print(p_opp_all_box)

# LDP vote share vs Komei vote share for all opposition analysis
p_scatter3 <- sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_v_opp_all)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share) +
  ggtitle(paste0("LDP vs Komei by All Opposition District - ", year, " Projection"))
print(p_scatter3)

# Dot-plots by Ordered Districts for all opposition analysis
if(exists("redist.plot.distr.custom.color")) {
  p_custom3 <- redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_v_opp_all,
                                color_var = ldp_v_komei) +
    scale_colour_gradient(low = "#f55881", high = "#3CA324") +
    ggtitle(paste0("Ruling vs All Opposition by District - ", year, " Projection"))
  print(p_custom3)
}

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Population deviation range:", 
    round(min(sim_smc_pref_sample$plan_dev), 3), "to", 
    round(max(sim_smc_pref_sample$plan_dev), 3), "\n")
cat("Compactness range:", 
    round(min(sim_smc_pref_sample$comp_edge), 3), "to", 
    round(max(sim_smc_pref_sample$comp_edge), 3), "\n")
cat("Ruling coalition vote share range:", 
    round(min(sim_smc_pref_sample$ruling_share), 3), "to", 
    round(max(sim_smc_pref_sample$ruling_share), 3), "\n")

# Save key plots
cat("Saving key plots...\n")
dir.create(here("data-out/partisan-analysis"), recursive = TRUE, showWarnings = FALSE)

ggsave(here(paste0("data-out/partisan-analysis/", pref_code, "_", pref_name, "_", year, "_population_deviation.png")), 
      plot = p_dev, width = 8, height = 6)
ggsave(here(paste0("data-out/partisan-analysis/", pref_code, "_", pref_name, "_", year, "_compactness.png")), 
      plot = p_comp, width = 8, height = 6)
ggsave(here(paste0("data-out/partisan-analysis/", pref_code, "_", pref_name, "_", year, "_ruling_coalition.png")), 
      plot = p_ruling_box, width = 10, height = 6)

cat("Partisan analysis completed successfully!\n")