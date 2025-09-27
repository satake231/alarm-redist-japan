###############################################################################
# Partisan Analysis for `01_hokkaido`
# Modified for House of Representatives (衆議院) data
# © ALARM Project, December 2024
###############################################################################

# Load data - Note: Using HoR suffixed files
pref_map <- readRDS(here(paste("data-out/map/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_lh_2022_map_hor.rds",  # HoR version
                              sep = "")))

sim_smc_pref_sample <- readRDS(here(paste("data-out/plans/",
                                          as.character(pref_code),
                                          "_",
                                          as.character(pref_name),
                                          "_lh_2022_plans_hor.rds",  # HoR version
                                          sep = "")))

# Population Deviation
redist.plot.hist(sim_smc_pref_sample, qty = plan_dev, bins = 10) +
  labs(x = "Population Deviation", 
      y = "Percentage of Plans",
      title = "Population Deviation Distribution (HoR Data)") +
  theme_bw()

# Compactness
redist.plot.hist(sim_smc_pref_sample, qty = comp_edge, bins = 10) +
  labs(x = "Fraction of Edges Kept", 
      y = "Percentage of Plans",
      title = "District Compactness (HoR Data)") +
  theme_bw()

# Election results by district:
# Ruling coalition vote share (LDP + Komei)
# Note: With HoR data, the coalition dynamics might be different from HoC
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      color_thresh = 0.5) +
  labs(title = "Ruling Coalition Vote Share by District (HoR 2024)")

# Boxplot
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                      geom = "boxplot") +
  labs(title = "Ruling Coalition Vote Share Distribution (HoR 2024)")

# LDP vote share vs Komei vote share
# Scatter Plot
sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_share)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share) +
  labs(title = "LDP vs Komei Vote Share by District (HoR 2024)",
      x = "LDP Vote Share",
      y = "Komei Vote Share")

# Dot-plots by Ordered Districts
redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_share,
                              color_var = ldp_v_komei) +
  scale_colour_gradient(low = "#f55881", high = "#3CA324") +
  labs(title = "Ruling Coalition Performance by District (HoR 2024)")

# Election results by district:
# Ruling coalition vs opposition coalition that excludes the DPP
# Note: Coalition composition may differ between HoC and HoR elections
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                      color_thresh = 0.5) +
  labs(title = "Ruling vs Opposition Coalition (excl. DPP) - HoR 2024")

# Boxplot
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                      geom = "boxplot") +
  labs(title = "Coalition Balance Distribution (HoR 2024)")

# LDP vote share vs Komei vote share (by coalition performance)
sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_v_opp_coalition_4)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share) +
  labs(title = "Coalition Components by District Order (HoR 2024)")

# Dot-plots by Ordered Districts
redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                              color_var = ldp_v_komei) +
  scale_colour_gradient(low = "#f55881", high = "#3CA324") +
  labs(title = "Coalition Performance Pattern (HoR 2024)")

# Election results by district:
# Ruling coalition vs all major opposition parties
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                      color_thresh = 0.5) +
  labs(title = "Ruling vs All Opposition Parties (HoR 2024)")

# Boxplot
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                      geom = "boxplot") +
  labs(title = "Total Opposition Challenge (HoR 2024)")

# LDP vote share vs Komei vote share (final analysis)
sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_v_opp_all)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share) +
  labs(title = "Coalition Structure by Total Opposition (HoR 2024)")

# Dot-plots by Ordered Districts
redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_v_opp_all,
                              color_var = ldp_v_komei) +
  scale_colour_gradient(low = "#f55881", high = "#3CA324") +
  labs(title = "Final Coalition Analysis (HoR 2024)")

# Additional HoR-specific analysis
# Since HoR has different party dynamics, we might want to look at specific parties
# that are unique or more prominent in HoR elections

# Check if new parties exist in the data
if ("nv_hoshu" %in% names(pref_map)) {
  # Japan Conservative Party analysis (unique to 2024 HoR)
  cat("\n=== Japan Conservative Party (日本保守党) Analysis ===\n")
  hoshu_share <- sim_smc_pref_sample %>%
    group_by(district, draw) %>%
    summarise(hoshu_vote_share = sum(pref_map$nv_hoshu[.data$precinct]) / 
                                sum(pref_map$nv_hoshu[.data$precinct] + 
                                    pref_map$nv_ldp[.data$precinct] +
                                    pref_map$nv_cdp[.data$precinct] +
                                    pref_map$nv_komei[.data$precinct] +
                                    pref_map$nv_ishin[.data$precinct]),
              .groups = "drop")
  
  print(summary(hoshu_share$hoshu_vote_share))
}

# Summary statistics comparison
cat("\n=== Party Vote Share Summary (HoR 2024) ===\n")
party_summary <- sim_smc_pref_sample %>%
  filter(draw == "lh_2022") %>%
  group_by(district) %>%
  summarise(
    LDP = mean(ldp_share),
    Komei = mean(komei_share),
    CDP = mean(cdp_share),
    Ishin = mean(ishin_share),
    JCP = mean(jcp_share),
    .groups = "drop"
  )
print(party_summary)

# Save analysis plots
ggsave(here(paste("figures/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_partisan_analysis_hor.pdf",
                  sep = "")),
      width = 12, height = 8)