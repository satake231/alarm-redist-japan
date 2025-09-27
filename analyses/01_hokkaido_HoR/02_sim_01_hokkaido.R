###############################################################################
# Simulations for `01_hokkaido`
# Modified for House of Representatives (衆議院) data
# © ALARM Project, December 2024
###############################################################################

# Note: This file requires minimal changes from the original
# The pref_mun object structure remains the same whether using HoC or HoR data
# The main difference is in the vote data (nv_* columns) which come from HoR instead of HoC

####-------------- 2. Method for Urban Prefectures-------------------------####
# Re-order and add 郡 codes
pref <- pref_mun %>%
  arrange(code, sub_code) %>%
  # In Hokkaido, the redistricting committee uses the 振興局 grouping as an administrative boundary.
  # Thus, We treat this 振興局 as `gun` (county) in the program,
  # because 振興局 is larger unit than 郡.
  dplyr::mutate(gun_code = case_when(
    code %in% c(01209,
                01210,
                01215,
                01216,
                01218,
                01222,
                01225,
                01226,
                01227,
                01228,
                01423,
                01424,
                01425,
                01427,
                01428,
                01429,
                01430,
                01431,
                01432,
                01433,
                01434,
                01436,
                01437,
                01438) ~ "sorachi",
    code %in% c(01101,
                01102,
                01103,
                01104,
                01105,
                01106,
                01107,
                01108,
                01109,
                01110,
                01217,
                01224,
                01231,
                01234,
                01235,
                01303,
                01304) ~ "ishikari",
    code %in% c(01203,
                01391,
                01392,
                01393,
                01394,
                01395,
                01396,
                01397,
                01398,
                01399,
                01400,
                01401,
                01402,
                01403,
                01404,
                01405,
                01406,
                01407,
                01408,
                01409) ~ "shiribeshi",
    code %in% c(01205,
                01213,
                01230,
                01233,
                01571,
                01575,
                01578,
                01581,
                01584,
                01585,
                01586) ~ "iburi",
    code %in% c(01601,
                01602,
                01604,
                01607,
                01608,
                01609,
                01610) ~ "hidaka",
    code %in% c(01202,
                01236,
                01331,
                01332,
                01333,
                01334,
                01337,
                01343,
                01345,
                01346,
                01347) ~ "oshima",
    code %in% c(01361,
                01362,
                01363,
                01364,
                01367,
                01370,
                01371) ~ "hiyama",
    code %in% c(01204,
                01220,
                01221,
                01229,
                01452,
                01453,
                01454,
                01455,
                01456,
                01457,
                01458,
                01459,
                01460,
                01461,
                01462,
                01463,
                01464,
                01465,
                01468,
                01469,
                01470,
                01471,
                01472) ~ "kamikawa",
    code %in% c(01212,
                01481,
                01482,
                01483,
                01484,
                01485,
                01486,
                01487) ~ "rumoi",
    code %in% c(01214,
                01511,
                01512,
                01513,
                01514,
                01516,
                01517,
                01518,
                01519,
                01520) ~ "soya",
    code %in% c(01208,
                01211,
                01219,
                01543,
                01544,
                01545,
                01546,
                01547,
                01549,
                01550,
                01552,
                01555,
                01559,
                01560,
                01561,
                01562,
                01563,
                01564) ~ "okhotsk",
    code %in% c(01207,
                01631,
                01632,
                01633,
                01634,
                01635,
                01636,
                01637,
                01638,
                01639,
                01641,
                01642,
                01643,
                01644,
                01645,
                01646,
                01647,
                01648,
                01649) ~ "tokachi",
    code %in% c(01206,
                01661,
                01662,
                01663,
                01664,
                01665,
                01667,
                01668) ~ "kushiro",
    code %in% c(01223,
                01691,
                01692,
                01693,
                01694) ~ "nemuro"))

# The rest of the file remains exactly the same as the original
# All simulation code continues unchanged...

# Make adjacency list
prefadj <- redist::redist.adjacency(pref)

# Repair adjacencies (same as original)
pref_add_edge <-
  matrix(c(
    # 檜山振興局奥尻町-せたな町
    which(pref$code == 1367),
    which(pref$code == 1371),

    # 宗谷振興局礼文町-利尻町
    which(pref$code == 1517),
    which(pref$code == 1518),

    # 宗谷振興局礼文町-利尻富士町
    which(pref$code == 1517),
    which(pref$code == 1519),

    # 宗谷振興局礼文町-稚内市
    which(pref$code == 1517),
    which(pref$code == 1214),

    # 稚内市-利尻富士町
    which(pref$code == 1214),
    which(pref$code == 1519)

  ), ncol = 2, byrow = TRUE)

# Add edges
prefadj <- geomander::add_edge(prefadj,
                              pref_add_edge[,1],
                              pref_add_edge[,2])

# Create redist.map object
pref_map <- redist::redist_map(pref,
                              ndists = ndists_new,
                              pop_tol= pop_tol,
                              total_pop = pop,
                              adj = prefadj,
                              planarize = 4612)

# Merge gun (continues exactly as original...)
pref_map_merged <- pref_map %>%
  mutate(code = as.character(code),
        sub_code = as.character(sub_code),
        gun_code = as.character(gun_code)) %>%
  mutate(freeze_code = if_else(gun_code %in% gun_exception,
                              code,
                              gun_code)) %>%
  mutate(freeze_code = if_else(freeze_code %in% split_code_lh_2022,
                              str_c(code, sub_code),
                              freeze_code)) %>%
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  select(-freeze_code)

# Add constraints
constr_pref = redist::redist_constr(pref_map_merged)
constr_pref = redist::add_constr_splits(constr_pref, strength = 1, admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref, strength = 1, admin = pref_map_merged$code)

# Run simulation (exactly the same)
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 4L,
  counties = pref_map_merged$code,
  constraints = constr_pref,
  pop_temper = 0.05)

# Check convergence
summary(sim_smc_pref)
hist(plans_diversity(sim_smc_pref))

# Pull back plans
sim_smc_pref_pullback <- pullback(sim_smc_pref)

# Add reference plan (same structure, just different filename for HoR)
pref %>%
  as.data.frame() %>%
  select("code",
        "gun_code",
        "pop",
        "mun_name",
        "sub_name") %>%
  write_excel_csv(here(paste("temp/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_lh_2022_hor.csv",  # Added _hor suffix
                            sep = "")))

# Read back the CSV
dist_lh_2022 <- read_csv(here(paste("data-raw/lh_2022/",
                                    as.character(pref_code),
                                    "_",
                                    as.character(pref_name),
                                    "_lh_2022.csv",  # Keep original for now
                                    sep = "")))

# Add reference plan
pref_map$lh_2022 <- dist_lh_2022$lh_2022
sim_smc_pref_ref <- add_reference(plans = sim_smc_pref_pullback,
                                  ref_plan = as.numeric(dist_lh_2022$lh_2022),
                                  name = "lh_2022")

# Add total_pop
for(i in 1:ndists_new){
  sim_smc_pref_ref$total_pop[which(sim_smc_pref_ref$draw == "lh_2022" &
                                    sim_smc_pref_ref$district == i)] <-
    sum(dist_lh_2022$pop[which(dist_lh_2022$lh_2022 == i)])
}

# Add precinct population
attr(sim_smc_pref_ref, "prec_pop") <- pref_map$pop

# Save objects with HoR suffix to distinguish from HoC version
saveRDS(pref, here(paste("data-out/shapefile/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_hor.Rds",  # Added _hor suffix
                        sep = "")))

saveRDS(prefadj, here(paste("data-out/adj/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_adj_hor.Rds",  # Added _hor suffix
                            sep = "")))

# pref_map object
write_rds(pref_map, here(paste("data-out/map/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_lh_2022_map_hor.rds",  # Added _hor suffix
                              sep = "")),
          compress = "xz")

saveRDS(sim_smc_pref_ref, here(paste("data-out/smc-out/",
                                    as.character(pref_code),
                                    "_",
                                    as.character(pref_name),
                                    "_",
                                    as.character(sim_type),
                                    "_",
                                    as.character(nsims * 4),
                                    "_hor.Rds",  # Added _hor suffix
                                    sep = "")))