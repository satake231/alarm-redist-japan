###############################################################################
# Simulations for `01_hokkaido_future`
# © ALARM Project, May 2023
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####

cat("=== STARTING HOKKAIDO FUTURE SIMULATION ===\n")
cat("Future projection year:", year, "\n")
cat("Target districts:", ndists_new, "(reduced from", ndists_old, ")\n")
cat("Population projection:", paste0("pop_", year), "\n\n")

# Re-order and add 郡 codes (振興局 codes for Hokkaido)
pref <- pref_mun %>%
  arrange(code, sub_code) %>%
  # In Hokkaido, the redistricting committee uses the 振興局 grouping as an administrative boundary.
  # Thus, We treat this 振興局 as `gun` (county) in the program,
  # because 振興局 is larger unit than 郡.
  dplyr::mutate(gun_code = case_when(
    # 空知総合振興局
    code %in% c(01209,  # 岩見沢市
                01210,  # 美唄市
                01215,  # 芦別市
                01216,  # 赤平市
                01218,  # 三笠市
                01222,  # 滝川市
                01225,  # 砂川市
                01226,  # 歌志内市
                01227,  # 深川市
                01228,  # 南幌町
                01423,  # 奈井江町
                01424,  # 上砂川町
                01425,  # 由仁町
                01427,  # 長沼町
                01428,  # 栗山町
                01429,  # 月形町
                01430,  # 浦臼町
                01431,  # 新十津川町
                01432,  # 妹背牛町
                01433,  # 秩父別町
                01434,  # 雨竜町
                01436,  # 北竜町
                01437,  # 沼田町
                01438) ~ "sorachi",
    # 石狩振興局
    code %in% c(01101,  # 札幌市中央区
                01102,  # 札幌市北区
                01103,  # 札幌市東区
                01104,  # 札幌市白石区
                01105,  # 札幌市豊平区
                01106,  # 札幌市南区
                01107,  # 札幌市西区
                01108,  # 札幌市厚別区
                01109,  # 札幌市手稲区
                01110,  # 札幌市清田区
                01217,  # 江別市
                01224,  # 千歳市
                01231,  # 恵庭市
                01234,  # 北広島市
                01235,  # 石狩市
                01303,  # 当別町
                01304) ~ "ishikari",
    # 後志総合振興局
    code %in% c(01203,  # 小樽市
                01391,  # 島牧村
                01392,  # 寿都町
                01393,  # 黒松内町
                01394,  # 蘭越町
                01395,  # ニセコ町
                01396,  # 真狩村
                01397,  # 留寿都村
                01398,  # 喜茂別町
                01399,  # 京極町
                01400,  # 倶知安町
                01401,  # 共和町
                01402,  # 岩内町
                01403,  # 泊村
                01404,  # 神恵内村
                01405,  # 積丹町
                01406,  # 古平町
                01407,  # 仁木町
                01408,  # 余市町
                01409) ~ "shiribeshi",
    # 胆振総合振興局
    code %in% c(01205,  # 室蘭市
                01213,  # 苫小牧市
                01230,  # 登別市
                01233,  # 伊達市
                01571,  # 豊浦町
                01575,  # 壮瞥町
                01578,  # 白老町
                01581,  # 厚真町
                01584,  # 洞爺湖町
                01585,  # 安平町
                01586) ~ "iburi",
    # 日高振興局
    code %in% c(01601,  # 日高町
                01602,  # 平取町
                01604,  # 新冠町
                01607,  # 浦河町
                01608,  # 様似町
                01609,  # えりも町
                01610) ~ "hidaka",
    # 渡島総合振興局
    code %in% c(01202,  # 函館市
                01236,  # 北斗市
                01331,  # 松前町
                01332,  # 福島町
                01333,  # 知内町
                01334,  # 木古内町
                01337,  # 七飯町
                01343,  # 鹿部町
                01345,  # 森町
                01346,  # 八雲町
                01347) ~ "oshima",
    # 檜山振興局
    code %in% c(01361,  # 江差町
                01362,  # 上ノ国町
                01363,  # 厚沢部町
                01364,  # 乙部町
                01367,  # 奥尻町
                01370,  # 今金町
                01371) ~ "hiyama",
    # 上川総合振興局
    code %in% c(01204,  # 旭川市
                01220,  # 名寄市
                01221,  # 富良野市
                01229,  # 士別市
                01452,  # 鷹栖町
                01453,  # 東神楽町
                01454,  # 当麻町
                01455,  # 比布町
                01456,  # 愛別町
                01457,  # 上川町
                01458,  # 東川町
                01459,  # 美瑛町
                01460,  # 上富良野町
                01461,  # 中富良野町
                01462,  # 南富良野町
                01463,  # 占冠村
                01464,  # 和寒町
                01465,  # 剣淵町
                01468,  # 下川町
                01469,  # 美深町
                01470,  # 音威子府村
                01471,  # 中川町
                01472) ~ "kamikawa",
    # 留萌振興局
    code %in% c(01212,  # 留萌市
                01481,  # 増毛町
                01482,  # 小平町
                01483,  # 苫前町
                01484,  # 羽幌町
                01485,  # 初山別村
                01486,  # 遠別町
                01487) ~ "rumoi",
    # 宗谷総合振興局
    code %in% c(01214,  # 稚内市
                01511,  # 猿払村
                01512,  # 浜頓別町
                01513,  # 中頓別町
                01514,  # 枝幸町
                01516,  # 豊富町
                01517,  # 礼文町
                01518,  # 利尻町
                01519,  # 利尻富士町
                01520) ~ "soya",
    # オホーツク総合振興局
    code %in% c(01208,  # 北見市
                01211,  # 網走市
                01219,  # 紋別市
                01543,  # 美幌町
                01544,  # 津別町
                01545,  # 斜里町
                01546,  # 清里町
                01547,  # 小清水町
                01549,  # 訓子府町
                01550,  # 置戸町
                01552,  # 佐呂間町
                01555,  # 遠軽町
                01559,  # 湧別町
                01560,  # 滝上町
                01561,  # 興部町
                01562,  # 西興部村
                01563,  # 雄武町
                01564) ~ "okhotsk",
    # 十勝総合振興局
    code %in% c(01207,  # 帯広市
                01631,  # 音更町
                01632,  # 士幌町
                01633,  # 上士幌町
                01634,  # 鹿追町
                01635,  # 新得町
                01636,  # 清水町
                01637,  # 芽室町
                01638,  # 中札内村
                01639,  # 更別村
                01641,  # 大樹町
                01642,  # 広尾町
                01643,  # 幕別町
                01644,  # 池田町
                01645,  # 豊頃町
                01646,  # 本別町
                01647,  # 足寄町
                01648,  # 陸別町
                01649) ~ "tokachi",
    # 釧路総合振興局
    code %in% c(01206,  # 釧路市
                01661,  # 釧路町
                01662,  # 厚岸町
                01663,  # 浜中町
                01664,  # 標茶町
                01665,  # 弟子屈町
                01667,  # 鶴居村
                01668) ~ "kushiro",
    # 根室振興局
    code %in% c(01223,  # 根室市
                01691,  # 別海町
                01692,  # 中標津町
                01693,  # 標津町
                01694) ~ "nemuro"))

# Determine which population column to use for future projection
pop_col <- paste0("pop_", year)

# Validate population column exists
if (!pop_col %in% names(pref)) {
  stop(paste("ERROR: Population column", pop_col, "not found in data"))
}

cat("Population data validation:\n")
cat("  Using column:", pop_col, "\n")
cat("  Total future population:", format(sum(pref[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("  Missing values:", sum(is.na(pref[[pop_col]])), "\n")
cat("  Average per district:", format(round(sum(pref[[pop_col]], na.rm = TRUE) / ndists_new), big.mark = ","), "\n\n")

# Make adjacency list
cat("Creating base adjacency matrix...\n")
prefadj <- redist::redist.adjacency(pref)
cat("Base adjacency created with", length(prefadj), "units\n")

# Add Hokkaido-specific ferry connections for islands
cat("Adding Hokkaido ferry connections...\n")

# Helper function to safely check municipality existence
check_municipality <- function(code) {
  indices <- which(pref$code == code)
  if(length(indices) > 0) {
    return(indices[1])  # Return first match
  } else {
    cat("Warning: Municipality", code, "not found\n")
    return(NULL)
  }
}

# Define ferry connections for Hokkaido islands
ferry_connections <- list(
  list(from = 1367, to = 1371, name = "奥尻町-せたな町"),
  list(from = 1517, to = 1518, name = "礼文町-利尻町"),
  list(from = 1517, to = 1519, name = "礼文町-利尻富士町"),
  list(from = 1517, to = 1214, name = "礼文町-稚内市"),
  list(from = 1214, to = 1519, name = "稚内市-利尻富士町")
)

# Add ferry connections
connections_added <- 0
for(connection in ferry_connections) {
  idx_from <- check_municipality(connection$from)
  idx_to <- check_municipality(connection$to)
  
  if(!is.null(idx_from) && !is.null(idx_to)) {
    prefadj <- geomander::add_edge(prefadj, idx_from, idx_to)
    cat("  Added:", connection$name, "\n")
    connections_added <- connections_added + 1
  }
}

cat("Ferry connections added:", connections_added, "/", length(ferry_connections), "\n\n")

# Create redist.map object using future population
cat("Creating redistricting map object...\n")
pref_map <- redist::redist_map(pref,
                              ndists = ndists_new,
                              pop_tol = pop_tol,
                              total_pop = !!sym(pop_col),
                              adj = prefadj,
                              planarize = 4612)

cat("Redistricting map created:\n")
cat("  Units:", nrow(pref_map), "\n")
cat("  Districts:", ndists_new, "\n")
cat("  Population tolerance:", pop_tol * 100, "%\n\n")

# Merge gun (振興局) units
cat("Merging 振興局 (gun) units...\n")
pref_map_merged <- pref_map %>%
  # Convert codes to character for consistent handling
  mutate(code = as.character(code),
         sub_code = as.character(sub_code),
         gun_code = as.character(gun_code)) %>%
  # Determine which units to freeze together
  # If a gun is in `gun_exception`, don't freeze it (allow splits)
  mutate(freeze_code = if_else(gun_code %in% gun_exception,
                              code,  # Keep individual municipalities
                              gun_code)) %>%  # Group by 振興局
  # For municipalities that can be split (札幌市), allow sub-unit level splits
  mutate(freeze_code = if_else(freeze_code %in% as.character(split_code_lh_2022),
                              str_c(code, sub_code),  # Keep sub-units separate
                              freeze_code)) %>%
  # Group and merge by the determined freeze code
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Clean up temporary column
  select(-freeze_code)

cat("Unit merging completed:\n")
cat("  Original units:", nrow(pref_map), "\n")
cat("  Merged units:", nrow(pref_map_merged), "\n")
cat("  Splittable 振興局:", paste(gun_exception, collapse = ", "), "\n")
cat("  Splittable municipalities:", paste(split_code_lh_2022, collapse = ", "), "\n\n")

# Set up redistricting constraints
cat("Setting up redistricting constraints...\n")
constr_pref <- redist::redist_constr(pref_map_merged)
constr_pref <- redist::add_constr_splits(constr_pref, 
                                        strength = 1, 
                                        admin = pref_map_merged$code)
constr_pref <- redist::add_constr_multisplits(constr_pref, 
                                             strength = 1, 
                                             admin = pref_map_merged$code)

cat("Constraints added:\n")
cat("  Split penalty: strength = 1\n")
cat("  Multi-split penalty: strength = 1\n\n")

# Run Sequential Monte Carlo simulation
cat("=== STARTING SMC SIMULATION ===\n")
cat("Configuration:\n")
cat("  Samples per run:", nsims, "\n")
cat("  Number of runs: 4\n")
cat("  Total samples:", nsims * 4, "\n")
cat("  Population temperance: 0.07\n")
cat("  This may take 30-60 minutes for future projections...\n\n")

set.seed(2020)
start_time <- Sys.time()

sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 4L,
  counties = pref_map_merged$code,
  constraints = constr_pref,
  pop_temper = 0.07)

end_time <- Sys.time()
cat("SMC simulation completed in:", round(as.numeric(end_time - start_time), 1), "minutes\n\n")

# Check simulation results
cat("=== SIMULATION DIAGNOSTICS ===\n")
summary(sim_smc_pref)

# Check plan diversity
cat("\nPlan diversity analysis:\n")
diversity_scores <- plans_diversity(sim_smc_pref)
cat("  Diversity mean:", round(mean(diversity_scores), 3), "\n")
cat("  Diversity median:", round(median(diversity_scores), 3), "\n")
cat("  Diversity range:", round(min(diversity_scores), 3), "-", round(max(diversity_scores), 3), "\n")

# Create diversity histogram
png(here(paste0("temp/diversity_", year, ".png")), width = 800, height = 600)
hist(diversity_scores, main = paste("Plan Diversity -", year, "Projection"), 
     xlab = "Diversity Score", breaks = 30)
dev.off()
cat("  Diversity histogram saved to temp/\n\n")

# Pull back plans to unmerged units
cat("Pulling back plans to original units...\n")
sim_smc_pref_pullback <- pullback(sim_smc_pref)
cat("Pullback completed\n\n")

# Handle reference plan (only if district count unchanged)
cat("=== REFERENCE PLAN HANDLING ===\n")
if (ndists_new == ndists_old) {
  cat("District count unchanged - adding reference plan\n")
  
  # Export current data for reference
  pref %>%
    as.data.frame() %>%
    select("code", "gun_code", "mun_name", "sub_name",
           pop = all_of(pop_col)) %>%
    write_excel_csv(here(paste("temp/",
                              pref_code, "_", pref_name, "_", year, "_export.csv",
                              sep = "")))
  
  # Try to read existing reference plan
  ref_file <- here(paste("data-raw/lh_2022/",
                        pref_code, "_", pref_name, "_lh_2022.csv",
                        sep = ""))
  
  if(file.exists(ref_file)) {
    dist_lh_2022 <- read_csv(ref_file, show_col_types = FALSE)
    
    # Add reference plan
    pref_map$lh_2022 <- dist_lh_2022$lh_2022
    sim_smc_pref_ref <- add_reference(plans = sim_smc_pref_pullback,
                                      ref_plan = as.numeric(dist_lh_2022$lh_2022),
                                      name = "lh_2022")
    
    # Add total_pop for reference plan
    for(i in 1:ndists_new){
      ref_pop <- sum(dist_lh_2022$pop[which(dist_lh_2022$lh_2022 == i)])
      sim_smc_pref_ref$total_pop[which(sim_smc_pref_ref$draw == "lh_2022" &
                                        sim_smc_pref_ref$district == i)] <- ref_pop
    }
    
    cat("Reference plan (lh_2022) added successfully\n")
  } else {
    cat("Reference file not found:", ref_file, "\n")
    sim_smc_pref_ref <- sim_smc_pref_pullback
  }
  
} else {
  cat("District count changed (", ndists_old, "→", ndists_new, ") - no reference plan\n")
  sim_smc_pref_ref <- sim_smc_pref_pullback
}

# Set precinct population attribute
attr(sim_smc_pref_ref, "prec_pop") <- pref_map[[pop_col]]
cat("Precinct population attribute set\n\n")

# Save simulation results
cat("=== SAVING RESULTS ===\n")

# Create output directories
output_dirs <- c("data-out/shapefile", "data-out/adj", "data-out/map", "data-out/smc-out")
for(dir in output_dirs) {
  dir.create(here(dir), recursive = TRUE, showWarnings = FALSE)
}

# Save files with year suffix
files_to_save <- list(
  list(obj = pref, 
       path = paste("data-out/shapefile/", pref_code, "_", pref_name, "_", year, ".Rds", sep = "")),
  list(obj = prefadj, 
       path = paste("data-out/adj/", pref_code, "_", pref_name, "_", year, "_adj.Rds", sep = "")),
  list(obj = pref_map, 
       path = paste("data-out/map/", pref_code, "_", pref_name, "_", year, "_lh_2022_map.rds", sep = "")),
  list(obj = sim_smc_pref_ref, 
       path = paste("data-out/smc-out/", pref_code, "_", pref_name, "_", sim_type, "_", year, "_", nsims * 4, ".Rds", sep = ""))
)

for(file_info in files_to_save) {
  if(grepl("\\.rds$", file_info$path, ignore.case = TRUE)) {
    write_rds(file_info$obj, here(file_info$path), compress = "xz")
  } else {
    saveRDS(file_info$obj, here(file_info$path))
  }
  cat("Saved:", basename(file_info$path), "\n")
}

# Final summary
cat("\n=== SIMULATION SUMMARY ===\n")
cat("Projection year:", year, "\n")
cat("Districts:", ndists_old, "→", ndists_new, "\n")
cat("Population:", format(sum(pref[[pop_col]], na.rm = TRUE), big.mark = ","), "\n")
cat("Simulated plans:", nsims * 4, "\n")
cat("Diversity mean:", round(mean(diversity_scores), 3), "\n")
cat("Processing time:", round(as.numeric(end_time - start_time), 1), "minutes\n")

# Municipality split information
cat("\nSplittable municipalities:\n")
for(code in split_code_lh_2022) {
  mun_name <- pref$mun_name[pref$code == code][1]
  if(!is.na(mun_name)) {
    cat("  ", code, ":", mun_name, "\n")
  }
}

# 振興局 information  
cat("\n振興局 (gun) exception (splittable):\n")
for(gun in gun_exception) {
  cat("  ", gun, "振興局\n")
}

cat("\nSimulation completed successfully!\n")
cat("Ready for post-processing analysis.\n")
cat("Files saved with", year, "suffix for future projection analysis.\n")