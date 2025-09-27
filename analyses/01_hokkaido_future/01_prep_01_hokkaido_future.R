###############################################################################
# Download and prepare data for `01_hokkaido` analysis (Future Projection)
# © ALARM Project, May 2023
###############################################################################

# Set up packages
library(redist)
library(geomander)
library(sf)
library(tidyverse)
library(here)

# Pull functions
setwd(here("function"))
files.sources <- list.files()
sapply(files.sources, source)
rm(files.sources)
setwd(here())

# (シミュレーションパラメータの定義は変更なし)
# ... (sim_type, nsims, pref_code, etc.)

# --- データダウンロード ---

# Clean Census shapefile (変更なし)
pref_shp_2020 <- download_shp(pref_code)
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf()

# Download and clean 2020 Census data (for population ratio)
pref_pop_2020 <- download_pop_2020(pref_code)
pref_pop_cleaned_2020 <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE) %>%
  rename(code = mun_code)

# Download and clean Future Population data
future_pop_raw <- download_future_pop()
#
# ★★★ ここからが将来人口データへの差し替え処理 ★★★
#
# 1. 2050年の市区町村別将来人口データを整形
future_pop_cleaned <- clean_future_pop(future_pop_raw, year = 2050)

# 2. 2020年国勢調査データから、市区町村内の小地域別人口比率を計算
pref_pop_ratio <- pref_pop_cleaned_2020 %>%
  group_by(code) %>%
  mutate(pop_ratio = pop / sum(pop, na.rm = TRUE)) %>%
  # 人口が0の場合のNaNを0に置換
  mutate(pop_ratio = if_else(is.nan(pop_ratio), 0, pop_ratio)) %>%
  select(code, sub_code, pop_ratio)

# 3. 地図データ(pref_shp_cleaned)に、小地域ごとの人口比率を結合し、
#    さらに市区町村ごとの将来人口を結合して、小地域ごとの将来人口を推計
pref_join_future <- pref_shp_cleaned %>%
  dplyr::mutate(sub_code = as.numeric(KIHON1)) %>%
  dplyr::left_join(pref_pop_ratio, by = c("code", "sub_code")) %>%
  dplyr::left_join(future_pop_cleaned, by = c("code", "mun_name")) %>%
  # 将来人口(pop.y)を2020年の人口比率(pop_ratio)で按分
  dplyr::mutate(pop = round(pop.y * pop_ratio)) %>%
  # 必要な列を選択し、列名を整理
  dplyr::select(code, mun_name, sub_code, sub_name, pop, geometry)

# ★★★ ここまでが将来人口データへの差し替え処理 ★★★

# Download election data (変更なし)
pref_2019_HoC_PR <- download_2019_HoC_PR(pref_code)
pref_2022_HoC_PR <- download_2022_HoC_PR(pref_code)

# Clean election data and estimate baseline votes (変更なし)
pref_2019_HoC_PR_cleaned <- clean_pref_2019_HoC_PR(pref_2019_HoC_PR)
pref_2022_HoC_PR_cleaned <- clean_pref_2022_HoC_PR(pref_2022_HoC_PR)
pref_HoC_PR <- clean_pref_HoC_PR(pref_2019_HoC_PR_cleaned, pref_2022_HoC_PR_cleaned)

# --- データマージ ---
# 按分した将来人口データ(pref_join_future)を使い、 partisan data を結合する
# `pref_join` の代わりに `pref_join_future` を使用
pref_mun <- dplyr::bind_rows(
  # Municipalities without splits
  pref_join_future %>% # ★変更
    dplyr::filter(code %in% c(split_code, split_code_lh_2022) == FALSE) %>%
    dplyr::group_by(code, mun_name) %>%
    dplyr::summarise(sub_code = first(sub_code),
                    sub_name = "-",
                    pop = sum(pop, na.rm = TRUE), # ★変更
                    geometry = sf::st_union(geometry)) %>%
    dplyr::left_join(pref_HoC_PR, by = "mun_name"),
  # Municipalities with splits
  pref_join_future %>% # ★変更
    dplyr::filter(code %in% c(split_code, split_code_lh_2022)) %>%
    dplyr::group_by(code, mun_name) %>% # ★mun_nameでグループ化
    dplyr::mutate(pop_ratio_partisan = pop / sum(pop, na.rm = TRUE)) %>%
    # 人口が0の場合のNaNを0に置換
    mutate(pop_ratio_partisan = if_else(is.nan(pop_ratio_partisan), 0, pop_ratio_partisan)) %>%
    dplyr::left_join(pref_HoC_PR, by = "mun_name") %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("nv"), ~ .x * pop_ratio_partisan)) %>%
    dplyr::select(-pop_ratio_partisan)
)

# Confirm that the population figure matches the total of the future projection
# 念のため、合計人口が将来推計人口の合計と一致するか確認
sum(pref_mun$pop, na.rm = TRUE)
sum(future_pop_cleaned[future_pop_cleaned$code >= pref_code*1000 & future_pop_cleaned$code < (pref_code+1)*1000, ]$pop, na.rm = TRUE)
sum(pref_mun$nv_ldp, na.rm = TRUE)
sum(pref_HoC_PR$nv_ldp, na.rm = TRUE)