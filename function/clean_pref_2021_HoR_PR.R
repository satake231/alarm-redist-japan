# clean_pref_2021_HoR_PR.R

# 必要なパッケージ
library(readr)
library(dplyr)
library(tidyr)

#' 衆議院比例区の都道府県別データを整形する
#'
#' @param file_path 生のデータファイルへのパス（.xlsから変換したCSVを想定）
#'
#' @return 整形済みのtibble（municipality, party, votes）
#'
clean_pref_2021_HoR_PR <- function(file_path) {
  
  # 1. ヘッダーを3行スキップして読み込み
  df_raw <- read_csv(
    file_path,
    skip = 3,
    # readrが自動で列名を推測するため、列名が長すぎる場合の警告を抑制
    show_col_types = FALSE 
  )
  
  # 2. 列名の変更と不要な「得票数計」列の削除
  df_renamed <- df_raw %>%
    rename(municipality = 1) %>% # 最初の列を"municipality"に
    select(-`得票数計`)          # 合計列は不要なため削除
    
  # 3. 横長から縦長へデータを変換
  df_tidy <- df_renamed %>%
    pivot_longer(
      cols = -municipality,      # municipality列以外のすべてを対象に
      names_to = "party",        # 新しい政党列の名前
      values_to = "votes"        # 新しい得票数列の名前
    ) %>%
    filter(!is.na(votes)) # NAの行（合計行の残骸など）を除去
  
  return(df_tidy)
}