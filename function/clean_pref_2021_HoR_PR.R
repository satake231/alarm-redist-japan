# clean_pref_2021_HoR_PR.R

# 必要なパッケージ
library(dplyr)
library(tidyr)

#' 衆議院比例区の都道府県別データを整形する
#'
#' @param pref_2021_HoR_PR download_2021_HoR()で読み込んだ生のデータフレーム
#'
#' @return 整形済みのtibble（municipality, party, votes）
#'
clean_pref_2021_HoR_PR <- function(pref_2021_HoR_PR) {
  
  # 1. ヘッダーが5行目から始まるため、5行目以降を対象とする
  df_raw <- pref_2021_HoR_PR %>%
    slice(5:n()) 

  # ヘッダー行を抽出し、列名を設定
  new_headers <- as.character(df_raw[1, ])
  df_raw <- df_raw[-1, ]
  colnames(df_raw) <- new_headers

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
    filter(!is.na(votes)) %>% # NAの行（合計行の残骸など）を除去
    mutate(votes = as.numeric(votes)) # 念のため数値型に変換

  return(df_tidy)
}