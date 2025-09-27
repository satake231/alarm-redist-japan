# 必要なパッケージ
library(dplyr)
library(tidyr)

#' 衆議院比例区の選挙データを整形する（2017年、2021年両対応 最終版）
#'
#' @param raw_df readxl::read_excel()などで読み込んだ生のデータフレーム
#'
#' @return 整形済みのtibble（municipality, party, votes）
#'
clean_pref_HoR_PR_bulletproof <- function(raw_df) {
  
  # 1. ヘッダー行を動的に特定
  header_row_index <- which(apply(raw_df, 1, function(row) any(grepl("市区町村", row))))[1]
  
  if (is.na(header_row_index)) {
    stop("ヘッダー行（'市区町村'を含む行）が見つかりません。")
  }
  
  # 2. ヘッダー行から列名の元になるベクトルを抽出
  header_vector <- as.character(raw_df[header_row_index, ])
  
  # 3. 実際に名前がある「有効な列」のインデックスだけを特定する
  #    これにより、名前のない空の列（NA）が完全に無視される
  valid_col_indices <- which(!is.na(header_vector) & header_vector != "")
  
  # 4. 「有効な列」の中から、合計列（"計"）を除外する
  valid_names <- header_vector[valid_col_indices]
  total_col_indices <- which(grepl("計", valid_names))
  
  # 合計列を除いた、本当に必要な列のインデックス（有効な列の中での位置）を取得
  cols_to_keep_indices <- setdiff(1:length(valid_names), total_col_indices)
  
  # 5. 元データ（raw_df）から、本当に必要な列のインデックスを取得
  final_original_indices <- valid_col_indices[cols_to_keep_indices]
  final_names <- valid_names[cols_to_keep_indices]
  
  # 6. 必要な列のデータだけを安全に抽出し、クリーンな名前を付ける
  df_processed <- raw_df %>%
    slice((header_row_index + 1):n()) %>%
    select(all_of(final_original_indices))
  
  colnames(df_processed) <- final_names
  
  # 7. 確実にクリーンになったデータで、残りの整形処理を実行
  df_tidy <- df_processed %>%
    rename(municipality = 1) %>%
    pivot_longer(
      cols = -municipality,
      names_to = "party",
      values_to = "votes"
    ) %>%
    filter(
      !is.na(votes) & votes != "",
      !is.na(municipality),
      !grepl("計", municipality)
    ) %>%
    mutate(votes = as.numeric(votes))
  
  return(df_tidy)
}

# 従来の関数名で呼び出せるようにエイリアス（別名）を設定
clean_pref_2017_HoR_PR <- clean_pref_HoR_PR_bulletproof
clean_pref_2021_HoR_PR <- clean_pref_HoR_PR_bulletproof