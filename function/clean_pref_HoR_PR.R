# function/clean_pref_HoR_PR.R

#' 衆議院比例区の基礎票を計算する
#'
#' @param pref_2017_HoR_PR_cleaned 整形済みの2017年衆院選データ
#' @param pref_2021_HoR_PR_cleaned 整形済みの2021年衆院選データ
#'
#' @return 横持ちに整形された基礎票データ
#'
clean_pref_HoR_PR <- function(pref_2017_HoR_PR_cleaned, pref_2021_HoR_PR_cleaned){

  # 政党名のマッピング（必要に応じて追加・修正してください）
  party_mapping <- c(
    "自由民主党" = "ldp",
    "立憲民主党" = "cdp",
    "公明党" = "komei",
    "日本共産党" = "jcp",
    "日本維新の会" = "ishin",
    "国民民主党" = "dpp",
    "社会民主党" = "sdp",
    "れいわ新選組" = "reiwa"
    # ... その他の政党
  )
  
  # 2つの選挙データを結合し、平均得票数を計算
  df_combined <- bind_rows(
    pref_2017_HoR_PR_cleaned,
    pref_2021_HoR_PR_cleaned
  ) %>%
    group_by(municipality, party) %>%
    summarise(total_votes = sum(votes, na.rm = TRUE)) %>%
    ungroup() %>%
    # 平均を計算する場合は、↑を修正
    # summarise(avg_votes = mean(votes, na.rm = TRUE)) %>%
    filter(party %in% names(party_mapping)) %>%
    mutate(party_en = recode(party, !!!party_mapping))

  # 横長のデータに変換
  df_wide <- df_combined %>%
    select(-party) %>%
    pivot_wider(
      names_from = party_en,
      values_from = total_votes,
      names_prefix = "nv_",
      values_fill = 0 # 票がない場合は0で埋める
    ) %>%
    rename(mun_name = municipality) # `01_hokkaido`のコードと列名を合わせる
  
  return(df_wide)
}