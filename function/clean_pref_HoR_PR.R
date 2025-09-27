# 必要なパッケージ
library(dplyr)
library(tidyr)
library(stringr)

#' 衆議院比例代表選挙データをクリーニングする（参議院版を参考に修正）
#'
#' @param raw_df readxl::read_excel()で読み込んだ生のデータフレーム
#' @param year 選挙年（2017または2021）
#'
#' @return 整形済みのデータフレーム（横持ち形式）
#'
#' @export
#'
clean_pref_HoR_PR <- function(raw_df, year = NULL) {
  
  # デバッグ用：データの最初の部分を確認
  # print(head(raw_df, 10))
  
  # 1. ヘッダー行を動的に特定
  header_row_index <- which(apply(raw_df, 1, function(row) {
    any(grepl("市区町村|市町村", as.character(row), perl = TRUE))
  }))[1]
  
  if (is.na(header_row_index)) {
    # 別のパターンで探す（第1列に"市区町村"が含まれる場合）
    header_row_index <- which(grepl("市区町村|市町村", as.character(raw_df[[1]])))[1]
  }
  
  if (is.na(header_row_index)) {
    stop("ヘッダー行（'市区町村'を含む行）が見つかりません。データ構造を確認してください。")
  }
  
  # 2. ヘッダー行から列名を抽出
  header_vector <- as.character(unlist(raw_df[header_row_index, ]))
  
  # 3. 有効な列のインデックスを特定
  valid_col_indices <- which(!is.na(header_vector) & header_vector != "")
  valid_names <- header_vector[valid_col_indices]
  
  # デバッグ用
  # print(paste("Found columns:", paste(valid_names, collapse = ", ")))
  
  # 4. 「計」を含む列を除外（ただし政党名は保持）
  # 政党名のパターンを先に定義
  party_patterns <- c(
    "自由民主党", "自民党", "自民",
    "立憲民主党", "立憲民主", "立憲", 
    "公明党", "公明",
    "日本共産党", "共産党", "共産",
    "日本維新の会", "維新の会", "維新",
    "国民民主党", "国民民主", "国民", 
    "社会民主党", "社民党", "社民",
    "れいわ新選組", "れいわ",
    "NHK党", "NHKから国民を守る党", "N国",
    "希望の党", "希望"
  )
  
  # 除外する列を特定（政党名を含まない「計」列など）
  exclude_indices <- which(grepl("計$|合計|無効|棄権|投票率|有権者", valid_names) & 
                          !grepl(paste(party_patterns, collapse = "|"), valid_names))
  
  if(length(exclude_indices) > 0){
    keep_indices <- setdiff(1:length(valid_names), exclude_indices)
    final_col_indices <- valid_col_indices[keep_indices]
    final_names <- valid_names[keep_indices]
  } else {
    final_col_indices <- valid_col_indices
    final_names <- valid_names
  }
  
  # 5. データ部分を抽出
  df_processed <- raw_df %>%
    slice((header_row_index + 1):n()) %>%
    select(all_of(final_col_indices))
  
  # 列名を設定
  colnames(df_processed) <- final_names
  
  # デバッグ用：処理後の列数を確認
  # print(paste("Columns after processing:", ncol(df_processed)))
  
  # 6. 市区町村名の正規化（最初の列が市区町村名）
  df_processed <- df_processed %>%
    rename(mun_name = 1) %>%
    filter(!is.na(mun_name) & mun_name != "") %>%
    filter(!grepl("^計$|^合計$|^平均$", mun_name)) %>%
    mutate(mun_name = str_trim(mun_name)) %>%
    mutate(mun_name = gsub("　", "", mun_name)) %>%  # 全角スペース削除
    mutate(mun_name = gsub(" ", "", mun_name))       # 半角スペース削除
  
  # 7. 各政党の列を数値に変換
  if(ncol(df_processed) > 1) {
    df_processed <- df_processed %>%
      mutate(across(-mun_name, ~ {
        x <- as.character(.)
        x <- gsub(",", "", x)           # カンマを削除
        x <- gsub("－", "0", x)         # 全角ダッシュを0に
        x <- gsub("-", "0", x)          # 半角ハイフンを0に
        x <- gsub("^\\s*$", "0", x)     # 空白を0に
        x[is.na(x) | x == ""] <- "0"    # NAや空文字を0に
        as.numeric(x)
      }))
  } else {
    warning("政党の列が見つかりませんでした。データ構造を確認してください。")
    return(df_processed)
  }
  
  # 8. 列名を正規化（nv_政党名_年の形式に）
  if(ncol(df_processed) > 1){
    # 政党名を英語略称に変換する関数
    normalize_party_name <- function(name) {
      name_lower <- tolower(name)
      
      if(grepl("自由民主党|自民党|自民", name)) return("ldp")
      if(grepl("立憲民主党|立憲民主|立憲", name)) return("cdp")
      if(grepl("公明党|公明", name)) return("komei")
      if(grepl("日本共産党|共産党|共産", name)) return("jcp")
      if(grepl("日本維新の会|維新の会|維新", name)) return("ishin")
      if(grepl("国民民主党|国民民主|国民", name)) return("dpp")
      if(grepl("社会民主党|社民党|社民", name)) return("sdp")
      if(grepl("れいわ新選組|れいわ", name)) return("reiwa")
      if(grepl("NHK党|NHKから国民を守る党|N国", name)) return("nhk")
      if(grepl("希望の党|希望", name)) return("kibou")
      
      # その他の政党は元の名前を簡略化
      return(gsub("[^[:alnum:]]", "", name_lower))
    }
    
    # 列名を変換
    new_names <- c("mun_name")
    for(i in 2:ncol(df_processed)){
      party_name <- normalize_party_name(names(df_processed)[i])
      if(!is.null(year)){
        new_names <- c(new_names, paste0("nv_", party_name, "_", year))
      } else {
        new_names <- c(new_names, paste0("nv_", party_name))
      }
    }
    names(df_processed) <- new_names
  }
  
  # 9. 合計列を追加
  if(ncol(df_processed) > 1){
    vote_cols <- names(df_processed)[grepl("^nv_", names(df_processed))]
    if(length(vote_cols) > 0){
      if(!is.null(year)){
        df_processed[[paste0("nv_total_", year)]] <- rowSums(df_processed[vote_cols], na.rm = TRUE)
      } else {
        df_processed[["nv_total"]] <- rowSums(df_processed[vote_cols], na.rm = TRUE)
      }
    }
  }
  
  return(df_processed)
}

#' 2017年衆議院選挙データのクリーニング
#' @export
clean_pref_2017_HoR_PR <- function(raw_df) {
  clean_pref_HoR_PR(raw_df, year = 2017)
}

#' 2021年衆議院選挙データのクリーニング  
#' @export
clean_pref_2021_HoR_PR <- function(raw_df) {
  clean_pref_HoR_PR(raw_df, year = 2021)
}

#' 衆議院選挙の基礎票を計算する（参議院版clean_pref_HoC_PRと同じ形式）
#'
#' @param pref_2017_HoR_PR_cleaned 整形済みの2017年衆院選データ
#' @param pref_2021_HoR_PR_cleaned 整形済みの2021年衆院選データ
#'
#' @return 基礎票データ（clean_pref_HoC_PRと同じ形式）
#'
#' @export
#'
clean_pref_HoR_PR_combined <- function(pref_2017_HoR_PR_cleaned, pref_2021_HoR_PR_cleaned){
  
  # スペースを削除
  pref_2017_HoR_PR_cleaned$mun_name <- gsub(' ','', pref_2017_HoR_PR_cleaned$mun_name)
  pref_2017_HoR_PR_cleaned$mun_name <- gsub('　','', pref_2017_HoR_PR_cleaned$mun_name)
  pref_2021_HoR_PR_cleaned$mun_name <- gsub(' ','', pref_2021_HoR_PR_cleaned$mun_name)
  pref_2021_HoR_PR_cleaned$mun_name <- gsub('　','', pref_2021_HoR_PR_cleaned$mun_name)
  
  # データを結合
  pref_join_HoR_PR <- full_join(pref_2017_HoR_PR_cleaned, 
                                pref_2021_HoR_PR_cleaned, 
                                by = "mun_name")
  
  # 基礎票を計算（参議院版と同じロジック）
  pref_HoR_PR <- pref_join_HoR_PR %>%
    mutate(
      # 主要政党の基礎票を計算
      # LDP
      nv_ldp = if(all(c("nv_ldp_2017", "nv_ldp_2021", "nv_total_2017", "nv_total_2021") %in% names(.))) {
        1/2 * (nv_ldp_2017/pmax(nv_total_2017, 1) + nv_ldp_2021/pmax(nv_total_2021, 1)) * 
          sqrt(pmax(nv_total_2017, 1) * pmax(nv_total_2021, 1))
      } else { 0 },
      
      # CDP（立憲民主党）
      nv_cdp = if(all(c("nv_cdp_2017", "nv_cdp_2021", "nv_total_2017", "nv_total_2021") %in% names(.))) {
        1/2 * (nv_cdp_2017/pmax(nv_total_2017, 1) + nv_cdp_2021/pmax(nv_total_2021, 1)) * 
          sqrt(pmax(nv_total_2017, 1) * pmax(nv_total_2021, 1))
      } else { 0 },
      
      # Komei（公明党）
      nv_komei = if(all(c("nv_komei_2017", "nv_komei_2021", "nv_total_2017", "nv_total_2021") %in% names(.))) {
        1/2 * (nv_komei_2017/pmax(nv_total_2017, 1) + nv_komei_2021/pmax(nv_total_2021, 1)) * 
          sqrt(pmax(nv_total_2017, 1) * pmax(nv_total_2021, 1))
      } else { 0 },
      
      # JCP（共産党）
      nv_jcp = if(all(c("nv_jcp_2017", "nv_jcp_2021", "nv_total_2017", "nv_total_2021") %in% names(.))) {
        1/2 * (nv_jcp_2017/pmax(nv_total_2017, 1) + nv_jcp_2021/pmax(nv_total_2021, 1)) * 
          sqrt(pmax(nv_total_2017, 1) * pmax(nv_total_2021, 1))
      } else { 0 },
      
      # Ishin（維新）
      nv_ishin = if(all(c("nv_ishin_2017", "nv_ishin_2021", "nv_total_2017", "nv_total_2021") %in% names(.))) {
        1/2 * (nv_ishin_2017/pmax(nv_total_2017, 1) + nv_ishin_2021/pmax(nv_total_2021, 1)) * 
          sqrt(pmax(nv_total_2017, 1) * pmax(nv_total_2021, 1))
      } else { 0 },
      
      # DPP（国民民主党）
      nv_dpp = if(all(c("nv_dpp_2017", "nv_dpp_2021", "nv_total_2017", "nv_total_2021") %in% names(.))) {
        1/2 * (nv_dpp_2017/pmax(nv_total_2017, 1) + nv_dpp_2021/pmax(nv_total_2021, 1)) * 
          sqrt(pmax(nv_total_2017, 1) * pmax(nv_total_2021, 1))
      } else if("nv_dpp_2021" %in% names(.) & "nv_total_2021" %in% names(.)) {
        # 2021年のみの場合
        nv_dpp_2021
      } else { 0 },
      
      # SDP（社民党）
      nv_sdp = if(all(c("nv_sdp_2017", "nv_sdp_2021", "nv_total_2017", "nv_total_2021") %in% names(.))) {
        1/2 * (nv_sdp_2017/pmax(nv_total_2017, 1) + nv_sdp_2021/pmax(nv_total_2021, 1)) * 
          sqrt(pmax(nv_total_2017, 1) * pmax(nv_total_2021, 1))
      } else { 0 },
      
      # Reiwa（れいわ新選組）- 2021年のみ
      nv_reiwa = if("nv_reiwa_2021" %in% names(.)) {
        nv_reiwa_2021
      } else { 0 },
      
      # NHK党
      nv_nhk = if("nv_nhk_2021" %in% names(.)) {
        nv_nhk_2021
      } else if("nv_nhk_2017" %in% names(.)) {
        nv_nhk_2017
      } else { 0 },
      
      # その他
      nv_other = 0  # 必要に応じて計算
    )
  
  # 合計を計算
  pref_HoR_PR <- pref_HoR_PR %>%
    mutate(nv_total = nv_ldp + nv_cdp + nv_komei + nv_jcp + nv_ishin + 
                    nv_dpp + nv_sdp + nv_reiwa + nv_nhk + nv_other)
  
  # 必要な列だけを選択（参議院版と同じ形式）
  pref_HoR_PR <- pref_HoR_PR %>%
    select(mun_name, nv_ldp, nv_cdp, nv_ishin, nv_komei, nv_jcp,
          nv_dpp, nv_reiwa, nv_sdp, nv_nhk, nv_other, nv_total)
  
  return(pref_HoR_PR)
}