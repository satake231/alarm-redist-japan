# 必要なパッケージ
library(dplyr)
library(readxl)

#' 将来推計人口データを整形する (エラー修正版)
#'
#' @param future_pop_raw readxl::read_excel()などで読み込んだ生のデータフレーム
#' @param year 推計年（例: 2050）
#'
#' @return 整形済みのtibble（code, mun_name, pop）
#'
#' @export
clean_future_pop <- function(future_pop_raw, year = 2050) {

  # 対象年の列名を生成 (例: "2050年")
  year_col_name <- paste0(year, "年")

  # データ整形
  future_pop_cleaned <- future_pop_raw %>%
    # 正しく読み込まれた列名でフィルタリング
    dplyr::filter(`市などの別` %in% c("0", "2", "3")) %>%
    # 必要な列を選択し、名前を変更
    dplyr::select(
      code = `コード`,
      mun_name = `市区町村`,
      pop = all_of(year_col_name) # 対象年を動的に選択
    ) %>%
    # データ型を変換
    dplyr::mutate(
      code = as.numeric(code),
      pop = as.numeric(pop)
    ) %>%
    # 市区町村名が欠損している行を除外
    na.omit()

  return(future_pop_cleaned)
}