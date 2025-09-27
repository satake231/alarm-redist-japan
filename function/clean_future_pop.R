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

clean_future_pop <- function(future_pop_raw) {
  # rename data frame
  future_pop_cleaned <- future_pop_raw |>
    dplyr::select(
      code = "...1",
      pop_2020 = "2020年...5",
      pop_2025 = "2025年...6",
      pop_2030 = "2030年...7",
      pop_2035 = "2035年...8",
      pop_2040 = "2040年...9",
      pop_2045 = "2045年...10",
      pop_2050 = "2050年...11",
    )

  return(future_pop_cleaned)
}
