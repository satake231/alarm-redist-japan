# 必要なパッケージを読み込み
# install.packages(c("readr", "dplyr", "tidyr")) # 未インストールの場合
library(readr)
library(dplyr)
library(tidyr)

#' 衆議院(HoR)比例代表の得票数データを整形する
#'
#' @param file_path 衆議院議員総選挙の得票数データファイル（CSV形式）へのパス
#'
#' @return 整形済みのtibble（市区町村, 政党, 得票数）
#'
#' @examples
#' # clean_hor_data("data-raw/1_2021_HoR.xls - 北海道.csv")
clean_hor_data <- function(file_path) {
  
  # ステップ1: 不要なヘッダー(3行)をスキップしてCSVを読み込む
  # localeを指定して文字化けを防ぐ
  df_raw <- read_csv(
    file_path,
    skip = 3,
    locale = locale(encoding = "CP932") # Windows/MacのExcel由来CSVの文字化け対策
  )
  
  # ステップ2: 列名の整理
  df_renamed <- df_raw %>%
    rename(
      municipality = `市区町村名＼政党名` # 最初の列名を変更
    ) %>%
    select(
      -`得票数計` # 分析時には不要な合計列を削除
    )
  
  # ステップ3: 「横長」から「縦長」へデータを変換
  df_tidy <- df_renamed %>%
    pivot_longer(
      cols = -municipality, # municipality列以外をすべて対象にする
      names_to = "party",   # 新しく作る「政党名」列の名前
      values_to = "votes"   # 新しく作る「得票数」列の名前
    ) %>%
    # NAの行（例：合計行など）があれば除去
    filter(!is.na(votes)) 
  
  return(df_tidy)
}