# install.packages("dplyr") # 必要に応じてインストール
# install.packages("knitr") # 必要に応じてインストール
library(dplyr)
library(knitr)

# アダムズ方式で議席を配分する関数
calculate_adams_method <- function(population_df, total_seats) {
  
  # 最適な除数を見つけるための二分探索
  low_divisor <- 1
  high_divisor <- sum(as.numeric(population_df$population))
  
  final_allocation <- data.frame()
  
  # 探索ループ
  while ((high_divisor - low_divisor) > 0.01) {
    divisor <- (low_divisor + high_divisor) / 2
    
    # 人口を除数で割り、小数点以下を切り上げて議席数を計算
    allocation <- population_df %>%
      mutate(seats = ceiling(population / divisor))
    
    current_seats <- sum(allocation$seats)
    
    if (current_seats > total_seats) {
      low_divisor <- divisor
    } else {
      high_divisor <- divisor
    }
    
    # 議席数が総議席数と一致した場合、その解を保存候補とする
    if (current_seats == total_seats) {
      final_allocation <- allocation
    }
  }
  
  # ループ内で合計が一致する解が見つからなかった場合、最終的な除数で再計算
  if (nrow(final_allocation) == 0) {
    final_divisor <- (low_divisor + high_divisor) / 2
    final_allocation <- population_df %>%
      mutate(seats = ceiling(population / final_divisor))
  }
  
  return(final_allocation %>% select(prefecture, seats))
}

# --- データ定義 ---

# 【修正】ご提供のCSVファイルに基づく2050年の推計人口 
population_2050 <- data.frame(
  prefecture = c("北海道", "青森県", "岩手県", "宮城県", "秋田県", "山形県", "福島県", 
                "茨城県", "栃木県", "群馬県", "埼玉県", "千葉県", "東京都", "神奈川県", 
                "新潟県", "富山県", "石川県", "福井県", "山梨県", "長野県", "岐阜県", 
                "静岡県", "愛知県", "三重県", "滋賀県", "京都府", "大阪府", "兵庫県", 
                "奈良県", "和歌山県", "鳥取県", "島根県", "岡山県", "広島県", "山口県", 
                "徳島県", "香川県", "愛媛県", "高知県", "福岡県", "佐賀県", "長崎県", 
                "熊本県", "大分県", "宮崎県", "鹿児島県", "沖縄県"),
  population = c(3820016, 754751, 783242, 1829565, 560429, 710838, 1247000, 
                2245065, 1502202, 1520630, 6633932, 5690156, 14399144, 8524492, 
                1525004, 761719, 896801, 572885, 611586, 1581949, 1468392, 
                2828823, 6676331, 1347202, 1222791, 2075975, 7263182, 4357576, 
                950365, 631619, 405528, 496994, 1510460, 2229527, 926183, 
                480669, 724120, 944634, 450980, 4479021, 620873, 868817, 
                1355329, 841343, 796631, 1170602, 1391013)
)

# 2022年からの衆議院小選挙区の定数
current_seats_df <- data.frame(
  prefecture = c("北海道", "青森県", "岩手県", "宮城県", "秋田県", "山形県", "福島県", 
                "茨城県", "栃木県", "群馬県", "埼玉県", "千葉県", "東京都", "神奈川県", 
                "新潟県", "富山県", "石川県", "福井県", "山梨県", "長野県", "岐阜県", 
                "静岡県", "愛知県", "三重県", "滋賀県", "京都府", "大阪府", "兵庫県", 
                "奈良県", "和歌山県", "鳥取県", "島根県", "岡山県", "広島県", "山口県", 
                "徳島県", "香川県", "愛媛県", "高知県", "福岡県", "佐賀県", "長崎県", 
                "熊本県", "大分県", "宮崎県", "鹿児島県", "沖縄県"),
  current_seats = c(12, 3, 3, 6, 3, 3, 4, 7, 5, 5, 16, 14, 30, 20, 5, 3, 3, 2, 2, 5, 5, 
                    8, 16, 4, 4, 6, 19, 12, 3, 2, 2, 2, 4, 6, 3, 2, 3, 3, 2, 11, 2, 4, 
                    4, 3, 3, 4, 4)
)

# --- 計算実行 ---
TOTAL_SEATS <- 289
predicted_allocation_2050 <- calculate_adams_method(population_2050, TOTAL_SEATS)

# --- 結果表示 ---
# データを結合し、増減を計算
results_df <- current_seats_df %>%
  left_join(predicted_allocation_2050, by = "prefecture") %>%
  rename(predicted_seats = seats) %>%
  mutate(change = predicted_seats - current_seats) %>%
  arrange(desc(change))

# 結果のサマリーを出力
cat("--- 2050年 将来推計人口に基づく衆議院小選挙区定数シミュレーション（アダムズ方式） ---\n")
cat(sprintf("総議席数: %d議席\n", TOTAL_SEATS))
cat(sprintf("配分議席合計: %d議席\n\n", sum(results_df$predicted_seats)))

# kableで見やすく整形して表示
cat("【定数が増加する都県】\n")
results_df %>%
  filter(change > 0) %>%
  kable()

cat("\n【定数が減少する道府県】\n")
results_df %>%
  filter(change < 0) %>%
  kable()

cat("\n【定数が変わらない府県】\n")
results_df %>%
  filter(change == 0) %>%
  kable()