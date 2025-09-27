###############################################################################
# 実験用コード（.xls形式 検証版）
# 目的：第48回衆院選のページにアクセスし、.xls形式の比例代表データへの
#       ダウンロードURLを正しく取得できるか検証する。
###############################################################################

# 必要なパッケージを読み込み
# もし未インストールなら install.packages(c("rvest", "stringr", "httr", "xml2")) を実行
library(rvest)
library(stringr)
library(httr)
library(xml2)

# テスト対象の都道府県コード（例：北海道）
test_pref_code <- 1
# テスト対象の選挙回（例：第48回）
test_election_number <- 48

# ---------------------------------------------------------------------------

tryCatch({
  
  cat("--- URL取得処理を開始します (.xls形式 検証版) ---\n")
  
  # 1. パラメータを設定
  pref_code_pad <- sprintf("%02d", test_pref_code)
  cat("対象都道府県コード:", pref_code_pad, "\n")
  cat("対象選挙回:", test_election_number, "\n")
  
  # 2. 第48回のURL構造に基づき、対象ページのURLを構築
  base_url <- sprintf("https://www.soumu.go.jp/senkyo/senkyo_s/data/shugiin%d/", test_election_number)
  page_url <- paste0(base_url, "shikuchouson_", pref_code_pad, ".html")
  cat("アクセスするページURL:", page_url, "\n")
  
  # 3. httr::GET() でページにアクセス
  response <- GET(page_url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"))
  
  httr::stop_for_status(response, task = "ウェブページへのアクセス")
  cat("HTTPリクエスト成功 (ステータス: 200)\n")
  
  # 4. 取得した内容をrvestで読み込み
  html_content <- read_html(response)
  
  # 5. ページ内の`.xls`ファイルへのリンクを抽出
  xls_links <- html_content %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("\\.xls$", ., value = TRUE)
    
  cat("ページ内で発見した.xlsファイルへのリンク:\n")
  print(xls_links)
  
  # 6. 「比例代表」を意味する "hirei" の文字列を含むリンクを探す
  pr_link <- xls_links[str_detect(xls_links, "hirei")]
  
  if (length(pr_link) == 1) {
    relative_path <- pr_link
    cat("比例代表の.xlsファイルの相対パス:", relative_path, "\n")
    
    # 7. 相対パスを完全なURLに変換
    absolute_url <- xml2::url_absolute(relative_path, base = page_url)
    
    cat("\n--- 処理成功 ---\n")
    cat("取得したダウンロードURL:\n")
    cat(absolute_url, "\n")
    
  } else {
    stop("比例代表の.xlsファイルが1つに特定できませんでした。")
  }
  
}, error = function(e) {
  
  cat("\n--- エラーが発生しました ---\n")
  cat("エラーメッセージ:", conditionMessage(e), "\n")
  
})