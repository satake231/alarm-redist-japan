###############################################################################
# Download House of Representatives election data (Proportional Representation)
# 衆議院選挙（比例代表）データのダウンロード（完成版）
#
# 機能：指定された選挙回の比例代表データを総務省のサイトからダウンロードします。
#       第49回（2021年）以降は.xlsx、それ以前は.xls形式と命名規則に自動対応します。
###############################################################################

#' Download House of Representatives Proportional Representation election data for a specific election cycle.
#' @param pref_code Prefecture code (1 for Hokkaido, 13 for Tokyo, etc.).
#' @param election_number The number of the election (e.g., 48 for the 2017 election, 49 for the 2021 election).
#' @return A data frame with HoR PR election results.
#' @examples
#' # Download data for the 49th election (2021, .xlsx format)
#' # data_49th <- download_HoR_PR(pref_code = 1, election_number = 49)
#'
#' # Download data for the 48th election (2017, .xls format)
#' # data_48th <- download_HoR_PR(pref_code = 1, election_number = 48)

download_2024_HoR_PR <- function(pref_code, election_number) {
  
  # Check for required packages
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Package 'rvest' is required.")
  if (!requireNamespace("httr", quietly = TRUE)) stop("Package 'httr' is required.")
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  
  tryCatch({
    
    # --- 1. Set parameters based on election number ---
    pref_code_pad <- sprintf("%02d", pref_code)
    
    # Determine settings based on the election cycle
    if (election_number >= 49) {
      file_ext_pattern <- "\\.xlsx$"
      page_url_template <- "shugiin%d_kaihyo%s.html"
      file_name_keyword <- "hirei"
      skip_rows <- 4
      use_keyword_logic <- TRUE
    } else {
      file_ext_pattern <- "\\.xls$"
      page_url_template <- "shikuchouson_%s.html"
      file_name_keyword <- "hirei" # This is a fallback
      skip_rows <- 5
      use_keyword_logic <- FALSE # Use position-based logic for older elections
    }
    
    # --- 2. Construct URL and fetch page content ---
    base_url <- sprintf("https://www.soumu.go.jp/senkyo/senkyo_s/data/shugiin%d/", election_number)
    
    page_name <- if (election_number >= 49) {
      sprintf(page_url_template, election_number, pref_code_pad)
    } else {
      sprintf(page_url_template, pref_code_pad)
    }
    page_url <- paste0(base_url, page_name)
    
    response <- httr::GET(page_url, 
                          httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"))
    httr::stop_for_status(response, task = paste("access the election results page:", page_url))
    
    # --- 3. Scrape and identify the correct file link ---
    html_content <- rvest::read_html(response)
    
    all_file_links <- html_content %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      grep(file_ext_pattern, ., value = TRUE)
      
    if (use_keyword_logic) {
      # For modern elections (>= 49th), find the link containing the keyword 'hirei'
      pr_relative_path <- all_file_links[stringr::str_detect(all_file_links, file_name_keyword)]
      if (length(pr_relative_path) != 1) {
        stop(paste("Could not uniquely identify the proportional representation file using keyword 'hirei' on page:", page_url))
      }
    } else {
      # For older elections (< 49th), assume the 2nd link is the proportional representation file
      if (length(all_file_links) < 2) {
        stop(paste("Expected at least 2 .xls files but found fewer on page:", page_url))
      }
      pr_relative_path <- all_file_links[2]
    }
    
    download_url <- xml2::url_absolute(pr_relative_path, base = page_url)
    
    # --- 4. Download and read the data ---
    temp_file <- tempfile(fileext = ifelse(election_number >= 49, ".xlsx", ".xls"))
    httr::GET(download_url, httr::write_disk(temp_file, overwrite = TRUE))
    election_data <- readxl::read_excel(temp_file, skip = skip_rows)
    
    return(election_data)
    
  }, error = function(e) {
    message(paste("An error occurred for election #", election_number, " and prefecture code ", pref_code, ":", sep=""))
    message(e$message)
    return(NULL)
  })
}