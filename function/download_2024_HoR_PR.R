###############################################################################
# Download 2024 House of Representatives election data (Proportional Representation)
# 2024年衆議院選挙（比例代表）データのダウンロード
###############################################################################

#' Download 2024 House of Representatives Proportional Representation election data
#' @param pref_code prefecture code (01 for Hokkaido)
#' @return data frame with HoR PR election results
#' @examples
#' hokkaido_2024_hor_pr <- download_2024_HoR_PR(1)

download_2024_HoR_PR <- function(pref_code) {
  
  # Prefecture names mapping (same as other download functions)
  pref_names <- c(
    "hokkaido", "aomori", "iwate", "miyagi", "akita",
    "yamagata", "fukushima", "ibaraki", "tochigi", "gunma",
    "saitama", "chiba", "tokyo", "kanagawa", "niigata",
    "toyama", "ishikawa", "fukui", "yamanashi", "nagano",
    "gifu", "shizuoka", "aichi", "mie", "shiga",
    "kyoto", "osaka", "hyogo", "nara", "wakayama",
    "tottori", "shimane", "okayama", "hiroshima", "yamaguchi",
    "tokushima", "kagawa", "ehime", "kochi", "fukuoka",
    "saga", "nagasaki", "kumamoto", "oita", "miyazaki",
    "kagoshima", "okinawa"
  )
  
  # Get prefecture name
  pref_name <- pref_names[pref_code]
  
  # File path for 2024 HoR PR data
  # Note: This assumes the data file is stored locally
  # In production, this would download from an official source
  file_path <- here::here(paste0("data-raw/hor_2024/", 
                                sprintf("%02d", pref_code), "_",
                                pref_name, "_hor_2024_pr.xlsx"))
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("2024 HoR PR data file not found for", pref_name, 
              "\nExpected path:", file_path))
  }
  
  # Read Excel file
  # Assuming the structure matches the analyzed format
  data <- readxl::read_excel(file_path, sheet = pref_name)
  
  return(data)
}