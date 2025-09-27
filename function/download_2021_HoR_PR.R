#' Download 2021 House of Representatives Elections Data
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile xlsx downloaded in data-raw/ directory and resulting xlsx
#'
#' @concept downloaddata
#'
#' @export
#'

download_2021_HoR_PR <- function(pref_code){

  # check if `data-raw `folder exists
  if(!file.exists("data-raw")){

    # if not, create data folder
    dir.create("data-raw")

  }

  # Format prefecture code
  pref_code_pad <- str_pad(pref_code, 2, pad = "0")

  # check if data exists
  if(file.exists(paste0('data-raw/', pref_code, '_2021_HoR.xls')) == TRUE){

    # read file
    pref_2021_HoR <- readxl::read_excel(paste0('data-raw/', pref_code, '_2021_HoR.xls'))

  }else{
    # if not, download file

    # obtain links to xlsx files
    links <-
      rvest::read_html(paste0("https://www.soumu.go.jp/senkyo/senkyo_s/data/shugiin49/shikuchouson_",
                              pref_code_pad, ".html")) %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      grep("xls", ., value = TRUE)

    # select link to relevant file (the second link corresponds to the PR data)
    link_PR_party <- links[2]

    print(link_PR_party)

    # download file
    download.file(paste0('https://www.soumu.go.jp' ,
                        link_PR_party),
                  paste0('data-raw/', pref_code, '_2021_HoR.xls'))

    # read file
    pref_2021_HoR <- readxl::read_excel(paste0('data-raw/', pref_code, '_2021_HoR.xls'))
  }

  # return file
  return(pref_2021_HoR)
}