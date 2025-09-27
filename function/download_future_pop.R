#' Download Future Population Projection Data
#'
#' @return an Excel file downloaded in data-raw/ directory
#'
#' @concept downloaddata
#'
#' @export
#'

download_future_pop <- function(){

  # check if data-raw/ folder exists in working directory
  if(!file.exists("data-raw")){

    # if not, create data folder
    dir.create("data-raw")

  }

  # download the files from the National Institute of Population and Social Security Research
  download.file("https://www.ipss.go.jp/pp-shicyoson/j/shicyoson23/2gaiyo_hyo/kekkahyo1.xlsx",
                "data-raw/future_pop.xlsx", mode = "wb")

  # return the data frame
  future_pop_raw <- readxl::read_excel("data-raw/future_pop.xlsx", skip = 3)

  return(future_pop_raw)
}
