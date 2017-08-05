#' Get data for MT speeches as sentences
#'
#' "Anyone who thinks it's smart to cut immigration is sentencing Australia to poverty" - MT
#'
#' @name sentences_MT
#' @param path Defaults to storage
#' @return Returns data frame
#' @export

sentences_MT <- function(path = "/TrendLock/data/speeches/sentences_MT.csv") {
    readr::read_csv(find::this(path))
}

#' Get data for BS speeches as sentences
#'
#' "Labor must work harder to attract and retain members." - BS
#'
#' @name sentences_BS
#' @param path Defaults to storage
#' @return Returns data frame
#' @export

sentences_BS <- function(path = "/TrendLock/data/speeches/sentences_BS.csv") {
  readr::read_csv(find::this(path))
}
