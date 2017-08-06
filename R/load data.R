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


#' Get data for MT speeches as words
#'
#' "Anyone who thinks it's smart to cut immigration is sentencing Australia to poverty" - MT
#'
#' @name word_MT
#' @param path Defaults to storage
#' @return Returns data frame
#' @export
word_MT <- function(path = "/TrendLock/data/speeches/word_MT.csv") {
  readr::read_csv(find::this(path))
}


#' Get data for BS speeches as words
#'
#' "Labor must work harder to attract and retain members." - BS
#'
#' @name word_BS
#' @param path Defaults to storage
#' @return Returns data frame
#' @export

word_BS <- function(path = "/TrendLock/data/speeches/word_BS.csv") {
  readr::read_csv(find::this(path))
}

#' Get and combine data for MT and BS (gas fun)
#'
#' "Labor must work harder to attract and retain members." - BS
#' "Anyone who thinks it's smart to cut immigration is sentencing Australia to poverty" - MT
#'
#' @name get_and_combine
#' @return Returns data frame
#' @export

get_and_combine <- function() {
  df_MT <- word_MT()
    # select(-media.title)

  df_BS <- word_BS() %>%
    clean_text()

  bind_rows(df_MT, df_BS)

}
