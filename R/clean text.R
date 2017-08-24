
#' Clean words
#'
#' Removes numeric characters, stop words and white space from words...
#'
#' @name clean_text
#' @param df Takes df with
#' @return Returns data frame
#' @export


clean_text <- function(df) {

  df %>%
    mutate(
      word = str_replace_all(word,c("," = "", "\\." = "")),
      word = str_replace_all(word, "'", ""),
      word = str_trim(word)) %>%
    filter(word != "") %>%
    anti_join(stop_words) # from tidytext
}
