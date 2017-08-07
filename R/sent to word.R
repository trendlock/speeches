#' Convert sentence stings to words
#'
#' Converts strings that are sentences to one word per row.
#' Uses \code{tidttext} function \code{unnest_tokens()}. Brilliant!
#'
#' @name sent_to_words
#' @param df Takes df with
#' @param keep_jorn Defaults to FALSE, filters out "jorn" speaker.
#' @return Returns data frame
#' @export


sent_to_words <- function(df, keep_jorn = F) {
  df <- df %>%
    unnest_tokens(word, sent) %>%
    select(pc.indx, word, everything())

  if (keep_jorn){
    df
  } else {
    df %>%
      filter(speaker. != "")
  }
}
