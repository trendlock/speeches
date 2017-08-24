#' @export
find_key_words <- function(df, text_var) {
  text_var <- enquo(text_var)

  message("wrangling text")
  df_results <- df %>%
    pull(output) %>%
    map( ~ list(
      output = .x,
      tokens = .x %>% str_split(" ") %>% unlist()
    )) %>%
    map( ~ list(
      output = .x$output,
      tokens = .x$tokens,
      key_test = .x$tokens %>% report_stop_words()
    )) %>%
    map(~ tibble(
      output = .x$output,
      all.stop = all(.x$key_test),
      key.words = .x$tokens[.x$key_test],
      trimmed = str_c(.x$tokens[.x$key_test], collapse = " ")
    )) %>%
    bind_rows()

  message("compiling results")
  inner_join(df, df_results, by = "output")
}


report_stop_words <- function(tokens) {
  tokens %>%
    map( ~ !.x %in% stop_words$word) %>%
    unlist()
}


