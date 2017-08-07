
#' @export

both_gas_talk <- function(df, key_words = c("gas",  "coal", "renewable")) {

  df <- df %>%
    filter(word %in% key_words)

  gas. <- df %>%
    mutate(week. = floor_date(media.date, "week")) %>%
    group_by(week., word, speaker.) %>%
    summarise(times.said = n())

  ggplot(gas., aes(x = week., y = times.said, col = word)) +
    geom_line(size = 1, alpha = 0.5) +
    facet_grid(speaker.~.)

}
