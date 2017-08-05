
#' @export

both_gas_talk <- function(df_MT, key_words = c("gas",  "coal", "renewable")) {

  df_MT <- sentences_MT()


  df_MT <- df_MT %>%
    add_speaker() %>%
    sent_to_words(keep_jorn = T) %>%
    clean_text()

  df_MT <- df_MT %>%
    filter(word %in% key_words) %>%
    select(-media.title)


  df_BS <- sentences_BS() %>%
    clean_text()%>%
    filter(word %in% key_words)

  df <- bind_rows(df_MT, df_BS)


  gas. <- df %>%
    mutate(week. = floor_date(media.date, "week")) %>%
    group_by(week., word, speaker.) %>%
    summarise(times.said = n())


  p <- ggplot(gas., aes(x = week., y = times.said, col = word)) +
    geom_line(size = 1, alpha = 0.5) +
    facet_grid(speaker.~.)

  print(p)
}
