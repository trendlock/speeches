df_MT <- sentences_MT()


df_MT <- df_MT %>%
  add_speaker() %>%
  sent_to_words(keep_jorn = T) %>%
  clean_text()

df_MT <- df_MT %>%
  select(-media.title)

write_csv(df_MT, "/Users/rosseji/Dropbox/TrendLock/data/speeches/words_MT.csv")
