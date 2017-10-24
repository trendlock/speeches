

# phrases example



pollies <- read_rds("/Users/rosseji/Dropbox/TrendLock/data/speeches/pollies.rda")

#clean
pollies <- pollies %>%
  filter(!sent %in% c("e&oe", "prime minister", "ends", "[ends]", "[inaudible]"))


pollies2 <- pollies %>%
  mutate(str_len = str_length(sent)) %>%
  filter(str_len > 25) %>%
  select(-str_len)

select_few <- unique(pollies2$media.date) %>%
  tail(100)


pollies2 <- pollies2 %>%
  filter(media.date %in% select_few)



output_4 <- unnest_tokens(pollies2, phrase, sent, "ngrams", n = 4)
output_3 <- unnest_tokens(pollies2, phrase, sent, "ngrams", n = 3)
output_2 <- unnest_tokens(pollies2, phrase, sent, "ngrams", n = 2)
output_1 <- unnest_tokens(pollies2, phrase, sent)


phrases <- bind_rows(output_1, output_2, output_3, output_4)

write_rds(phrases, "/Users/rosseji/Dropbox/TrendLock/data/speeches/phrases_test.rda")


phrases_test <- read_rds("/Users/rosseji/Dropbox/TrendLock/data/speeches/phrases_test.rda")

phrases_test_sum <- phrases_test %>%
  clean_text(phrase)

phrases_test_sum <- phrases_test_sum %>%
  group_by(media.date, phrase) %>%
  mutate(times.said = n()) %>%
  filter(times.said > 2)


phrases_test_sum <- phrases_test_sum %>%
  select(-spch.idx, -media.title, -sent.idx, -output)

phrases_test_sum <- phrases_test_sum %>%
  distinct(media.date, speaker, phrase, .keep_all = T)


write_rds(phrases_test_sum, "/Users/rosseji/Dropbox/TrendLock/data/speeches/phrases_test_rdy.rda")

# write word list
opts <- unique(phrases_test_sum$phrase)
opts <- opts %>%
  set_names(opts)
write_rds(opts, "/Users/rosseji/Dropbox/TrendLock/data/speeches/phrases_test_opts.rda")

