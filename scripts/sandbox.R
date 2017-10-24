
phrases <- read_rds("/Users/rosseji/Dropbox/TrendLock/data/speeches/phrases.rda")

unique(phrases$media.title)

test_1 <- phrases %>%
  filter(media.title == "Remarks at Business Roundtable Breakfast") %>%
  find_key_words(phrase)

test_1_sent_7 <- test_1 %>%
  filter(sent.idx == 7)

test_1_sent_23 <- test_1 %>%
  filter(sent.idx == 23)




test_2 <- phrases %>%
  filter(media.title == "Press Conference with Chief of the Defence Force, Air Chief Marshal Mark Binskin AC") %>%
  find_key_words(phrase)

test_2_sent_4_13 <- test_2 %>%
  filter(sent.idx == c(4, 13))

test_2_sent_11 <- test_2 %>%
  filter(sent.idx == c(11))








# mt sent to phrases... ==========================

pollies <- read_rds("/Users/rosseji/Dropbox/TrendLock/data/speeches/pollies.rda")

#clean
pollies <- pollies %>%
  filter(!sent %in% c("e&oe", "prime minister", "ends", "[ends]", "[inaudible]"))


pollies2 <- pollies %>%
  mutate(str_len = str_length(sent)) %>%
  filter(str_len > 25) %>%
  select(-str_len)

select_few <- unique(pollies2$media.title) %>%
  head(10)


pollies2 <- pollies2 %>%
  filter(media.title %in% select_few)



output_4 <- unnest_tokens(pollies2, phrase, sent, "ngrams", n = 4)
output_3 <- unnest_tokens(pollies2, phrase, sent, "ngrams", n = 3)
output_2 <- unnest_tokens(pollies2, phrase, sent, "ngrams", n = 2)
output_1 <- unnest_tokens(pollies2, phrase, sent)


phrases <- bind_rows(output_1, output_2, output_3, output_4)

write_rds(phrases, "/Users/rosseji/Dropbox/TrendLock/data/speeches/phrases_test.rda")
