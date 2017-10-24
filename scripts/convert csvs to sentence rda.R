

# get data and build

mt <- read_csv("/Users/rosseji/Dropbox/TrendLock/data/speeches/archive/sentences_MT.csv") %>%
  add_speaker() %>%
  unnest_tokens(sent, sent, "sentences")

unique(mt$speaker.)

mt <- mt %>%
  filter(!is.na(speaker.), !is.na)



mt2 <- mt %>%
  group_by(media.date, media.title, speaker.)

mt2$spch.idx <- group_indices(mt2)

mt3 <- mt2 %>%
  split(.$spch.idx) %>%
  map( ~ mutate(.x, sent.idx = row_number())) %>%
  bind_rows()


mt3 <- mt3 %>%
  select(-pc.indx)
# extract_tokens




write_rds(mt3, "/Users/rosseji/Dropbox/TrendLock/data/speeches/pollies.rda")


pollies <- mt
write_rds(pollies, "/Users/rosseji/Dropbox/TrendLock/data/speeches/pollies.rda")




pollies <- read_rds("/Users/rosseji/Dropbox/TrendLock/data/speeches/pollies.rda")
