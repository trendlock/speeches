

# trimming data by bundle and adding count for each date...

df_words <- read_csv("/Users/rosseji/dev/apps/showcase/data/words_comb.csv")

write_csv(df_words, "/Users/rosseji/Dropbox/TrendLock/data/speeches/words_comb_aug.csv")


df_words2 <- df_words %>%
  group_by(media.date, word, speaker.) %>%
  mutate(counter= 1) %>%
  mutate(word.count = sum(counter)) %>%
  distinct(media.date, word, speaker., .keep_all = T)

df_words2 <- df_words2 %>%
  select(-counter, -week.)
# take out words only mentioned once
df_words2 <- df_words2 %>%
  filter(word.count != 1)
write_csv(df_words2, "/Users/rosseji/dev/apps/showcase/data/words_comb.csv")

###### ============
top_ls <- read_csv("/Users/rosseji/dev/apps/showcase/data/top_words.csv") %>%
  split(.$id.name)



compare_words <- df_words %>%


ggplot(df_words, aes)
