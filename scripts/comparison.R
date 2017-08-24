
words <- read_csv("/Users/rosseji/dev/apps/showcase/data/words_comb.csv")

df_jobs <- words %>%
  filter(word == "jobs") %>%
  select(media.date,speaker., word.count)

df_jobs2 <- df_jobs %>%
  spread(speaker., word.count) %>%
  mutate(Turnbull = ifelse(is.na(Turnbull), 0, Tu))

df_jobs2[is.na(df_jobs2)] <- 0

df_jobs2 <- df_jobs2 %>%
  mutate(diff = Shorten - Turnbull)

ggplot(df_jobs2, aes(x = media.date, y = diff))+
  geom_line()
