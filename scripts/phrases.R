df_MT <- sentences_MT()


df_MT <- df_MT %>%
  add_speaker()

test <- df_MT %>%
  filter(media.title == "Doorstop with Luke Howarth MP, Member for Petrie")

unique(df_MT$speaker.)


df_MT <- df_MT %>%
  filter(speaker. %in% c("pm", "Malcolm Turnbull"))

df_MT <- df_MT %>%
  select(media.date,media.loc, sent)

# be cool to make a list of funs and args... one day
#unnest_tokens_ls <- list(fun = unnest_tokens, args = NULL)

test4 <- unnest_tokens(df_MT,output, sent, "ngrams", n = 4)
test3 <- unnest_tokens(df_MT,output, sent, "ngrams")
test2 <- unnest_tokens(df_MT,output, sent, "ngrams", n = 2)
test1 <- unnest_tokens(df_MT,output, sent)


test <- test1 %>%
  bind_rows(test2) %>%
  bind_rows(test3) %>%
  bind_rows(test4)


saveRDS(test, "/Users/rosseji/Documents/temp data/speech wrangling/phrase_df.rda")
test <- read_rds( "/Users/rosseji/Documents/temp data/speech wrangling/phrase_df.rda")


test_sum <- test %>%
  group_by(media.date, output) %>% # will need to add speaker grp
  summarise(times.said = n()) %>%
  filter(times.said > 2)


saveRDS(test_sum, "/Users/rosseji/Documents/temp data/speech wrangling/phrase_count_df.rda")


# looking at coal....
coal_phrase <- test_sum %>%
  filter(str_detect(output, "coal"))
df <- find_key_words(coal_phrase, output)



trial_set <- test_sum %>%
  head(50000)



df <- df %>%
  mutate(output = paste0("`", output, "`"))

df <- df %>%
  rename(label = output)

df <- df[-(1:1906),]

df <- df %>%
  mutate(value = str_replace_all(label, "`", ""))


write_csv(df, "/Users/rosseji/dev/apps/speechApp/data/words_phrases.csv")




unique_text <- unique(df$value) %>%
  head(10000)
choices_vec <- unique_text %>%
  set_names(unique_text)
saveRDS(choices_vec,  "/Users/rosseji/dev/apps/speechApp/data/all_options.rda")


df_truc <- df %>%
  mutate()
