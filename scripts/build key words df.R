
phrase_count <- read_rds("/Users/rosseji/Documents/temp data/speech wrangling/phrase_count_df.rda")

trial_set <- phrase_count %>%
  .[1:25000,]

df <- find_key_words(trial_set, output)
saver(df,"/Users/rosseji/Documents/temp data/speech wrangling/wrangle_set1.rda")

trial_set <- phrase_count %>%
  .[25001:50000,]

df <- find_key_words(trial_set, output)
saveRDS(df,"/Users/rosseji/Documents/temp data/speech wrangling/wrangle_set2.rda")




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
