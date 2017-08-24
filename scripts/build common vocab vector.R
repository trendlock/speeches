library(tidyverse)
library(stringr)
df <- tidytext::stop_words


common_vocab <- df %>% 
  distinct(word) %>% 
  filter(!word %in% c(
    "new", "likely", "necessary", "old"
  ))


vec <- common_vocab %>% 
  pull(word)

saveRDS(vec,"/Users/rosseji/Documents/temp data/speech wrangling/common_vocab.rda")

