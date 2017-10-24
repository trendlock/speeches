

# scrape content from /shorten html/


html.files <- list.files("/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/", full.names = T)


html.files2 <- sample(html.files, 10, replace = F)



df <- html.files2 %>%
  map( ~ shorten_strip_internals(.x)) %>%
  bind_rows()




write_csv(df, "/Users/rosseji/Dropbox/TrendLock/data/speeches/sentences_BS.csv")
