

# scrape content from /shorten html/


html.files <- list.files("/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/", full.names = T)


html.files2 <- html.files[1130:1150]



df <- html.files2 %>%
  map( ~ shorten_scrape(.x)) %>%
  bind_rows()




write_csv(df, "/Users/rosseji/Dropbox/TrendLock/data/speeches/sentences_BS.csv")
