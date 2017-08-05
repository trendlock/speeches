
# getting date all
html.files <- list.files("/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/", full.names = T)


html.files2 <- html.files[650:700]


html.files2 <- sample(html.files, 100)

b.ls <- html.files2 %>%
  map( ~ shorten_get_date(.x))

b.ls

