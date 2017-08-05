
html.files <- list.files("/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/", full.names = T)


html.files2 <- html.files[100:150]

#html.files2 <- sample(html.files, 10)

df <- html.files2 %>%
  map( ~ .x %>%
         shorten_scrape())

html.files2 %>%
  map( ~ .x %>%
         try_dates())


try_dates <- function(x) {

  which. <- x
  h <- read_html(x)

  date.str1 <- h %>%
    html_nodes("div#content b") %>%
    html_text() %>%
    detect(has_month)

  date.str2 <- h %>%
    html_nodes("div#content strong") %>%
    html_text() %>%
    detect(has_month)

  date.str3 <- h %>%
    html_nodes("div#content h3") %>%
    html_text() %>%
    detect(has_month)

  date.str4 <- h %>%
    html_nodes("div.headline h2") %>%
    html_text() #%>%
    #detect(has_month)

  list(which = which., date.str1 = date.str1, date.str2 = date.str2, date.str3 = date.str3, date.str4 = date.str4)
}




date_fun_2 <- function(h) {
  date.str <- h %>%
    html_nodes("div#content h3") %>%
    html_text() %>%
    detect(speeches:::has_month)
}


