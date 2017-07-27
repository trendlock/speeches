
# get list of files

# special case
file. <- "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_07-50-13-ENDEAVOUR_HILLS_STATEMENT.html"

#common case
html.files <- c("/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_07-42-04-Television_Interview__G20.html",
                  "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_05-43-41-TRANSCRIPT_DOORSTOP_MEL.html",
                  "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_06-45-00-TV:_Weekend_Sunshine__Lab.html",
                  "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_08-10-46-Doorstop_Wyndham_Vale_GP_S.html",
                  "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_07-49-07-Press_Conference_Belmore_-.html")

html.files <- list.files("/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/", full.names = T)

has_month <- function(x) str_detect(x, month.name %>% toupper()) %>% any()


shorten_get_date <- function(x) {
  h <- read_html(x)

  len.test <- h %>%
    html_nodes("div#content b") %>%
    html_text() %>%
    length()

  if (len.test > 0){
    date.str <- h %>%
      html_nodes("div#content b") %>%
      html_text() %>%
      detect(has_month)
  } else {
    date.str <- h %>%
      html_nodes("div#content strong") %>%
      html_text() %>%
      detect(has_month)
  }

  # case for when the returned string has other text
  if (str_length(date.str) > 30){

    date.str %>%
      str_split("\\n")

  }

}

html.files2 <- html.files[700:750]

b.ls <- html.files2 %>%
  map( ~ shorten_get_date(.))


h %>%
  html_nodes("div#content b") %>%
  html_text() %>%
  .[1:4]


title.opt.ls <- list(title.opt1 = list(length = length(title.opt1)),
                     title.opt2 = list(length = length(title.opt2)),
                     title.opt3 = list(length = length(title.opt3)))

# check if h2 title has a month string in it


any. <- any(str_detect(
  title.opt1 %>% tolower(),
  month.name %>% tolower()))

if (any.){
  best.title <- title.opt1
} else {
  best.title <- which(title.opt.ls %>% map("length") == max(title.opt.ls %>% map_dbl("length"))) %>%
    names()
}



if (is.object(best.title)) {
  title. <- best.title
} else {
  title. <- get(best.title)
}


#date.

title.opt3 <- title.opt3 %>%
  map_chr(tolower)

title.opt3 %>%
  str_detect(month.name %>% tolower())
str_detect(
  title.opt3 %>% tolower(),
  month.name %>% tolower())

