
# get list of files



file. <- "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_07-42-04-Television_Interview__G20.html"
file. <- "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_05-43-41-TRANSCRIPT_DOORSTOP_MEL.html"
file. <- "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_06-45-00-TV:_Weekend_Sunshine__Lab.html"

file. <- "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_08-10-46-Doorstop_Wyndham_Vale_GP_S.html"
file. <- "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_07-50-13-ENDEAVOUR_HILLS_STATEMENT.html"
file. <- "/Users/rosseji/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_07-49-07-Press_Conference_Belmore_-.html"


h <- read_html(file.)

content1 <- h %>%
  html_nodes("div#content p") %>%
  html_text()

content2 <- h %>%
  html_nodes("div#content b") %>%
  html_text()

content3 <- h %>%
  html_nodes("div#content") %>%
  html_text() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_trim() %>%
  .[. != ""]

cont.ls <- list(content1 = list(length = sum(str_length(content1))),
               content2 = list(length = sum(str_length(content2))),
               content3 = list(length = sum(str_length(content3))))

best.content <- which(cont.ls %>% map("length") == max(cont.ls %>% map_dbl("length"))) %>%
  names()

content <- get(best.content)

title.opt1 <- h %>%
  html_nodes("h2") %>%
  html_text()

title.opt2 <- h %>%
  html_nodes("div#intro strong") %>%
  html_text()

title.opt3 <- h %>%
  html_nodes("div#content b") %>%
  html_text()


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

