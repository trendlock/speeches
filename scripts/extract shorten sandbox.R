
# get list of files



file. <- "/Users/Lilian/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_07-42-04-Television_Interview__G20.html"
file. <- "/Users/Lilian/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_05-43-41-TRANSCRIPT_DOORSTOP_MEL.html"
file. <- "/Users/Lilian/Dropbox/TrendLock/data/speeches/shorten html/media-2017-07-22_06-45-00-TV:_Weekend_Sunshine__Lab.html"



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

cont.ls <- list(content1 = list(length = length(content1)),
               content2 = list(length = length(content2)),
               content3 = list(length = length(content3)))

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

if (!any(str_detect(
  title.opt1 %>% tolower(),
  c(month.abb, month.name)%>% tolower()))){

}

best.title <- which(title.opt.ls %>% map("length") == max(title.opt.ls %>% map_dbl("length"))) %>%
  names()

content <- get(best.content)


tibble()


