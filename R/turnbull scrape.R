


turnbull_strip <- function(path = "/TrendLock/data/speeches/turnbull html") {

  html_files <- list.files(glue("{find::this(path)}"), full.names = T)

  df <- html_files %>%
    map( ~ .x %>% turnbull_strip_internals()) %>%
    bind_rows()

  write_csv(df, glue("{find::this(/TrendLock/data/speeches/turnbull html/sentences_MT.csv)}"))

}

turnbull_strip_internals <- function(x) {

  message(x)
  h <- read_html(x)

  tbl <- h %>%
    html_nodes("p") %>%
    html_text() %>%
    enframe() %>%
    set_names(c("pc.indx", "sent"))

  media.title <- h %>%
    html_nodes("h1") %>%
    html_text()


  media.loc <- h %>%
    html_nodes("p.field-location") %>%
    html_text() %>%
    str_trim()
  if (length(media.loc) == 0) media.loc <- NA

  media.date <- tbl[1,] %>%
    separate(sent, c("date.", "x", "y"), sep = "\\|") %>%
    pull(date.) %>%
    str_trim() %>%
    str_replace_all(c(" " = "-", "th" = "")) %>%
    parse_date_time2(orders = "d!-Om!-Y!")

  tbl[-1,] %>%
    mutate(media.date,
           media.title,
           media.loc)
}
