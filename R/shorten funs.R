
scrape_urls <- function(idx.url, base.url = base.url) {

  h <- speeches:::read_html_safe(idx.url)

  if(is.null(h$error)) h <- h$result else return("error")

  urls <- h %>%
    html_nodes("div.grid a") %>%
    html_attr("href")  %>%
    paste0(base.url, .)

  output <- h %>%
    html_nodes("div.grid span.date") %>%
    html_text() %>%
    strsplit(" - ") %>%
    map( ~ set_names(.x, c("date", "type")))

  Sys.sleep(5)
  message(glue("Pages from ~ {output %>% map_chr('date') %>%.[1]}"))

  output %>%
      map2(urls, ~ c(.x, url = .y)) %>%
      set_names(glue("url.{1:length(.)}"))

}


save_page_html <- function(url.comp) {

  h <- speeches:::read_html_safe(url.comp["url"])

  if(is.null(h$error)) h <- h$result else return(c(url.comp["url"], error = T))

  title. <- h %>%
    html_nodes("h2") %>%
    html_text() %>%
    str_trunc(30)

  title. <- glue("{Sys.time() %>% str_replace_all(c(':' = '-'))}-{title.}")

  title. <- str_replace_all(title., c(" - " = "_", " " = "_", "\\," = "",
                                      "\\." = "", "\\$" = "",
                                      "/" = "_"))

  message(title.)
  write_html(h, glue("{find::this()}/TrendLock/data/speeches/shorten html/media-{title.}.html"))
  write_csv(tibble(url = url.comp["url"]), glue("{find::this()}/TrendLock/data/speeches/shorten status/completed_urls.csv"), append = T)
  Sys.sleep(5)

}
