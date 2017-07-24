
# shorten transcripts


home.url <- "http://www.billshorten.com.au/transcripts"

h <- speeches:::read_html_safe(home.url)

if(is.null(h$error)) h <- h$result else stop("error")


# all index pages into a list
base.url <- speeches:::idx_base_url(h)

idx.url.ls <- paste0(
  base.url, 2:speeches:::last_page_val(h)
  ) %>%

  set_names(paste0(
    "idx", 2:speeches:::last_page_val(h)
    ))


# these steps actually makes GET requests...
url.ls <- idx.url.ls %>%
  map( ~ speeches:::scrape_urls(.x, "http://www.billshorten.com.au")) %>%
  flatten()


# saves html to file
url.ls %>%
  walk( ~ speeches:::save_page_html(.x))






