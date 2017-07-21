
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


# this step actually makes GET requests...
url.ls <- idx.url.ls %>%
  tail(2) %>%
  map( ~ speeches:::scrape_urls(.x, "http://www.billshorten.com.au")) %>%
  flatten()

url.ls %>%
  walk( ~ speeches:::save_page_html(.x))

# and scrape the html and name it!

# can df it...
df <- check$idx2 %>%
  enframe() %>%
  unnest(value)




pres.vec %>%
  map( ~ speeches:::read_html_safe(glue("{base.url}{.x}")))


h <- list(a = list(result = h$result, error = NULL),
          b = list(result = h$result, error = NULL),
          c = list(result = NULL, error = "timeout"))

passed <- function(x) is.null(x$error)

h %>%
  map_if(passed, "result")




home.url <- "http://www.billshorten.com.au/transcripts?page=9"

h <- read_html(home.url)

h %>%
  html_nodes("nav.pagination-container a") %>%
  html_attr("href")


last_page_url(h)

<nav class="pagination-container">
