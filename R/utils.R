

read_html_safe <- safely(xml2::read_html)

is_readr <- function(x) str_detect(x, "readr")




last_page_val <- function(h) {
  h %>%
    html_nodes("nav.pagination-container a") %>%
    html_attr("href") %>%
    tail(2) %>%
    head(1) %>%
    str_split("=") %>%
    unlist() %>%
    .[2]
}


idx_base_url <- function(h) {
  h %>%
    html_node("nav.pagination-container a") %>%
    html_attr("href") %>%
    str_split("=") %>%
    unlist() %>%
    .[1] %>%
    paste0("=")
}

next_page_url <- function(h) {
  h %>%
    html_nodes("nav.pagination-container a") %>%
    html_attr("href") %>%
    tail(1)
}

prev_page_url <- function(h) {
  h %>%
    html_nodes("nav.pagination-container a") %>%
    html_attr("href") %>%
    head(1)
}
