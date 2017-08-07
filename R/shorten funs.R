#' @export

shorten_strip <- function(path = "/TrendLock/data/speeches/shorten html") {

    stop("currently playing up... sorry")
  html_files <- list.files(glue("{find::this(path)}"), full.names = T)

  df <- html_files %>%
    sample(10) %>%
    map( ~ .x %>% shorten_strip_internals()) %>%
    bind_rows()

  write_csv(df, glue("{find::this('/TrendLock/data/speeches/sentences_BS.csv')}"))
}


shorten_strip_internals <- function(x) {

  message(x)
  h <- read_html(x)

  is_audio <- h %>%
    html_nodes("div#content") %>%
    as.character() %>%
    str_detect(".mp3")

  if(is_audio) {
    message("Audio :(")
    return(NULL)
  }

  date.loc <- h %>%
    shorten_scrape_date()

  if(date.loc == "No date") return(NULL)

  df <- h %>%
    shorten_scrape_content() %>%
    enframe() %>%
    set_names(c("pc.indx", "sent")) %>%
    shorten_add_speaker()

  print(date.loc["date"])
  print(date.loc["location"])


  df %>%
    mutate(media.date = date.loc["date"],
           media.loc = date.loc["location"])

}



shorten_scrape_content <- function(h) {

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

  content <-  get(best.content)

  # remove all lines that are in all upper case... seem to be mostly not the speech
  content <- content[content != toupper(content)]

  # remove strings with "SUBJECT" in it
  has_SUBJECT <- function(x) str_detect(x, "SUBJECT")

  subj_objs <- content %>%
    detect(has_SUBJECT)

  content <- content[!content %in% subj_objs]

  # remove common phrases

  # break content at full stops and question marks
  content <- content %>%
    str_split("\\.") %>%
    unlist() %>%
    str_split("\\?") %>%
    unlist() %>%
    str_split(":") %>%
    unlist() %>%
    str_trim()

  not_cool <- c("Do you like this post?","Tweet", "ENDS","or",
                "", "Do you like this post","Be the first to comment",
               "Sign in with","Facebook","Twitter","Optional email code",
                "Or sign in with email","Remember me", "Create an account",
               "Optional email code", "Create an account", "Sign in with email",
               "Please check your e-mail for a link to activate your account")

  content <- content[!content %in% not_cool]

  content
}


shorten_scrape_date <- function(h) {

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
    html_text() %>%
    detect(has_month)

  if(!is_null(date.str4)) {
    date.str <- date.str4 %>%
      str_split("-") %>%
      unlist() %>%
      str_trim() %>%
      detect(has_month)
    return(date.str)
  }

  cont.ls <- list(content1 = class(date.str1),
                  content2 = class(date.str2),
                  content3 = class(date.str3))

  if(is.null(date.str1) & is.null(date.str2) & is.null(date.str3)) message("no date..."); return("No date")

  best.content <- which(!is_null(cont.ls)) %>%
    names()


  date.str <- get(best.content)

  if(is_null(date.str)) { message("No date"); return("No date")}
  message("Has date -----")
  filtered.phrase <- c("E&OE TRANSCRIPT", "RADIO INTERVIEW", "DOORSTOP", "PRESS CONFERENCE", "DOORSTOP INTERVIEW")

  #case for when the returned string has other text
  if (str_count(date.str) > 30){

    # gets more than date, will try and preserve location
    comp.str <- date.str %>%
      str_split("\\\n") %>%
      unlist()

    if(length(comp.str) > 1){

      date.part <- comp.str %>%
        detect(has_month)

      other.parts <- comp.str[comp.str != date.part]
      other.parts <- other.parts[!str_count(other.parts) > 30]

      type. <- "1"

      # matches list of Austalian locations

      if (any(str_detect(other.parts, "SUBJECT"))){
        comp.str <- comp.str %>%
          strsplit("SUBJECT") %>%
          unlist()

        date.part <- comp.str %>%
          detect(has_month)

        other.parts <- comp.str[comp.str != date.part]
        other.parts <- other.parts[!str_count(other.parts) > 30]
        type. <- "1a"
      }

      # case for SUBJECT in object
      if (str_detect(date.part, "SUBJECT")){
        comp.str <- comp.str %>%
          strsplit("SUBJECT") %>%
          unlist()

        date.part <- comp.str %>%
          detect(has_month)
        type. <- "1b"
      }

      other.parts <- other.parts[!other.parts %in% filtered.phrase]

      c(type = type., date = date.part, location = other.parts)


    } else { # case for if string is single and still long
      comp.str <- comp.str %>%
        strsplit("SUBJECT") %>%
        unlist()

      date.part <- comp.str %>%
        detect(has_month)

      other.parts <- comp.str[comp.str != date.part]
      other.parts <- other.parts[!str_count(other.parts) > 30]
      other.parts <- other.parts[!other.parts %in% filtered.phrase]
      c(type = 2, date = date.part, location = other.parts)

    }

  } else { # case when single short string
    c(type = 3, date = date.str, location = NA)
  }
}



#' @export


shorten_scrape_urls <- function(idx.url, base.url = base.url) {

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

#' @export

shorten_save_page_html <- function(url.comp) {

  h <- speeches:::read_html_safe(url.comp["url"])

  if(is.null(h$error)) h <- h$result else return(c(url.comp["url"], error = T))

  title. <- h %>%
    html_nodes("h2") %>%
    html_text() %>%
    str_trunc(30)

  title. <- glue("{Sys.time() %>% str_replace_all(c(':' = '-'))}-{title.}")

  title. <- str_replace_all(title., c(" - " = "_",
                                      " " = "_",
                                      "\\," = "",
                                      "\\." = "",
                                      "\\$" = "",
                                      "/" = "_"))

  message(title.)
  write_html(h, glue("{find::this()}/TrendLock/data/speeches/shorten html/media-{title.}.html"))
  write_csv(tibble(url = url.comp["url"]), glue("{find::this()}/TrendLock/data/speeches/shorten status/completed_urls.csv"), append = T)
  Sys.sleep(5)

}


