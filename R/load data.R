#' Get data for MT speeches as sentences
#'
#' "Anyone who thinks it's smart to cut immigration is sentencing Australia to poverty" - MT
#'
#' @name sentences_MT
#' @param path Defaults to storage
#' @return Returns data frame
#' @export

sentences_MT <- function(path = "/Dropbox/TrendLock/data/speeches/sentences_MT.csv") {
    readr::read_csv(find::this(path))
}


#' Get data for MT speeches as words
#'
#' "Anyone who thinks it's smart to cut immigration is sentencing Australia to poverty" - MT
#'
#' @name word_MT
#' @param path Defaults to storage
#' @return Returns data frame
#' @export
word_MT <- function(path = "/Dropbox/TrendLock/data/speeches/word_MT.csv") {
  readr::read_csv(find::this(path))
}

#' Get data for BS speeches as sentences
#'
#' "Labor must work harder to attract and retain members." - BS
#'
#' @name sentences_BS
#' @param path Defaults to storage
#' @return Returns data frame
#' @export

sentences_BS <- function(path = "/Dropbox/TrendLock/data/speeches/sentences_BS.csv") {
  message("missing dates... :(")
  readr::read_csv(find::this(path))
}

#' Get data for BS speeches as words
#'
#' "Labor must work harder to attract and retain members." - BS
#'
#' @name word_BS
#' @param path Defaults to storage
#' @return Returns data frame
#' @export

word_BS <- function(path = "/Dropbox/TrendLock/data/speeches/word_BS.csv") {
  readr::read_csv(find::this(path))
}

#' Get combine data for BS and MT
#'
#' Blah
#'
#' @name word_comb
#' @param path Defaults to storage
#' @return Returns data frame
#' @export

word_comb <- function(path = "/Dropbox/TrendLock/data/speeches/words_comb.csv") {
  readr::read_csv(find::this(path))
}

#' Get and combine data for MT and BS (gas fun)
#'
#' "Labor must work harder to attract and retain members." - BS
#' "Anyone who thinks it's smart to cut immigration is sentencing Australia to poverty" - MT
#'
#' @name get_and_combine
#' @return Returns data frame
#' @export

get_and_combine <- function(path = "/Dropbox/TrendLock/data/speeches/words_comb.csv") {
    df_MT <- word_MT()
    df <- word_BS() %>%
      clean_text() %>%
      bind_rows(df_MT) %>%
      mutate(week. = floor_date(media.date, "week")) %>%
      further_cleaning()



    df2 <- df %>%
      group_by(media.date, word, speaker.) %>%
      mutate(counter= 1) %>%
      mutate(word.count = sum(counter)) %>%
      distinct(media.date, word, speaker., .keep_all = T)

    df2 <- df2 %>%
      select(-counter, -week.)

    df2 <- df2 %>%
      filter(word.count != 1)

    cords_df <-

  write_csv(df2, find::this(path))

}

#' @export

build_app_data <- function() {
  get_and_combine("/dev/apps/showcase/data/words_comb.csv")

  all_words_df <- read_csv("/Users/rosseji/dev/apps/showcase/data/words_comb.csv")

  top_ls <- build_top_words(all_words_df)

  top_df <- top_ls %>%
    map2(names(top_ls), ~ mutate(.x, id.name = .y)) %>%
    bind_rows() %>%
    mutate(labels = paste(name, value, sep = " "))

  top_df %>%
    write_csv("/Users/rosseji/dev/apps/showcase/data/top_words.csv")

  all_words_df %>%
    filter(!word %in% top_df$value) %>%
    group_by(word) %>%
    summarise(times.said = n()) %>%
    arrange(desc(times.said)) %>%
    head(2000) %>%
    select(word) %>%
    write_csv("/Users/rosseji/dev/apps/showcase/data/other_words.csv")

}

#' @export

build_top_words <- function(df) {


  df_top <- df %>%
    group_by(word, speaker.) %>%
    summarise(times.said = n()) %>%
    arrange(desc(times.said))

  mt <- df_top %>%
    filter(speaker. == "Turnbull") %>%
    head(10) %>%
    pull(word)%>%
    enframe()

  bs <- df_top %>%
    filter(speaker. == "Shorten") %>%
    head(10) %>%
    pull(word)%>%
    enframe()

  jorn <- df_top %>%
    filter(speaker. == "Journalist") %>%
    head(10) %>%
    pull(word) %>%
    enframe()

  df_top <- df %>%
    group_by(word) %>%
    summarise(times.said = n()) %>%
    arrange(desc(times.said))

  energy <- df_top %>%
    filter(word %in% c("gas",
                       "coal",
                       "enviroment",
                       "wind",
                       "solar",
                       "renewable",
                       "energy",
                       "electricity",
                       "power",
                       "grid",
                       "climate")) %>%
    head(10) %>%
    pull(word) %>%
    enframe()

  list(mt = mt,
       bs = bs,
       jorn = jorn,
       energy = energy)

}
