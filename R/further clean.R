#' @export
further_cleaning <- function(df) {

  df %>%
    filter(!word %in% c("minister",
                        "people",
                        "government",
                        "dont",
                        "it’s",
                        "weve",
                        "party",
                        "that’s")) %>%
    mutate(word = ifelse(word %in% c("australia", "australians", "australian"),"Australia", word)) %>%
    mutate(word = ifelse(word %in% c("jobs", "job"),"jobs", word)) %>%
    mutate(word = ifelse(word %in% c("shorten"),"Bill Shorten", word)) %>%
    mutate(word = ifelse(word %in% c("turnbull"),"Malcolm Turnbull", word)) %>%
    mutate(word = ifelse(word == "prime","The Prime Minister", word)) %>%
    mutate(word = ifelse(word %in% c("tony"),"Tony Abbott", word)) %>%
    mutate(word = ifelse(word %in% c("labor"),"The Labour Party", word)) %>%
    mutate(word = ifelse(word %in% c("liberal"),"The Liberal Party", word)) %>%

    filter(!word %in% c("malcolm", "abbott", "bill")) %>%
    mutate(speaker. = case_when(
      speaker. == "pm" ~ "Turnbull",
      speaker. == "bill" ~ "Shorten",
      speaker. == "jorn" ~ "Journalist"
    ))


}
