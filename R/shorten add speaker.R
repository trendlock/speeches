
#' @export
shorten_add_speaker <- function(df, keep = F) {

  bill_str <- c("BILL SHORTEN", "SHORTEN", "BILL", "BILL SHORTEN, LEADER OF THE OPPOSITION")

  df <- df %>%
    mutate(
      sent = str_trim(sent),  #if line is in upper but not BILL...
      speaker. = case_when(toupper(sent) == sent & !toupper(sent) %in% bill_str ~ "jorn",
                           toupper(sent) == sent & toupper(sent) %in% bill_str ~ "bill"))

  df <- df %>%
    fill(speaker.) %>% # annying but this line not working unless split from the call above??
    mutate(speaker. = case_when(is.na(speaker.) ~ "bill",
                                !is.na(speaker.) ~ speaker.))


  if (keep){
    df
  } else {
    df %>%
      filter(!toupper(sent) == sent)
  }
}

#' shorten_add_speaker <- function(df, keep = T) {
#'
#'
#'
#'   bill_str <- c("BILL SHORTEN", "SHORTEN", "BILL")
#'   jorn_strs <- c("JOURNALIST", "QUESTION", "REPORTER", "REPORTER")
#'
#'   df <- df %>%
#'     mutate(
#'       sent = str_trim(sent),
#'       speaker. = case_when(sent %in% jorn_strs ~ "jorn",
#'                            sent %in% bill_str ~ "bill"))
#'
#'   df <- df %>%
#'     fill(speaker.) %>% # annying but this line not working unless split from the call above??
#'     mutate(speaker. = case_when(is.na(speaker.) ~ "bill",
#'                                 !is.na(speaker.) ~ speaker.)) %>%
#'     filter(!sent %in% c(jorn_strs, bill_str))
#'
#'
#'   if (keep){
#'     df
#'   } else {
#'     df %>%
#'       filter(speaker. == "bill")
#'   }
#' }
