
# processing raw to just keep good dates...

df <- sentences_BS(path = "/TrendLock/data/speeches/sentences_BS_raw.csv")

df_date  <- df %>%
  filter(media.date != "TELEVISION INTERVIEWCNN – INTERVIEW WITH Christiane AMANPOUR THURSDAY, 24 JULY 2014")



df <- sentences_BS()

df_date  <- df %>%
  mutate(media.date2 = lubridate::parse_date_time(media.date %>% str_replace_all(c("," = "", days.))  %>%
                                                    str_trim() %>% str_replace_all(" ", "-"),
                                                  orders = "%d%b%Y"))

df_date <- df_date %>%
  filter(!is.na(media.date2)) %>%
  select(-media.date) %>%
  rename(media.date = media.date2)

write_csv(df_date, "/Users/rosseji/Dropbox/TrendLock/data/speeches/sentences_BS.csv")
