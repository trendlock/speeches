

# location analysis sandbox

df_words <- read_csv("/Users/rosseji/dev/apps/showcase/data/words_comb.csv")

locations_index <- read_csv("/Users/rosseji/dev/apps/showcase/data/words_loc.csv")
locations_index <- locations_index %>%
  distinct(Location, .keep_all = T)

locations_index <- locations_index %>%
  mutate(media.loc = tolower(Location))%>%
  select(-top_words,-Location, -Speaker)
#f_coords <- fetch::geo_coords(unique(df_words$media.loc))
# could clean up some queries

df_words <- df_words %>%
  mutate(media.loc = tolower(media.loc))

df_words <- inner_join(df_words, locations_index, by = "media.loc")

# df_coords <- df_coords %>%
#   rename(media.loc = where.)

df_words2 <- df_words %>%
  #inner_join(df_coords, by = "media.loc") %>%
  filter(!is.na(lon),
         media.loc != "/S:",
         speaker. != "Journalist")


df_loc <- df_words2 %>%
  mutate(Period = lubridate::floor_date(media.date, "year")) %>%
  group_by(media.loc, speaker., word) %>%
  mutate(word.count.loc = sum(word.count)) %>%
  distinct(media.loc, speaker., word, .keep_all = T)


df_loc_top <- df_loc %>%
  split(list(.$media.loc, .$speaker.)) %>%
  map( ~ .x %>% arrange(desc(word.count.loc)) %>%
         head(10))

# now some df's will be empty becuase one of the speakers didn't speak there.



empty_to_null <- function(df) {
  if( dim(df)[1] != 0){
    df
  } else {
    NULL
  }
}

df_loc_topx <- df_loc_top %>%
  map( ~ empty_to_null(.x))


# remove Null

df_loc_topx2 <- df_loc_topx %>%
  map( ~ !is_null(.x)) %>%
  unlist() %>%
  set_names(NULL)

df_loc_top <- df_loc_top[df_loc_topx2]


df_loc_top$`abc am.Shorten`$media.date %>% as.character()

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

g_paste <- function(...) {
  x <- c(...) %>% str_c(collapse = "+") %>%
    str_replace_all(" ", "\\+")
  glue("https://www.google.com.au/search?q={x}")
}


df_loc_top_spread <- df_loc_top %>%
  map( ~ .x %>%
         select(speaker., lon, lat, media.loc, media.date, word)) %>%
  map( ~ tibble(Speaker = first(.x$speaker.),
                lon = first(.x$lon),
                lat = first(.x$lat),
                Location = first(.x$media.loc)  %>% toupper(),
                Date = c(unique(.x$media.date) %>% as.character()) %>% str_c(collapse = ", "),
                top_words = str_c(.x$word, collapse = ", ") %>% simpleCap(),
                Search = glue("<a href='{g_paste(first(.x$speaker.), first(.x$media.loc), unique(.x$media.date) %>% as.character())}' target='_blank'>Search for speech</a>")))

df_to_map <- df_loc_top_spread %>%
  bind_rows()
df_to_map$Search[1]
write_csv(df_to_map, "/Users/rosseji/dev/apps/showcase/data/words_loc.csv")

library(leaflet)
library(htmltools)

leaflet(df_to_map) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~paste("Speaker", Speaker, "<br>",
                                        "Location:", Location, "<br>",
                                        "Date:", Date, "<br>",
                                        "Top words:", top_words, "<br>",
                                        Search))
