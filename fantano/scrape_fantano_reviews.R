library(tidyverse)
library(rvest)


artists <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
  html_elements(".artistTitle") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(artist = value)

albums <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
  html_elements(".albumTitle") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(album = value)

ratings <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
  html_elements(".rating") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(rating = value)


review_link <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
  html_elements(".ratingText") %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  rename(review_link = value)

info_link <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
  html_elements(".albumBlock") %>% 
  html_elements(".image") %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  rename(info_link = value) %>% 
  mutate(info_link = paste0("https://www.albumoftheyear.org", info_link))

fantano <- bind_cols(artists, albums, ratings, review_link, info_link)


  
## this function gets all of the info from each corresponding album link
get_info_tibble <- function(html){
  Sys.sleep(2)
  info_tibble <- read_html(html) %>% 
    html_elements(".detailRow") %>% 
    html_text2() %>% 
    as_tibble() %>% 
    separate(value, into = c("value", "description"), sep = "/", extra = "merge") %>% 
    mutate(description = str_to_lower(description) %>% 
             str_trim()) %>% 
    pivot_wider(names_from = description, values_from = value) %>% 
    janitor::clean_names() %>% 
    rowwise() %>% 
    mutate(website = ifelse("website" %in% names(.), website, NA),
           producer = ifelse("producer" %in% names(.), producer, NA),
           genre = ifelse("genre" %in% names(.), genre, NA),
           label = ifelse("label" %in% names(.), label, NA),
           format = ifelse("format" %in% names(.), format, NA),
           release_date = ifelse("release_date" %in% names(.), release_date, NA)) %>% 
    select(release_date, format, label, genres, producer, website)
  return(info_tibble)
}


## get all of the relevant info link info.
info_link_info <- fantano %>% 
  pull(info_link) %>% 
  as.list() %>% 
  map_df(~get_info_tibble(.))

fantano_full <- fantano %>% 
  bind_cols(info_link_info) %>% 
  mutate(release_date = lubridate::mdy(release_date)) %>% 
  mutate(across(where(is.character), ~str_trim(.))) 

## strategy will be to get all of the pages stuff first, and then can use the info link stuff to get relevant info.


         