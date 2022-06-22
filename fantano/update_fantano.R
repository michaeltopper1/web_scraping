## Purpose of script: appends the fantano dataset with new stuff
##
## Author: Michael Topper
##
## Date Last Edited: 2022-06-22
##

library(tidyverse)
library(rvest)

fantano_initial <- read_csv("fantano/fantano_albums.csv")

html <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/")


# function for getting album info -----------------------------------------
## this makes sure that each row does not have 0 length tibble
## if it does, it replaces with an NA
check_rows <- function(x) {
  colname <- names(x)
  if (nrow(x) == 0) {
    x <- x %>% 
      add_row(!!sym(colname):=NA)
  }
  else{x}
  return(x)
}

get_info_tibble <- function(html){
  Sys.sleep(5)
  html_page <- read_html(html)
  info_tibble <- html_page %>% 
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
  
  
  
  critic_score <- html_page %>% 
    html_elements(".albumCriticScoreBox") %>% 
    html_elements(".albumCriticScore") %>% 
    html_text2() %>% 
    as_tibble() %>% 
    rename(critic_score = value) %>% 
    check_rows()
  
  number_critic_reviews <- html_page %>% 
    html_elements(".albumCriticScoreBox") %>% 
    html_elements(".text.numReviews") %>% 
    html_text2() %>% 
    as_tibble() %>% 
    rename(number_critic_reviews = value) %>% 
    check_rows()
  
  user_score <- html_page %>% 
    html_elements(".albumUserScoreBox") %>% 
    html_elements(".albumUserScore") %>% 
    html_text2() %>% 
    as_tibble() %>% 
    rename(user_score = value) %>% 
    check_rows()
  
  number_user_reviews <- html_page %>% 
    html_elements(".albumUserScoreBox") %>% 
    html_elements(".text.numReviews") %>% 
    html_text2() %>% 
    as_tibble() %>% 
    rename(number_user_reviews = value) %>% 
    check_rows()
  
  total_length <- html_page %>% 
    html_elements(".totalLength") %>% 
    html_text2() %>% 
    as_tibble() %>% 
    rename(total_length = value) %>% 
    check_rows()
  
  tracks_page <- html_page %>% 
    html_elements(".trackList") %>% 
    html_table() %>% 
    pluck(1) 
  
  if (length(tracks_page != 0)) {
    tracks <- tracks_page %>% 
      extract(X2, "track_time", "(\\d{1,3}:\\d{1,2})", remove = F) %>%
      extract(X2, "track_name", "(.{1,})\\d{1,2}:", remove = F) %>% 
      extract(X2, "features", "feat.(.{1,})", remove = F) %>% 
      mutate(number_tracks = n()) %>% 
      mutate(across(where(is.character), ~str_trim(.))) %>% 
      mutate(X2 = glue::glue("{track_name} [{X3}] ({track_time})")) %>% 
      summarize(tracks = paste(X2, collapse = " | "),
                features = paste(features, collapse = " | "),
                number_tracks = mean(number_tracks, na.rm = T),
                avg_track_rating = mean(X3, na.rm = T))
  }else{
    tracks <- tribble(~tracks, ~features, ~number_tracks, ~avg_track_rating,
                      NA, NA, NA, NA)
  }
  
  info_tibble <- info_tibble %>% 
    bind_cols(tracks, total_length, number_user_reviews, user_score, number_critic_reviews, critic_score)
  return(info_tibble)
}
# scraping all the new albums ---------------------------------------------

## gets all the album information from the needle drop main page

artists <- html %>% 
  html_elements(".artistTitle") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(artist = value)

albums <- html %>% 
  html_elements(".albumTitle") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(album = value)

## this finds the lowest node that all of the albums have in common
rating_row_container <- html %>% 
  html_elements(".ratingRowContainer") 

## checks to make sure everything is there under the node. If not, return NA
ratings <- sapply(rating_row_container, function(x){
  x %>%  html_elements(".rating") %>% 
    html_text2() 
}) %>% sapply(function(x) ifelse(length(x) == 0, NA, x)) %>% 
  as_tibble() %>% 
  rename(rating = value)

review_link <- sapply(rating_row_container, function(x){
  x %>%  html_elements(".ratingText") %>% 
    html_elements("a") %>% 
    html_attr("href")
}) %>% sapply(function(x) ifelse(length(x) == 0, NA, x)) %>% 
  as_tibble() %>% 
  rename(review_link = value)

info_link <- html %>% 
  html_elements(".albumBlock") %>% 
  html_elements(".image") %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  rename(info_link = value) %>% 
  mutate(info_link = paste0("https://www.albumoftheyear.org", info_link))

append_fantano <- bind_cols(artists, albums, ratings, review_link, info_link)

## changes rating to double just to make sure that we can compare with setdiff
append_fantano <- append_fantano %>% 
  mutate(rating = as.double(rating))



albums_new <- setdiff(append_fantano, fantano_initial)

## writing the new fantano_albums.csv
albums_new %>% 
  bind_rows(fantano_initial) %>% 
  write_csv("fantano/fantano_albums.csv")

info_link_info <- albums_new %>% 
  pull(info_link) %>% 
  as.list() %>% 
  map_df(~get_info_tibble(.)) 

fantano_final_initial <- read_csv("fantano/final_fantano.csv")

info_link_info %>% 
  mutate(critic_score = as.double(critic_score)) %>% 
  bind_rows(fantano_final_initial) %>% 
  write_csv("fantano/final_fantano.csv")
