library(tidyverse)
library(rvest)


fantano <- read_csv("fantano/fantano_albums.csv")

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


## gets all the information I want

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


info_link_info <- fantano %>% 
  pull(info_link) %>% 
  as.list() %>% 
  map_df(~get_info_tibble(.))

final_fantano_data <- fantano %>% 
  bind_cols(info_link_info) 


final_fantano_data %>% 
  write_csv("fantano/final_fantano.csv")
