library(tidyverse)
library(rvest)

fantano <- tribble(~artist, ~album, ~rating, ~review_link, ~info_link)

for (i in 1:300) {
  Sys.sleep(5)
  html <- read_html(paste0("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/", i, "/"))
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
  
  fantano <- fantano %>% bind_rows(append_fantano)
}


fantano_distinct <- fantano %>% 
  distinct()

fantano_distinct %>% 
  write_csv("fantano/fantano_albums.csv")
##how to solve the missing reviews?

# 
# artists <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
#   html_elements(".artistTitle") %>% 
#   html_text2() %>% 
#   as_tibble() %>% 
#   rename(artist = value)
# 
# albums <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
#   html_elements(".albumTitle") %>% 
#   html_text2() %>% 
#   as_tibble() %>% 
#   rename(album = value)
# 
# ratings <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
#   html_elements(".rating") %>% 
#   html_text2() %>% 
#   as_tibble() %>% 
#   rename(rating = value)
# 
# 
# review_link <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
#   html_elements(".ratingText") %>% 
#   html_elements("a") %>% 
#   html_attr("href") %>% 
#   as_tibble() %>% 
#   rename(review_link = value)
# 
# info_link <- read_html("https://www.albumoftheyear.org/publication/57-the-needle-drop/reviews/") %>% 
#   html_elements(".albumBlock") %>% 
#   html_elements(".image") %>% 
#   html_elements("a") %>% 
#   html_attr("href") %>% 
#   as_tibble() %>% 
#   rename(info_link = value) %>% 
#   mutate(info_link = paste0("https://www.albumoftheyear.org", info_link))
# 
# fantano <- bind_cols(artists, albums, ratings, review_link, info_link)