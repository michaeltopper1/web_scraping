
library(tidyverse)
library(lubridate)

final_fantano <- read_csv("fantano/final_fantano.csv")

final_fantano <- final_fantano %>% 
  rename(fantano_rating = rating) %>% relocate(number_user_reviews, number_critic_reviews) %>% 
  separate(number_user_reviews, c("number_user_reviews", "extra"), "&") %>% 
  mutate(number_user_reviews = str_replace(number_user_reviews, "Based on ", "")) %>% 
  extract(number_critic_reviews, into = "number_critic_reviews", "(\\d{1,})") %>% 
  mutate(across(c("number_user_reviews", "number_critic_reviews"), ~readr::parse_number(.))) %>% 
  select(-extra) %>% 
  relocate(release_date, artist, album, fantano_rating, review_link) 

final_fantano %>% 
  write_csv("fantano/xxmaster_data.csv")

# final_fantano <- final_fantano %>% 
#   extract(release_date, "release_year", "(\\d{4}$)", remove = F) %>% 
#   extract(release_date, "release_day", "(\\d{1,2}),",remove = F) %>% 
#   extract(release_date, "release_month", "(^[A-Z][a-z]{1,9})\\s", remove = F) %>% 
#   mutate(release_date = mdy(paste0(release_month," ", release_day, ", ", release_year))) 

