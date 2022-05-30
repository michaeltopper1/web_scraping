
library(tidyverse)
library(rvest)

send_text = function(message){
  system(paste('osascript -e \'tell application "Messages"\' -e \'send "', message, '" to buddy "805-914-4285" of (service 1 whose service type is iMessage)\' -e \'end tell\''))
}

send_text("hi")
## Read in the results

results <- read_html("https://santabarbara.craigslist.org/search/two-bedroom-apartment?availabilityMode=0&postal=93111&search_distance=15") %>% 
  html_elements(".result-info") %>% 
  html_text() %>% 
  as_tibble() %>% 
  rename("description" = "value") 

results_list <-  results %>% 
  unlist() %>% 
  str_split("\n") 

date <- map_chr(results_list, ~str_trim(.) %>% 
                  str_remove_all("favorite this post|hide this posting|restore this posting|pic|restore") %>% 
                  stringi::stri_remove_empty() %>% 
                  magrittr::extract(1)) %>% 
  as_tibble() %>% 
  rename(date_posted = value) %>% 
  mutate(date_posted = lubridate::mdy(glue::glue("{date_posted} 2022")))

description <- map_chr(results_list, ~str_trim(.) %>% 
                         str_remove_all("favorite this post|hide this posting|restore this posting|pic|restore") %>% 
                         stringi::stri_remove_empty() %>% 
                         magrittr::extract(2)) %>% 
  as_tibble() %>% 
  rename(description = value)

price <- map_chr(results_list, ~str_trim(.) %>% 
                   str_remove_all("favorite this post|hide this posting|restore this posting|pic|restore") %>% 
                   stringi::stri_remove_empty() %>% 
                   magrittr::extract(3)) %>% 
  as_tibble() %>% 
  rename(price = value) %>% 
  mutate(price = parse_number(price))


link <- read_html("https://santabarbara.craigslist.org/search/two-bedroom-apartment?availabilityMode=0&postal=93111&search_distance=15") %>% 
  html_elements(".result-row") %>% 
  html_element("a") %>% 
  html_attr("href") %>%
  as_tibble() %>% 
  rename(link = value)


listings <- bind_cols(date, price, description, link) %>% 
  arrange(desc(date_posted))

listings %>% 
  pull(link)

test_row <- listings %>% 
  slice(1)

test_row

listings_2 <- listings %>% 
  slice(2:n())

plyr::match_df(listings_2, test_row) %>% nrow() == 0




