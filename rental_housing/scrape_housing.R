
library(tidyverse)
library(rvest)
library(gmailr)


# get directory -----------------------------------------------------------
directory <- getwd()

# emailing ----------------------------------------------------------------

## setting up authentication
gm_auth_configure(key = "AIzaSyABdgLBK3-gs-CAjsBZDXGUCFiMQXpk9T4",
                  path = "/Users/michaeltopper/web_scraping/rental_housing/client_email.json")

##choosing the email which I would like to send from
gm_auth(email = "miketopper123@gmail.com")

## Read in the results
link <- "https://santabarbara.craigslist.org/search/apa?sort=date&availabilityMode=0&postal=93111&search_distance=20"

html_page <- read_html(link) 
description <- html_page %>% 
  html_elements(".result-info") %>% 
  html_elements(".result-heading") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(description = value)

price <- html_page %>% 
  html_elements(".result-info") %>% 
  html_elements(".result-meta") %>% 
  html_elements(".result-price") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(price = value) %>% 
  mutate(price = readr::parse_number(price))

## takes a root node that everyone has in common
meta_info <- html_page %>% 
  html_elements(".result-info") %>% 
  html_elements(".result-meta") 

## takes the root note that is in common and finds info for each - replaces with NA if not in there.
bedrooms <- sapply(meta_info, function(x) {
  x %>% html_elements(".housing") %>% 
    html_text2()
}) %>% 
  sapply(function(x) {
    ifelse(length(x) == 0, NA, x)
  }) %>% 
  as_tibble() %>% 
  separate(value, into = c("bedrooms", "square_feet"), "-", extra = "merge") %>% 
  mutate(square_feet = ifelse(str_detect(bedrooms, "ft2"), bedrooms, square_feet )) %>% 
  extract(bedrooms, into = "bedrooms", "(\\d{1,2})br") %>% 
  extract(square_feet, "square_feet", "(\\d{1,4})ft2") 


##extracts the links needed
link <- html_page %>% 
  html_elements(".result-row") %>% 
  html_elements(".result-heading") %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  rename(link = value)


housing <- bind_cols(description, bedrooms, link, price) %>% 
  mutate(bedrooms = readr::parse_number(bedrooms),
         square_feet = readr::parse_number(square_feet))

## writing the initial data set
# housing %>%
#   write_csv("rental_housing/initial_housing.csv")

initial_housing <- read_csv("/Users/michaeltopper/web_scraping/rental_housing/initial_housing.csv")

differences_michael <- setdiff(housing, initial_housing) %>% 
  filter(bedrooms < 3 & price < 4000)

differences_danny <- setdiff(housing, initial_housing) %>% 
  filter(bedrooms < 2 & price <= 2500) 
  
differences_sheng <- setdiff(housing, initial_housing) %>% 
  filter(price < 7000)




text_user <- function(phone_number, provider, differences) {
  
  ## getting the provider name to lowercase
  provider <- provider %>% 
    str_trim() %>% str_to_lower()
  
  ## getting the provider to put in the right ending for gmail
  if ( provider== "verizon") {
    email_to <- paste0(phone_number, "@vtext.com")
  }
  if (provider == "att") {
    email_to <- paste0(phone_number, "@txt.att.net")
  }
  if (provider == "tmobile"){
    email_to <- paste0(phone_number, "@tmomail.net")
  }
  if (provider == "sprint") {
    email_to <- paste0(phone_number, "@messaging.sprintpcs.com")
  }
  
  if (nrow(differences) != 0) {
    link <- differences %>% 
      pull(link)
    
    bedroom_numbers <- differences %>% 
      select(bedrooms) %>% 
      pull()
    
    ## getting all the prices of the new stuff
    prices <- differences %>% 
      select(price) %>% 
      pull()
    
    
    for (i in 1:length(link)) {
      ## create the subject line
      email_subject <- paste0("Craigslist Alert! ", bedroom_numbers[i], " bedroom: ", "$", prices[i])
      
      ## this will be the message body which is just a hyperlink to the craigslist post
      housing_link <- link[i]
      
      ## send the text
      text <-
        gm_mime() %>%
        gm_to(email_to) %>%
        gm_from("miketopper123@gmail.com") %>%
        gm_subject(email_subject) %>%
        gm_text_body(housing_link)
      gm_send_message(text)
    }
    
    housing %>%
      write_csv("/Users/michaeltopper/web_scraping/rental_housing/initial_housing.csv")
  } else {
    message <- "No new properties"
    text <-
      gm_mime() %>%
      gm_to(email_to) %>%
      gm_from("miketopper123@gmail.com") %>%
      gm_subject("Sorry there bucko!") %>%
      gm_text_body(message)
    gm_send_message(text)
  }
  
}

text_user(phone_number = "6266367829", provider = "tmobile", differences = differences_sheng)
text_user(phone_number = "8059144285", provider = "verizon", differences = differences_michael)
text_user(phone_number = "9166900204", provider = "verizon", differences = differences_danny)


