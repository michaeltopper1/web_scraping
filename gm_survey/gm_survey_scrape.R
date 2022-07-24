library(tidyverse)
library(rvest)

link <- "https://www.nba.com/news/2021-22-gmsurvey"
read_html(link) %>% 
  html_elements("p") %>% 
  html_text2() %>% 
  str_detect("?$")

answers <- read_html(link) %>% 
  html_elements("h4 + p") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(answers = value)


questions <- read_html(link) %>% 
  html_elements("h4") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(questions = value) %>% 
  mutate(question = ifelse(str_detect(questions, "\\?$"), 1, 0)) %>% 
  filter(question ==1)

survey <- bind_cols(questions, answers)

survey %>% 
  mutate(answers = strsplit(answers, "\\d\\.")) %>% 
  unnest(answers) %>% View()
