
library(tidyverse)
library(rvest)


html <- "https://www.econjobrumors.com/"
pages <- c(2:14)

econ_job_market <- read_html(html) %>% 
  html_elements(css = "#latest") %>% 
  html_table() %>% 
  pluck(1) %>% 
  janitor::clean_names()

for (i in pages) {
  link <- paste0(html, "page/", i)
  read_html(link) %>% 
    html_elements(css = "#latest") %>% 
    html_table() %>% 
    pluck(1) %>% 
    janitor::clean_names()
}
