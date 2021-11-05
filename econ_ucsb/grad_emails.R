## Purpose of script: scrapes the entire phd students and their corresponding emails on ucsb's site
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-05
##

library(rvest)
library(RSelenium)
library(tidyverse)


## opens the driver
rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]


remDr$navigate("https://econ.ucsb.edu/")
remDr$findElement(using = "link text", "People")$clickElement()
remDr$findElement(using = "link text", "Graduate Students")$clickElement()

## getting page source for directory
html <- remDr$getPageSource()[[1]]
directory_page <- read_html(html)


# getting all graduate students -------------------------------------------

students <- directory_page %>% 
  html_elements(".group-second h3") %>% 
  html_text(trim = T) %>% 
  as_tibble() %>% 
  rename(student_name = value) 
student_names <- students %>% 
  pull()

final_data <- tibble(student = student_names, email = rep("", length(student_names)))

# looping through clicking each student -----------------------------------

for (i in 1:length(student_names)) {
  ## click on the students name 
  remDr$findElement(using = "link text", student_names[i])$clickElement()
  ## get the source of the student name
  student_page <- remDr$getPageSource()[[1]]
  ## extract their email address
  email <- read_html(student_page) %>% 
    html_elements(".field--item [href $='edu']") %>% 
    html_text(trim = T) %>% 
    as_tibble() %>% 
    rename(email = value) %>% 
    slice(1) %>% pull()
  final_data$email[[i]] <- email
  Sys.sleep(2)
  remDr$goBack()
}

save(final_data, file = "econ_ucsb/grad_emails.Rda")

remDr$close()

