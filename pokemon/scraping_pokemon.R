##Scrapping pokemon card website
## can't seem to get this to load the entire page!

library(RSelenium)
library(rvest)
library(tidyverse)

## opens the driver
rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]

link_texts <- c("Base Set", "Promo", "Fossil")
## navigates to the correct page
remDr$navigate("https://www.pricecharting.com/category/pokemon-cards")

for (name in link_texts) {
  ## finds the link and clicks on it
  remDr$findElement(using = "link text", name)$clickElement()
  ## gets the table path
  remDr$findElement(using = "css", "body")$clickElement()
  ## finds the table - this line may be extraneous
  table <- remDr$findElement(using = "css", "body")
  ## scrolls to the bottom of the table
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(1)
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(1)
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(1)
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(1)
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(1)
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(1)
  ## get the entire page source that's been loaded
  html <- remDr$getPageSource()[[1]]
  ## read in the page source
  page <- read_html(html)
  
  data_name <- str_to_lower(str_replace(name, " ","_"))
  ## extract the tabular table
  df <- page %>% 
    html_elements("#games_table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    select(1:4)
  assign(data_name, df)
  Sys.sleep(3)
  remDr$navigate("https://www.pricecharting.com/category/pokemon-cards")
}

## close driver
remDr$close()
rD$server$stop()

