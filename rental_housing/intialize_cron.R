library(cronR)

cmd <- cron_rscript(here::here(paste0("rental_housing/scrape_housing.R")))
cron_add(cmd, frequency = "hourly", description = "scraping Craigslist")
