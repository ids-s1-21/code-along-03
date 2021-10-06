#This code was used to scrape the data for the country codes.
#You do not need to run this code to participate in the code-along!
#It's included for reference.
#It's also not guaranteed to break if Wikipedia changes!  It worked on
#6th October 2021.

library(rvest)
library(robotstxt)

paths_allowed("https://en.wikipedia.org")
#Returns TRUE (at time of writing!)

#IOC:

IOC_page <- read_html("https://en.wikipedia.org/wiki/List_of_IOC_country_codes")

codes <- IOC_page %>%
  html_elements(".monospaced:nth-child(2)") %>%
  html_text()

countries <- IOC_page %>%
  html_elements("td:nth-child(2) a:nth-child(2)") %>%
  html_text()

IOC_lookup <-
  tibble(country = countries, IOC_code = codes)
saveRDS(IOC_lookup, "data/IOC_lookup.rds")

#ISO:

ISO_page <- read_html("https://en.wikipedia.org/wiki/ISO_3166-1")

codes2 <- ISO_page %>%
  html_elements("td:nth-child(3) .monospaced") %>%
  html_text()

countries2 <- ISO_page %>%
  html_elements("span a , a:nth-child(2)") %>%
  html_text() %>%
  magrittr::extract(15:263)

ISO_lookup <-
  tibble(country = countries2, ISO_code = codes2)
saveRDS(ISO_lookup, "data/ISO_lookup.rds")
