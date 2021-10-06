Code-along, Week 03: Paralympics
================
Alex Homer
7 October 2021

``` r
library(tidyverse)
```

## Read data

The primary data are drawn from the “Tidy Tuesday” project: [2021 Week
32:
Paralympics](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-03/readme.md)
(credit: [International Paralympic
Committee](https://db.ipc-services.org/sdms/hira/web/index)). The
secondary data are from the Organisation for Economic Co-operation and
Development [1]. The lookup tables were scraped by me from Wikipedia [2]

``` r
paralympic_data <-read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv"
)

oecd_data <- read_csv("data/oecd_pop.csv")
ISO_lookup <- readRDS("data/ISO_lookup.rds")
IOC_lookup <- readRDS("data/IOC_lookup.rds")
```

The rest of this document will be filled in during Thursday’s live
coding session.

[1] OECD (2021), Population (indicator). doi:
[10.1787/d434f82b-en](https://doi.org/10.1787/d434f82b-en) (Accessed on
06 October 2021)

[2] [List of IOC country
codes](https://en.wikipedia.org/wiki/List_of_IOC_country_codes) and [ISO
3166-1](https://en.wikipedia.org/wiki/ISO_3166-1). Scraped on 6th
October 2021.
