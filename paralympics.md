Code-along, Week 03: Paralympics
================
Alex Homer
7 October 2021

``` r
library(tidyverse)
library(janitor)
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
paralympic_data <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv"
)

oecd_data <- read_csv("data/oecd_pop.csv")
ISO_lookup <- readRDS("data/ISO_lookup.rds")
IOC_lookup <- readRDS("data/IOC_lookup.rds")
```

Cleaning up data

``` r
oecd_cleaned <- oecd_data %>%
  clean_names() %>%
  select(-c(flag_codes, indicator, frequency)) %>%
  #mutate(measure = tolower(measure), subject = tolower(subject)) %>%
  pivot_wider(names_from = c(measure, subject), values_from = value) %>%
  rename(
    annual_growth = AGRWTH_TOT, pop_men = MLN_PER_MEN,
    pop_women = MLN_PER_WOMEN, pop_total = MLN_PER_TOT
  ) %>%
  left_join(ISO_lookup, by = c("location" = "ISO_code")) #%>%
  # mutate(
  #   case_when
  # )

oecd_table <- oecd_cleaned %>%
  count(country)
```

``` r
oecd_data %>%
  clean_names() %>%
  select(-c(flag_codes, indicator, frequency))
```

    ## # A tibble: 15,848 × 5
    ##    location subject measure  time value
    ##    <chr>    <chr>   <chr>   <dbl> <dbl>
    ##  1 AUS      TOT     AGRWTH   1951  2.97
    ##  2 AUS      TOT     AGRWTH   1952  2.55
    ##  3 AUS      TOT     AGRWTH   1953  2.07
    ##  4 AUS      TOT     AGRWTH   1954  1.94
    ##  5 AUS      TOT     AGRWTH   1955  2.37
    ##  6 AUS      TOT     AGRWTH   1956  2.45
    ##  7 AUS      TOT     AGRWTH   1957  2.28
    ##  8 AUS      TOT     AGRWTH   1958  2.10
    ##  9 AUS      TOT     AGRWTH   1959  2.17
    ## 10 AUS      TOT     AGRWTH   1960  2.17
    ## # … with 15,838 more rows

``` r
temp <- clean_names(oecd_data)
temp <- select(temp, -c(flag_codes, indicator, frequency))
```

``` r
paralympic_oecd <- paralympic_data %>%
  select(-country) %>%
  left_join(IOC_lookup, by = c("abb" = "IOC_code")) %>%
  inner_join(oecd_cleaned, by = c("country", "year" = "time"))
  
paralympic_table <- paralympic_oecd %>%
  count(country) %>%
  rename(n.para = n)

oecd_table %>%
  full_join(paralympic_table, by = "country") %>%
  filter(is.na(n.para))
```

    ## # A tibble: 8 × 3
    ##   country                                                  n n.para
    ##   <chr>                                                <int>  <int>
    ## 1 Costa Rica                                              71     NA
    ## 2 Czechia                                                 71     NA
    ## 3 Korea, Republic of                                      71     NA
    ## 4 Malta                                                   71     NA
    ## 5 Romania                                                 71     NA
    ## 6 Russian Federation                                      71     NA
    ## 7 United Kingdom of Great Britain and Northern Ireland    71     NA
    ## 8 United States of America                                71     NA

[1] OECD (2021), Population (indicator). doi:
[10.1787/d434f82b-en](https://doi.org/10.1787/d434f82b-en) (Accessed on
06 October 2021)

[2] [List of IOC country
codes](https://en.wikipedia.org/wiki/List_of_IOC_country_codes) and [ISO
3166-1](https://en.wikipedia.org/wiki/ISO_3166-1). Scraped on 6th
October 2021.
