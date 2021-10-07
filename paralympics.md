Code-along, Week 03: Paralympics
================
Alex Homer
7 October 2021

``` r
library(tidyverse)
library(janitor)
#For this to work, you need the `janitor` package installed.  If it isn't
#installed you'll get an error: try running
# install.packages("janitor")
#in the console, then knitting again.
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

## Cleaning up and joining data

Our data are about countries, but neither our primary nor our secondary
data frame has a column which (consistently) displays the country names.
They have columns for three-letter country codes, but they are different
sets of codes (International Olympic Committee codes
vs. [ISO](https://en.wikipedia.org/wiki/International_Organization_for_Standardization)
codes). So we’ll need to use the lookup tables to add country names.

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
  left_join(ISO_lookup, by = c("location" = "ISO_code"))

paralympic_cleaned <- paralympic_data %>%
  select(-country) %>% #Remove incomplete country column
  left_join(IOC_lookup, by = c("abb" = "IOC_code"))
  #This creates a new country column
```

The problem is even the country names aren’t the same. Let’s see which
OECD countries—under these names—apparently haven’t won Paralympic
medals in the time frame.

``` r
paralympic_oecd_table <- paralympic_cleaned %>%
  inner_join(oecd_cleaned, by = c("country", "year" = "time")) %>%
  #Only keeps countries that have names/years in the OECD data and the Paralympic data
  count(country, name = "n_para")

oecd_countries <- oecd_cleaned %>%
  select(country) %>%
  distinct()

oecd_countries %>%
  left_join(paralympic_oecd_table, by = "country") %>%
  filter(is.na(n_para))
```

    ## # A tibble: 8 × 2
    ##   country                                              n_para
    ##   <chr>                                                 <int>
    ## 1 Czechia                                                  NA
    ## 2 Korea, Republic of                                       NA
    ## 3 United Kingdom of Great Britain and Northern Ireland     NA
    ## 4 United States of America                                 NA
    ## 5 Russian Federation                                       NA
    ## 6 Costa Rica                                               NA
    ## 7 Malta                                                    NA
    ## 8 Romania                                                  NA

``` r
  #If the count is NA, that means it didn't appear in the dataset that was
  #counted, so the full_join statement has had to introduce an NA.

  #There are probably other ways to do this which may be more elegant!
```

We can now manually check these countries against the original datasets.
Of these, Costa Rica, Malta and Romania appear under those names in both
lookup datasets, so they must just not appear in the Paralympic dataset.
The other five appear in the Paralympic data, but have different names.
For consistency (since not all countries appear in the OECD data), we’ll
keep the names from the IOC lookup table that was used for the
Paralympic data [3], so we’ll rename some of the OECD data before
joining.

``` r
paralympic_full <- oecd_cleaned %>%
  mutate(
    country = case_when(
      country == "Czechia"                                              ~ "Czech Republic",
      country == "Korea, Republic of"                                   ~ "South Korea",
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "Great Britain",
      country == "United States of America"                             ~ "United States",
      country == "Russian Federation"                                   ~ "Russia",
      TRUE                                                              ~ country #Other cases
    )
  ) %>%
  right_join(paralympic_cleaned, by = c("country", "time" = "year")) %>%
  #Right join because we want to keep every row in the Paralympic data, and add more
  #information if available
  rename(year = time) %>% #Change back to Paralympic variables
  select(-location) #We don't need the ISO codes any more
```

This is the way to do this in one more pipeline at this stage. In
practice, after doing the investigations above to work out which country
names to change, you might go back and just combine the pipelines from
the start. So it would be fine to just start with the following code to
join the data, and if the purpose of this document weren’t to teach you
about joining data frames that’s what I would have done!

## Cleaning up and joining, take two

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
  left_join(ISO_lookup, by = c("location" = "ISO_code")) %>%
  mutate(
    country = case_when(
      country == "Czechia"                                              ~ "Czech Republic",
      country == "Korea, Republic of"                                   ~ "South Korea",
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "Great Britain",
      country == "United States of America"                             ~ "United States",
      country == "Russian Federation"                                   ~ "Russia",
      TRUE                                                              ~ country #Other cases
    )
  )

paralympic_full <- paralympic_data %>%
  select(-country) %>%
  left_join(IOC_lookup, by = c("abb" = "IOC_code")) %>%
  left_join(oecd_cleaned, by = c("country", "year" = "time")) %>%
  select(-location)
```

The columns are in a different order from what we got the first time
around, but it’s otherwise the same data frame. (Notice this only works
because we never overwrote the originally loaded data frames after
loading them. If we had we’d have to load them again.)

[1] OECD (2021), Population (indicator). doi:
[10.1787/d434f82b-en](https://doi.org/10.1787/d434f82b-en) (Accessed on
06 October 2021)

[2] [List of IOC country
codes](https://en.wikipedia.org/wiki/List_of_IOC_country_codes) and [ISO
3166-1](https://en.wikipedia.org/wiki/ISO_3166-1). Scraped on 6th
October 2021.

[3] Yes, this means that we’re referring to the United Kingdom team as
“Great Britain”, which only refers to three of the four nations of the
UK in most other contexts. I don’t like it either, but it’s pretty
standard (the UK Paralympic team calls itself “ParalympicsGB”), so
you’ll have to take it up with the IOC!
