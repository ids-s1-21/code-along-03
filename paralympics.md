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

#To install the "emo" package (also necessary to knit this) run:
# devtools::install_github("hadley/emo")
#You may need to install the "devtools" package first, in the normal way.
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

One thing we can notice is that not all the countries in the
`oecd_cleaned` data yet have a name. Let’s see which.

``` r
oecd_cleaned %>%
  select(country, location) %>%
  distinct() %>%
  filter(is.na(country)) %>%
  select(location)
```

    ## # A tibble: 4 × 1
    ##   location
    ##   <chr>   
    ## 1 OECD    
    ## 2 WLD     
    ## 3 G20     
    ## 4 EU27

Seems like the remaining values are overall figures for the OECD, the
world, the [G20](https://en.wikipedia.org/wiki/G20) and for what is now
the
[EU27](https://en.wikipedia.org/wiki/Member_state_of_the_European_Union).
We’ll add names, so that it doesn’t get confused later with events that
don’t have a country name in the Paralympic data.

``` r
oecd_cleaned <- oecd_cleaned %>%
  mutate(
    country = case_when(
      location == "OECD" ~ "Organisation for Economic Co-operation and Development",
      location == "WLD"  ~ "World",
      location == "G20"  ~ "Group of 20",
      location == "EU27" ~ "European Union"
    )
  )
```

It might also be useful to know what fraction of the world population
each country represented at each year. Let’s do that.

``` r
#First get the world data out...
world_pop <- oecd_cleaned %>%
  filter(location == "WLD") %>%
  select(time, pop_total, pop_men, pop_women) %>%
  rename(
    world_pop_total = pop_total,
    world_pop_men = pop_men,
    world_pop_women = pop_women
  )

#... then add it back in using a join
oecd_cleaned <- oecd_cleaned %>%
  left_join(world_pop, by = "time") %>%
  mutate(
    frac_men = pop_men/world_pop_men,
    frac_women = pop_women/world_pop_women,
    frac_total = pop_total/world_pop_total
  ) %>%
  select(-c(world_pop_total, world_pop_men, world_pop_women))

#We could have achieved this all in one pipeline using a `right_join`, but this
#is hopefully easier to follow!
```

💡 *This is a useful general technique for moving summary data that’s
been stored in its own rows, into a new column attached to all the rows
to which it relates. Could you work out how to do the same so that, for
the EU countries in the OECD, the proportion of the EU population which
they represent is shown?* 💡

A problem remains for joining the data, though: even the country names
aren’t entirely the same. Let’s see which OECD countries—under these
names—apparently haven’t won Paralympic medals in the time frame.

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

    ## # A tibble: 4 × 2
    ##   country                                                n_para
    ##   <chr>                                                   <int>
    ## 1 Organisation for Economic Co-operation and Development     NA
    ## 2 World                                                      NA
    ## 3 Group of 20                                                NA
    ## 4 European Union                                             NA

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
  pivot_wider(names_from = c(measure, subject), values_from = value) %>%
  rename(
    annual_growth = AGRWTH_TOT, pop_men = MLN_PER_MEN,
    pop_women = MLN_PER_WOMEN, pop_total = MLN_PER_TOT
  ) %>%
  left_join(ISO_lookup, by = c("location" = "ISO_code")) %>%
  mutate(
    country = case_when( #Combining into one case_when
      location == "OECD" ~ "Organisation for Economic Co-operation and Development",
      location == "WLD"  ~ "World",
      location == "G20"  ~ "Group of 20",
      location == "EU27" ~ "European Union",
      location == "CZE"  ~ "Czech Republic", #Might as well use codes for all of it!
      location == "KOR"  ~ "South Korea",
      location == "GBR"  ~ "Great Britain",
      location == "USA"  ~ "United States",
      location == "RUS"  ~ "Russia",
      TRUE               ~ country #Other cases
    )
  )

world_pop <- oecd_cleaned %>%
  filter(location == "WLD") %>%
  select(time, pop_total, pop_men, pop_women) %>%
  rename(
    world_pop_total = pop_total,
    world_pop_men = pop_men,
    world_pop_women = pop_women
  )

oecd_cleaned <- oecd_cleaned %>%
  left_join(world_pop, by = "time") %>%
  mutate(
    frac_men = pop_men/world_pop_men,
    frac_women = pop_women/world_pop_women,
    frac_total = pop_total/world_pop_total
  ) %>%
  select(-c(world_pop_total, world_pop_men, world_pop_women))

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

## Analysis

Let’s see if there’s any link between winning medals in a sport and the
population of your country. The sport appears to be contained in the
`type` variable.

``` r
paralympic_full %>% count(type)
```

    ## # A tibble: 11 × 2
    ##    type                  n
    ##    <chr>             <int>
    ##  1 Archery             416
    ##  2 Athletics          7741
    ##  3 Basketball          594
    ##  4 Fencing             670
    ##  5 Powerlifting        403
    ##  6 Rugby               176
    ##  7 Swimming           6233
    ##  8 Table Tennis       1393
    ##  9 Triathlon            18
    ## 10 Volleyball          575
    ## 11 Wheelchair Tennis  1370

We’ll use the fraction of the world population for people of a given
(binary) gender [4] as a variable to standardise by. This is going to
take some manipulation.

``` r
paralympic_full <- paralympic_full %>%
  mutate(
    frac_for_gender = case_when(
      gender == "Men"   ~ frac_men,
      gender == "Women" ~ frac_women,
      TRUE              ~ frac_total #Fallback if gender field is missing
    )
  )

paralympic_full %>%
  ggplot(aes(x = type, y = frac_for_gender)) +
  geom_violin()
```

![](paralympics_files/figure-gfm/sport%20v%20population-1.png)<!-- -->

This gives a warning that values were removed (hidden in the knitted
document), because we only have the data for the OECD countries.

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

[4] Some people are non-binary, of course, and so don’t fit into either
of these categories. The statistics from the OECD don’t reflect that,
much as the Census 2021 in England and Wales asked for respondents to
state a binary sex as well as giving a free text box to state their
gender. In Paralympic sports there are only two gender categories.
