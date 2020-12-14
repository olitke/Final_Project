---
title: "Final Project"
author: "Kristin Bryan, Olivia Litke, Mary McDonnell"
date: "12/13/2020"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
---




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(openintro)
```

```
## Loading required package: airports
```

```
## Loading required package: cherryblossom
```

```
## Loading required package: usdata
```

```r
library(maps)
```

```
## 
## Attaching package: 'maps'
```

```
## The following object is masked from 'package:purrr':
## 
##     map
```

```r
library(ggmap)
```

```
## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
```

```
## Please cite ggmap if you use it! See citation("ggmap") for details.
```

```r
library(gplots)
```

```
## 
## Attaching package: 'gplots'
```

```
## The following object is masked from 'package:stats':
## 
##     lowess
```

```r
library(RColorBrewer)
library(sf)
```

```
## Linking to GEOS 3.8.1, GDAL 3.1.1, PROJ 6.3.1
```

```r
library(ggthemes)
library(usmap)
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggmap':
## 
##     wind
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```


```r
covid19 <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

mask_data <- read.csv("https://raw.githubusercontent.com/kristinbryan/covid-19-data/master/mask-use/mask-use-by-county.csv")

census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   state = col_character(),
##   est_pop_2018 = col_double()
## )
```


# Welcome to our final!


## Research and Project Inspiration

In July, the New York Times published an article entitled [A Detailed Map of Who Is Wearing Masks in the U.S.](https://www.nytimes.com/interactive/2020/07/17/upshot/coronavirus-face-mask-map.html) The interactive map was informative regarding the presence of mask-wearing across the United States. To create this map, columnists Josh Katz, Margot Sanger-Katz and Kevin Quealy analyzed information from the County-level Data for Mask Use in the United States. This dataset, paired with the COVID-19 data we've been using in our class throughout the module, allowed us to assess the relationship between mask-wearing and COVID cases across the country. Specifically, are states where mask-wearing is greater, are COVID cases lower?

We started by gathering and slimming down our data:


```r
# combining and setting up our data sets 

covid_data <- covid19 %>% 
  left_join(mask_data, 
            by = c("fips" = "COUNTYFP"))
```
## COVID-19 Map

In order to  find the correlation between total COVID-19 cases and mask wearing, we created two individual maps for each to display the COVID-19 data and mask data in each state. 


```r
#covid map

states_map = map_data("state")

covid_data %>% 
  filter(date == max(date)) %>% 
  mutate(state = str_to_lower(state)) %>% 
  left_join(census_pop_est_2018, 
            by = "state") %>% 
  group_by(state) %>% 
  mutate(case_rate = sum(cases)/est_pop_2018) %>% 
  ggplot(aes(fill = case_rate)) + 
  geom_map(aes(map_id = state),
           map = states_map) + 
  expand_limits(x = states_map$long, y = states_map$lat) + 
  theme_map()
```

Our COVID-19 data shows the most recent case rates in each state. The map shows that currently, cases are highest in the mid-west. 

## Mask Map

To find out how often people in the US were wearing masks, we created a map to display mask use. This map shows the percentage of people who always wear masks when leaving the house.  


```r
county_map = map_data("county") %>% 
  dplyr::rename(state = region, region = subregion)

covid_data %>% 
  mutate(county = str_to_lower(county)) %>% 
  drop_na() %>% 
  ggplot(aes(fill = ALWAYS)) + 
  geom_map(aes(map_id = county),
           map = county_map) + 
  expand_limits(x = county_map$long, y = county_map$lat) + 
  theme_map()

# figure out which date(s) we want to use
```

Our mask data shows the frequency of mask wearing in each state. The New York Times article analyzed the map and determined that mask use is high in the Northeast and the West, and lower in the Plains and parts of the South. The article also focused on differences on the local level. For instance, mask wearing is high in Washington, D.C, but in rural subsets of Maryland and Virginia, norms seem to differ. The columnists found that generally in urban areas, mask wearing was more common. 

However, the New York Times also reported that mask-wearing in the U.S was relatively high. This could be because they conducted a self reported survey. The survey asked: "how often do you wear a mask in public when you expect to be within 6ft of another person?" The options were: "always, frequently, sometimes, rarely, and never." On average, around 80% of Americans report mask use either "always" or "frequently" in public within 6ft of others. 

The article further stressed the implications of a national mask wearing mandate. The columnists maintain that it would be difficult to achieve such a thing. However, research suggests that mandates are effective. For example, implementation of laws  with increasing the use of seatbelts and the vaccination rates among school-aged children. Laws may be helpful to decrease health risk and improve overall percentages.

Katz, Sanger-Katz, and Quealy also illustrated that mask use is often a partisan issue politically. President Trump and other Republican officials have downplayed the necessity of mask-wearing, and expressed negative reactions to a national mandate. The survey shows that Republicans are generally less likely to wear masks, despite self-reports being high among Americans. Partisanship, it turns out, is the biggest indicator of mask-wearing likelihood, not age or where you live, or other variables, according to the article.

Rather than political party, and by county, we wanted to determine the relationship between mask-wearing and COVID cases at the state level. 

## Masks vs. COVID-19

So what is the relationship between mask wearing and COVID-19 infection? Below, we mapped the data to show the trajectory of COVID-19 cases from the first instances of the pandemic to now in relationship to the frequency of mask use in each county. 


```r
# what is the correllation between mask wearing and covid cases? make a map here. either make a faceted graph or a map here

# final map.... maybe we could overlay it with colored dots that show percentages of mask wearing in each county
```

We successfully joined the mask dataset with our timely COVID dataset, and created maps that illustrate that relationship. We can conclude that states with higher infection rates generally report... mask use is...





>>>>>>> f615ca4fe7f735bf9f293f89b1d3d4e392070fc9
