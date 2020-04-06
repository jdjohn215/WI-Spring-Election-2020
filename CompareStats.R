rm(list = ls())

library(tidyverse)
library(sf)
library(tmap)

covid.compare <- readRDS("data/CompareVotesAndCOVIDCases_2020-04-06 15:00:49.rds")

d <- covid.compare %>%
  select(county_name, share19, share20, performance, positive)

hardest.hit.counties <- d %>%
  top_n(5, positive) %>%
  pull(county_name)
hardest.hit.counties

# share of cases in the hardest-hit counties
(sum(d$positive[d$county_name %in% hardest.hit.counties])/sum(d$positive))*100

# share of absentee ballots returned in hardest-hit counties in 2019
hardest.hit.share.19 <- (sum(d$share19[d$county_name %in% hardest.hit.counties]))*100

# share of absentee ballots returned in hardest-hit counties in 2020
hardest.hit.share.20 <- (sum(d$share20[d$county_name %in% hardest.hit.counties]))*100

# change in share for these counties
change.in.share <- hardest.hit.share.20 - hardest.hit.share.19

wi.county.sf <- tigris::counties(state = "55", cb = TRUE, class = "sf") %>%
  select(county_name = NAME) %>%
  mutate(county_name = str_to_upper(county_name))

d2 <- inner_join(wi.county.sf, d) %>%
  mutate(positive = replace(positive, positive == 0, NA),
         performance = performance*100)

tm_shape(d2) +
  tm_polygons() +
  tm_shape(d2) +
  tm_fill(col = "positive", breaks = c(-Inf,10,100,500,1000,Inf),
          textNA = "none", colorNA = "white", title = "COVID-19 diagnoses") +
  tm_layout(frame = FALSE)

tm_shape(d2) +
  tm_polygons() +
  tm_shape(d2) +
  tm_fill(col = "performance",
          breaks = c(-Inf, -1, -0.5, 0, 0.5, 1, Inf))

