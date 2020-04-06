rm(list = ls())

library(tidyverse)
library(rvest)
options(scipen =  999)

ballots20 <- read_html("https://elections.wi.gov/node/6817") %>%
  html_node(".Table") %>%
  html_table(header = T) %>%
  janitor::clean_names() %>%
  filter(county_name != "Total") %>%
  mutate(county_name = word(county_name, 1, -2))

ballots19 <- read_html("https://elections.wi.gov/node/6487") %>%
  html_node(".Table") %>%
  html_table(header = T) %>%
  janitor::clean_names() %>%
  filter(county_name != "Total") %>%
  mutate(county_name = word(county_name, 1, -2))

ballots <- inner_join(ballots19 %>% select(county_name, ballots_returned_19 = absentee_ballots_returned_count),
                      ballots20 %>% select(county_name, ballots_returned_20 = absentee_ballots_returned_count))

covid <- read_csv("https://opendata.arcgis.com/datasets/360c2995846e4af99461cb80d3ed8c27_1.csv") %>%
  janitor::clean_names() %>%
  mutate(name = str_to_upper(name))

compare <- inner_join(ballots, covid, by = c("county_name" = "name"))

d <- compare %>%
  mutate(share19 = ballots_returned_19/sum(ballots_returned_19),
         share20 = ballots_returned_20/sum(ballots_returned_20),
         performance = share20 - share19)

ggplot(d, aes(positive, performance)) +
  geom_point() +
  geom_text(aes(label = str_to_title(county_name)),
            check_overlap = TRUE) +
  scale_y_continuous(labels = scales::percent,
                     name = "Change in share of total from 2019 to 2020") +
  scale_x_continuous(name = "Positive diagnoses of COVID-19",
                     labels = scales::number_format(big.mark = ",")) +
  labs(title = "Fewer absentee ballots have been returned in counties with more cases of COVID-19",
       subtitle = "Comparison of absentee ballots returned the day before the spring election in 2019 and 2020",
       caption = "Data is from WI Election Commission & WI DHS. See github.com/jdjohn215/COVID_election") 

ggplot(d, aes(positive, performance)) +
  geom_point() +
  scale_x_log10()

lm(performance ~ positive, data = d) %>% summary()
