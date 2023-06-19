# some descriptive stats and plots

library(tidyverse)
library(shiny)
library(gt)


hop_brew_values <- read_tsv("hop_brew_values.txt")
hop_aromas <- read_tsv("hop_aromas.txt")

head(hop_brew_values)
head(hop_aromas)

# how many hops in total
paste("There are", length(hop_aromas$hop_name), "hops in this database. Information was retrieved from www.beermaverick.com on June 17, 2023.")

# where are they from and what is their purpose
# just countries
hop_aromas %>% 
  count(country) %>%
  arrange(desc(n)) %>%
  mutate(country_code = countrycode::countrycode(country, 'country.name', 'genc2c')) %>% 
  gt() %>%
  cols_move_to_start(country_code) %>% 
  fmt_flag(columns = country_code) %>% 
  cols_label(country_code = '',
             country = md('**Country**'),
             n = md('**Hop strains**'))

# countries + purpose
hop_aromas %>% 
  group_by(country) %>%
  mutate(n_hops = n()) %>%
  group_by(country, n_hops, hop_purpose) %>% 
  summarise(n_purpose = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = hop_purpose, values_from = n_purpose, values_fill = 0) %>% 
  arrange(desc(n_hops)) %>%
  mutate(Aroma = Aroma/n_hops,
         Bittering = Bittering/n_hops,
         Dual = Dual/n_hops) %>% 
  mutate(country_code = countrycode::countrycode(country, 'country.name', 'genc2c')) %>% 
  gt() %>%
  cols_move_to_start(country_code) %>% 
  fmt_flag(columns = country_code) %>% 
  fmt_percent(columns = c(Aroma, Bittering, Dual), decimals = 0) %>% 
  cols_label(country_code = '',
             country = md('**Country**'),
             n_hops = md('**Number of hop strains**')) %>% 
  tab_spanner(label = md('**Purpose**'),
              columns = c(Aroma, Bittering, Dual))





