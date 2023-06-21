# some descriptive stats and plots

library(tidyverse)
library(shiny)
library(gt)


hop_brew_values <- read_tsv("hop_brew_values.txt")
hop_aromas <- read_tsv("hop_aromas.txt")

head(hop_brew_values)
head(hop_aromas)

# summary of all
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


# how many hops in total
paste("There are", length(hop_aromas$hop_name), "hops in this database. Information was retrieved from www.beermaverick.com on June 17, 2023.")

# where are they from and what is their purpose
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


# when filtered by country
hop_aromas %>% 
  mutate(hop_name = paste0("<a href=", link, ">", hop_name,"</a>")) %>% 
  select(hop_name, hop_purpose, country_code, country) %>% 
  gt() %>% 
  cols_move_to_end(columns = "hop_purpose") %>% 
  fmt_flag(columns = country_code) %>%
  fmt_url(columns = "hop_name") %>% 
  cols_label(hop_name = md('**Hop name**'),
             country_code = '',
             country = md('**Country**'),
             hop_purpose = md('**Purpose**'))




# Radial barplot to display aroma profiles 

radial_barplot <- function(hop){
  
  hop_aromas |> 
    filter(hop_name == hop) |> 
    pivot_longer(
      cols = -c(hop_name, hop_purpose, country, link, aroma_tags, country_code),
      names_to = "aroma", 
      values_to = "aroma_value"
    ) |> 
    mutate(aroma = str_replace(aroma, "(?<=[:lower:])(?=[:upper:])", "\n"))|>
    ggplot(aes(x = fct_inorder(aroma), y = aroma_value, group = hop_name)) +
    geom_col(aes(fill = aroma), show.legend = F) +
    geom_segment(aes(xend = fct_inorder(aroma), y = 5, yend  = 0), linetype = 2, alpha = 0.5) +
    coord_polar() +
    theme_minimal() + 
    theme(legend.position = "none",
      axis.text.y = element_blank(),
      # panel.border = element_rect(fill = NA),
      plot.margin = margin(0,0,0,0,"mm"),
      plot.background = element_rect(fill = NA, colour = NA), 
      panel.background =  element_rect(fill = NA, colour = NA), 
      panel.border =  element_rect(fill = NA, colour = NA),
      axis.line.x = element_blank()) +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(limits = c(0,5)) +
    annotate("text", x = 0, y = c(0:5), label= c(0:5)) + 
    scale_fill_viridis_d()
}


radial_barplot("Citra")  
radial_barplot("Galaxy")  
radial_barplot("Styrian Wolf")  


# Summary plots for a list of hops

example_hops <- hop_aromas |> 
  filter(hop_name %in% c("Citra", "Galaxy", "Styrian Wolf"))

hop_aromas |> 
  select(hop_name, Citrus:Pine) |> 
  drop_na() |> 
  slice_sample(n = 10) |> 
  pivot_longer(cols = -hop_name, names_to = "aroma", values_to = "value") |> 
  mutate(aroma  = str_replace(aroma, "Fruit", "\nFruit")) |> 
  ggplot(aes(x = fct_inorder(aroma), y = value)) + 
  geom_boxplot(aes(fill = aroma), show.legend = F) +
  theme_classic() +
  labs(x = NULL, y = "Aroma intensity") +
  scale_y_continuous(limits = c(0,5)) +
  scale_fill_viridis_d()


# Oil breakdown plot
hop_oils <- c("All Others", "Myrcene", "Humulene", "Farnesene", "Caryophyllene")
hop_oils_plot_df <-hop_brew_values |> 
  filter(brew_value %in% hop_oils) |> 
  left_join(
    hop_brew_values |> 
      filter(brew_value == "Total Oils (mL/100g)") |> 
      select(hop_name, brew_value, range_mean) |> 
      pivot_wider(names_from = brew_value, values_from = range_mean )
  ) |> 
  rename(total_oil = `Total Oils (mL/100g)`) |> 
  mutate(brew_value = fct_relevel(brew_value, "Farnesene", "Caryophyllene","Myrcene", "Humulene", "All Others"))

hop_oils_plot_df |> 
  mutate(range_mean = range_mean/100) |> 
  ggplot(aes(x = fct_inorder(brew_value), y = range_mean, group = hop_name)) +
  geom_point() +
  geom_path(alpha = 0.5, aes(col = total_oil)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Total oil breakdown", y = NULL, col = "Total Oils\n(mL/100g)")  + 
  theme_classic() +
  scale_color_gradient(low = "white", high = "forestgreen")


hop_brew_values |> 
  filter(brew_value == "Alpha Acid %") |> 
  arrange(-range_mean) |> 
  mutate(rank = row_number(),
    hop_of_interest = hop_name%in%test_hop) |> 
  ggplot(aes(x = rank, y = range_mean)) + 
  geom_point() +
  geom_point(data = . %>% hop_of_interest, col = "red") +
  theme_classic()

brew_value_ranges <- hop_brew_values |> 
  group_by(brew_value) |> 
  summarise(min = min(range_min, na.rm = T), max = max(range_max, na.rm = T))

hop_brew_values |> 
  filter(hop_name == "Astra") |> 
  drop_na() |> 
  filter(!brew_value %in% hop_oils) |> 
  ggplot(aes(y = fct_rev(brew_value), x = range_mean)) +
  geom_point() +
  geom_segment(aes(x = range_min, xend = range_max, yend = brew_value)) + 
  labs(x = NULL, y = NULL) +
  theme_classic() +
  facet_wrap(~brew_value, scales = "free", ncol = 1)




hop_brew_values <- hop_brew_values |> 
  mutate(range_min = ifelse(range_min > range_mean, NA, range_min))

highlight_brew_value_rank <- function(brew_value_of_interest, hop){
  plot_data <- hop_brew_values |> 
    filter(brew_value == brew_value_of_interest) |> 
    arrange(-range_mean) |> 
    mutate(rank = row_number(),
      hop_of_interest = hop_name%in%hop)
  
  hop_of_interest <- plot_data |> filter(hop_of_interest)
  
  plot_data |> 
    ggplot(aes(x = rank, y = range_mean)) + 
    geom_segment(aes(xend = rank, yend = range_max, y = range_min), col = "grey80") +
    geom_point(col = "grey50") +
    geom_segment(data = plot_data |>  filter(hop_of_interest), aes(xend = rank, yend = range_max, y = range_min), col = "red") +
    geom_point(data = plot_data |>  filter(hop_of_interest), col = "red") +
    theme_classic() + 
    labs(title = paste0(brew_value_of_interest), y = NULL,
      subtitle = paste0(hop, " range: ",hop_of_interest$range_min," to ", hop_of_interest$range_max, ", mean ", hop_of_interest$range_mean, "\nRank: ", hop_of_interest$rank, "/", max(plot_data$rank)),
      x = "Rank among all hops")
}

hop_brew_values <- hop_brew_values |> 
  mutate(range_min= ifelse(range_min <= range_mean, range_min, NA)) |> 
  mutate(range_max= ifelse(range_mean <= range_max, range_max, NA))


highlight_brew_value_rank("Alpha Acid %", "Astra")
highlight_brew_value_rank("Total Oils (mL/100g)", "Styrian Wolf")
highlight_brew_value_rank("Myrcene", "Styrian Wolf")


hop_brew_values |> 
  distinct(brew_value)
