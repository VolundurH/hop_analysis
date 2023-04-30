library(tidyverse)

hopmaverick <- rvest::read_html("https://beermaverick.com/hops/")

# hop table
hop_table <- hopmaverick %>%
  rvest::html_elements("table") %>% 
  rvest::html_children() %>% 
  rvest::html_text2() %>% 
  as_tibble() %>% 
  mutate(country_id = !str_detect(value, "\t"),
    drop_later = country_id) %>% 
  mutate(country_id = cumsum(country_id)) %>% 
  group_by(country_id) %>% 
  mutate(country = first(value, country_id)) %>% 
  filter(!drop_later) %>% 
  ungroup() %>% 
  separate(value, into = c("hop_name", "hop_purpose"), sep = "\t") %>% 
  select(-c(country_id, drop_later)) 


links <- hopmaverick %>%
  rvest::html_elements("table") %>% 
  rvest::html_children() %>% 
  rvest::html_elements("a") %>%
  rvest::html_attrs() %>% 
  as_vector() %>% 
  as_tibble()


# query a single page first
test <- rvest::read_html("https://beermaverick.com/hop/galaxy/")
test %>% 
  rvest::html_elements("script") %>% 
  rvest::html_text() %>% 
  as_tibble() %>% 
  filter(str_detect(value, "aromaChart")) %>% 
  slice_head() %>% 
  mutate(labels = str_extract(value, "(?<=labels: \\[)[^\\]]+"), 
    datasets = str_extract(value, "(?<=data: \\[)[^\\]]+")) %>% 
  select(-value) %>% 
  mutate(datasets = str_remove_all(datasets, "\n")) %>% 
  mutate(labels = str_split(labels, ", "),
    datasets = str_split(datasets, ",")) %>% 
  unnest(c(labels, datasets)) %>%
  mutate(datasets = as.numeric(datasets)) %>% 
  mutate(labels = str_remove_all(labels, "'") %>% str_remove("Resin / ") %>% str_replace(" ", "_")) %>% 
  pivot_wider(names_from = labels, values_from = datasets)
  

# make this into a function, make it return a tibble:
extract_aroma_chart <- function(html_object){
  aroma_table <- html_object %>% 
    rvest::html_elements("script") %>% 
    rvest::html_text() %>% 
    as_tibble() %>% 
    filter(str_detect(value, "aromaChart")) %>% 
    slice_head() %>% 
    mutate(labels = str_extract(value, "(?<=labels: \\[)[^\\]]+"), 
      datasets = str_extract(value, "(?<=data: \\[)[^\\]]+")) %>% 
    select(-value) %>% 
    mutate(datasets = str_remove_all(datasets, "\n")) %>% 
    mutate(labels = str_split(labels, ", "),
      datasets = str_split(datasets, ",")) %>% 
    unnest(c(labels, datasets)) %>% 
    mutate(datasets = as.numeric(datasets)) %>% 
    mutate(labels = str_remove_all(labels, "'") %>% str_remove("Resin / ") %>%  str_replace(" ", "_")) %>% 
    pivot_wider(names_from = labels, values_from = datasets)
}

hop_table <- hop_table %>% 
  bind_cols(links) %>% 
  mutate(html = map(value, ~rvest::read_html(paste0("https://beermaverick.com", .x)))) 

hop_aromas <- hop_table %>% 
  mutate(info = map(html,  ~extract_aroma_chart(.x))) %>% 
  select(-c(value, html)) %>% 
  unnest(info) 

# save
setwd("Documents/hop_analysis/")

hop_table %>% 
  write_rds("hop_table_main.rds")

hop_aromas %>% 
  write_tsv("hop_aromas.txt")



# brewing values ----------------------------------------------------------





