library(tidyverse)

hopmaverick <- rvest::read_html("https://beermaverick.com/hops/")

# start by fetching the flavor and aroma profiles from beermaverick.
# These are the values that are plotted in a spider plot on each hop page, e.g. https://beermaverick.com/hop/astra/

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
  rvest::html_attr("href") %>% 
  as_tibble()


# query a single page first
test <- rvest::read_html("https://beermaverick.com/hop/astra/")
test %>% 
  rvest::html_elements("script") %>% 
  rvest::html_text() %>% 
  as_tibble() %>% 
  filter(str_detect(value, "aromaChart")) %>% 
  slice_head() %>% 
  mutate(labels = str_extract(value, "(?<=labels: \\[)[^\\]]+"), 
    datasets = str_extract(value, "(?<=data: \\[)[^\\]]+")) %>% 
  select(-value) %>% 
  mutate(datasets = str_remove_all(datasets, "\\s|\n") |> str_split(",")) %>% 
  mutate(labels = str_remove_all(labels, "\\s|\n") |> str_split(",")) |> 
  unnest(c(labels, datasets)) %>%
  mutate(datasets = as.numeric(datasets)) %>% 
  mutate(labels = str_remove_all(labels, "'") %>% str_remove("Resin/") %>% str_replace(" ", "_")) %>% 
  pivot_wider(names_from = labels, values_from = datasets)
  

# make this into a function, make it return a tibble:
extract_aroma_chart_from_link <- function(link){
  
  rvest::read_html(link) |> 
    rvest::html_elements("script") %>% 
    rvest::html_text() %>% 
    as_tibble() %>% 
    filter(str_detect(value, "aromaChart")) %>% 
    slice_head() %>% 
    mutate(labels = str_extract(value, "(?<=labels: \\[)[^\\]]+"), 
      datasets = str_extract(value, "(?<=data: \\[)[^\\]]+")) %>% 
    select(-value) %>% 
    mutate(datasets = str_remove_all(datasets, "\\s|\n") |> str_split(",")) %>% 
    mutate(labels = str_remove_all(labels, "\\s|\n") |> str_split(",")) |> 
    unnest(c(labels, datasets)) %>%
    mutate(datasets = as.numeric(datasets)) %>% 
    mutate(labels = str_remove_all(labels, "'") %>% str_remove("Resin/") %>% str_replace(" ", "_")) %>% 
    pivot_wider(names_from = labels, values_from = datasets)
  
}

# How much faster is it to use furrr's future_map() vs purrr's map()?

# map()
tictoc::tic()
hop_table %>% 
  bind_cols(links) %>% 
  mutate(html = map(value, ~rvest::read_html(paste0("https://beermaverick.com", .x)))) 
tictoc::toc()

# future_map()
tictoc::tic()
future::plan("multisession")
hop_table %>% 
  bind_cols(links) %>% 
  mutate(html = furrr::future_map(value, ~rvest::read_html(paste0("https://beermaverick.com", .x)))) 
future::plan("default")
tictoc::toc()


# Create hop aroma table using our function we just created and future_map()
tictoc::tic()
future::plan("multisession")
hop_aromas <- hop_table %>% 
  bind_cols(links) %>%
  mutate(value = paste0("https://beermaverick.com", value)) |> 
  rename(link = value) |> 
  mutate(html = furrr::future_map(link, ~extract_aroma_chart_from_link(.x)))
future::plan("default")
tictoc::toc()

hop_aromas <- hop_aromas |> 
  unnest_wider(html)

# save
setwd("Documents/hop_analysis/")

hop_table %>% 
  write_rds("hop_table_main.rds")

hop_aromas %>% 
  write_tsv("hop_aromas.txt")



# brewing values ----------------------------------------------------------

# beermaverick also has a list of brewing values on each hop page. 
# These values include alpha and beta acid percentages, how well they can be stored and a breakdown of the oils found in each hop.  

hop_table <- read_rds("hop_table_main.rds")
hop_aromas <- read_tsv("hop_aromas.txt")

brew_values <- c("Alpha Acid %", "Beta Acid %", "Alpha-Beta Ratio", "Hop Storage Index \\(HSI\\)", 
                 "Co-Humulone as % of Alpha", "Total Oils \\(mL\\/100g\\)", "Myrcene", 
                 "Humulene","Caryophyllene","Farnesene","All Others"
)


test %>% 
  rvest::html_elements(".brewvalues") %>% 
  rvest::html_text2() %>% 
  as_tibble() %>% 
  mutate(value = str_split(value, "avg|\n› ")) %>% 
  unnest(value) %>% 
  mutate(value = str_replace_all(value, "\\s", " ")) %>% 
  mutate(value = ifelse(str_detect(value, "Co-Humulone"), str_extract(value, "Co-Humulone.*"), value)) %>% 
  mutate(value = str_remove(value, "^ ") %>% str_remove(" +$")) %>% 
  filter(!value %in% c("Total Oil Breakdown:", "")) %>% 
  mutate(title = str_extract(value, paste0(brew_values, collapse = "|")),
         description = str_remove(value, paste0(brew_values, collapse = "|"))) %>% 
  select(-value) %>% 
  mutate(range = str_extract(description, "\\d+\\.?\\:?\\d?\\ ?-\\ ?\\d+\\.?\\:?\\d?(%| mL)?")) %>% 
  mutate(mean_val = str_extract(description, "\\d+\\.?\\:?\\d?[^\\d]?(mL)?$")) %>% 
  select(-description)


extract_brew_values <- function(x) {
  
  brew_value_table <- x %>% 
    rvest::html_elements(".brewvalues") %>% 
    rvest::html_text2() %>% 
    as_tibble() %>% 
    mutate(value = str_split(value, "avg|\n› ")) %>% 
    unnest(value) %>% 
    mutate(value = str_replace_all(value, "\\s", " ")) %>% 
    mutate(value = ifelse(str_detect(value, "Co-Humulone"), str_extract(value, "Co-Humulone.*"), value)) %>% 
    mutate(value = str_remove(value, "^ ") %>% str_remove(" +$")) %>% 
    filter(!value %in% c("Total Oil Breakdown:", "")) %>% 
    mutate(title = str_extract(value, paste0(brew_values, collapse = "|")),
           description = str_remove(value, paste0(brew_values, collapse = "|"))) %>% 
    select(-value) %>% 
    mutate(range = str_extract(description, "\\d+\\.?\\:?\\d?\\ ?-\\ ?\\d+\\.?\\:?\\d?(%| mL)?")) %>% 
    mutate(mean_val = str_extract(description, "\\d+\\.?\\:?\\d?[^\\d]?(mL)?$")) %>% 
    select(-description)
  
  return(brew_value_table)
}


hop_brew_values <- hop_table %>% 
  mutate(brew_values = map(html, ~extract_brew_values(.x)))


hop_brew_values <- hop_brew_values %>% 
  select(-c(html, value)) %>% 
  unnest(brew_values) %>% 
  rename(value = title)

# clean up a bit more
hop_brew_values <- hop_brew_values %>% 
  drop_na(value) %>% 
  mutate(across(.cols = c(range, mean_val), ~str_remove_all(.x, ":\\d| |%|mL"))) %>%
  separate(range, into = c("range_min", "range_max"), sep = "-") %>% 
  rename(range_mean = mean_val) %>% 
  mutate(across(.cols = c(range_min, range_max, range_mean), ~as.numeric(.x)))


hop_brew_values %>% 
  write_tsv("hop_brew_values.txt")



# first attempt at parsing out brew values --------------------------------


# test %>% 
#   rvest::html_elements(".brewvalues") %>% 
#   rvest::html_text2() %>% 
#   as_tibble() %>% 
#   mutate(value = str_split(value, "\n")) %>% 
#   unnest(value) %>%
#   filter(!str_detect(value, "Alpha Acid \\% \\(AA\\)|Total Oil Breakdown")) %>% 
#   mutate(grp = str_detect(value, "^\\d") %>% lag() %>% replace_na(F)) %>% 
#   mutate(grp = cumsum(grp)) %>% 
#   # separate(value, into = c("test", "test2"), sep = "\t")
#   summarise(value = paste0(value, collapse = "::"), .by = grp) %>%
#   separate(value, into = c("cat", "avg"), sep = "::") %>% 
#   separate(cat, into = c("cat", "value"), sep = "\t") %>% 
#   mutate(cat = ifelse(str_detect(cat, "Hop Storage Index"), str_extract(cat, "Hop Storage Index.*"), cat),
#          cat = str_remove(cat, "› "),
#          cat = ifelse(str_detect(cat, "Low cohumulone"), paste0("Co-Humulone as % of Alpha", str_extract(cat, "Low cohumulone.*")), cat),
#          cat = ifelse(str_detect(cat, "^Alpha acids"), paste0("Alpha Acid %",cat), cat)) %>% 
#   mutate(title = str_extract(cat, paste0(brew_values, collapse = "|")),
#          description = str_remove(cat, paste0(brew_values, collapse = "|"))) %>% 
#   select(-cat) %>% 
#   mutate(avg = str_remove(avg, " avg") %>% str_remove("\t")) %>% 
#   mutate(range = ifelse(str_detect(value, "-"), value, avg ), 
#          range_mean = ifelse(!str_detect(value, "-"), value, avg)) %>% 
#   select(-c(grp, value, avg))


# extract_brew_values <- function(x) {
#   brew_value_table <- x %>% 
#     rvest::html_elements(".brewvalues") %>% 
#     rvest::html_text2() %>% 
#     as_tibble() %>% 
#     mutate(value = str_split(value, "\n")) %>% 
#     unnest() %>%
#     filter(!str_detect(value, "Alpha Acid \\% \\(AA\\)|Total Oil Breakdown")) %>% 
#     mutate(grp = str_detect(value, "^\\d") %>% lag() %>% replace_na(F)) %>% 
#     mutate(grp = cumsum(grp)) %>% 
#     # separate(value, into = c("test", "test2"), sep = "\t")
#     summarise(value = paste0(value, collapse = "::"), .by = grp) %>%
#     separate(value, into = c("cat", "avg"), sep = "::") %>% 
#     separate(cat, into = c("cat", "value"), sep = "\t") %>% 
#     mutate(cat = ifelse(str_detect(cat, "Hop Storage Index"), str_extract(cat, "Hop Storage Index.*"), cat),
#            cat = str_remove(cat, "› "),
#            cat = ifelse(str_detect(cat, "Low cohumulone"), paste0("Co-Humulone as % of Alpha", str_extract(cat, "Low cohumulone.*")), cat),
#            cat = ifelse(str_detect(cat, "^Alpha acids"), paste0("Alpha Acid %",cat), cat)) %>% 
#     mutate(title = str_extract(cat, paste0(brew_values, collapse = "|")),
#            description = str_remove(cat, paste0(brew_values, collapse = "|"))) %>%     select(-cat) %>% 
#     mutate(avg = str_remove(avg, " avg") %>% str_remove("\t")) %>% 
#     mutate(range = ifelse(str_detect(value, "-"), value, avg ), 
#            range_mean = ifelse(!str_detect(value, "-"), value, avg)) %>% 
#     select(-c(grp, value, avg))
#   
#   return(brew_value_table)
# }


