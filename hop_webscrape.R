library(tidyverse)

# Read in the contents of the main website, which includes a table of all the hops we are interested in. 
hopmaverick <- rvest::read_html("https://beermaverick.com/hops/")

# start by fetching the flavor and aroma profiles from beermaverick.
# These are the values that are plotted in a spider plot on each hop page, e.g. https://beermaverick.com/hop/astra/


# The rvest package has a bunch of functions to parse through HTML contents. Here we used html_elements("table") to extract the hop table, and html_children() and html_text2() to parse it into a workable format. We then use the standard dplyr functions to tidy the data properly. 
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

# Every hop strain in the table contains a link to that strain's page. These pages contain more information on the strain, such as what flavors and aromas are associated with that strain, how much oil the strain has, etc. These pages are all structured in the same way, so retrieving the data should not be too complicated. 

# Here we use the same rvest functions as before, but now we retrieve the embedded links in the table.
links <- hopmaverick %>%
  rvest::html_elements("table") %>% 
  rvest::html_children() %>% 
  rvest::html_elements("a") %>%
  rvest::html_attr("href") %>% 
  as_tibble()

# We now have a table of hops with an associated link. For every hop we want to follow the link and retrieve information for that page, and parse that into a master table.


# Hop aroma table ---------------------------------------------------------



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


# Timings: map() vs future_map() for scraping -----------------------------



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


# End of timings ----------------------------------------------------------



# Create hop aroma table using our function we just created and future_map()

future::plan("multisession")
hop_aromas <- hop_table %>% 
  bind_cols(links) %>%
  mutate(value = paste0("https://beermaverick.com", value)) |> 
  rename(link = value) |> 
  mutate(html = furrr::future_map(link, ~extract_aroma_chart_from_link(.x)))
future::plan("default")


hop_aromas <- hop_aromas |> 
  unnest_wider(html) |> 
  mutate(country = str_replace(country, "Replublic", "Republic"))

hop_aromas %>% 
  write_tsv("hop_aromas.txt")



# brewing values ----------------------------------------------------------

# beermaverick also has a list of brewing values on each hop page. 
# These values include alpha and beta acid percentages, how well they can be stored, and a breakdown of the oils found in each hop.  

# This information is a little trickier to get, since not all hops have all the information fields on the page. Some hop do not even have any.



brew_values <- c("Alpha Acid %", "Beta Acid %", "Alpha-Beta Ratio", "Hop Storage Index \\(HSI\\)",  "Co-Humulone as % of Alpha", "Total Oils \\(mL\\/100g\\)", "Myrcene", 
                 "Humulene","Caryophyllene","Farnesene","All Others"
)

test <-  rvest::read_html("https://beermaverick.com/hop/citra/")

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


extract_brew_values <- function(link) {
  
  brew_value_table <- rvest::read_html(link) |> 
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


future::plan("multisession")
hop_brew_values <- hop_table %>% 
  bind_cols(links) %>%
  mutate(value = paste0("https://beermaverick.com", value)) |> 
  rename(link = value) |> 
  mutate(brew_values = furrr::future_map(link, ~extract_brew_values(.x)))
future::plan("default")

hop_brew_values <- hop_brew_values |> 
  unnest(brew_values) |> 
  rename(brew_value = title)

# clean up a bit more
hop_brew_values <- hop_brew_values %>% 
  drop_na(brew_value) %>% 
  mutate(across(.cols = c(range, mean_val), ~str_remove_all(.x, ":\\d| |%|mL"))) %>%
  separate(range, into = c("range_min", "range_max"), sep = "-") %>% 
  rename(range_mean = mean_val) %>% 
  mutate(across(.cols = c(range_min, range_max, range_mean), ~as.numeric(.x)))

hop_brew_values %>% 
  write_tsv("hop_brew_values.txt")



# Hop aroma tags ----------------------------------------------------------

# Finally we want to extract aroma tags if they are present on the page. This is actually very easy:

test <-  rvest::read_html("https://beermaverick.com/hop/citra/")

test |> 
  rvest::html_elements("em") |> 
  rvest::html_children() |> 
  rvest::html_text2()

extract_aroma_tags_from_link <- function(link){
  rvest::read_html(link) |> 
    rvest::html_elements("em") |> 
    rvest::html_children() |> 
    rvest::html_text2()
}
  
future::plan("multisession")
hop_aroma_tags <- hop_table |> 
  bind_cols(links) |> 
  mutate(value = paste0("https://beermaverick.com", value)) |> 
  rename(link = value) |> 
  mutate(brew_values = furrr::future_map(link, ~extract_aroma_tags_from_link(.x)))
future::plan("default")

hop_aroma_tags <- hop_aroma_tags |> 
  rowwise() |> 
  mutate(brew_values = str_flatten_comma(brew_values))

# Since this is so similar to the hop_aroma data we will just add this info to that table. 

hop_aromas <- hop_aromas |> 
  left_join(hop_aroma_tags) |> 
  rename(aroma_tags = brew_values)

hop_aromas |> 
  write_tsv("hop_aromas.txt")

# fix brew values

hop_brew_values <- hop_brew_values |> 
  mutate(range_min = ifelse(hop_name == "Walhalla" & brew_value == "Alpha Acid %", 5.97, range_min)) |> 
  mutate(range_min = ifelse(hop_name == "Walhalla" & brew_value == "Beta Acid %", 3.75, range_min)) 

hop_brew_values |> 
  write_tsv("hop_brew_values.txt")
