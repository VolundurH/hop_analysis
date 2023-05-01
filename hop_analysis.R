library(tidyverse)

hop_aromas <- read_tsv("hop_aromas.txt")
hop_brew_values <- read_tsv("hop_brew_values.txt")

theme_set(theme_classic())

hop_aromas %>% 
  drop_na() %>% 
  pivot_longer(cols = 4:12, names_to = "Cat", values_to = "value") %>% 
  ggplot(aes(x = fct_inorder(Cat), y = value, group = hop_name)) + 
  geom_path(col = "grey50") +
  geom_point(aes(col = Cat), size = 3) + 
  coord_polar() + 
  theme(legend.position = "none") + 
  labs(x = "Category")


edge_table <- tibble("name"="A", "from"=0, "to"=0, "n"=0)

for (i in 4:11) {
  edge_table <- edge_table %>% 
    bind_rows(
      hop_aromas %>% 
        select(i, i+1) %>% 
        drop_na() %>% 
        mutate(name = paste0(colnames(.), collapse = ",")) %>% 
        rename(from = 1, to = 2) %>% 
        count(name, from, to)
    )
}

edge_table <- edge_table %>% 
  filter(name != "A") %>% 
  separate(name, into = c("name_from", "name_to"), sep = ",")

node_table <- hop_aromas %>% 
  drop_na() %>% 
  pivot_longer(cols = 4:12, names_to = "Cat", values_to = "value") %>% 
  mutate(Cat = fct_inorder(Cat)) %>% 
  count(Cat, value)

node_table %>% 
  ggplot() + 
  geom_point(aes(x = fct_inorder(Cat), y = value, size = n, col = Cat)) +
  geom_segment(data = edge_table,  aes(x = fct_inorder(name_from), xend = name_to, y = from, yend = to, size = log(n)),
               alpha = 0.5, color = "grey50") + 
  ylim(c(-1,5)) +
  theme(axis.text.y = element_blank() ) +
  labs(x = NULL, y = NULL) +
  coord_polar()






hop_brew_values_wide <- hop_brew_values %>% 
  select(-range_min, -range_max) %>% 
  drop_na() %>% 
  pivot_wider(names_from =value, values_from = range_mean) 

hop_brew_values_wide %>% 
  ggplot(aes(x = `Alpha Acid %`, y = `Beta Acid %`, col = hop_purpose)) +
  geom_point() +
  scale_color_viridis_d() + 
  labs(col = NULL)

hop_table_all <- hop_brew_values_wide %>% 
  left_join(hop_aromas)



# taken from https://juliasilge.com/blog/cocktail-recipes-umap/ 

library(tidymodels)

# PCA ----

pca_prep <- recipe(~., data = hop_table_all) %>% 
  update_role(hop_name, hop_purpose, country, new_role = "id") %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors()) %>% 
  prep() 

pca_prep %>% 
  tidy(3) %>% 
  filter(component %in% paste0("PC", 1:3)) %>% 
  ggplot(aes(x = value, y = terms, fill = terms)) + 
  geom_col(show.legend = F) +
  facet_wrap(~component) +
  labs(x = NULL, y = NULL)

pca_prep %>% 
  juice() %>% 
  ggplot(aes(PC1, PC2, col = hop_purpose)) + 
  geom_point() +
  scale_color_viridis_d()


# UMAP ----

library(embed)

umap_rec <- recipe(~., data = hop_table_all) %>% 
  update_role(hop_name,hop_purpose,country, new_role = "id") %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_umap(all_predictors(), neighbors = 3, seed = c(321, 321))

umap_prep <- prep(umap_rec)

juice(umap_prep) %>% 
  ggplot(aes(UMAP1, UMAP2, label = hop_name)) +
  geom_point(aes(col = hop_purpose), size = 3, alpha = 0.6) +
  geom_text(check_overlap = TRUE, hjust = "inward", size = 3) +
  labs(x = NULL, y = NULL, col = NULL)

