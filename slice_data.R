dmap(mtcars, summary)

# dmap() also supports sliced data frames:
sliced_df <- mtcars[1:5] %>% slice_rows("cyl")
sliced_df %>% dmap(mean)
sliced_df %>% dmap(~ .x / max(.x))

# This is equivalent to the combination of by_slice() and dmap()
# with 'rows' collation of results:
sliced_df %>% by_slice(dmap, mean, .collate = "rows")


#######
# figure out later? 
slice %>% select(ends_with("calc")) %>% 
  t() %>% as_tibble() %>% 
  mutate(var_names = group_var_names) %>%
  left_join(categories[c("var_names", "group_labels")], by = "var_names")  %>%
  select(-var_names) %>%
  dplyr::group_by(group_labels) %>%
  dplyr::summarise(across(everything(), list = sum), .groups = 'drop')


slice %>% select(ends_with("calc")) %>% 
  t() %>% as_tibble() %>% 
  mutate(var_names = group_var_names) %>%
  left_join(categories[c("var_names", "group_labels")], by = "var_names")  %>%
  select(-var_names) %>%
  dplyr::group_by(group_labels) %>%
  summarise(n= n())

# the other option is to create an external array and then loop through the columns
# but I don't like that as much
slice
colnames(a)

library(purrrlyr)
slice %>% select(ends_with("calc")) %>% 
  t() %>% tibble() %>% 
  mutate(var_names = group_var_names) %>%
  left_join(categories[c("var_names", "group_labels")], by = "var_names")  %>%
  select(-var_names) %>%
  slice_rows("group_labels") %>% dmap(sum)

for(i in 1:length(colnames(a))) {
  print(typeof(a[, i]))
}

a <- slice %>% select(ends_with("calc")) %>% 
  t () %>% as_tibble() %>%
  mutate(var_names = group_var_names) %>%
  left_join(categories[c("var_names", "group_labels")], by = "var_names")  %>%
  select(-var_names) 

# replacing the NA with Total
a["group_labels"][is.na(a["group_labels"])] <- "Total"

b <- a %>% unnest(cols = c(colnames(a))) %>% replace(is.na(.), 0)

b %>%
  dmap(summary) %>%
  by_slice(dmap, sum, .collate = "rows")

sliced_df %>% by_slice(dmap, mean, .collate = "rows")

a %>% unnest(cols = colnames(a))