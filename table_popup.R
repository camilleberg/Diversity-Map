# script fo rtable popup


# map _func
rm(list = ls())

{input <- "val_pob_us_regions"
tract_df <- pob_diversity_tract
scale_bool <- T
}

{
tract_df <- tract_df %>% st_as_sf()

selected_val <- str_sub(input, start = 5)

tract_df$current_data <- tract_df[[input]]

NUM_VARIABLES <- tract_df %>% 
  select(starts_with(selected_val)) %>%  
  colnames() %>% 
  length()

NUM_VARIABLES <- NUM_VARIABLES - 1

max_val <- 1 - (1/(NUM_VARIABLES))

pal_option <- "RdYlBu"

# pal_option <- addalpha(pal_option, alpha = .5)

#pal <- colorQuantile(palette = pal_option, domain = tract_df$current_data, n = 5)
if (scale_bool){
  
  pal <- colorNumeric(palette = pal_option, domain = c(0, max_val),
                      na.color = "#505050")
  legend_label <- "Diversity Index Score"
}else {
  pal <- colorQuantile(palette = pal_option, domain = tract_df$current_data, n = 5)
  
  legend_label <- "Percentile of Diversity Index"
}
}

# popup = tract_df %>% 
#             as_tibble() %>% 
#             mutate(`Census Tract` = str_sub(NAME, start = 14, end = -32)) %>% 
#             select(`Census Tract`, contains('Total', ignore.case = F), starts_with(selected_val)) %>%
#             rename_with( ~ str_sub(.x, start = str_length(input) - 2), starts_with(selected_val)) %>%
#             rename_with( ~ gsub("_", " ", .x, fixed = TRUE)) %>%
#             popupTable(feature.id = F,
#                        row.numbers = F),


tract_df %>%
  mutate(current_data = ifelse(startsWith(NAME, "Census Tract 98"), NaN,current_data)) %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>% 
  addTiles() %>%
  addProviderTiles(provider = "CartoDB.Positron")  %>%
  addPolygons(
    stroke = F,
    smoothFactor = 0,
    fillOpacity = 0.7,
    color = ~ pal(current_data), group = 'current_data',
    popup = popupTable(table_popup_fxn(tract_df, input), 
                       feature.id = F,row.numbers = F)
  ) %>%
  addLegend("bottomright", 
            pal = pal,
            values = ~ current_data,
            title = legend_label,
            opacity = 1,
            na.label = 'Tracts with little or no population')

# table function

table_popup_fxn <- function(tract_df, input) {
  
  # renamng columns
  table_df <- tract_df %>% as_tibble() %>% mutate(`Census Tract` = str_sub(NAME, start = 14, end = -32)) %>%
    select(`Census Tract`, contains('Total', ignore.case = F), starts_with(selected_val)) 
  
  table_df <- table_df %>%  
    rename_with(.cols = 2, ~"Total") %>%
    dplyr::mutate_at(vars(3:ncol(table_df)), list(share=~./Total)) %>%
    select(`Census Tract`, ends_with("Total"), ends_with("share")) %>%
    rename_with( ~ str_sub(.x, start = str_length(input) - 2), ends_with("share")) %>%
    rename_with( ~ gsub("_", " ", .x, fixed = TRUE)) %>%
    mutate(Total = as.character(Total)) 
    # mutate_if(is.numeric,scales::percent)
  
  # renaming columns per variable
  if(str_contains(input, "hh")) {
    table_df <- table_df %>% rename_with(.cols = 2, ~"Total Households")
  } else if(str_contains(input, "educ")) {
    table_df <- table_df %>% rename_with(.cols = 2, ~"Total Persons 25+")
  }else if(str_contains(input, "lang")) {
    table_df <- table_df %>% rename_with(.cols = 2, ~"Total Persons 5+")
  } else {
    table_df <- table_df %>% rename_with(.cols = 2, ~"Total Persons")
  }
  return(table_df)
}

# share function

library(scales)
library(sjmisc)
df %>% ftransformv(1:2, function(x) .[[3]])
