

rm(list = ls())

{input <- "val_age_all"
tract_df <- age_diversity_tract
scale_bool <- T
}

{
  tract_df <- tract_df %>% st_as_sf()
  
  selected_val <- str_sub(input, start = 5)
  tract_df$current_data <- tract_df[[input]]
  
  # reading in the data
  p_all <- paste0("./graph_files/", list.files(path = "./graph_files/", pattern = selected_val)) %>% readRDS()
  
  p_tracts <-tibble::enframe(p_all) %>% tidyr::pivot_wider() %>% t()
  
  tract_df <- tract_df %>%
    mutate(pie_graph = p_tracts)
  
  NUM_VARIABLES <- tract_df %>% 
    select(starts_with(selected_val)) %>%  
    ncol() - 1
  
  max_val <- 1 - (1/(NUM_VARIABLES))
  
  
  jank_minimum <- tract_df[tract_df$NAME == 'Census Tract 9815.01, Suffolk County, Massachusetts',]
  jank_minimum$current_data <- 0
  #jank_minimum$geometry <- list(list(c(-71.136596, -71.136273, 42.360161, 42.360903)))
  jank_minimum$NAME <- 'min_val'
  tract_df <- rbind(jank_minimum %>% st_as_sf(), tract_df)
  
  jank_max <- tract_df[tract_df$NAME == 'Census Tract 9813, Suffolk County, Massachusetts',]
  jank_max$current_data <- max_val
  jank_max$NAME <- 'max_val'
  tract_df <- rbind(jank_max, tract_df)
  
  #tract_df <- tract_df %>% st_as_sf()
  
  pal_option <- "RdYlBu"
  
  # pal_option <- addalpha(pal_option, alpha = .5)
  
  #pal <- colorQuantile(palette = pal_option, domain = tract_df$current_data, n = 5)
  if (scale_bool){
    
    #####FIGURE OUT WHY I NEED TO SUBTRACT 1
    pal <- colorNumeric(palette = pal_option, domain = c(0, max_val),
                        na.color = "#505050")
    legend_label <- "Diversity Index Score"
    
  }else {
    pal <- colorQuantile(palette = pal_option, domain = tract_df$current_data, n = 5)
    
    legend_label <- "Percentile of Diversity Index"
  }
}





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
    popup = popupGraph(p_all, type = "html")
    ) %>%
  addLegend("bottomright", 
            pal = pal,
            values = ~ current_data,
            title = legend_label,
            opacity = 1,
            na.label = 'Tracts with little or no population')
