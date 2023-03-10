PIE_CHART_TRACT_FUNC <- function(tract_name){
  
  #Gets rid of prefix. Used for starts with functionality
  selected_val <- str_sub(input, start = 5)
  
  tract_df_graph <- tract_df %>% 
    filter(NAME == paste0(tract_name)) %>%
    select(starts_with(selected_val))
  
  pie_labels <- colnames(tract_df_graph) %>% str_sub(start = str_length(selected_val) + 2) %>% str_replace_all("_", " ") 
  pie_labels <- pie_labels %>% factor(levels = pie_labels, ordered = T)
  pie_values <- as.numeric(as.vector(tract_df_graph[1,]))
  
  
  pie_chart <- plot_ly(labels = ~pie_labels, values = ~pie_values, 
                       type = 'pie', sort = F,
                       marker = list(colors = c("#fdb462","#8dd3c7","#ffffb3","#bebada","#b3de69",
                                                "#fccde5","#d9d9d9","#9c755f","#d37295","#00ffd0","#9467bd"))) %>%
    layout(title = paste0(tract_name),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  pie_chart
  
}

p_all_plotly <- lapply(tract_df$NAME, PIE_CHART_TRACT_FUNC)

p_all_plotly_html <- lapply(p_all_plotly, function(plot) {
  f1 = fl = tempfile(fileext = ".html")
  saveWidget(plot, file = fl)
  return(fl) 
  }
)


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
    color = ~ pal(current_data), group = 'current_data') %>%
  leafpop:::addPopupIframes(source = p_all_plotly_html, group = 'current_data') %>%
  addLegend("bottomright", 
            pal = pal,
            values = ~ current_data,
            title = legend_label,
            opacity = 1,
            na.label = 'Tracts with little or no population')



