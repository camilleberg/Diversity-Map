
PIE_CHART_FUNC <- function(input, neigh_df){
  
  #do I need to do this?
  neigh_df <- neigh_df %>% as_tibble()
  
  #Gets rid of prefix. Used for starts with functionality
  selected_value <- str_sub(input, start = 5)
  
  BOs_DF <- neigh_df %>%
    filter(tract20_nbhd == "Citywide") %>% 
    select(starts_with(selected_value))
  
  pie_labels <- colnames(BOs_DF) %>% str_sub(start = str_length(selected_value) + 2) %>% str_replace_all("_", " ") 
  pie_labels <- lapply(pie_labels, pie_legend_label_fxn) %>% unlist()
  pie_labels <- pie_labels %>% factor(levels = pie_labels, ordered = T) 
  pie_values <- as.numeric(as.vector(BOs_DF[1,]))
  
  citywide_sample <- tibble(Categories = pie_labels, Citywide = pie_values)
  
  pie_chart <- plot_ly(labels = ~pie_labels, values = ~pie_values, 
                       type = 'pie', sort = F,
                       marker = list(colors = c("#fdb462","#8dd3c7","#ffffb3","#bebada","#b3de69",
                                                         "#fccde5","#d9d9d9","#9c755f","#d37295","#00ffd0","#9467bd"))) %>%
                                                           layout(title = paste0(selected_value),
                                                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
                                                                  legend = list(tracegroupgap = 30))
  pie_chart
  
}


PIE_CHART_FUNC("val_race_seven", race_diversity_neighborhood)
neigh_df <- race_diversity_neighborhood
input <- "val_race_seven"

# this inserts breaks every 25 pixels 
pie_legend_label_fxn <- function(pie_label_name) {
  return(paste(strwrap(pie_label_name, width = 25), collapse = "<br>"))
}


