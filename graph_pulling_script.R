# tract pie chart script

rm(list = ls())
library(tidycensus)
library(tidyverse)
library(leaflet)
library(leafpop)
library(mapview)
library(sf)
library(stringr)
library(plotly) 
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(janitor)

# reading in all the data
all_tract_data <- list.files(pattern = "tract.RDS") %>%
  map(readRDS) 

# separating out the div values 
for(i in 1:length(all_tract_data)) {
  temp <- all_tract_data[[i]] %>% as_tibble() %>% select(c(NAME, starts_with("val_")))
  var_name <- strsplit(colnames(temp)[2], "_")[[1]][2]
  assign(paste0(var_name, "_values"), temp, envir = .GlobalEnv)
}

tract_df <- age_values



# need input and tract_df
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
    layout(title = strsplit(paste0(tract_name), ",")[[1]][1],
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  pie_chart
  
}

# running thorugh all to create the plots
p_all_plotly <- lapply(tract_df$NAME, PIE_CHART_TRACT_FUNC)

# running through to save as html file 
p_all_plotly_html <- lapply(p_all_plotly, function(plot) {
  f1 = fl = tempfile(fileext = ".html")
  saveWidget(plot, file = fl)
  return(fl) }
)


