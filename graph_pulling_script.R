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
library(htmlwidgets)

# reading in all the data from the output folder 
setwd("./output")
all_tract_data <- list.files(pattern = "tract.RDS") %>%
  map(readRDS) 
setwd("..")

# separating out the div values 
for(i in 1:length(all_tract_data)) {
  temp <- all_tract_data[[i]] %>% as_tibble() 
  var_name <- strsplit(colnames(temp)[4], "_")[[1]][1]
  assign(paste0(var_name, "_values"), temp, envir = .GlobalEnv)
}


# need input and tract_df
PIE_CHART_TRACT_FUNC <- function(tract_name, tract_df_graph, selected_val){
  
  tract_df_graph <- tract_df_graph %>% filter(NAME == paste0(tract_name))
  
  # labeling variables 
  pie_labels <- colnames(tract_df_graph) %>% str_sub(start = str_length(selected_val) + 2) %>% str_replace_all("_", " ") 
  pie_labels <- pie_labels %>% factor(levels = pie_labels, ordered = T)
  pie_values <- as.numeric(as.vector(tract_df_graph[1,]))
  
  # creating the graph 
  pie_chart <- plot_ly(labels = ~pie_labels, values = ~pie_values, 
                       type = 'pie', sort = F,
                       marker = list(colors = c("#fdb462","#8dd3c7","#ffffb3","#bebada","#b3de69",
                                                         "#fccde5","#d9d9d9","#9c755f","#d37295","#00ffd0","#9467bd"))) %>%
                                                           layout(title = strsplit(paste0(tract_name), ",")[[1]][1],
                                                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  pie_chart
  
}

# uses tghe chart function to make all the charts per tracts
  # and also converts it into a useable format 
plot_html_fxn <- function(tract_df, input) {
  
  # Gets rid of prefix. Used for starts with functionality
  selected_val <- str_sub(input, start = 5)

  tract_df_graph <- tract_df %>% 
    select(c(NAME, starts_with(selected_val)))
  
  # this creates the chart for each tract and stores them as a list
  p_all_plotly <- lapply(tract_df$NAME, PIE_CHART_TRACT_FUNC, 
                         tract_df_graph = tract_df_graph, 
                         selected_val = selected_val)
  
  # this writes the list file as an external html file
  # this is nec to add as a popup in leaflet maps (idk why tho)
  p_all_plotly_html <- lapply(p_all_plotly, function(plot) {
    f1 = fl = tempfile(fileext = ".html")
    saveWidget(plot, file = fl)
    return(fl) 
  }
  )
  write_rds(p_all_plotly_html, paste0("graph_files/graph_", selected_val, ".RDS"))
}

# function to write input values
input_val_fxn <- function(tract_df) {
  var <- strsplit(deparse(substitute(tract_df)), "_")[[1]][1]
  input_vals <- colnames(tract_df)[grepl("^val_", colnames(tract_df))]
  assign(paste0(var, "_input_vals"), input_vals, envir = .GlobalEnv)
}
  # in case the oter thing (below) doesn't work

plot_output_fxn <- function(tract_df) {
  input_vals <- colnames(tract_df)[grepl("^val_", colnames(tract_df))]
  for(i in 1:length(input_vals)) {
    plot_html_fxn(tract_df = tract_df, input_vals[i])
  }
}
plot_output_fxn(race_values)
plot_output_fxn(pob_values)
plot_output_fxn(age_values)
plot_output_fxn(hh_values)
plot_output_fxn(lang_values)
plot_output_fxn(educ_values)
