{
  library(DT)
  library(leaflet)
  library(leafpop)
  library(plotly)
  library(RColorBrewer)
  library(sf)
  library(shiny)
  library(shinythemes)
  library(stringr)
  library(tidycensus)
  library(tidyverse)
  library(viridis)
  library(shinyjs)
  library(shinydlplot)
}

#Setup ###################################################################################################
{
  #Functions & Constants ##################################################################################################
  {
    css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
    html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))  # Convert CSS to HTML
    
    #Text for raw or scaled button
    raw_scale_text <- '<p><span style="color:#003d50"><span style="font-size:11pt"><span style="font-family:Arial">The raw data option shows the actual scores from the diversity index calculation. <br> 
    The scaled option ranks all of the tracts in the city. This creates more visual distinction as there will always be a most and least diverse tract. 
    It can accentuate any differences in the city if tracts have similar index value.</span></span></span></h2>'
    
    #was going to do more. Might pull from spreadsheet in the future
    TAB_NAMES <- c("Map", "City Comparison Table","Neighborhood Table","City Graph","Neighborhood Graph")
    
    raw_scale_dialog <- modalDialog(title = HTML('<h1><span style="color:#0fa6b4"><strong> Raw Data vs. Scaled Data</strong></span></h1>'), 
                                    HTML(raw_scale_text), easyClose = TRUE)
    
    #Options for table, currently not being used
    TABLE_OPTIONS <- list(searching = F, lengthChange = F, info = F, paging = F, order = list(2, 'desc'), rownames = F)
    
    #popup dialogue functions
    GET_POPUP <- function(var){
      modalDialog(title = HTML(paste0('<h1><span style="color:#0fa6b4"><strong>', fancy_text(var),'</strong></span></h1>')),
        HTML(toString(text[text$title_name==var,2])), easyClose = TRUE)}
    
    pie_legend_label_fxn <- function(pie_label_name) {  
      return(paste(strwrap(pie_label_name, width = 25), collapse = "<br>"))
    }
    
    #Builds citywide pie chart
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
                           rotation = ifelse(input %in% pob_choices$category_val, 270, 0),
                           marker = list(colors = c("#fdb462","#8dd3c7","#ffffb3","#bebada","#b3de69",
                                              "#fccde5","#d9d9d9","#9c755f","#d37295","#00ffd0","#9467bd"))) %>%
        layout(title = fancy_text(selected_value),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      pie_chart
    }
    
    NEIGH_BUTTON_FUNC <- function(input, div_df){
      
      div_df[[input]] <- div_df[[input]] %>% 
        as.numeric() %>% 
        round(digits = 2) 
      
      div_df %>% 
        select(tract20_nbhd, input)
    }
    
    CITY_BUTTON_FUNC <- function(input, div_df){
      
      div_df[[input]] <- div_df[[input]] %>% 
        as.numeric() %>% 
        round(digits = 2) 
      
      div_df %>% 
        select(City, input)
    }
    
    NEIGH_GRAPH_FUNC <- function(input, div_df){
      
      NEW_DF <- div_df %>% 
        mutate(cur_val = 1)
      
      NEW_DF$cur_val <- div_df[[input]] %>% round(digits = 2)
      
      NEW_DF %>%
        select(cur_val, tract20_nbhd) %>% 
        plot_ly(y = ~tract20_nbhd,
                x = ~cur_val,
                name = "Neighborhood",
                type = "bar",
                orientation = "h",
                color = ~tract20_nbhd == "Citywide", 
                colors = c("#2597ba","#cf4666"),
                height = 750) %>%
        hide_legend() %>% 
        layout(xaxis = list(title = "Diversity Index Value", side ="top"),
               yaxis = list(title = '', categoryorder = "total ascending"))
    }
    
    CITY_GRAPH_FUNC <- function(input, div_df){
      
      NEW_DF <- div_df %>% 
        mutate(cur_val = 1)
      
      NEW_DF$cur_val <- div_df[[input]] %>% round(digits = 2)
      
      NEW_DF %>%
        select(cur_val, City) %>% 
        plot_ly(y = ~City,
                x = ~cur_val,
                name = "City",
                type = "bar",
                orientation = "h",
                color = ~City == "Boston city, Massachusetts", 
                colors = c("#2597ba","#cf4666"),
                height = 750) %>%
        hide_legend() %>% 
        layout(xaxis = list(title = "Diversity Index Value", side ="top"),
               yaxis = list(title = '', categoryorder = "total ascending"))
    }
    
    MAP_FUNC <- function(input, scale_bool, tract_df){
      
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
      
      # p_all <- read_rds(paste0("graph_files/graph_", selected_val, ".RDS"))
      p_all <- paste0("./graph_files/", list.files(path = "./graph_files/", pattern = selected_val)) %>% readRDS()
      
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
      
      tract_df %>%
        #filter(!(NAME %in% c("min_val", "max_val"))) %>% 
        #add_row(current_data = 0) %>% 
        mutate(current_data = ifelse(startsWith(NAME, "Census Tract 98"), NaN,current_data)) %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(height = "100%",
                width = "100%") %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
          # popup = tract_df %>% 
          #             as_tibble() %>% 
          #             mutate(`Census Tract` = str_sub(NAME, start = 14, end = -32)) %>% 
          #             select(`Census Tract`, contains('Total', ignore.case = F), starts_with(selected_val)) %>%
          #             rename_with( ~ str_sub(.x, start = str_length(input) - 2), starts_with(selected_val)) %>%
          #             rename_with( ~ gsub("_", " ", .x, fixed = TRUE)) %>%
          #             popupTable(feature.id = F,
          #                        row.numbers = F),
        addPolygons(
          popup = tract_df %>%
                      as_tibble() %>%
                      mutate(`Census Tract` = str_sub(NAME, start = 14, end = -32)) %>%
                      select(`Census Tract`, contains('Total', ignore.case = F), starts_with(selected_val)) %>%
                      rename_with( ~ str_sub(.x, start = str_length(input) - 2), starts_with(selected_val)) %>%
                      rename_with( ~ gsub("_", " ", .x, fixed = TRUE)) %>%
                      popupTable(feature.id = F, row.numbers = F),
                    stroke = F,
                    smoothFactor = 0,
                    fillOpacity = 0.7,
                    color = ~ pal(current_data)) %>%
          # addPolygons(
          #   stroke = F,
          #   smoothFactor = 0,
          #   fillOpacity = 0.7,
          #   color = ~ pal(current_data), group = 'current_data', 
          #   popup = popupGraph(p_all)
          # ) %>% 
        # leafpop:::addPopupIframes(source = p_all, group = 'current_data') %>%
        addLegend("bottomright", 
                  pal = pal,
                  values = ~ current_data %>% append(values = c(0, max_val)),
                  title = legend_label,
                  opacity = 1,
                  na.label = 'Tracts with little or no population') 

    }
    
    min.col <- function(x){
      max.col(-x)
    }
    
    INVERSE_RANK <- function(x){rank(-x)}
    
    COL_MINMAX_CITIES <- function(var){
      
      nice_name <- fancy_text(var)
      
      bos <- all_cities_ranked %>% 
        #colnames(DF)[max.col(DF,ties.method="first")]
        filter(City == "Boston city, Massachusetts") %>% 
        select(contains(var))
      lowest_col <- colnames(bos)[min.col(bos)]
      highest_col <- colnames(bos)[max.col(bos)]
      
      low_n <- bos[[lowest_col]]
      high_n <- bos[[highest_col]]
      
      paste0("<h3>", nice_name, "</h3> <p> <strong>#", 
             low_n, "</strong> when choosing <strong>", fancy_text(lowest_col), "</strong> <p> <strong>#", 
             high_n, "</strong> when choosing <strong>", fancy_text(highest_col), "</strong>")
    }
    GET_MINMAX_CITY <- function(){
        paste0("<h2>Highest and Lowest Rankings out of 25 Comparable Cities</h2>",
               COL_MINMAX_CITIES("race"), COL_MINMAX_CITIES("pob"), COL_MINMAX_CITIES("hh"), 
               COL_MINMAX_CITIES("educ"), COL_MINMAX_CITIES("lang"), COL_MINMAX_CITIES("age"))
    }
    
    COL_MINMAX_NEIGH <- function(var, neigh){
      
      nice_name <- fancy_text(var)
      
      just_bos <- all_cities_ranked %>% 
        filter(City == "Boston city, Massachusetts") %>% 
        rename(tract20_nbhd = City)
      
      bos <- just_bos %>% 
        rbind(all_neigh_ranked) %>% 
        filter(tract20_nbhd == neigh) %>% 
        select(contains(var))
      
      lowest_col <- colnames(bos)[min.col(bos)]
      highest_col <- colnames(bos)[max.col(bos)]
      
      low_n <- bos[[lowest_col]]
      high_n <- bos[[highest_col]]
      paste0("<h3>", nice_name, "</h3> <p> ",
             "<strong>#", low_n,  "</strong> when choosing <strong>", fancy_text(lowest_col), "</strong> <p> ",
             "<strong>#", high_n, "</strong> when choosing <strong>", fancy_text(highest_col), "</strong>")
    }
    GET_MINMAX_NEIGH <- function(neigh_input){
      
      paste0("<h2>Highest and lowest rankings out of 24 Neighborhoods: <strong>", neigh_input, "</strong> </h2>",
             COL_MINMAX_NEIGH("race", neigh_input), COL_MINMAX_NEIGH("pob", neigh_input), COL_MINMAX_NEIGH("hh", neigh_input),
             COL_MINMAX_NEIGH("educ", neigh_input), COL_MINMAX_NEIGH("lang", neigh_input), COL_MINMAX_NEIGH("age", neigh_input))
    }
    
    BUILD_RADIO_BUTTONS <- function(id, lab){
      radioButtons(inputId = id,
                   label = lab,
                   choices = list("Raw Data" = T,
                                  "Scaled Data" = F))
    }
    
    GET_BUTTONS <- function(cat){
      choice_str <- cat
      
      categories_titles %>%
        filter(title == choice_str) %>%
        mutate(category_val = paste0("val_", choice_str, "_", group_name)) %>% 
        select(category_name, category_val)
    }
    
    fancy_text <- function(string){
      
      string <- string %>% toString()
      
      fancy_list <- name_titles
      
      fancy_list2 <- categories_titles %>% 
        mutate(full_name = paste0(title, "_", group_name))
      
      fancy_list3 <- categories_titles %>% 
        mutate(full_name = paste0("rank_val_", title, "_", group_name))
      
      if(string %in% fancy_list$title) {
        fancy_string <- fancy_list$title_name[fancy_list$title==string]
        return(fancy_string)
      }else if(string %in% fancy_list2$full_name) {
        fancy_string <- fancy_list2$category_name[fancy_list2$full_name==string]
        return(fancy_string)
      }else if(string %in% fancy_list3$full_name) {
        fancy_string <- fancy_list3$category_name[fancy_list3$full_name==string]
        return(fancy_string)
      }else{
        return(string)
      }
      
    }
    
  }
  
  #Pulling in data ##################################################################################################
  {
    race_div_tract <- read_rds("data/race_diversity_tract.RDS") %>% 
      mutate(current_data = 1)
    
    race_div_neigh <- read_rds("data/race_diversity_neighborhood.RDS")
    race_div_cities <- read_rds("data/race_diversity_cities.RDS")
    
    race_div_cities<- race_div_cities%>% 
      mutate(City = NAME) %>% 
      select(-NAME)
    
    age_div_tract <- read_rds("data/age_diversity_tract.RDS") %>% 
      mutate(current_data = 1)
    
    age_div_neigh <- read_rds("data/age_diversity_neighborhood.RDS")
    age_div_cities <- read_rds("data/age_diversity_cities.RDS")
    
    age_div_cities<- age_div_cities%>% 
      mutate(City = NAME) %>% 
      select(-NAME)
    
    educ_div_tract <- read_rds("data/educ_diversity_tract.RDS") %>% 
      mutate(current_data = 1)
    
    educ_div_neigh <- read_rds("data/educ_diversity_neighborhood.RDS")
    educ_div_cities <- read_rds("data/educ_diversity_cities.RDS")
    
    educ_div_cities<- educ_div_cities%>% 
      mutate(City = NAME) %>% 
      select(-NAME)
    
    hh_div_tract <- read_rds("data/hh_diversity_tract.RDS") %>% 
      mutate(current_data = 1)
    
    hh_div_neigh <- read_rds("data/hh_diversity_neighborhood.RDS")
    hh_div_cities <- read_rds("data/hh_diversity_cities.RDS")
    
    hh_div_cities<- hh_div_cities %>% 
      mutate(City = NAME) %>% 
      select(-NAME)
    
    lang_div_tract <- read_rds("data/lang_diversity_tract.RDS") %>%
      mutate(current_data = 1)
    
    lang_div_neigh <- read_rds("data/lang_diversity_neighborhood.RDS")
    lang_div_cities <- read_rds("data/lang_diversity_cities.RDS")
    
    lang_div_cities <- lang_div_cities %>% 
      mutate(City = NAME) %>% 
      select(-NAME)
    
    pob_div_tract <- read_rds("data/pob_diversity_tract.RDS") %>% 
      mutate(current_data = 1)
    
    pob_div_neigh <- read_rds("data/pob_diversity_neighborhood.RDS")
    pob_div_cities <- read_rds("data/pob_diversity_cities.RDS") 
    
    pob_div_cities<- pob_div_cities%>% 
      mutate(City = NAME) %>% 
      select(-NAME)
  }
  
  #Titles and choices ##################################################################################################
  {
    categories <- readxl::read_xlsx("div_map_categories.xlsx", sheet = "categories")
    text <- readxl::read_xlsx("div_map_categories.xlsx", sheet = "text_descriptions")
    
    categories_titles <- readxl::read_xlsx("div_map_categories.xlsx", sheet = "categories") %>%
      filter(is_category == 1) %>% 
      select(title, group_name, category_name)
    
    name_titles <- categories %>%
      filter(is_title == 1) %>% 
      select(title, title_name)
    
    race_choices <- GET_BUTTONS("race")
    pob_choices  <- GET_BUTTONS("pob")
    hh_choices <- GET_BUTTONS("hh")
    lang_choices  <- GET_BUTTONS("lang")
    educ_choices  <- GET_BUTTONS("educ")
    age_choices  <- GET_BUTTONS("age")
    
    neigh_choices <- race_div_neigh$tract20_nbhd %>%  unique()
    
    neigh_choices <- neigh_choices[! neigh_choices %in% c('Citywide')]
    
    css_pieces <- read_csv("tab_css.csv")
    
    css_text <- ""
    for (i in 1:length(css_pieces$tab_titles)) {
      
      current_text <- paste0(".tabbable > .nav > li > a[data-value='", css_pieces$tab_titles[i], "'] {color: #ffffff; font-family: Arial,Helvetica,sans-serif;background-color:", 
                     css_pieces$tab_colors2[i], 
                     "; }")
      css_text <- paste(css_text, current_text)
    }
    
  }
  
  #Combining dfs ##################################################################################################
  {
    race_div_tract$current_data <- race_div_tract[race_choices$category_val[[1]]]
    age_div_tract$current_data <- age_div_tract[age_choices$category_val[[1]]] 
    educ_div_tract$current_data <- educ_div_tract[educ_choices$category_val[[1]]] 
    hh_div_tract$current_data <- hh_div_tract[hh_choices$category_val[[1]]]
    lang_div_tract$current_data <- lang_div_tract[lang_choices$category_val[[1]]]
    pob_div_tract$current_data <- pob_div_tract[pob_choices$category_val[[1]]]
    
    all_neigh <- race_div_neigh %>% 
      left_join(pob_div_neigh)  %>% 
      left_join(hh_div_neigh)  %>%
      left_join(lang_div_neigh)  %>%
      left_join(educ_div_neigh)  %>%
      left_join(age_div_neigh)
    
    all_cities <- race_div_cities %>% 
      left_join(pob_div_cities)  %>% 
      left_join(hh_div_cities)  %>%
      left_join(lang_div_cities)  %>%
      left_join(educ_div_cities)  %>%
      left_join(age_div_cities)
    
    all_cities_ranked <- all_cities %>% 
      select(City, starts_with("val_")) %>% 
      mutate(across(starts_with("val_"), INVERSE_RANK, .names = "rank_{col}")) %>% 
      select(City, starts_with("rank_"))
    
    all_neigh_ranked <- all_neigh %>% 
      select(tract20_nbhd, starts_with("val_")) %>% 
      mutate(across(starts_with("val_"), INVERSE_RANK, .names = "rank_{col}")) %>% 
      select(tract20_nbhd, starts_with("rank_"))
  }
}
#UI ##################################################################################################
ui <- fluidPage(
  
  # useShinyjs(),
  theme = shinytheme("lumen"),
  HTML(html_fix),
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  tags$style(HTML(css_text)),

  titlePanel("Diversity Map"),

  tabsetPanel(
    tabPanel("About",
             {HTML(text$title_text[1])}
    ),
    tabPanel("Race & Ethnicity",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "race",
                              label = actionButton("race_popup", "Category Breakdown"),
                              choiceNames = race_choices$category_name, choiceValues = race_choices$category_val),
                 BUILD_RADIO_BUTTONS("race_bool", lab = actionButton("race_help", "Legend Type")),
                 plotlyOutput("race_pie")),
               mainPanel(
                 tabsetPanel(
                   tabPanel(TAB_NAMES[1],
                            tags$style(type = "text/css", "#race_map {height: calc(85vh) !important;}"),
                            leafletOutput("race_map")),
                   tabPanel(TAB_NAMES[5],  
                            fluidRow(
                              column(width = 10), column(downloadButton("race_neigh_download", label = "Download Data"), width = 2),
                              column(width = 12,
                            plotlyOutput("race_graph_neigh")))),                   
                   tabPanel(TAB_NAMES[4],  
                            fluidRow(
                              column(width = 10), column(downloadButton("race_cities_download", label = "Download Data"), width = 2),
                              column(width = 12,
                            plotlyOutput("race_graph_cities"))))
                 )
               )
             )
    ),
    tabPanel("Region of Birth",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "pob",
                              label = actionButton("pob_popup", "Category Breakdown"),
                              choiceNames = pob_choices$category_name, choiceValues = pob_choices$category_val),
                 BUILD_RADIO_BUTTONS("pob_bool", lab = actionButton("pob_help", "Legend Type")),
                 plotlyOutput("pob_pie")),
               mainPanel(
                 tabsetPanel(
                   tabPanel(TAB_NAMES[1],
                            tags$style(type = "text/css", "#pob_map {height: calc(85vh) !important;}"),
                            leafletOutput("pob_map")),
                   tabPanel(TAB_NAMES[5], 
                            fluidRow(
                              column(width = 10), column(downloadButton("pob_neigh_download", label = "Download Data"), width = 2),
                              column(width = 12,
                                     plotlyOutput("pob_graph_neigh")))),
                   tabPanel(TAB_NAMES[4], 
                            fluidRow(
                              column(width = 10), column(downloadButton("pob_cities_download", label = "Download Data"), width = 2),
                              column(width = 12,
                            plotlyOutput("pob_graph_cities"))))
                 )
               )
             )
    ),
    tabPanel("Household Income",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "hh",
                              label = actionButton("hh_popup", "Category Breakdown"),
                              choiceNames = hh_choices$category_name, choiceValues = hh_choices$category_val),
                 BUILD_RADIO_BUTTONS("hh_bool", lab = actionButton("hh_help", "Legend Type")),
                 plotlyOutput("hh_pie")),
               mainPanel(
                 tabsetPanel(
                   tabPanel(TAB_NAMES[1],
                            tags$style(type = "text/css", "#hh_map {height: calc(85vh) !important;}"),
                            leafletOutput("hh_map")),
                   tabPanel(TAB_NAMES[5], 
                            fluidRow(
                              column(width = 10), column(downloadButton("hh_neigh_download", label = "Download Data"), width = 2),
                              column(width = 12,
                            plotlyOutput("hh_graph_neigh")))),
                   tabPanel(TAB_NAMES[4], 
                            fluidRow(
                              column(width = 10), column(downloadButton("hh_cities_download", label = "Download Data"), width = 2),
                              column(width = 12,
                                     plotlyOutput("hh_graph_cities")))),
                 )
               )
             )
    ),
    tabPanel("Language Spoken at Home",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "lang",
                              label = actionButton("lang_popup", "Category Breakdown"),
                              choiceNames = lang_choices$category_name, choiceValues = lang_choices$category_val),
                 BUILD_RADIO_BUTTONS("lang_bool", lab = actionButton("lang_help", "Legend Type")),
                 plotlyOutput("lang_pie")),
               mainPanel(
                 tabsetPanel(
                   tabPanel(TAB_NAMES[1],
                            tags$style(type = "text/css", "#lang_map {height: calc(85vh) !important;}"),
                            leafletOutput("lang_map")),
                   tabPanel(TAB_NAMES[5], 
                            fluidRow(
                              column(width = 10), column(downloadButton("lang_neigh_download", label = "Download Data"), width = 2),
                              column(width = 12,
                                     plotlyOutput("lang_graph_neigh")))),
                   tabPanel(TAB_NAMES[4], 
                            fluidRow(
                              column(width = 10), column(downloadButton("lang_cities_download", label = "Download Data"), width = 2),
                              column(width = 12,
                            plotlyOutput("lang_graph_cities"))))
                 )
               )
             )
    ),
    tabPanel("Educational Attainment",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "educ",
                              label = actionButton("educ_popup", "Category Breakdown"),
                              choiceNames = educ_choices$category_name, choiceValues = educ_choices$category_val),
                 BUILD_RADIO_BUTTONS("educ_bool", lab = actionButton("educ_help", "Legend Type")),
                 plotlyOutput("educ_pie")),
               mainPanel(
                 tabsetPanel(
                   tabPanel(TAB_NAMES[1],
                            tags$style(type = "text/css", "#educ_map {height: calc(85vh) !important;}"),
                            leafletOutput("educ_map")),
                   tabPanel(TAB_NAMES[5], 
                            fluidRow(
                              column(width = 10), column(downloadButton("educ_neigh_download", label = "Download Data"), width = 2),
                              column(width = 12,
                            plotlyOutput("educ_graph_neigh")))),
                   tabPanel(TAB_NAMES[4], 
                            fluidRow(
                              column(width = 10), column(downloadButton("educ_cities_download", label = "Download Data"), width = 2),
                              column(width = 12,
                                     plotlyOutput("educ_graph_cities"))))
                 )
               )
             )
    ),
    tabPanel("Age",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "age",
                              label = actionButton("age_popup", "Category Breakdown"),
                              choiceNames = age_choices$category_name, choiceValues = age_choices$category_val),
                 BUILD_RADIO_BUTTONS("age_bool", lab = actionButton("age_help", "Legend Type")),
                 plotlyOutput("age_pie")),
               mainPanel(
                 tabsetPanel(                       
                   tabPanel(TAB_NAMES[1],
                            tags$style(type = "text/css", "#age_map {height: calc(85vh) !important;}"),
                            leafletOutput("age_map")),
                   tabPanel(TAB_NAMES[5], 
                            fluidRow(
                              column(width = 10), column(downloadButton("age_neigh_download", label = "Download Data"), width = 2),
                              column(width = 12,
                                     plotlyOutput("age_graph_neigh")))),
                   tabPanel(TAB_NAMES[4], 
                            fluidRow(
                              column(width = 10), column(downloadButton("age_cities_download", label = "Download Data"), width = 2),
                              column(width = 12,
                            plotlyOutput("age_graph_cities"))))
                 )
               )
             )
    ),
    tabPanel("Rankings",
             tabsetPanel( 
               tabPanel("Boston",
                        sidebarLayout(
                          sidebarPanel("hello"),
                          mainPanel(
                            htmlOutput("bos_minmax")))),
               tabPanel("Neighborhoods",  
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons(inputId = "neigh_select", 
                                         label = "Select Neighborhood", 
                                         choices = neigh_choices)),
                          mainPanel(
                            htmlOutput("neigh_minmax")
                          )
                        )
               )
             )
    )
  )
)

#Server #############################################################################################################################
server <- function(input, output, session) {
  
  #MAPS #############################################################################################################################
  output$race_map <- renderLeaflet({MAP_FUNC(input$race, input$race_bool, race_div_tract)})
  output$pob_map <- renderLeaflet({MAP_FUNC(input$pob, input$pob_bool, pob_div_tract)})
  output$hh_map <- renderLeaflet({MAP_FUNC(input$hh, input$hh_bool, hh_div_tract)})
  output$lang_map <- renderLeaflet({MAP_FUNC(input$lang, input$lang_bool, lang_div_tract)})
  output$educ_map <- renderLeaflet({MAP_FUNC(input$educ, input$educ_bool, educ_div_tract)})
  output$age_map <- renderLeaflet({MAP_FUNC(input$age, input$age_bool, age_div_tract)})
  
  #PIE CHARTS ######################################################################################################################
  output$race_pie <- renderPlotly({PIE_CHART_FUNC(input$race, race_div_neigh)})
  output$pob_pie <- renderPlotly({PIE_CHART_FUNC(input$pob, pob_div_neigh)})
  output$hh_pie <- renderPlotly({PIE_CHART_FUNC(input$hh, hh_div_neigh)})
  output$lang_pie <- renderPlotly({PIE_CHART_FUNC(input$lang, lang_div_neigh)})
  output$educ_pie <- renderPlotly({PIE_CHART_FUNC(input$educ, educ_div_neigh)})
  output$age_pie <- renderPlotly({PIE_CHART_FUNC(input$age, age_div_neigh)})
  
  #NEIGHBORHOOD GRAPH ##############################################################################################################
  output$race_graph_neigh <- renderPlotly({NEIGH_GRAPH_FUNC(input$race, race_div_neigh)})
  output$pob_graph_neigh <- renderPlotly({NEIGH_GRAPH_FUNC(input$pob, pob_div_neigh)})
  output$hh_graph_neigh <- renderPlotly({NEIGH_GRAPH_FUNC(input$hh, hh_div_neigh)})
  output$lang_graph_neigh <- renderPlotly({NEIGH_GRAPH_FUNC(input$lang, lang_div_neigh)})
  output$educ_graph_neigh <- renderPlotly({NEIGH_GRAPH_FUNC(input$educ, educ_div_neigh)})
  output$age_graph_neigh <- renderPlotly({NEIGH_GRAPH_FUNC(input$age, age_div_neigh)})
  
  #CITY GRAPH ######################################################################################################################
  output$race_graph_cities <- renderPlotly({CITY_GRAPH_FUNC(input$race, race_div_cities)})
  output$pob_graph_cities <- renderPlotly({CITY_GRAPH_FUNC(input$pob, pob_div_cities)})
  output$hh_graph_cities <- renderPlotly({CITY_GRAPH_FUNC(input$hh, hh_div_cities)})
  output$lang_graph_cities <- renderPlotly({CITY_GRAPH_FUNC(input$lang, lang_div_cities)})
  output$educ_graph_cities <- renderPlotly({CITY_GRAPH_FUNC(input$educ, educ_div_cities)})
  output$age_graph_cities <- renderPlotly({CITY_GRAPH_FUNC(input$age, age_div_cities)})
  
  #CAT POPUPS ##########################################################################################################################
  observeEvent(input$race_popup, {showModal(GET_POPUP("race"))})
  observeEvent(input$pob_popup, {showModal(GET_POPUP("pob"))})
  observeEvent(input$hh_popup, {showModal(GET_POPUP("hh"))})
  observeEvent(input$lang_popup, {showModal(GET_POPUP("lang"))})
  observeEvent(input$educ_popup, {showModal(GET_POPUP("educ"))})
  observeEvent(input$age_popup, {showModal(GET_POPUP("age"))})

  #HELP POPUPS ####################################################################################################################
  observeEvent(input$race_help, {showModal(raw_scale_dialog)})
  observeEvent(input$pob_help, {showModal(raw_scale_dialog)})
  observeEvent(input$lang_help, {showModal(raw_scale_dialog)})
  observeEvent(input$hh_help, {showModal(raw_scale_dialog)})
  observeEvent(input$educ_help, {showModal(raw_scale_dialog)})
  observeEvent(input$age_help, {showModal(raw_scale_dialog)})
  
  #MINMAX #########################################################################################################################
  output$bos_minmax <- renderText(GET_MINMAX_CITY())
  output$neigh_minmax <- renderText(GET_MINMAX_NEIGH(input$neigh_select))
  
  #NEIGHBORHOOD BUTTONS ###########################################################################################################
  output$race_neigh_download <- downloadHandler(
    filename = "race_ethnicity_neighborhood.csv", content = function(file) 
      {readr::write_csv(NEIGH_BUTTON_FUNC(input$race, race_div_neigh), file)})
  
  output$pob_neigh_download <- downloadHandler(
    filename = "place_of_birth_neighborhood.csv", content = function(file) 
      {readr::write_csv(NEIGH_BUTTON_FUNC(input$pob, pob_div_neigh), file)})
  
  output$hh_neigh_download <- downloadHandler(
    filename = "hh_neighborhood.csv", content = function(file) 
      {readr::write_csv(NEIGH_BUTTON_FUNC(input$hh, hh_div_neigh), file)})
  
  output$lang_neigh_download <- downloadHandler(
    filename = "language_neighborhood.csv", content = function(file) 
      {readr::write_csv(NEIGH_BUTTON_FUNC(input$lang, lang_div_neigh), file)})
  
  output$educ_neigh_download <- downloadHandler(
    filename = "education_neighborhood.csv", content = function(file) 
      {readr::write_csv(NEIGH_BUTTON_FUNC(input$educ, educ_div_neigh), file)})
  
  output$age_neigh_download <- downloadHandler(
    filename = "age_neighborhood.csv", content = function(file) 
      {readr::write_csv(NEIGH_BUTTON_FUNC(input$age, age_div_neigh), file)})
  
  #CITY BUTTONS ####################################################################################################################
  output$race_cities_download <- downloadHandler(
    filename = "race_ethnicity_cities.csv", content = function(file) 
      {readr::write_csv(CITY_BUTTON_FUNC(input$race, race_div_cities), file)})
  
  output$pob_cities_download <- downloadHandler(
    filename = "place_of_birth_cities.csv", content = function(file) 
      {readr::write_csv(CITY_BUTTON_FUNC(input$pob, pob_div_cities), file)})
  
  output$hh_cities_download <- downloadHandler(
    filename = "hh_cities.csv", content = function(file) 
      {readr::write_csv(CITY_BUTTON_FUNC(input$hh, hh_div_cities), file)})
  
  output$lang_cities_download <- downloadHandler( 
    filename = "language_cities.csv", content = function(file) 
      {readr::write_csv(CITY_BUTTON_FUNC(input$lang, lang_div_cities), file)})
  
  output$educ_cities_download <- downloadHandler(
    filename = "education_cities.csv", content = function(file) 
      {readr::write_csv(CITY_BUTTON_FUNC(input$educ, educ_div_cities), file)})
  
  output$age_cities_download <- downloadHandler(
    filename = "age_cities.csv", content = function(file) 
      {readr::write_csv(CITY_BUTTON_FUNC(input$age, age_div_cities), file)})
  
  }
# Run the application 
shinyApp(ui = ui, server = server)