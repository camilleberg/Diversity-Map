#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidycensus)
library(tidyverse)
library(leaflet)
library(mapview)
library(plotly)
library(sf)
library(stringr)
library(shiny)
library(shinythemes)
library(htmlwidgets)
library(leafpop)

lang_div_tract <- read_rds("Language_diversity_tract.RDS") %>% 
    mutate(current_data = val_eng_not)
lang_div_neigh <- read_rds("Language_diversity_neighborhood.RDS")
lang_div_cities <- read_rds("Language_diversity_cities.RDS")

# bos <- neighborhood_div %>% 
#     as_tibble() %>%
#     filter(NEIGHBORHOOD == 'Citywide') %>% 
#     mutate(city =  "Boston") %>% 
#     select(city, eng_not, all,eng_eur_asian_other,eng_span_other,eng_span_french_othereur_chin_viet_other)
# 
# 
# sample_city_scores <- bos %>% rbind(c("New York", .55, .51,.53,.85,.35),
#                                     c("LA",.6,.75,.63,.81,.4),
#                                     c("...",.34,.64,.24,.64,.54)) %>% 
#     mutate(current_data = all)


#top_25_cities <- c("Boston", "New York", "LA", "...")
#made_up_scores <- c(.5, .6, .4, 0)

#sample_city_scores <- tibble(top_25_cities, made_up_scores)


css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))  # Convert CSS to HTML


#language_div <- language_div 
# language_div <- readRDS("Language_diversity.RDS") %>% 
#     mutate(current_data = all) %>% 
#     filter(!(GEOID == "25025981300" | GEOID == "25025981800"))

lang_max <- c('val_eng_not' = 2,
              'val_all' = 13,
              'val_eng_eur_asian_other' = 4,
              'val_eng_span_other' = 3,
              'val_eng_span_french_othereur_chin_viet_other' = 7)

 

indexed_block <- readRDS("block_data.RDS") %>% 
    mutate(actual_data = two_cat)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("lumen"),
    HTML(html_fix),

    # Application title
    titlePanel("Diversity Map"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel("Race Ethnicity",
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons(inputId = "race_cat",
                                      label = "Race and Ethnicity Category",
                                      choices = list("White/Non-White" = "two_cat",
                                                     "Five Categories" = "five_cat",
                                                     "Twelve Categories" = "twelve_cat")),
                         
                         radioButtons(inputId = "block_tract",
                                      label = "Tract or Block?",
                                      choices = c("Block","Tract"))
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         leafletOutput("race_map")
                     )
                 )
        ),
        tabPanel("Region of Birth"),
        tabPanel("Income"),
        tabPanel("Language Spoken at Home",
                     sidebarLayout(
                         sidebarPanel(
                             radioButtons(inputId = "lang",
                                          label = "Language Spoken at Home Categories",
                                          choices = list("English/Non-English" = "val_eng_not",
                                                         "English, Spanish, Other" = "val_eng_span_other",
                                                         "English, European Languages, Asian Languages, Other" = "val_eng_eur_asian_other",
                                                         "English, Spanish, French, Other European Languages, Chinese, Vietnamese, Other" = "val_eng_span_french_othereur_chin_viet_other",
                                                         "All Languages" = "val_all")),
                             
                             radioButtons(inputId = "raw_boolean",
                                          label = "Raw Numbers or Scaled?",
                                          choices = list("Raw Data" = T,
                                                         "Scaled" = F)),
                             plotlyOutput("lang_pie")
                             
                             # radioButtons(inputId = "block_tract",
                             #              label = "Tract or Block?",
                             #              choices = c("Block","Tract"))
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                             leafletOutput("lang_map")
                         ),
                 
                 ),
                 fluidRow(
                     column(3,
                            dataTableOutput("lang_table_cities")),
                     column(3,
                            dataTableOutput("lang_table_neigh")),
                     column(6)),
        ),
        tabPanel("Educational Attainment"),
        tabPanel("Age")
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$race_map <- renderLeaflet({
        
        indexed_block$actual_data <- indexed_block[[input$race_cat]]
        
        pal <- colorQuantile(palette = "viridis", domain = indexed_block$actual_data, n = 5)
        
        indexed_block %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(actual_data)) %>%
            addLegend("bottomright", 
                      pal = pal,
                      values = ~ actual_data,
                      title = "Percentile of Diversity Index",
                      opacity = 1)
    })
    
    output$lang_map <- renderLeaflet({
        
        
        lang_div_tract$current_data <- lang_div_tract[[input$lang]]
        
        selected_val <- str_sub(input$lang, start = 5)
        
        # lang_div_tract <- lang_div_tract %>% 
        #     mutate(tooltip = tooltip_func(tract_string = NAME, col_string = selected_val))

        
        #ifelse(input)
        pal_option <- "viridis"
        
        pal <- colorQuantile(palette = pal_option, domain = lang_div_tract$current_data, n = 5)
        if (input$raw_boolean){
            
            pal <- colorNumeric(palette = pal_option, domain = c(0, 1 - (1/lang_max[input$lang])),
                                na.color = "#808080")
            legend_label <- "Diversity Index Score"
            
        }else {
            pal <- colorQuantile(palette = pal_option, domain = lang_div_tract$current_data, n = 5)
            
            legend_label <- "Percentile of Diversity Index"
        }

        
        lang_div_tract %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%",
                    height = "100%") %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(popup = lang_div_tract %>% 
                            as_tibble() %>% 
                            mutate(`Census Tract` = str_sub(NAME, start = 14, end = -32)) %>% 
                            select(`Census Tract`, starts_with(selected_val)) %>% 
                            popupTable(feature.id = F,
                                       row.numbers = F),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(current_data)) %>%
            addLegend("bottomright", 
                      pal = pal,
                      values = ~ current_data,
                      title = legend_label,
                      opacity = 1,
                      na.label = 'Tracts with little or no population')
    })
    
    output$lang_table_neigh <- renderDataTable({
        
        lang_div_neigh[[input$lang]] <- lang_div_neigh[[input$lang]] %>% 
            as.numeric() %>% 
            round(digits = 2) 
        
        
        lang_div_neigh %>% 
            select(tract20_nbhd, input$lang)}
        ,
        options = list(searching = F,
                       lengthChange = F,
                       info = F,
                       paging = F
            
        )
        
        
        
        
    )
    
    output$lang_table_cities <- renderDataTable({
        
        
        lang_div_cities[[input$lang]] <- lang_div_cities[[input$lang]] %>% 
            as.numeric() %>% 
            round(digits = 2) 
            
        lang_div_cities %>% 
            select(City, input$lang)}
        ,
        options = list(searching = F,
                       lengthChange = F,
                       info = F,
                       paging = F
                       
        )
        
        
    )
    
    output$lang_pie <- renderPlotly({
        
        selected_val <- str_sub(input$lang, start = 5)
        
        lang_div_boston <- lang_div_neigh %>%
          filter(tract20_nbhd == "Citywide") %>%
          select(starts_with(selected_val))
        
        
        pie_labels <- colnames(lang_div_boston) %>% str_sub(start = str_length(selected_val) + 2)
        pie_values <- as.numeric(as.vector(lang_div_boston[1,]))
        
        citywide_sample <- tibble(Categories = pie_labels, Citywide = pie_values)
        
        
        fig <- plot_ly(citywide_sample, labels = ~Categories, values = ~Citywide, type = 'pie')
        fig <- fig %>% layout(title = selected_val,
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
