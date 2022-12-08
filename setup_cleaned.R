remotes::install_github("walkerke/tidycensus")
devtools::install_github("r-spatial/leafpop")
library(tidycensus)
library(tidyverse)
library(leaflet)
library(leafpop)
library(mapview)
library(sf)
library(stringr)
library(plotly) 
library(readr)



###FIRST SET WD TO SOURCE FILE LOCATION

# Loading in constants and functions ---------------------------------------


VARS <- tidycensus::load_variables(dataset = 'acs5', year = 2020, cache = T)


div_prop <- function(var, total_pop) {
  x <- (var/total_pop)^2
  return(x)
}

div_calc_lang <- function(census_language_pull){
  
  
  
  df_groups <- census_language_pull %>% 
    mutate(eng_not_Speaks_only_English =`Speak only English`,
           eng_not_Other = Total - `Speak only English`,
           
           all_Speaks_only_English = (`Speak only English`),
           all_Spanish = (`Spanish:`),
           all_French_Haitian_or_Cajun = (`French, Haitian, or Cajun:`),
           all_German = (`German or other West Germanic languages:`),
           all_Russian_Polish_or_other_Slavic  =  (`Russian, Polish, or other Slavic languages:`),
           all_Other_Indo_European = (`Other Indo-European languages:`),
           all_Korean = (`Korean:`),
           all_Chinese =  (`Chinese (incl. Mandarin, Cantonese):`),
           all_Vietnamese = (`Vietnamese:`),
           all_Tagalog = (`Tagalog (incl. Filipino):`),
           all_Other_Asian_and_Pacific_Island = (`Other Asian and Pacific Island languages:`),
           all_Arabic = (`Arabic:`),
           all_Other_Unspecified = (`Other and unspecified languages:`),
           
           eng_eur_asian_other_Speaks_only_English = (`Speak only English`),
           eng_eur_asian_other_European = (`Spanish:`+
                                        `French, Haitian, or Cajun:`+
                                        `German or other West Germanic languages:`+
                                        `Russian, Polish, or other Slavic languages:`+
                                        `Other Indo-European languages:`),
           eng_eur_asian_other_Asian = (`Korean:` + 
                                          `Chinese (incl. Mandarin, Cantonese):` + 
                                          `Vietnamese:` +
                                          `Tagalog (incl. Filipino):` +
                                          `Other Asian and Pacific Island languages:`),
           eng_eur_asian_other_Other = (`Arabic:` + 
                                          `Other and unspecified languages:`),
           
           eng_span_other_Speaks_only_English = (`Speak only English`),
           eng_span_other_Spanish= (`Spanish:`),
           eng_span_other_Other =(`French, Haitian, or Cajun:`+
                                    `German or other West Germanic languages:`+
                                    `Russian, Polish, or other Slavic languages:`+
                                    `Other Indo-European languages:`+
                                    `Korean:` + 
                                    `Chinese (incl. Mandarin, Cantonese):` + 
                                    `Vietnamese:` +
                                    `Tagalog (incl. Filipino):` +
                                    `Other Asian and Pacific Island languages:`+
                                    `Arabic:` + 
                                    `Other and unspecified languages:`),
           
           eng_span_french_othereur_chin_viet_other_Speaks_only_English= (`Speak only English`), 
           eng_span_french_othereur_chin_viet_other_Spanish=(`Spanish:`), 
           eng_span_french_othereur_chin_viet_other_French_Haitian_or_Cajun=(`French, Haitian, or Cajun:`),
           eng_span_french_othereur_chin_viet_other_Other_Indo_European=(`Other Indo-European languages:`),
           eng_span_french_othereur_chin_viet_other_Chinese=(`Chinese (incl. Mandarin, Cantonese):`),
           eng_span_french_othereur_chin_viet_other_Vietnamese=(`Vietnamese:`),
           eng_span_french_othereur_chin_viet_other_Other=(`German or other West Germanic languages:` +
                                                             `Russian, Polish, or other Slavic languages:`+
                                                             `Korean:` +
                                                             `Tagalog (incl. Filipino):` +
                                                             `Other Asian and Pacific Island languages:` +
                                                             `Arabic:` +
                                                             `Other and unspecified languages:`))
  
  df_values <- df_groups %>% 
    mutate(val_eng_not = 1 - (div_prop(eng_not_Speaks_only_English,Total)+
                                div_prop(eng_not_Other, Total)),
           
           val_all =  1 -  (div_prop(all_Speaks_only_English, Total) +
                              div_prop(all_Spanish, Total) + 
                              div_prop(all_French_Haitian_or_Cajun, Total) + 
                              div_prop(all_German, Total) + 
                              div_prop(all_Russian_Polish_or_other_Slavic, Total) +    
                              div_prop(all_Korean, Total) + 
                              div_prop(all_Chinese, Total) + 
                              div_prop(all_Vietnamese, Total) + 
                              div_prop(all_Tagalog, Total) + 
                              div_prop(all_Other_Asian_and_Pacific_Island, Total) + 
                              div_prop(all_Arabic, Total) + 
                              div_prop(all_Other_Unspecified, Total)),
           
           val_eng_eur_asian_other = 1 - (div_prop( eng_eur_asian_other_Speaks_only_English, Total) + 
                                            div_prop( eng_eur_asian_other_European, Total) + 
                                            div_prop( eng_eur_asian_other_Asian,Total) + 
                                            div_prop( eng_eur_asian_other_Other,Total)),
           
           val_eng_span_other= 1 - (div_prop(eng_span_other_Speaks_only_English,Total) + 
                                      div_prop(eng_span_other_Spanish,Total) + 
                                      div_prop(eng_span_other_Other,Total)),
           
           val_eng_span_french_othereur_chin_viet_other= 1 - (div_prop(eng_span_french_othereur_chin_viet_other_Speaks_only_English,Total) + 
                                                                div_prop(eng_span_french_othereur_chin_viet_other_Spanish,Total) + 
                                                                div_prop(eng_span_french_othereur_chin_viet_other_French_Haitian_or_Cajun,Total) + 
                                                                div_prop(eng_span_french_othereur_chin_viet_other_Other_Indo_European,Total) +
                                                                div_prop(eng_span_french_othereur_chin_viet_other_Chinese, Total) +
                                                                div_prop(eng_span_french_othereur_chin_viet_other_Vietnamese, Total) +
                                                                div_prop(eng_span_french_othereur_chin_viet_other_Other, Total)))
}

TRACT_TO_NEIGHBORHOOD <- readxl::read_xlsx("geo20_tract_block group comparison.xlsx")

lang_cities_pull <- read_csv("C16001_Cities.csv")



# Language Data Prep ------------------------------------------------------

#selecting the correct variable and removing the extra fields
lang_vars <- VARS %>% 
  filter(concept == "LANGUAGE SPOKEN AT HOME FOR THE POPULATION 5 YEARS AND OVER") %>%
  filter(str_detect(label, '\"', negate = T)) %>% 
  mutate(nameE = paste0(name, "E"))

#the variable codes to use as input for get_acs
lang_names <- lang_vars$name

#Call to census api to get relevant variables
#language_at_home_acs5 
lang_pull_raw <- tidycensus::get_acs(geography = "tract", 
                                             variable = lang_names, 
                                             output = "wide",
                                             state = "MA",
                                             county = "Suffolk",
                                             geometry = TRUE,
                                             year = 2020,
                                             cache_table = T,
                                             show_call = TRUE) %>% 
filter(!(str_detect(NAME,"Census Tract 99")|str_detect(NAME,"Census Tract 18")
         |str_detect(NAME,"Census Tract 17")|str_detect(NAME,"Census Tract 16")
         |str_detect(NAME,"Census Tract 9812.01")|str_detect(NAME,"Census Tract 9801.01")))


#Removing margin of error columns
lang_pull_small <- lang_pull_raw %>% 
  select(!ends_with("M"))


##Formatting data
dat <- lang_vars %>% select(nameE,label) %>% 
  mutate(label_small = str_sub(label, start = (str_locate(label, 'Estimate!!Total:!!')[,2] + 1))) %>% 
  select(-label)

dat$label_small[1]<- "Total"

lang_pull_small <- lang_pull_small %>% 
  rename_with(~deframe(dat)[.x], .cols = dat$nameE) %>% 
  select(GEOID, NAME, any_of(dat$label_small), geometry)




##Doing the diversity calculations
lang_div_tract <- div_calc_lang(lang_pull_small)

lang_div_cities <- div_calc_lang(lang_cities_pull)



##Grouping by neighborhood calculations
lang_div_tract_by_neigh <- lang_div_tract %>% 
  left_join(TRACT_TO_NEIGHBORHOOD, by = c('NAME'='tract20')) %>% 
  filter((!is.na(tract20_nbhd))| Total==0)

neigh_sum <- lang_div_tract_by_neigh %>% 
  filter(tract20_nbhd != "_Census Tract 9901.01, Suffolk County, Massachusetts") %>% 
  as_tibble() %>% 
  group_by(tract20_nbhd) %>% 
  summarise(Total=sum(Total),
            `Speak only English`=sum(`Speak only English`),
            `Spanish:`=sum(`Spanish:`),
            `French, Haitian, or Cajun:`=sum(`French, Haitian, or Cajun:`), 
            `German or other West Germanic languages:`=sum(`German or other West Germanic languages:`),
            `Russian, Polish, or other Slavic languages:`=sum(`Russian, Polish, or other Slavic languages:`),
            `Other Indo-European languages:`=sum(`Other Indo-European languages:`),
            `Korean:`=sum(`Korean:`),
            `Chinese (incl. Mandarin, Cantonese):`=sum(`Chinese (incl. Mandarin, Cantonese):`),
            `Vietnamese:`=sum(`Vietnamese:`),
            `Tagalog (incl. Filipino):`=sum(`Tagalog (incl. Filipino):`),
            `Other Asian and Pacific Island languages:`=sum(`Other Asian and Pacific Island languages:`),
            `Arabic:`=sum(`Arabic:`),
            `Other and unspecified languages:`=sum(`Other and unspecified languages:`))

city_sum <- lang_div_tract_by_neigh %>% 
  as_tibble() %>% 
  summarise(Total=sum(Total),
            `Speak only English`=sum(`Speak only English`),
            `Spanish:`=sum(`Spanish:`),
            `French, Haitian, or Cajun:`=sum(`French, Haitian, or Cajun:`), 
            `German or other West Germanic languages:`=sum(`German or other West Germanic languages:`),
            `Russian, Polish, or other Slavic languages:`=sum(`Russian, Polish, or other Slavic languages:`),
            `Other Indo-European languages:`=sum(`Other Indo-European languages:`),
            `Korean:`=sum(`Korean:`),
            `Chinese (incl. Mandarin, Cantonese):`=sum(`Chinese (incl. Mandarin, Cantonese):`),
            `Vietnamese:`=sum(`Vietnamese:`),
            `Tagalog (incl. Filipino):`=sum(`Tagalog (incl. Filipino):`),
            `Other Asian and Pacific Island languages:`=sum(`Other Asian and Pacific Island languages:`),
            `Arabic:`=sum(`Arabic:`),
            `Other and unspecified languages:`=sum(`Other and unspecified languages:`)) %>% 
  mutate(tract20_nbhd="Citywide")

neigh_city_sum <- city_sum %>% 
  rbind(neigh_sum)


###Neighborhood diversity calculations
lang_div_neigh <- div_calc_lang(neigh_city_sum)



# Writing calculated rds files --------------------------------------------


# write_rds(lang_div_tract, "Language_diversity_tract.RDS")
# write_rds(lang_div_neigh, "Language_diversity_neighborhood.RDS")
# write_rds(lang_div_cities, "Language_diversity_cities.RDS")



# Sample Map --------------------------------------------------------------
normalized_pal <- leaflet::colorQuantile(palette = "viridis", domain = lang_div_tract@data[chosen_graph], n = 5)
raw_pal <- colorNumeric(palette = "viridis", domain = c(0, 1 - (1/lang_max[chosen_graph])),
                        na.color = "#808080")



test_string$eng_s <- lang_div_tract[1,] 

test_string %>% 
  colnames(starts_with("eng_span_other"))
  
  #mutate(breakdown_string = paste(starts_with("eng_span_other"), sep = "\n"))
  
tooltip_func <- function(col_string, tract_string){
  #test_filter <- "all"
  #test_filter_tract <- "Census Tract 604, Suffolk County, Massachusetts"
  
  
  selected_columns <- read_rds("Language_diversity_tract.RDS") %>% 
    as_tibble() %>% 
    filter(NAME == tract_string) %>% 
    select(starts_with(col_string))
  
  
  selected_columns_names <- selected_columns %>% 
    `colnames<-`(str_sub(colnames(selected_columns), start = str_count(col_string) + 2))
  
  
  s <- capture.output(print(`colnames<-`(x = as.data.frame(t(selected_columns_names)), value = tract_string)))
  s <- paste(s, collapse = "<br>")
  
  s
  
}

s <- tooltip_func(lang_div_tract, "all", lang_div_tract$NAME[1])



test_filter <- "all"
test_filter_tract <- "Census Tract 604, Suffolk County, Massachusetts"

test_df <- lang_div_tract %>% 
  as_tibble() %>% 
  filter(NAME == test_filter_tract) %>% 
  select(starts_with(test_filter))


test_df2 <- test_df %>% 
  `colnames<-`(str_sub(colnames(test_df), start = str_count(test_filter) + 2))


s <- capture.output(print(`colnames<-`(x = as.data.frame(t(test_df2)), value = test_filter_tract)))
s <- paste(s, collapse = "<br>")

  



language_div_neighborhood %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ raw_pal(eng_not)) %>%
  addLegend("bottomright", 
            pal = raw_pal,
            values = ~ eng_not,
            title = "Percentile of Diversity Index",
            opacity = 1,
            na.label = "Tracts with little or no population")


lang_div_tract %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ tooltip,
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











