# Region of Birth data pull
# Created: 12/8/2022
# last updated: 

# this is to pull various data relating to the diversity map

# libraries and setup
rm(list = ls())

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

# div_calc_pob <- function(census_pob_pull){
  
  df_groups <- census_pob_pull %>% 
    mutate(nat_for_nat = `Native:`, 
           nat_for_for = `Foreign:`, 
           
           all_Northeast = '', 
           all_Midwest = '', 
           all_South = '', 
           all_West = '', 
           all_NA = '', 
           all_LA = '', 
           all_Asia = '', 
           all_Oceania = '', 
           all_Africa ='', 
           all_Europe = '', 
           
           continent_NA = '', 
           continent_LA = '', 
           continent_ASia = '', 
           continent_Africa = '', 
           continent_Oceania = '', 
           continent_Europe = '', 
           
           )
  
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
#}

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

