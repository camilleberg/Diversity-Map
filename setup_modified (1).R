remotes::install_github("walkerke/tidycensus")
library(tidycensus)
library(tidyverse)
library(leaflet)
library(mapview)
library(sf)
library(stringr)
library(plotly)

########################################################
#Outline of steps for making map data
#Load in block level data for acs
#read in census data from downloaded file
##########hope to modify this step to use tidycensus
# OLD CODE IGNORE ----TODELETE --------------------------------------------


{#boston_data <- get_pums(state = "MA",
  #                         survey = "acs5",
  #                         year = 2019,
  #                         puma = c("03301", "03302", "03303", "03304", "03305"),
  #                         variables = c("RAC1P",
  #                                       "HISP",
  #                                       "AGEP"),
  #                         show_call = T)
  # 
  # 
  # 
  # suffolk_tract <- get_acs(geography = "tract", 
  #                          variables = "B02001", 
  #                          output = "wide",
  #                          state = "MA", 
  #                          geometry = TRUE,
  #                          year = 2019)
  # 
  # 
  # 
  # 
  # suffolk_geo_tract <- tidycensus::get_decennial(geography = "tract", 
  #                                variable = "P2_002N", 
  #                                output = "wide",
  #                                state = "MA",
  #                                county = "Suffolk",
  #                                geometry = TRUE,
  #                                year = 2020,
  #                                show_call = TRUE) 
}

{
  # numeric_p2 <- trimmed_p2 %>%
  #   mutate_at(c(3:76), as.numeric) %>% 
  #   select(GEO_ID,NAME,P002001,P002002,P002005,P002006,P002007,P002008,P002009,P002010,P002011,P002012,P002028,P002049,P002065,P002072)
  # 
  # #adding easier to read labels: This isn't necessary but makes things more readable, will probably be modified
  # 
  # labelled_p2 <- numeric_p2 %>% 
  #   mutate(total = P002001,
  #          hisp = P002002,
  #          white = P002005,
  #          black = P002006,
  #          aian = P002007,
  #          asian = P002008,
  #          nhpi = P002009,
  #          other = P002010,
  #          two_or_more = P002011,
  #          two = P002012,
  #          three = P002028,
  #          four = P002049,
  #          five = P002065,
  #          six = P002072) %>% 
  #   mutate(big_other = aian + nhpi + other + two_or_more,
  #          other_two_plus = other + two_or_more)
  # 
  # 
  # test_p2 <- labelled_p2 %>% 
  #   mutate(five_cat = 1 - (div_prop(hisp,total) + div_prop(white,total) + div_prop(black,total)
  #                          + div_prop(asian,total) + div_prop(big_other,total)))
  # 
  # test_p2_func <- labelled_p2 %>% 
  #   mutate(five_cat = 1 - (div_prop(hisp,total) + div_prop(white,total) + div_prop(black,total)
  #                          + div_prop(asian,total) + div_prop(big_other,total)))
  # 
  # 
  # indexed_p2 <- labelled_p2 %>% 
  #   mutate(two_cat = 1 - (div_prop(white, total) + div_prop(total - white, total)),
  #          five_cat = 1 - (div_prop(hisp,total) + div_prop(white,total) + div_prop(black,total) + div_prop(asian,total) + div_prop(big_other,total)),
  #          twelve_cat= 1 - (div_prop(hisp,total) + div_prop(white,total) + div_prop(black,total) + div_prop(asian,total) + div_prop(aian,total) +
  #                             div_prop(nhpi,total) + div_prop(other,total) + div_prop(two,total) + div_prop(three,total) + div_prop(four,total)
  #                           + div_prop(five,total)+ div_prop(six,total)))
  # 
  # 
  # indexed_p2_func <- labelled_p2 %>% 
  #   mutate(two_cat = 1 - (div_prop(white, total) + div_prop(total - white, total)),
  #          five_cat = 1 - (div_prop(hisp,total) + div_prop(white,total) + div_prop(black,total) + div_prop(asian,total) + div_prop(big_other,total)),
  #          twelve_cat= 1 - (div_prop(hisp,total) + div_prop(white,total) + div_prop(black,total) + div_prop(asian,total) + div_prop(aian,total) +
  #                             div_prop(nhpi,total) + div_prop(other,total) + div_prop(two,total) + div_prop(three,total) + div_prop(four,total)
  #                           + div_prop(five,total)+ div_prop(six,total)))
  # 
  # 
  # #####
  # 
  # #several vectors of selected groups
  # #Mutate new columns with that combine selected groups with unique name
  # #check to see if sum equals total
  # #1 - sum of div_prop on those columns
  # 
  # 
  # 
  # #indexed_p2 %>% select(white_non, P002005, P002001) %>% View()
  # 
  # small_indexed <- indexed_p2 %>% 
  #   select(NAME, two_cat, five_cat,twelve_cat) 
  # 
  # 
  # 
  # indexed_block <- suffolk_block %>% 
  #   left_join(small_indexed) 
  # 
  # 
  # write_rds(indexed_block, "block_data.RDS")
  # 
}


#outdated code for not smart way of doing things
{
  #FILE PATH FOR WORK COMPUTER
  
  {P2 <- read_csv("C:/Users/michaelch/Downloads/DECENNIALPL2010.P2_2022-09-21T091051/DECENNIALPL2010.P2-Data.csv")
  
  P2_labels <- read_csv("C:/Users/michaelch/Downloads/DECENNIALPL2010.P2_2022-09-21T091051/DECENNIALPL2010.P2-Column-Metadata.csv")
  }
  
  #FILE PATH FOR MAC
  {
    P2 <- read_csv("DECENNIALPL2010.P2_2022-09-21T091051/DECENNIALPL2010.P2-Data.csv")
    
    P2_labels <- read_csv("DECENNIALPL2010.P2_2022-09-21T091051/DECENNIALPL2010.P2-Column-Metadata.csv")
  }
  
  # indexes <- c(1, 2, seq(3,147, by = 2))
  # 
  # trimmed_p2_ <- P2 %>% 
  #   select(all_of(indexes))
  # 
  
  
  #Cleaning up data, for final project this won't need to be done
  trimmed_p2 <- P2[-1,]
  
  trimmed_p2 <- trimmed_p2 %>%
    select(-P002001ERR)
  
  # 
  # trimmed_p2_labels <- P2_labels %>% 
  #   filter(row_number() %in% indexes)
  # 
}


# Loading in constants ----------------------------------------------------



vars <- tidycensus::load_variables(dataset = 'acs5', year = 2020, cache = T)


div_prop <- function(var, total_pop) {
  x <- (var/total_pop)^2
  return(x)
}

TRACT_TO_NEIGHBORHOOD <- readxl::read_xlsx("geo20_tract_block group comparison.xlsx")

CITY_LIST <- read_csv("C16001_Cities.csv")



# Language Data Prep ------------------------------------------------------
  


#Language spoken at home data wrangling


#selecting the correct variable and removing the extra fields
language_at_home_summary <- vars %>% 
  filter(concept == "LANGUAGE SPOKEN AT HOME FOR THE POPULATION 5 YEARS AND OVER") %>%
  filter(str_detect(label, '\"', negate = T)) %>% 
  mutate(nameE = paste0(name, "E"))

#the variable codes to use as input for get_acs
language_at_home_names <- language_at_home_summary$name
  

#Call to census api to get relevant variables
language_at_home_acs5 <- tidycensus::get_acs(geography = "tract", 
                                             variable = language_at_home_names, 
                                             output = "wide",
                                             state = "MA",
                                             county = "Suffolk",
                                             geometry = TRUE,
                                             year = 2020,
                                             cache_table = T,
                                             show_call = TRUE) #%>% 
 filter(!(str_detect(NAME,"Census Tract 99")|str_detect(NAME,"Census Tract 18")
          |str_detect(NAME,"Census Tract 17")|str_detect(NAME,"Census Tract 16")
          |str_detect(NAME,"Census Tract 9812.01")|str_detect(NAME,"Census Tract 9801.01")))


#Removing margin of error columns
lang_small <- language_at_home_acs5 %>% 
  select(!ends_with("M"))

dat <- language_at_home_summary %>% select(nameE,label) %>% 
  mutate(label_small = str_sub(label, start = (str_locate(label, 'Estimate!!Total:!!')[,2] + 1))) %>% 
  select(-label)

dat$label_small[1]<- "Total"


# names(lang_small)[base::match(dat[,"nameE"], names(lang_small))] = dat[,"label"]
# names(mt)[match(dat[,"old"], names(mt))] = dat[,"new"]

lang_small <- lang_small %>% 
  rename_with(~deframe(dat)[.x], .cols = dat$nameE) %>% 
  select(GEOID, NAME, any_of(dat$label_small), geometry)
  


language_div <- lang_small %>% 
  mutate(eng_not = 1 - (div_prop(`Speak only English`,Total)+
                          div_prop((Total - `Speak only English`), Total)),
         
         all = 1 - (div_prop(`Speak only English`,Total)+
                      div_prop(`Spanish:`,Total)+
                      div_prop(`French, Haitian, or Cajun:`,Total)+ 
                      div_prop(`German or other West Germanic languages:`,Total) +
                      div_prop(`Russian, Polish, or other Slavic languages:`,Total)+
                      div_prop(`Other Indo-European languages:`,Total) + div_prop(`Korean:`,Total)+
                      div_prop(`Chinese (incl. Mandarin, Cantonese):`,Total)+
                      div_prop(`Vietnamese:`,Total)+
                      div_prop(`Tagalog (incl. Filipino):`,Total)+
                      div_prop(`Other Asian and Pacific Island languages:`,Total)+
                      div_prop(`Arabic:`,Total)+
                      div_prop(`Other and unspecified languages:`,Total)),
         
         eng_eur_asian_other = 1 - (div_prop(`Speak only English`,Total) + 
                                      div_prop(`Spanish:`+
                                                 `French, Haitian, or Cajun:`+
                                                 `German or other West Germanic languages:`+
                                                 `Russian, Polish, or other Slavic languages:`+
                                                 `Other Indo-European languages:`,Total) + 
                                      div_prop(`Korean:` + 
                                                 `Chinese (incl. Mandarin, Cantonese):` + 
                                                 `Vietnamese:` +
                                                 `Tagalog (incl. Filipino):` +
                                                 `Other Asian and Pacific Island languages:`,Total) + 
                                      div_prop(`Arabic:` + 
                                                 `Other and unspecified languages:`,Total)),
         
         eng_span_other= 1 - (div_prop(`Speak only English`,Total) + 
                                div_prop(`Spanish:`,Total) + 
                                div_prop(`French, Haitian, or Cajun:`+
                                           `German or other West Germanic languages:`+
                                           `Russian, Polish, or other Slavic languages:`+
                                           `Other Indo-European languages:`+
                                           `Korean:` + 
                                           `Chinese (incl. Mandarin, Cantonese):` + 
                                           `Vietnamese:` +
                                           `Tagalog (incl. Filipino):` +
                                           `Other Asian and Pacific Island languages:`+
                                           `Arabic:` + 
                                           `Other and unspecified languages:`,Total)),
         
         eng_span_french_othereur_chin_viet_other= 1 - (div_prop(`Speak only English`,Total) + 
                                                          div_prop(`Spanish:`,Total) + 
                                                          div_prop(`French, Haitian, or Cajun:`,Total) + 
                                                          div_prop(`Other Indo-European languages:`,Total) +
                                                          div_prop(`Chinese (incl. Mandarin, Cantonese):`, Total) +
                                                          div_prop(`Vietnamese:`, Total) +
                                                          div_prop(`German or other West Germanic languages:` +
                                                                     `Russian, Polish, or other Slavic languages:`+
                                                                     `Korean:` +
                                                                     `Tagalog (incl. Filipino):` +
                                                                     `Other Asian and Pacific Island languages:` +
                                                                     `Arabic:` +
                                                                     `Other and unspecified languages:`, Total)))

# lang_max <- c('eng_not' = 2,
#               'all' = 13,
#               'eng_eur_asian_other' = 4,
#               'eng_span_other' = 3,
#               'eng_span_french_othereur_chin_viet_other' = 7)

#colnames(language_div)

#write_rds(language_div, "Language_diversity.RDS")

language_div_tibble <- as.tibble(language_div)

chosen_graph <- colnames(language_div_tibble[18])

language_div_tibble[chosen_graph]

# normalized_pal <- colorQuantile(palette = "viridis", domain = language_div_tibble[chosen_graph], n = 5)
# raw_pal <- colorNumeric(palette = "viridis", domain = c(0, 1 - (1/lang_max[chosen_graph])),
#                         na.color = "#808080")

#pal_five <- colorQuantile(palette = "viridis", domain = indexed_block$five_cat, n = 5)
#pal_twelve <- colorQuantile(palette = "viridis", domain = indexed_block$twelve_cat, n = 5)



language_div_neighborhood <- language_div %>% 
  left_join(TRACT_TO_NEIGHBORHOOD, by = c('NAME'='tract20')) %>% 
  filter(!is.na(tract20_nbhd))
  

# colors2 <- colorFactor(palette = "Set1",
#                     
#                     domain = language_div_neighborhood$NEIGHBORHOOD)

neighborhood_sum <- language_div_neighborhood %>% 
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

citywide <- language_div_neighborhood %>% 
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

neighborhood_sum2 <- citywide %>% 
  rbind(neighborhood_sum)

neighborhood_div_group <- neighborhood_sum2 %>% 
  mutate(eng_not_eng =`Speak only English`,
         eng_not_not = Total - `Speak only English`,
         
         # all12_eng = (`Speak only English`,Total)+
         #                all12_span =div_prop(`Spanish:`,Total)+
         #                all12_french =div_prop(`French, Haitian, or Cajun:`,Total)+ 
         #                all12_germ =  div_prop(`German or other West Germanic languages:`,Total) +
         #                all12_slav =  div_prop(`Russian, Polish, or other Slavic languages:`,Total)+
         #                all12_other_indo_eur =  div_prop(`Other Indo-European languages:`,Total) + div_prop(`Korean:`,Total)+
         #                all12_chin =  div_prop(`Chinese (incl. Mandarin, Cantonese):`,Total)+
         #                all12_viet = div_prop(`Vietnamese:`,Total)+
         #                all12_taga = div_prop(`Tagalog (incl. Filipino):`,Total)+
         #                all12 = div_prop(`Other Asian and Pacific Island languages:`,Total)+
         #                all12 =div_prop(`Arabic:`,Total)+
         #                all12 =div_prop(`Other and unspecified languages:`,Total)),
         
         eng_eur_asian_other_eng = (`Speak only English`),
         eng_eur_asian_other_eur = (`Spanish:`+
                                      `French, Haitian, or Cajun:`+
                                      `German or other West Germanic languages:`+
                                      `Russian, Polish, or other Slavic languages:`+
                                      `Other Indo-European languages:`),
         eng_eur_asian_other_asian = (`Korean:` + 
                                        `Chinese (incl. Mandarin, Cantonese):` + 
                                        `Vietnamese:` +
                                        `Tagalog (incl. Filipino):` +
                                        `Other Asian and Pacific Island languages:`),
         eng_eur_asian_other_other = (`Arabic:` + 
                                        `Other and unspecified languages:`),
         
         eng_span_other_eng = (`Speak only English`),
         eng_span_other_span = (`Spanish:`),
         eng_span_other_other =(`French, Haitian, or Cajun:`+
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
         
         eng_span_french_othereur_chin_viet_other_eng= (`Speak only English`), 
         eng_span_french_othereur_chin_viet_other_span=(`Spanish:`), 
         eng_span_french_othereur_chin_viet_other_fren=(`French, Haitian, or Cajun:`),
         eng_span_french_othereur_chin_viet_other_other_indo_eur=(`Other Indo-European languages:`),
         eng_span_french_othereur_chin_viet_other_chin=(`Chinese (incl. Mandarin, Cantonese):`),
         eng_span_french_othereur_chin_viet_other_viet=(`Vietnamese:`),
         eng_span_french_othereur_chin_viet_other_other=(`German or other West Germanic languages:` +
                                                           `Russian, Polish, or other Slavic languages:`+
                                                           `Korean:` +
                                                           `Tagalog (incl. Filipino):` +
                                                           `Other Asian and Pacific Island languages:` +
                                                           `Arabic:` +
                                                           `Other and unspecified languages:`))

neighborhood_div_values <- neighborhood_div_group %>% 
  mutate(val_eng_not = 1 - (div_prop(eng_not_eng,Total)+
                              div_prop(eng_not_not, Total)),
         all = 1,
         
         val_eng_eur_asian_other = 1 - (div_prop( eng_eur_asian_other_eng, Total) + 
                                          div_prop( eng_eur_asian_other_eur, Total) + 
                                          div_prop( eng_eur_asian_other_asian,Total) + 
                                          div_prop( eng_eur_asian_other_other,Total)),
         
         val_eng_span_other= 1 - (div_prop(eng_span_other_eng,Total) + 
                                    div_prop(eng_span_other_span,Total) + 
                                    div_prop(eng_span_other_other,Total)),
         
         val_eng_span_french_othereur_chin_viet_other= 1 - (div_prop(eng_span_french_othereur_chin_viet_other_eng,Total) + 
                                                              div_prop(eng_span_french_othereur_chin_viet_other_span,Total) + 
                                                              div_prop(eng_span_french_othereur_chin_viet_other_fren,Total) + 
                                                              div_prop(eng_span_french_othereur_chin_viet_other_other_indo_eur,Total) +
                                                              div_prop(eng_span_french_othereur_chin_viet_other_chin, Total) +
                                                              div_prop(eng_span_french_othereur_chin_viet_other_viet, Total) +
                                                              div_prop(eng_span_french_othereur_chin_viet_other_other, Total)))


cities_language_div_group <- city_comp %>% 
  mutate(eng_not_eng =`Speak only English`,
         eng_not_not = Total - `Speak only English`,
         
         # all12_eng = (`Speak only English`,Total)+
         #                all12_span =div_prop(`Spanish:`,Total)+
         #                all12_french =div_prop(`French, Haitian, or Cajun:`,Total)+ 
         #                all12_germ =  div_prop(`German or other West Germanic languages:`,Total) +
         #                all12_slav =  div_prop(`Russian, Polish, or other Slavic languages:`,Total)+
         #                all12_other_indo_eur =  div_prop(`Other Indo-European languages:`,Total) + div_prop(`Korean:`,Total)+
         #                all12_chin =  div_prop(`Chinese (incl. Mandarin, Cantonese):`,Total)+
         #                all12_viet = div_prop(`Vietnamese:`,Total)+
         #                all12_taga = div_prop(`Tagalog (incl. Filipino):`,Total)+
         #                all12 = div_prop(`Other Asian and Pacific Island languages:`,Total)+
         #                all12 =div_prop(`Arabic:`,Total)+
         #                all12 =div_prop(`Other and unspecified languages:`,Total)),
         
         eng_eur_asian_other_eng = (`Speak only English`),
         eng_eur_asian_other_eur = (`Spanish:`+
                                      `French, Haitian, or Cajun:`+
                                      `German or other West Germanic languages:`+
                                      `Russian, Polish, or other Slavic languages:`+
                                      `Other Indo-European languages:`),
         eng_eur_asian_other_asian = (`Korean:` + 
                                        `Chinese (incl. Mandarin, Cantonese):` + 
                                        `Vietnamese:` +
                                        `Tagalog (incl. Filipino):` +
                                        `Other Asian and Pacific Island languages:`),
         eng_eur_asian_other_other = (`Arabic:` + 
                                        `Other and unspecified languages:`),
         
         eng_span_other_eng = (`Speak only English`),
         eng_span_other_span = (`Spanish:`),
         eng_span_other_other =(`French, Haitian, or Cajun:`+
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
         
         eng_span_french_othereur_chin_viet_other_eng= (`Speak only English`), 
         eng_span_french_othereur_chin_viet_other_span=(`Spanish:`), 
         eng_span_french_othereur_chin_viet_other_fren=(`French, Haitian, or Cajun:`),
         eng_span_french_othereur_chin_viet_other_other_indo_eur=(`Other Indo-European languages:`),
         eng_span_french_othereur_chin_viet_other_chin=(`Chinese (incl. Mandarin, Cantonese):`),
         eng_span_french_othereur_chin_viet_other_viet=(`Vietnamese:`),
         eng_span_french_othereur_chin_viet_other_other=(`German or other West Germanic languages:` +
                                                           `Russian, Polish, or other Slavic languages:`+
                                                           `Korean:` +
                                                           `Tagalog (incl. Filipino):` +
                                                           `Other Asian and Pacific Island languages:` +
                                                           `Arabic:` +
                                                           `Other and unspecified languages:`))

cities_language_div_values <- cities_language_div_group %>% 
  mutate(val_eng_not = 1 - (div_prop(eng_not_eng,Total)+
                              div_prop(eng_not_not, Total)),
         all = 1,
         
         val_eng_eur_asian_other = 1 - (div_prop( eng_eur_asian_other_eng, Total) + 
                                          div_prop( eng_eur_asian_other_eur, Total) + 
                                          div_prop( eng_eur_asian_other_asian,Total) + 
                                          div_prop( eng_eur_asian_other_other,Total)),
         
         val_eng_span_other= 1 - (div_prop(eng_span_other_eng,Total) + 
                                    div_prop(eng_span_other_span,Total) + 
                                    div_prop(eng_span_other_other,Total)),
         
         val_eng_span_french_othereur_chin_viet_other= 1 - (div_prop(eng_span_french_othereur_chin_viet_other_eng,Total) + 
                                                              div_prop(eng_span_french_othereur_chin_viet_other_span,Total) + 
                                                              div_prop(eng_span_french_othereur_chin_viet_other_fren,Total) + 
                                                              div_prop(eng_span_french_othereur_chin_viet_other_other_indo_eur,Total) +
                                                              div_prop(eng_span_french_othereur_chin_viet_other_chin, Total) +
                                                              div_prop(eng_span_french_othereur_chin_viet_other_viet, Total) +
                                                              div_prop(eng_span_french_othereur_chin_viet_other_other, Total)))
  

# citywide_graph <- citywide %>% 
#   mutate(English = `Speak only English`, Spanish = `Spanish:`, Non_English = Total - (`Speak only English` + `Spanish:`)) %>% 
#   select(English, Spanish, Non_English, NEIGHBORHOOD) %>% 
#   spread(key = NEIGHBORHOOD)

test_val <- "eng_not"

city_values <- neighborhood_div_values %>% 
  filter(tract20_nbhd == "Citywide") %>% 
  select(starts_with(test_val)) #%>% 

pie_labels <- colnames(city_values) %>% str_sub(start = str_length(test_val) + 2)
pie_values <- as.numeric(as.vector(city_values[1,]))

citywide_sample <- tibble(Categories = pie_labels, Citywide = pie_values)

rownames(citywide_graph) <- colnames(citywide)
colnames(citywide_graph) <- rownames(citywide)


  
USPersonalExpenditure <- as_tibble("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

citywide_sample <- read_csv("citywide_sample.csv")

test_string <- lang_div_tract[1,]



  
fig <- plot_ly(citywide_sample, labels = ~Categories, values = ~Citywide, type = 'pie')
fig <- fig %>% layout(title = test_val,
                      
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

ggplot(citywide_graph, aes(x="", y=NEIGHBORHOOD)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


citywide_graph <- citywide %>% 
  mutate(English = `Speak only English`, Non_English = Total - `Speak only English`)



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


language_div_neighborhood %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ paste(tract20_nbhd, "\n", NAME),
              stroke = TRUE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ colors2(tract20_nbhd)) %>%
  addLegend("bottomright", 
            pal = colors2,
            values = ~ tract20_nbhd,
            title = "Percentile of Diversity Index",
            opacity = 1,
            na.label = "Tracts with little or no population")

indexed_block %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = TRUE,
              weight = 1,
              
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal_five(five_cat)) %>% 
  addLegend("bottomright", 
            pal = pal_five,
            values = ~ five_cat,
            title = "Percentile of Diversity Index",
            opacity = 1)

indexed_block %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal_twelve(twelve_cat)) %>%
  addLegend("bottomright", 
            pal = pal_twelve,
            values = ~ twelve_cat,
            title = "Percentile of Diversity Index",
            opacity = 1)

