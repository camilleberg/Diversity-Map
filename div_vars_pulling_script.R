# to help

## TO DO:
# fix high school / GED error 
#   - the census isn't pulling the High School graduate
#   - might have to do manually 

# make sure to run up to line 26 in either 
# setup_other_vars.R or
# setup_cleaned.R

## SETUP ------------------------------------------------------------------
rm(list = ls())

# loading libraries
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
library(readxl)
library(stringr)
library(dplyr)

## PULLING AND ORGANZIING THE DATA -----------------------------------------

VARS <- tidycensus::load_variables(dataset = 'acs5', year = 2020, cache = T)

# categories <- read_xlsx("Diversity Map Categories.xlsx")
categories <- read.csv("Diversity Map Categories.csv")

# creating unique variable names 
categories$var_names <- paste0(categories$title, "_", categories$group_name, "_",
                               str_replace_all(categories$var_description, " ", "_"))
categories$group_labels <- paste0(categories$title, "_", categories$group_name)

# initialing empty column to assign new var
categories$census_label <- NA

# looping through to fill with the relevant census var names
for(i in 1:nrow(categories)){
    # selecting only the table of interest
    table <- VARS %>%
      filter(grepl(paste0("^", categories$table_name[i]), VARS$name)) %>%
      select(label, name)
    
    # selecting from the table of interest 
    if(categories$title[i] == "educ" | "age") {
      

      if(categories$var_name_ending[i] == "High school graduate (includes equivalency)") {
        # manually fixing the high school thing
        male <- "B15002_011"; female <- "B15002_028"
      } else {
        # aggregating the male and female var names because they're segregated for educ
        male <- table$name[grepl(paste0(categories$var_name_ending[i], "$"), table$label)][1]
        female <- table$name[grepl(paste0(categories$var_name_ending[i], "$"), table$label)][2]
      
        # assigning new label
        categories$census_label[i] <- paste0(male, ",", female)
        # NOTE: the comma here should match the comma later as a separator 
      }
      
    } else {
      # assigning new label 
      categories$census_label[i] <- table$name[grepl(paste0(categories$var_name_ending[i], "$"), table$label)]
    }
}                                             

# grouping and combining all the labels to the distinct groups
census_groups <- categories %>%
  group_by(var_names) %>%
  mutate(labels = paste0(census_label, collapse = ",")) %>%
  distinct(labels)

# un-listing variable names 
acs_pull_labels <- unlist(strsplit(unique(categories$census_label), ",")) 
acs_pull_labels <- acs_pull_labels[!acs_pull_labels == "NA"]
  # fix NA issue later (this is related to the High School thing)

# pulling census data
other_var_raw <- tidycensus::get_acs(geography = "tract", 
                    variable = acs_pull_labels, #34, 
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

# Removing margin of error columns
other_var_small <- other_var_raw %>% 
  select(!ends_with("M"))
rm(other_var_raw)

# removing the E from the end of the variable names
colnames(other_var_small)[3:length(colnames(other_var_small))-1] <- 
  substr(colnames(other_var_small)[3:length(colnames(other_var_small))-1], 1, nchar(colnames(other_var_small)[3:length(colnames(other_var_small))-1])-1)
 
# expand the categories into new table with variables for each column #

# initializing labeling convention
col_var_names <- c()
for(i in 1:24) {
  col_var_names[i] <- paste0("var.", i)
}
  # 24 was chosen through looking at the data, but there's probably a way to soft-code it

# expanding out the var names into separate cells
census_groups_expanded <- left_join(census_groups,separate(census_groups, col = labels, into = col_var_names, sep = ","), 
          by = "var_names")

# making addition columns 
div_groups_labels <- census_groups_expanded %>% 
  select(-labels) %>%
  t() %>% as_tibble() %>% 
  row_to_names(row_number = 1) 

# adding the calculated rows to the census data # 

# initializing the df 
other_var_groups <- tibble(GEOID = other_var_small$GEOID, 
                           NAME = other_var_small$NAM, # I don't know why it's doing this but 
                           geometry = other_var_small$geometry)

# selecting only those of interest and adding them

grouping_fxn <- function(div_label) {
  x <- which(colnames(div_groups_labels) == paste0(div_label))
  
  # this basically selects the relevant census variables related to each group
  # and then adds them
  return(other_var_small %>% 
    as_tibble() %>%
    select(
        colnames(other_var_small)[colnames(other_var_small) %in% (div_groups_labels[,x] %>% array())[[1]]]
      ) %>%
      mutate(val = rowSums(across())) %>% select(val) %>%
      rename(!!sym(div_label) := val)
  )
}

for(i in 1:length(colnames(div_groups_labels))) {
  other_var_groups <- cbind(other_var_groups, grouping_fxn(colnames(div_groups_labels)[i]))
}


## ANALYZING THE DATA ------------------------------------------------------

# basically, for the value columns, it's the difference of 1 and the sum of all the diff categories 

# from setup_cleaned.R
div_prop <- function(var, total_pop) {
  x <- (var/total_pop)^2
  return(x)
}

# separating the df into the general demographic variables
  # NOTE: this part will be hard coded

educ_raw <- other_var_groups %>% 
  select(c(GEOID, NAME, geometry, starts_with("educ")))

pob_raw <- other_var_groups %>% 
  select(c(GEOID, NAME, geometry, starts_with("pob")))

# performing the calculations

div_index_fxn <- function(dat, var_type, geography) {
  if(geography == "tract") {
    slice <- dat %>% 
      rename(total = ends_with("Total")) %>%
      select(!c(GEOID, NAME, geometry))
  } else if(geography == "nbhd") {
    slice <- dat %>% 
      rename(total = ends_with("Total")) %>%
      select(-tract20_nbhd) %>%
      mutate_all(as.numeric)
  }
  
  
  # this calculates the divraw numbers
  for(i in 1:(ncol(slice))) {
    slice <- cbind(slice, div_prop(slice[, i], slice$total))
    colnames(slice)[ncol(slice)] <- paste0(colnames(slice)[i], "_calc")
  }
  
  # this takes all the variables and adds them based off which group they've been assigned to
  # i.e. this is the function that creates the index
  group_calc_fxn <- function(group_label) {
    label <- paste0("val_", paste0(group_label))
    return(slice %>%
             select(ends_with("calc")) %>%
             mutate(
               div_val = select(., starts_with(paste0(group_label))) %>% rowSums()
             ) %>% select(div_val) %>%
             rename(!!sym(label):= div_val)
           )
  }
  
  # variable/group names
  group_names <- categories$group_labels[grepl(paste0("^", var_type), categories$group_labels)]
  
  # initializing empty df
  div_ind <- group_calc_fxn(group_names[1])
  
  # looping through
  for(i in 2:length(group_names)) {
    div_ind <- cbind(div_ind, group_calc_fxn(group_names[i]))
  }
  
  return(div_ind[unique(colnames(div_ind))])
}

# adding diversity index calculations to larger variable data frames
pob_tract <- cbind(pob_raw, div_index_fxn(pob_raw, "pob", geography = "tract")[-1])
educ_tract <- cbind(educ_raw, div_index_fxn(educ_raw, "educ", geography = "tract")[-1])
  # this is to remove the total column

## NEIGHBORHOOD AND CITY ---------------------------------------------------

### NEIGHBORHOOD
TRACT_TO_NEIGHBORHOOD <- readxl::read_xlsx("geo20_tract_block group comparison.xlsx")

## for place of birth
pob_nbhd <- pob_raw %>%
  left_join(TRACT_TO_NEIGHBORHOOD, by = c('NAME'='tract20')) %>% 
  filter((!is.na(tract20_nbhd))| pob_total_Total == 0) %>%
  filter(tract20_nbhd != "_Census Tract 9901.01, Suffolk County, Massachusetts") %>% 
  as_tibble() %>%   
  select(-c(GEOID, NAME, geometry, GEO_ID, GEO_ID2)) %>%
  group_by(tract20_nbhd) %>%
  summarise(across(everything(), ~ sum(., is.na(.), 0))) 

# adding Boston row
total <- c(tract20_nbhd="Citywide", apply(pob_nbhd[,-1], FUN = sum, MAR = 2))
pob_nbhd <- rbind(pob_nbhd, total)

pob_nbhd <- cbind(pob_nbhd, div_index_fxn(pob_nbhd, "pob", "nbhd")[-1])


## for education
educ_nbhd <- educ_raw %>%
  left_join(TRACT_TO_NEIGHBORHOOD, by = c('NAME'='tract20')) %>% 
  filter((!is.na(tract20_nbhd))| educ_total_Total == 0) %>%
  filter(tract20_nbhd != "_Census Tract 9901.01, Suffolk County, Massachusetts") %>% 
  as_tibble() %>%   
  select(-c(GEOID, NAME, geometry, GEO_ID, GEO_ID2)) %>%
  group_by(tract20_nbhd) %>%
  summarise(across(everything(), ~ sum(., is.na(.), 0)))  

# adding Boston row
total <- c(tract20_nbhd="Citywide", apply(educ_nbhd[,-1], FUN = sum, MAR = 2))
educ_nbhd <- rbind(educ_nbhd, total)

educ_nbhd <- cbind(educ_nbhd, div_index_fxn(educ_nbhd, "educ", "nbhd")[-1])

### CITY
# other cities (?)


## WRITING OUT THE DATA ----------------------------------------------------

write_rds(pob_tract, "Place_of_Birth_diversity_tract.RDS")
write_rds(pob_nbhd, "Place_of_Birth_diversity_neighborhood.RDS")
# write_rds(lang_div_cities, "Language_diversity_cities.RDS")

write_rds(educ_tract, "Education_diversity_tract.RDS")
write_rds(educ_nbhd, "Education_diversity_neighborhood.RDS")
