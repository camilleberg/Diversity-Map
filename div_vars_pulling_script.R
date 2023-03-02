## Script for Diversity Index Map  ##

# Written by: Camille Bergeron
# Written on: December 12, 2022

# Last Modified: January 19, 2023
# added race variable

## TO DO:
# make more automated
# soft code the length of the census group expanded
# automate the nbhd code


## SETUP ------------------------------------------------------------------
rm(list = ls())

library(tidycensus)
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
library(janitor)

# note: you may need to run these libraries individually 

# API check
Sys.getenv("CENSUS_API_KEY")

# setting working directory
proj_folder <- paste0("C:/Users/", Sys.info()[["user"]], "/Box/Research/Active Projects/Interactive Diversity Map/data_pulling")

## PULLING AND ORGANZIING THE DATA -----------------------------------------

VARS <- tidycensus::load_variables(dataset = 'acs5', year = 2020, cache = T)
categories <- readxl::read_xlsx(paste0(proj_folder, "/div_map_categories.xlsx"), sheet = "categories")

# creating unique variable names 
categories$var_names <- paste0(categories$title, "_", categories$group_name, "_",
                               str_replace_all(categories$var_description, " ", "_"))
categories$group_labels <- paste0(categories$title, "_", categories$group_name)

# adjusting for special characters in the income
categories$var_name_ending <- str_replace_all(categories$var_name_ending, "\\$", "\\\\$")

# initialing empty column to assign new var
categories$census_label <- NA

# looping through to fill with the relevant census var names
for(i in 1:nrow(categories)){
  # selecting only the table of interest
  table <- VARS %>%
    filter(grepl(paste0("^", categories$table_name[i], "_"), VARS$name)) %>%
    select(label, name)
  
  # selecting from the table of interest 
  if(categories$title[i] == "educ" | categories$title[i] == "age") {
    
    
    if(grepl("^High school", categories$var_name_ending[i])) {
      # manually fixing the high school thing
      male <- "B15002_011"; female <- "B15002_028"
    } else {
      # aggregating the male and female var names because they're segregated for educ
      male <- table$name[grepl(paste0(categories$var_name_ending[i], "$"), table$label)][1]
      female <- table$name[grepl(paste0(categories$var_name_ending[i], "$"), table$label)][2]
    }
    
    # assigning new label
    categories$census_label[i] <- paste0(male, ",", female)
    # NOTE: the comma here should match the comma later as a separator 
    
  } else if(grepl("^Chinese", categories$var_name_ending[i])) {
    categories$census_label[i] <- "C16001_021"
  } else if(grepl("^Tagalog", categories$var_name_ending[i])) {
    categories$census_label[i] <- "C16001_027"
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

# pulling census data
other_var_pull_raw <- tidycensus::get_acs(geography = "tract", 
                                          variable = acs_pull_labels, #34, 
                                          output = "wide",
                                          state = "MA",
                                          county = "Suffolk",
                                          geometry = TRUE,
                                          year = 2021,
                                          cache_table = T,
                                          show_call = TRUE) %>% 
  filter(!(str_detect(NAME,"Census Tract 99")|str_detect(NAME,"Census Tract 18")
           |str_detect(NAME,"Census Tract 17")|str_detect(NAME,"Census Tract 16")
           |str_detect(NAME,"Census Tract 9812.01")|str_detect(NAME,"Census Tract 9801.01")))

# Removing margin of error columns
other_var_small <- other_var_pull_raw %>% 
  select(!ends_with("M"))
rm(other_var_pull_raw)

# removing the E from the end of the variable names
colnames(other_var_small)[3:length(colnames(other_var_small))-1] <- 
  substr(colnames(other_var_small)[3:length(colnames(other_var_small))-1], 1, nchar(colnames(other_var_small)[3:length(colnames(other_var_small))-1])-1)

# expand the categories into new table with variables for each column #

# initializing labeling conventio
col_var_names <- c()
for(i in 1:80) {
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

grouping_fxn <- function(div_label, city = F) {
  x <- which(colnames(div_groups_labels) == paste0(div_label))
  
  if(city == T){
    other_var_small <- cities_small
  }
  
  # this basically selects the relevant census variables related to each group
  # and then adds them
  dat <- other_var_small %>% 
    as_tibble() %>%
    select(
      colnames(other_var_small)[colnames(other_var_small) %in% (div_groups_labels[,x] %>% array())[[1]]]
    ) %>%
    mutate(val = rowSums(across())) %>% select(val) %>%
    rename(!!sym(div_label) := val)
  
  return(dat)
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

# new function to split based on variables / titles 
var_split_fxn <- function(name) {
  pull_raw <-  other_var_groups %>% 
    select(c(GEOID, NAME, geometry, starts_with(name)))
  
  # saving to the global environment
  assign(paste0(name, "_pull_raw"), pull_raw, envir = .GlobalEnv)
}

# looping through all unique variables to split the data
all_titles <- unique(categories$title)

for(i in 1:length(all_titles)){
  var_split_fxn(all_titles[i])
}

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
  } else if(geography == "city") {
    slice <- dat %>% 
      rename(total = ends_with("Total")) %>%
      select(!c(GEOID, NAME))
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
             ) %>% select(div_val) %>% mutate(div_val = 1 - div_val) %>%
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

# writing out function 
tract_calc_fxn <- function(pull_raw) {
  var <- strsplit(deparse(substitute(pull_raw)), "_")[[1]][1]
  
  # adding div values
  div_tract <- cbind(pull_raw, div_index_fxn(pull_raw, var, geography = "tract")[-1])
  
  write_rds(div_tract, paste0(var, "_diversity_tract.RDS"))
}

## NEIGHBORHOOD AND CITY ---------------------------------------------------

### NEIGHBORHOOD
TRACT_TO_NEIGHBORHOOD <- readxl::read_xlsx("geo20_tract_block group comparison.xlsx")


# function for nbhds
nbhd_calc_fxn <- function(pull_raw) {
  var <- strsplit(deparse(substitute(pull_raw)), "_")[[1]][1]
  
  #aggregating tracts to nbhds
  div_neigh <- pull_raw %>%
    left_join(TRACT_TO_NEIGHBORHOOD, by = c('NAME'='tract20')) %>% 
    filter((!is.na(tract20_nbhd))| paste0(var, "_total_Total") == 0) %>%
    filter(tract20_nbhd != "_Census Tract 9901.01, Suffolk County, Massachusetts") %>% 
    as_tibble() %>%   
    select(-c(GEOID, NAME, geometry, GEO_ID, GEO_ID2)) %>%
    group_by(tract20_nbhd) %>%
    summarise(across(everything(), ~ sum(., is.na(.), 0))) 
  
  # adding Boston row
  total <- c(tract20_nbhd="Citywide", apply(div_neigh[,-1], FUN = sum, MAR = 2))
  div_neigh <- rbind(div_neigh, total)
  
  # calculating div values 
  div_neigh <- cbind(div_neigh, div_index_fxn(div_neigh, var, "nbhd")[-1])
  
  write_rds(div_neigh, paste0(var, "_diversity_neighborhood.RDS"))
}


### CITY
cities_of_int <- readxl::read_xlsx("div_map_categories.xlsx", sheet = "cities")

cities_raw <- get_acs(geography = "place", 
                      variable = acs_pull_labels, #34, 
                      output = "wide",
                      year = 2021,
                      cache_table = T,
                      show_call = TRUE) 

## Essentially, all the steps from earlier wil just be repeated here
cities_small <- cities_raw %>%
  filter(NAME %in% unlist(cities_of_int)) %>%
  select(!ends_with("M"))
rm(cities_raw)

# removing the E from the end of the variable names
colnames(cities_small)[3:length(colnames(cities_small))-1] <- 
  substr(colnames(cities_small)[3:length(colnames(cities_small))-1], 1, nchar(colnames(cities_small)[3:length(colnames(cities_small))-1])-1)

# adding the calculated rows to the census data # 

# initializing the df 
cities_groups <- tibble(GEOID = cities_small$GEOID, 
                        NAME = cities_small$NAM) # I don't know why it's doing this but 

for(i in 1:length(colnames(div_groups_labels))) {
  cities_groups <- cbind(cities_groups, grouping_fxn(colnames(div_groups_labels)[i], city = T))
}

# city calculation function
city_calc_fxn <- function(pull_raw) {
  var <- strsplit(deparse(substitute(pull_raw)), "_")[[1]][1]
  
  # selecting the proper data
  pull_city_raw <- cities_groups %>% 
    select(c(GEOID, NAME, starts_with(var)))
  
  # calculating the index and combining
  div_city <- cbind(pull_city_raw, div_index_fxn(pull_city_raw, var, geography = "city")[-1])
  
  write_rds(div_city, paste0(var, "_diversity_cities.RDS"))
}


## WRITING OUT THE DATA ----------------------------------------------------

tract_calc_fxn(race_pull_raw)
nbhd_calc_fxn(race_pull_raw)
city_calc_fxn(race_pull_raw)

tract_calc_fxn(educ_pull_raw)
nbhd_calc_fxn(educ_pull_raw)
city_calc_fxn(educ_pull_raw)

tract_calc_fxn(lang_pull_raw)
nbhd_calc_fxn(lang_pull_raw)
city_calc_fxn(lang_pull_raw)

tract_calc_fxn(age_pull_raw)
nbhd_calc_fxn(age_pull_raw)
city_calc_fxn(age_pull_raw)

tract_calc_fxn(hh_income_pull_raw)
nbhd_calc_fxn(hh_income_pull_raw)
city_calc_fxn(hh_income_pull_raw)

tract_calc_fxn(pob_pull_raw)
nbhd_calc_fxn(pob_pull_raw)
city_calc_fxn(pob_pull_raw)

## READING IN THE DATA ----------------------------------------------------

# age_div_tract <- read_rds("data/Age_diversity_tract.RDS") #%>% 
# mutate(current_data = val_gens)
# age_div_neigh <- read_rds("data/Age_diversity_neighborhood.RDS")
# age_div_cities <- read_rds("data/Age_diversity_cities.RDS")
# 
# educ_div_tract <- read_rds("data/Education_diversity_tract.RDS") %>% 
#   mutate(current_data = val_higher_ed)
# educ_div_neigh <- read_rds("data/Education_diversity_neighborhood.RDS")
# educ_div_cities <- read_rds("data/Education_diversity_cities.RDS")
# 
# hh_income_div_tract <- read_rds("data/Household_Income_diversity_tract.RDS") %>% 
#   mutate(current_data = val_median)
# hh_income_div_neigh <- read_rds("data/Household_Income_diversity_neighborhood.RDS")
# hh_income_div_cities <- read_rds("data/Household_Income_diversity_cities.RDS")
# 
# lang_div_tract <- read_rds("data/Language_diversity_tract.RDS") %>% 
#   mutate(current_data = val_eng_not)
# lang_div_neigh <- read_rds("data/Language_diversity_neighborhood.RDS")
# lang_div_cities <- read_rds("data/Language_diversity_cities.RDS")
# 
# pob_div_tract <- read_rds("data/Place_of_Birth_diversity_tract.RDS") %>% 
#   mutate(current_data = val_nat_for)
# pob_div_neigh <- read_rds("data/Place_of_Birth_diversity_neighborhood.RDS")
# pob_div_cities <- read_rds("data/Place_of_Birth_diversity_cities.RDS")

## DEBUG SCRIPT/ QUALITY CONTROL EXAMPLE ------------------------------------------

pob_diversity_neighborhood[1, ] %>%
  select(starts_with("pob_US_top10")) %>%
  unlist %>% as.numeric() %>% sum() 

## this should equal 0 
pob_diversity_neighborhood %>%
  select(starts_with("pob_top10_")) %>%
  unlist %>% as.numeric() %>% sum() -( pob_diversity_neighborhood$pob_total_Total %>%
  as.numeric() %>% sum())

pob_diversity_cities[12, ] %>%
  select(starts_with("pob_top10_")) %>%
  unlist %>% as.numeric() %>% sum() 
