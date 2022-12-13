# to help

# make sure to run up to line 26 in either 
# setup_other_vars.R or
# setup_cleaned.R

# creating unique variable names 
categories$var_names <- paste0(categories$title, "_", categories$group_name, "_",
                               str_replace_all(categories$var_description, " ", "_"))

# initialing empty column to assign new var
categories$census_label <- NA
  
for(i in 1:nrow(categories)){
    # selecting only the table of interest
    table <- VARS %>%
      filter(grepl(paste0("^", categories$table_name[i]), VARS$name)) %>%
      select(label, name)
    
    # selecting from the table of interest 
    if(categories$title[i] == "educ") {
      
      # aggregating the male and female var names because they're segregated for educ
      male <- table$name[grepl(paste0(categories$var_name_ending[i], "$"), table$label)][1]
      female <- table$name[grepl(paste0(categories$var_name_ending[i], "$"), table$label)][2]
      
      # assigning new label
      categories$census_label[i] <- paste0(male, ",", female)
        # NOTE: the comma here should match the comma later as a separator 
      
    } else {
      # assigning new label 
      categories$census_label[i] <- table$name[grepl(paste0(categories$var_name_ending[i], "$"), table$label)]
    }
}                                             

# grouping and combining all the labels to the distinct groups
census_groups <- categories %>%
  group_by(var_names) %>%
  mutate(labels = paste0(census_label, collapse = ", ")) %>%
  distinct(labels)

# unlisting variable names 
acs_pull_labels <- unlist(strsplit(unique(categories$census_label), ",")) 
acs_pull_labels <- acs_pull_labels[!acs_pull_labels == "NA"]
  # fix NA issue later

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

#Removing margin of error columns
other_var_small <- other_var_raw %>% 
  select(!ends_with("M"))

##Formatting data
dat <- lang_vars %>% select(nameE,label) %>% 
  mutate(label_small = str_sub(label, start = (str_locate(label, 'Estimate!!Total:!!')[,2] + 1))) %>% 
  select(-label)

dat$label_small[1]<- "Total"

lang_pull_small <- %>% 
  select(GEOID, NAME, any_of(dat$label_small), geometry)

