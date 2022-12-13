# to help

# make sure to run up to line 26 in either 
# setup_other_vars.R or
# setup_cleaned.R

VARS$label[grepl("^B05002", VARS$name)]

# reading in the categories
categories <- read.csv("Diversity Map Categories.csv")

# grabbing the variables
VARS$label[grepl("Native:$", VARS$label)]

# variable test for pob
i <- 1

table <- VARS %>% 
  filter(grepl(paste0("^", categories$table_name[i]), VARS$name)) %>%
  select(label)

vars <- table$label[grepl(paste0(categories$var_name_ending[i], "$"), table$label)]

var_name <- paste0(categories$title[i], "_", categories$group_name[i], "_",
       str_replace_all(categories$var_description[i], " ", "_"))

VARS$diversity_desg[i] = var_name

diversity_desg_fxn <- function(census_data) {
  for(i in 1:nrow(census_data)) {
    table <- census_data %>% 
      filter(grepl(paste0("^", categories$table_name[i]), VARS$name)) %>%
      select(label)
    
    vars <- table$label[grepl(paste0(categories$var_name_ending[i], "$"), table$label)]
    
    var_name <- paste0(categories$title[i], "_", categories$group_name[i], "_",
                       str_replace_all(categories$var_description[i], " ", "_"))
    
    census_data$diversity_desg[i] = var_name
  }
}

VARS$diversity_desg <- NA
diversity_desg_fxn(VARS)
