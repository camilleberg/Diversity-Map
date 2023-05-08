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
  paste0("<h3>", nice_name, "</h3> <p> <strong>#", low_n, 
         "</strong> when choosing <strong>", fancy_text(lowest_col), "</strong> <p> <strong>#", high_n,
         "</strong> when choosing <strong>", fancy_text(highest_col), "</strong>")
}
BUILD_MINMAX_CITY <- function(){
  paste0("<h2>Highest and Lowest Rankings out of 25 Comparable Cities</h2>",
         COL_MINMAX_CITIES("race"),
         COL_MINMAX_CITIES("pob"),
         COL_MINMAX_CITIES("hh"),
         COL_MINMAX_CITIES("educ") ,
         COL_MINMAX_CITIES("lang"),      
         COL_MINMAX_CITIES("age"))
}

COL_MINMAX_NEIGH <- function(var, neigh){
  
  nice_name <- fancy_text(var)
  
  just_bos <- all_cities_ranked %>% 
    #colnames(DF)[max.col(DF,ties.method="first")]
    filter(City == "Boston city, Massachusetts") %>% 
    rename(tract20_nbhd = City)
  
  bos <- just_bos %>% 
    rbind(all_neigh_ranked)
    #colnames(DF)[max.col(DF,ties.method="first")]
    filter(tract20_nbhd == neigh) %>% 
    select(contains(var))
  
  lowest_col <- colnames(bos)[min.col(bos)]
  highest_col <- colnames(bos)[max.col(bos)]
  
  low_n <- bos[[lowest_col]]
  high_n <- bos[[highest_col]]
  paste0("<h3>", nice_name, "</h3> <p> ",
         "<strong>#", low_n, 
         "</strong> when choosing <strong>", fancy_text(lowest_col), "</strong> <p> ",
         "<strong>#", high_n,
         "</strong> when choosing <strong>", fancy_text(highest_col), "</strong>")
}
BUILD_MINMAX_NEIGH <- function(neigh_input){
  
  paste0("<h2>Highest and lowest rankings out of 24 Neighborhoods: <strong>", neigh_input, "</strong> </h2>",
         COL_MINMAX_NEIGH("race", neigh_input),
         COL_MINMAX_NEIGH("pob", neigh_input),
         COL_MINMAX_NEIGH("hh", neigh_input),
         COL_MINMAX_NEIGH("educ", neigh_input) ,
         COL_MINMAX_NEIGH("lang", neigh_input),      
         COL_MINMAX_NEIGH("age", neigh_input))
  
}


bos <- all_cities_ranked %>% 
  #colnames(DF)[max.col(DF,ties.method="first")]
  filter(City == "Boston city, Massachusetts") %>% 
  rename(tract20_nbhd = City)




















