
## functions for initiating the analysis
pull_country_list <- function(con, metric_version){
  ## a list of countries included in the analysis
  ## this should reflect the specific model run
  t <- vimpact::get_touchstone(con, metric_version)
  country_list <- DBI::dbGetQuery(con, "SELECT distinct country FROM coverage_set
                             JOIN coverage ON coverage.coverage_set = coverage_set.id
                             WHERE touchstone = $1", t)[["country"]]
  stopifnot(length(country_list) == 112L)
  message("This analysis is taking 202110 impact metrics in which 112 countries are included. 
          Update if a different version of impact metric.")
  return(country_list)
}

add_country_metadata <- function(dat){
  
  country_metadata <- country_group_list(con, 
                                         subset_countries = NULL, 
                                         country_touchstone = "202310gavi")
  country_metadata <- country_metadata %>% filter(country_group_name == "who_region")%>%
    select(-country_group_name) %>% rename(who_region = group_id)
  
  dat <- dat %>% left_join(country_metadata, by = "country")
  
}

who_palette <- function(disease=NULL){
  who_colours <- c("#e56e5a", 
                   "#94518e", 
                   "#bc8e5a", 
                   "#4d6a9c", 
                   "#3c8e1d",
                   "#38abba")
  
  col_df <- data.frame(disease = c("PAHO", 
                                   "AFRO",
                                   "EMRO", 
                                   "EURO", 
                                   "SEARO", 
                                   "WPRO"), 
                       colour = who_colours)
  
  #arrange to diseases
  if(!is.null(disease)){
    who_colours <- who_colours[col_df$disease%in% disease]
    col_df <- col_df[col_df$disease%in% disease,]
  } 
  #arrange alphabetically
  who_colours <- who_colours[order(col_df$disease)]
  col_df <- col_df[order(col_df$disease),]
  
  return(list(pal = who_colours, col_df=col_df))
}
