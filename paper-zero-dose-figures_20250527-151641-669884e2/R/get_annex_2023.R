## About this script
## It generates stats from stochastics for 2023 runs.
## It is not used in model-review-comparison-template as yet, but will be used for anything else related to 2023 runs.


## this function prepares country groups
## by default - it provides FOUR country groupings: gavi57, gavi73, who_region, and iso.
## parameter subset_countries is useful if only a subset of countries are of interest, saying PINE countries, provide this parameter as either a vector or list of ISO3 codes
country_group_list <- function(con, subset_countries = NULL, country_touchstone = "202310gavi"){
  ## find all countries
  country_all <- DBI::dbGetQuery(con, sprintf("SELECT DISTINCT country FROM burden_estimate_country_expectation
                                 JOIN burden_estimate_expectation
                                 ON burden_estimate_expectation.id = burden_estimate_expectation
                                 WHERE version LIKE '%%%s%%'", country_touchstone))[["country"]]
  stopifnot(length(country_all) == 117L)
  
  ## find latest country metadata
  t <- DBI::dbGetQuery(con, "SELECT MAX(touchstone) FROM country_metadata")[[1]]
  ## gavi57
  gavi57 <- data.frame(country_group_name = "gavi57", 
                       group_id = "gavi57",
                       country = c("AFG", "BDI", "BEN", "BFA", "BGD", "CAF", "CIV", "CMR", "COD", 
                                   "COG", "COM", "DJI", "ERI", "ETH", "GHA", "GIN", "GMB", "GNB", 
                                   "HTI", "IND", "KEN", "KGZ", "KHM", "LAO", "LBR", "LSO", "MDG", 
                                   "MLI", "MMR", "MOZ", "MRT", "MWI", "NER", "NGA", "NIC", "NPL", 
                                   "PAK", "PNG", "PRK", "RWA", "SDN", "SEN", "SLB", "SLE", "SOM", 
                                   "SSD", "STP", "SYR", "TCD", "TGO", "TJK", "TZA", "UGA", "UZB", 
                                   "YEM", "ZMB", "ZWE")) %>%
    distinct()
  stopifnot(nrow(gavi57) == 57L)
  ## gavi73
  gavi73 <- data.frame(country_group_name = "gavi73",
                       group_id = "gavi73",
                       country = DBI::dbGetQuery(con, "SELECT DISTINCT country FROM country_metadata WHERE gavi73")[["country"]])
  stopifnot(nrow(gavi73) == 73L)
  
  ## who region
  who_region <- DBI::dbGetQuery(con, "SELECT who_region AS group_id, country
                                      FROM country_metadata WHERE touchstone = $1", t) 
  who_region <- bind_rows(who_region,
                          data.frame(country = c("DMA", "GRD", "MDV", "LCA", "VCT"),
                                     group_id = c("PAHO", "PAHO", "SEARO", "PAHO", "PAHO"))) %>%
    mutate(country_group_name = "who_region") %>%
    distinct()
  stopifnot(all(country_all %in% who_region$country))
  stopifnot(all(who_region$country %in% country_all))
  
  iso <- who_region %>%
    mutate(country_group_name = "iso",
           group_id = country)
  ## make a data frame of country grouping
  ## country_group_name -> grouping type: iso3, gavi73, who_region
  ## group_id -> grouping is based on this id
  ## country -> country iso3 code
  country_group <- bind_rows(gavi57, gavi73, who_region, iso)
  
  ## flexibility added - a subset of countries
  if (!is.null(subset_countries)){
    subset_countries <- unlist(as.list(subset_countries))
    if (!all(subset_countries %in% country_all)){
      stop(sprintf("subset country code(s) not identified: %s", setdiff(subset_countries, country_all)))
    }
    subset <- data.frame(country_group_name = "subset",
                         group_id = subset_countries,
                         country = subset_countries)
    country_group <- bind_rows(country_group, subset)
  }
  
  return(country_group)
  
}

## pull data by model and disease (model average)
## con: montagu
## annex: annex
## table: table
## diseases: a list of diseases e.g. list("MenA", "COVID", "YF")
## country_attr: a list of country_group_list e.g. list("gavi57", "gavi73", "who_region")
## subset_countries: a subset of countries of interest, when this parameter is not null, country_attr should be "subset"
## period_attr: a list of periods e.g. list(2000:2022, 2023:2030, 2000:2030) -> the function will check your input and reshape a little bit to avoid repeated data extraction
pull_data_by_model_disease <- function(con, annex, table, 
                                       outcomes = c("deaths", "cases", "dalys"),
                                       diseases = NULL, 
                                       country_attr = NULL,
                                       subset_countries = NULL,
                                       period_attr = NULL, 
                                       get_all_quantiles = FALSE){
  ## <1> check parameters
  message("checking inputs")
  ## input has to be NULL or a list
  ## NULL input will be replaced by default
  ## lit input will be checked against identifiable set
  stopifnot(is.null(diseases) | is.list(diseases))
  stopifnot(is.null(country_attr) | is.list(country_attr))
  stopifnot(is.null(subset_countries) | (!is.null(subset_countries) & "subset" %in% unlist(country_attr)))
  stopifnot(is.null(period_attr) | is.list(period_attr))
  
  ## check 1: diseases
  disease_all <- list("MenA", "COVID", "YF", "Malaria", "Cholera", "HepB", "Measles", 
                      "Rubella", "HPV", "Typhoid")
  if (is.null(diseases)){
    diseases <- disease_all
  }
  if (!all(unlist(diseases) %in% unlist(disease_all))){
    stop(
      print(sprintf("disease(s) %s not identifiable", 
                    setdiff(unlist(diseases), unlist(disease_all))
      )
      )
    )
  }
  ## check 2: country_attributes
  country_group <- country_group_list(con, subset_countries)
  country_attr_all <- as.list(unique(country_group$country_group_name))
  if (is.null(country_attr)) {
    country_attr <- country_attr_all
  }
  if (!all(unlist(country_attr) %in% unlist(country_attr_all))){
    stop(print(sprintf("country attribute(s) %s not identifiable", 
                       setdiff(unlist(country_attr), unlist(country_attr_all)))
    )
    )
  }
  
  ## check 3: period_attr
  if (is.null(period_attr)){
    period_attr <- list(2000:2022, 2023:2030, 2000:2030)
  } 
  ## re-shape the input period attribute to minimize extractions
  period_all <- unique(unlist(period_attr))
  p <- list()
  p[["full_period"]] <- period_all
  for(i in seq_along(period_attr)){
    if (identical(period_attr[[i]],  period_all)){
      p[["full_period"]] <- period_all
    } else if (length(period_attr[[i]]) > 1){
      p[[paste(range(period_attr[[i]]), collapse = "-")]] <- period_attr[[i]]
    }
  }
  period_attr <- p
  
  message("good.")
  ## <2> pull data
  message("Pulling data.")
  
  meta <- DBI::dbReadTable(annex, "stochastic_file") %>%
    filter(grepl("202310", touchstone)) %>%
    select(-touchstone, - version, - creation_date)
  
  bootstrap <- bootstrap_proxy(annex)
  
  all_cols <- DBI::dbListFields(annex, table)
  if (!grepl("intervention", table)){
    outcome_cols <- c(paste0(outcomes, "_novac"), paste0(outcomes, "_impact"))
  } else{
    outcomes <- unique(gsub("_cwyx","",outcomes)) ## CWYX is identified via vaccine not outcome for Y0V
    outcome_cols <- paste0(outcomes, "_averted")
  }
  if (!all(outcome_cols %in% all_cols)){
    stop(print(sprintf("outcome code(s) not identified %s", setdiff(outcome_cols, all_cols))))
  }
  outcome_cols0 <- paste("SUM(", paste(outcome_cols, collapse = "), SUM("), ")")
  group0 <- ifelse(grepl("intervention", table), 
                   "scenario_type,modelling_group,vaccine,activity_type", 
                   "scenario_type,stochastic_file_id")
  ## 1. for each disease, country and period attribute, pull data
  
  sql1 <- paste("SELECT %s, run_id, %s",
                "FROM %s",
                "WHERE disease IN %s",
                "AND country IN %s",
                "AND year IN %s",
                "GROUP BY %s, run_id")
  
  dat <- NULL
  
  for(i in seq_along(diseases)){
    disease_i <- diseases[[i]]
    
    for(j in seq_along(country_attr)){
      country_attr_j <- country_group[country_group$country_group_name == country_attr[[j]], ]
      
      for(k in seq_along(period_attr)){
        period_attr_k <- period_attr[[k]]
        
        message(sprintf("pulling data for %s, %s, %s \n",
                        disease_i, country_attr[[j]], names(period_attr)[k]))
        
        if (country_attr[[j]] == "iso"){
          group_id <- "iso"
          
          if(names(period_attr)[k] == "full_period"){
            # do in one go per year
            group1 <- paste0(group0,",country,year")
            d1 <- DBI::dbGetQuery(annex, sprintf(sql1, group1, outcome_cols0, table,
                                                 vimpact:::sql_in(disease_i), 
                                                 vimpact:::sql_in(country_attr_j$country), 
                                                 vimpact:::sql_in(period_attr_k, FALSE),
                                                 group1))
            d1 <- cal_stats(d1, disease_i, group_id, period_attr_k, group1, outcome_cols, table, bootstrap, meta, get_all_quantiles) 
          } else {
            d1 <- NULL
          }
          #do in one go full period
          group2 <- paste0(group0,",country")
          d2 <- DBI::dbGetQuery(annex, sprintf(sql1, group2, outcome_cols0, table,
                                               vimpact:::sql_in(disease_i), 
                                               vimpact:::sql_in(country_attr_j$country), 
                                               vimpact:::sql_in(period_attr_k, FALSE),
                                               group2))
          d2 <- cal_stats(d2, disease_i, group_id, period_attr_k, group2, outcome_cols, table, bootstrap, meta, get_all_quantiles)
        } else {
          d1 <- NULL
          d2 <- NULL
          for(l in unique(country_attr_j$group_id)){
            
            if(names(period_attr)[k] == "full_period"){
              group1 <- paste0(group0,",year")
              d11 <- DBI::dbGetQuery(annex, sprintf(sql1, group1, outcome_cols0, table,
                                                   vimpact:::sql_in(disease_i), 
                                                   vimpact:::sql_in(country_attr_j$country[country_attr_j$group_id == l]), 
                                                   vimpact:::sql_in(period_attr_k, FALSE),
                                                   group1))
              if(nrow(d11) > 0){
                d11 <- cal_stats(d11, disease_i, l, period_attr_k, group1, outcome_cols, table, bootstrap, meta, get_all_quantiles) 
              }
            } else {
              d11 <- NULL
            }
            d1 <- bind_rows(d1, d11)
            
            group2 <- group0
            d12 <- DBI::dbGetQuery(annex, sprintf(sql1, group2, outcome_cols0, table,
                                                 vimpact:::sql_in(disease_i), 
                                                 vimpact:::sql_in(country_attr_j$country[country_attr_j$group_id == l]), 
                                                 vimpact:::sql_in(period_attr_k, FALSE),
                                                 group2))
            if(nrow(d12) > 0){
              d12 <- cal_stats(d12, disease_i, l, period_attr_k, group2, outcome_cols, table, bootstrap, meta, get_all_quantiles)
            } else {
              d12 <- NULL
            }
            d2 <- bind_rows(d2, d12)
          }
        }
        
        dat <- bind_rows(dat, d1, d2)
        
      }
    }
    
  }
  return(dat)
}



## pull stats across diseases - impact_cluster_XXX tables
## con: montagu
## annex: annex
## table: table
## country_attr: a list of country_group_list e.g. list("gavi57", "gavi73", "who_region")
## subset_countries: a subset of countries of interest, when this parameter is not null, country_attr should be "subset"
## period_attr: a list of periods e.g. list(2000:2022, 2023:2030, 2000:2030)
pull_stats_across_diseases <- function(con, annex, table,
                                       outcomes = c("deaths", "dalys"),
                                       country_attr = NULL,
                                       subset_countries = NULL,
                                       period_attr = NULL,
                                       get_all_quantiles = FALSE){
  stop("This also has to be updated to speed up the extraction.")
  ## <1> check parameters
  message("checking inputs")
  ## input has to be NULL or a list
  ## NULL input will be replaced by default
  ## lit input will be checked against identifiable set
  stopifnot(is.null(country_attr) | is.list(country_attr))
  stopifnot(is.null(subset_countries) | (!is.null(subset_countries) & "subset" %in% unlist(country_attr)))
  stopifnot(is.null(period_attr) | is.list(period_attr))
  
  ## check 1: country_attributes
  country_group <- country_group_list(con, subset_countries)
  country_attr_all <- as.list(unique(country_group$country_group_name))
  if (is.null(country_attr)) {
    country_attr <- country_attr_all
  }
  if (!all(unlist(country_attr) %in% unlist(country_attr_all))){
    stop(print(sprintf("country attribute(s) %s not identifiable", 
                       setdiff(unlist(country_attr), unlist(country_attr_all)))
    )
    )
  }
  
  ## check 2: period_attr
  if (is.null(period_attr)){
    period_attr <- list(2000:2022, 2023:2030, 2000:2030)
  } 
  ## re-shape the input period attribute to minimize extractions
  period_all <- unique(unlist(period_attr))
  p <- list()
  p[["full_period"]] <- period_all
  for(i in seq_along(period_attr)){
    if (identical(period_attr[[i]],  period_all)){
      p[["full_period"]] <- period_all
    } else if (length(period_attr[[i]]) > 1){
      p[[paste(range(period_attr[[i]]), collapse = "-")]] <- period_attr[[i]]
    }
  }
  period_attr <- p
  
  message("good.")
  ## <2> pull data
  message("Pulling data.")
  
  all_cols <- DBI::dbListFields(annex, table)
  outcome_cols <- c(paste0(outcomes, "_novac"), paste0(outcomes, "_impact"))
  
  if (!all(outcome_cols %in% all_cols)){
    stop(print(sprintf("outcome code(s) not identified %s", setdiff(outcome_cols, all_cols))))
  }
  outcome_cols0 <- paste("SUM(", paste(outcome_cols, collapse = "), SUM("), ")")
  group0 <- "scenario_type"
  ## 1. for each disease, country and period attribute, pull data
  
  sql1 <- paste("SELECT boots_id, %s , %s",
                "FROM %s",
                "WHERE country IN %s",
                "AND year IN %s",
                "GROUP BY %s, boots_id")
  
  dat <- list()
  m <- 1
  
  for(j in seq_along(country_attr)){
    country_attr_j <- country_group[country_group$country_group_name == country_attr[[j]], ]
    message(sprintf("pulling data for %s \n",
                    country_attr[[j]]))
    
    for(k in seq_along(period_attr)){
      period_attr_k <- period_attr[[k]]
      
      for(l in unique(country_attr_j$group_id)){
        
        d <- DBI::dbGetQuery(annex, sprintf(sql1, group0, outcome_cols0, table,
                                            vimpact:::sql_in(country_attr_j$country[country_attr_j$group_id == l]), 
                                            vimpact:::sql_in(period_attr_k, FALSE),
                                            group0))
        if(nrow(d) > 0){
          names(d) <- c("boots_id", unlist(strsplit(group0, ",")), outcome_cols)
          
          
          ## calculate stats
          d2 <- get_stats(d, unlist(strsplit(group0, ",")), get_all_quantiles)
          
          dat[[m]] <- d2 %>%
            mutate(group_id = l,
                   period = ifelse(length(period_attr_k) > 1, 
                                   paste(range(period_attr_k), collapse = "-"),
                                   as.character(period_attr_k)))
          
          m <- m+1
        }
      }
    }
  }
  
  
  dat <- bind_rows(dat)
  
  return(dat)
  
}


get_stats <- function(d, group, get_all_quantiles = FALSE){
  
  if(get_all_quantiles == TRUE){
    
    b1 <- 
      d %>%
      group_by_at(group) %>%
      summarise_at(vars(matches("deaths|dalys|cases")), 
                   list(mid = ~ mean(., na.rm = TRUE), 
                        med =  ~ quantile(., 0.50, na.rm = TRUE),
                        `95_lo` = ~ quantile(., 0.025, na.rm = TRUE), 
                        `95_hi` = ~ quantile(., 0.975, na.rm = TRUE), 
                        `80_lo` = ~ quantile(., 0.1, na.rm = TRUE), 
                        `80_hi` = ~ quantile(., 0.9, na.rm = TRUE), 
                        `50_lo` = ~ quantile(., 0.25, na.rm = TRUE), 
                        `50_hi` = ~ quantile(., 0.75, na.rm = TRUE))) 
    
  }else{
    
    b1 <- 
      d %>%
      group_by_at(group) %>%
      summarise_at(vars(matches("deaths|dalys|cases")), 
                   list(mean = ~ mean(., na.rm = TRUE), 
                        q1 = ~ quantile(., 0.025, na.rm = TRUE), 
                        q3 = ~ quantile(., 0.975, na.rm = TRUE))) 
    
  }
  
  return(b1)
}

## this function is to use YF re sampling strategy as proxy for COVID
## this is needed for generating covid model average estimates
## since covid is not considered for cross-disease aggregations, I didn't do re-sampling for the disease
bootstrap_proxy <- function(annex){
  meta <- DBI::dbReadTable(annex, "stochastic_file") %>%
    filter(grepl("202310", touchstone)) %>%
    select(-touchstone, - version, - creation_date)
  
  bootstrap <- DBI::dbReadTable(annex, "bootstrap_2023") # -> use YF as proxy for covid resampling - both two models
  v <- bind_rows(merge(meta %>% filter(modelling_group == "IC-Garske"),
                       meta %>% filter(modelling_group == "IC-Ghani"), by = c("is_cohort", "is_under5")), 
                 merge(meta %>% filter(modelling_group == "UND-Perkins"),
                       meta %>% filter(modelling_group == "LSHTM-Liu"), by = c("is_cohort", "is_under5"))) %>%
    select(id.x, id.y, modelling_group.y)
  w <- bootstrap %>% filter(stochastic_file_id %in% v$id.x) %>%
    mutate(stochastic_file_id_new = v$id.y[match(stochastic_file_id, v$id.x)]) %>%
    mutate(modelling_group_new = v$modelling_group.y[match(stochastic_file_id, v$id.x)]) %>%
    mutate(disease = "COVID",
           modelling_group = modelling_group_new,
           stochastic_file_id = stochastic_file_id_new) %>%
    select(-modelling_group_new, - stochastic_file_id_new)
  
  bootstrap <- bind_rows(bootstrap, w)
  return(bootstrap)
}

## as a function
cal_stats <- function(d, disease_i, group_id, period_attr_k, group, outcome_cols, table, bootstrap, meta, get_all_quantiles){
  
  names(d) <- c(unlist(strsplit(group, ",")), "run_id", outcome_cols)
  
  
  ## if per model, directly calculate stats
  d2 <- get_stats(d, unlist(strsplit(group, ",")), get_all_quantiles) %>% as.data.frame()
  if(!grepl("intervention", table)){
    d2 <- d2 %>%
      mutate(modelling_group = meta$modelling_group[match(stochastic_file_id, meta$id)]) %>%
      select(-stochastic_file_id)
  }
  dat <- d2
  if("year" %in% names(dat)){
    dat$period <-  as.character(dat$year)
    dat$year <- NULL
  } else {
    dat$period <- paste(range(period_attr_k), collapse = "-")
  }
  if("country" %in% names(dat)){
    dat$group_id <- dat$country
    dat$country <- NULL
  } else {
    dat$group_id <- group_id
  }
  
  
  ## if per disease, merge with bootstrap and then get stats, 
  ##  for covid, get simple model average as we don't bootstrap for covid, maybe we should
  if (grepl("intervention", table)){
    d2 <- d %>%
      mutate(run_id = as.integer(run_id)) %>%
      left_join(bootstrap %>% filter(disease == disease_i), by = c("modelling_group", "run_id"), relationship = "many-to-many")
    gg <- c( ifelse("country" %in% names(d2), "country", NA),
             ifelse("year" %in% names(d2), "year", NA),
             "scenario_type", "vaccine", "activity_type")
  } else {
    d2 <- d %>%
      left_join(bootstrap  %>% filter(disease == disease_i), by = c("stochastic_file_id", "run_id"), relationship = "many-to-many")
    gg <- c( ifelse("country" %in% names(d2), "country", NA),
             ifelse("year" %in% names(d2), "year", NA),
             "scenario_type")
  }
  d3 <- get_stats(d2, gg[!is.na(gg)], get_all_quantiles)
  
  dat2 <- d3 %>% mutate(modelling_group = "model_average")
  if("year" %in% names(d3)){
    dat2$period =  as.character(dat2$year)
    dat2$year <- NULL
  } else {
    dat2$period = paste(range(period_attr_k), collapse = "-")
  }
  if("country" %in% names(dat2)){
    dat2$group_id <- dat2$country
    dat2$country <- NULL
  } else {
    dat2$group_id <- group_id
  }
  
  return(bind_rows(dat, dat2) %>% mutate(disease = disease_i))
  
}

# ## examples
# ## please note 
# # - pulling data from annex takes time
# # - the more complex the parameters are set, the more time you need to wait for the output 
# # - so please do consider what you need out of annex
# 
# ## examples 1
# ## pull cross view, 
# ## deaths and cases outcomes
# ## YF and MenA
# ## group by "gavi73" countries and "who_region", other attributes you can include are "gavi57" and "iso"
# ## no subset countries of interest, if you do, see the next example
# ## aggregate for 3 periods 2000-2022, 2023-2030 and 2000-2030
# x1 = pull_data_by_model_disease(con, annex, 
#                                table = "cross_all_2023",
#                                outcomes = c("deaths", "cases"),
#                                diseases = list("YF", "MenA"), 
#                                country_attr = list("iso", "gavi73", "who_region"), 
#                                subset_countries = NULL,
#                                period_attr = list(2000:2022, 2023:2030, 2000:2030))
# 
# ## example 2
# ## pull cohort view, 
# ## only deaths 
# ## YF and MenA
# ## group by "gavi73" countries and "who_region", other attributes you can include are "gavi57" and "iso", 
# ## no subset countries of interest, if you do, see the next example; if you run with attribute "iso", DO NOT provide subset_countries - redundant
# ## output per year from 2015 to 2020 and aggregate for 2015-2020
# ## sorry period is character, it is nice for 2015-2020, but not ideal when it looks like 2015-2015, 2016-2016, ...
# ## the following code makes it nicer if you are running the function for per year
# ## mutate(year = as.integer(stringr::str_extract(period, "^.{4}")))
# x2 = pull_data_by_model_disease(con, annex, 
#                                table = "cohort_all_2023",
#                                outcomes = c("deaths", "deaths_cwyx"),
#                                diseases = list("MenA"), 
#                                country_attr = list("gavi73", "subset"), 
#                                subset_countries = list("PAK", "IND", "NGA", "ETH"),
#                                period_attr = c(as.list(2015:2020), list(2015:2020)))
# 

# ## example 3
# ## pull aggregated estimates - impact_cluster_XXX tables
# d <- pull_stats_across_diseases(con, annex, table,
#                                 outcomes = outcomes,
#                                 country_attr = country_attr,
#                                 subset_countries = subset_countries,
#                                 period_attr = period_attr,
#                                 get_all_quantiles = get_all_quantiles)

