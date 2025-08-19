disease_palette <- function(disease=NULL){
  vimc_colours <- c("#008080",
                    "#1927A1",
                    "#E68424",
                    "#EF4050",
                    "#E7BF44",
                    "#3EEFE1",
                    "#9573B5",
                    "#A10084",
                    "#F3DFA2",
                    "#645CD1",
                    "#A1D15C",
                    "#D15B8F",
                    "#6F3B3A",
                    "#FFFF00",
                    "grey20",
                    "grey50", 
                    "grey80"
  )
  
  col_df <- data.frame(disease = c("COVID",
                                   "Cholera",
                                   "JE",
                                   "Measles",
                                   "HepB",
                                   "Hib",
                                   "HPV",
                                   "Malaria",
                                   "MenA",
                                   "PCV",
                                   "Rota",
                                   "Rubella",
                                   "Typhoid",
                                   "YF",
                                   "Diphtheria",
                                   "Tetanus",
                                   "Pertussis"), 
                       colour = vimc_colours)
  
  #arrange to diseases
  if(!is.null(disease)){
  vimc_colours <- vimc_colours[col_df$disease%in% disease]
  col_df <- col_df[col_df$disease%in% disease,]
  } 
  #arrange alphabetically
  vimc_colours <- vimc_colours[order(col_df$disease)]
  col_df <- col_df[order(col_df$disease),]
  
  return(list(pal = vimc_colours, col_df=col_df))
}

#-----------------------------------------------------------------------------------

# TEST
# library(dplyr)
# ggplot(disease_palette()$col_df %>% mutate(y = 3))+geom_col(aes(x=disease, y=y, fill = disease))+scale_fill_manual(values = disease_palette()$pal)+theme_minimal()

#-----------------------------------------------------------------------------------

darker_col <- function(color, how_much = 30){
  colorRampPalette(c(color, "black"))(100)[how_much]
}

lighter_col <- function(color, how_much = 30){
  colorRampPalette(c(color, "white"))(100)[how_much]
}

#-----------------------------------------------------------------------------------
vaccine_palette <- function(vaccine=NULL){
  
  vaccines <- c("COVID","Cholera", "Malaria", "Measles","DTP3","HepB","HepB_BD","Hib3","HPV","JE","MCV1", "MCV2","MenA","MenACWYX","PCV3","RCV2","Rota","Rubella","Typhoid", "YF" ) 
  
  df <- data.frame(vaccine = vaccines, 
                   colour = NA)
  
  df$colour <- sapply(X = 1:nrow(df),
                      FUN = function(x)ifelse(df$vaccine[x] %in% disease_palette()$col_df$disease,
                                              disease_palette()$col_df$colour[disease_palette()$col_df$disease == df$vaccine[x]],
                                              NA))
  
  # then get the rest
  df$colour[df$vaccine == "DTP3"] <- "#000000"
  df$colour[df$vaccine == "HepB_BD"] <- lighter_col(df$colour[df$vaccine == "HepB"], 50)
  df$colour[df$vaccine == "Hib3"] <- disease_palette("Hib")$pal
  df$colour[df$vaccine == "MCV1"] <- lighter_col(df$colour[df$vaccine == "Measles"], 50)
  df$colour[df$vaccine == "MCV2"] <- darker_col(df$colour[df$vaccine == "Measles"], 50)
  df$colour[df$vaccine == "MenACWYX"] <- darker_col(df$colour[df$vaccine == "MenA"], 50)
  df$colour[df$vaccine == "PCV3"] <- disease_palette("PCV")$pal
  df$colour[df$vaccine == "RCV2"] <- darker_col(df$colour[df$vaccine == "Rubella"], 50)
  
  
  #arrange to vaccines
  if(!is.null(vaccine)){
    df <- df[df$vaccine%in% vaccine,]
  } 
  #arrange alphabetically
  df <- df[order(df$vaccine),]
  
  return(list(pal = df$colour, col_df=df))
}

#-----------------------------------------------------------------------------------

# TEST
# library(dplyr)
# ggplot(vaccine_palette()$col_df %>% mutate(y = 3))+geom_col(aes(x=vaccine, y=y, fill = vaccine))+scale_fill_manual(values = vaccine_palette()$pal)+theme_minimal()
