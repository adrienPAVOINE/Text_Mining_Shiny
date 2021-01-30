# devtools::install_github("ColinFay/rgeoapi")

library(ggplot2)
library(maps)
library(leaflet)
library(httr)

# Operateur pour InfoCodePostal
`%||%` <- function(a,b) if(is.null(a)) b else a

# Informations depuis le code postal
InfoCodePostal <- function(codePostal) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"),
                        codesPostaux = vector("character"),
                        codeDepartement = vector("character"),
                        codeRegion = vector("character"),
                        population = vector("character"),
                        surface  = vector("character"),
                        lat  = vector("character"),
                        long = vector("character"))
  if(nchar(codePostal) == 4) {
    codePostal <- paste0("0", codePostal)
  }
  url <- paste0("https://geo.api.gouv.fr/communes?codePostal=", 
                codePostal, 
                "&fields=nom,codeInsee,codesPostaux,codeDepartement,codeRegion,codeRegion,population,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("Input n'est pas un code postal (was ", codePostal,")")
      identity <- default
    } else {
      identity <- lapply(content, function(obj){
        data.frame(name = obj$nom %||% NA, 
                   codeInsee = obj$code %||% NA,
                   codesPostaux = obj$codesPostaux %||% NA,
                   codeDepartement = obj$codeDepartement %||% NA,
                   codeRegion = obj$codeRegion %||% NA,
                   population = obj$population %||% NA,
                   surface  = obj$surface %||% NA,
                   lat  = obj$centre$coordinates [2] %||% NA,
                   long = obj$centre$coordinates [1] %||% NA,
                   stringsAsFactors = FALSE)
      }) %>% do.call(rbind, .)  
    }
  
    return(identity)
  } else {
    warning("Input n'est pas un code postal (was ", codePostal,")")
    identity <- default
    return(identity)
  }
}

# Longitude depuis le Code Postal
LongFromCodePostal <- function(codePostal) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"),
                        codesPostaux = vector("character"),
                        codeDepartement = vector("character"),
                        codeRegion = vector("character"),
                        population = vector("character"),
                        surface  = vector("character"),
                        lat  = vector("character"),
                        long = vector("character"))
  if(nchar(codePostal) == 4) {
    codePostal <- paste0("0", codePostal)
  }
  url <- paste0("https://geo.api.gouv.fr/communes?codePostal=", 
                codePostal, 
                "&fields=nom,codeInsee,codesPostaux,codeDepartement,codeRegion,codeRegion,population,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("Input n'est pas un code postal (was ", codePostal,")")
      identity <- default
    } else {
      identity <- lapply(content, function(obj){
        data.frame(name = obj$nom %||% NA, 
                   codeInsee = obj$code %||% NA,
                   codesPostaux = obj$codesPostaux %||% NA,
                   codeDepartement = obj$codeDepartement %||% NA,
                   codeRegion = obj$codeRegion %||% NA,
                   population = obj$population %||% NA,
                   surface  = obj$surface %||% NA,
                   lat  = obj$centre$coordinates [2] %||% NA,
                   long = obj$centre$coordinates [1] %||% NA,
                   stringsAsFactors = FALSE)
      }) %>% do.call(rbind, .)  
    }
    
    return(identity[1,'long'])
  } else {
    warning("Input n'est pas un code postal (was ", codePostal,")")
    identity <- default
    return(identity)
  }
}

# Latitude depuis le code postal
LatFromCodePostal <- function(codePostal) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"),
                        codesPostaux = vector("character"),
                        codeDepartement = vector("character"),
                        codeRegion = vector("character"),
                        population = vector("character"),
                        surface  = vector("character"),
                        lat  = vector("character"),
                        long = vector("character"))
  if(nchar(codePostal) == 4) {
    codePostal <- paste0("0", codePostal)
  }
  url <- paste0("https://geo.api.gouv.fr/communes?codePostal=", 
                codePostal, 
                "&fields=nom,codeInsee,codesPostaux,codeDepartement,codeRegion,codeRegion,population,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("Input n'est pas un code postal (was ", codePostal,")")
      identity <- default
    } else {
      identity <- lapply(content, function(obj){
        data.frame(name = obj$nom %||% NA, 
                   codeInsee = obj$code %||% NA,
                   codesPostaux = obj$codesPostaux %||% NA,
                   codeDepartement = obj$codeDepartement %||% NA,
                   codeRegion = obj$codeRegion %||% NA,
                   population = obj$population %||% NA,
                   surface  = obj$surface %||% NA,
                   lat  = obj$centre$coordinates [2] %||% NA,
                   long = obj$centre$coordinates [1] %||% NA,
                   stringsAsFactors = FALSE)
      }) %>% do.call(rbind, .)  
    }
    
    return(identity[1,'lat'])
  } else {
    warning("Input n'est pas un code postal (was ", codePostal,")")
    identity <- default
    return(identity)
  }
}


# Espace de travail
setwd("~/Documents/M2/Text_Mining/Projet")

# importation des donnees
text_df <- read_excel("BDD_CEDA_dec_2020_complete - ANONYME.xlsx")
text_df = as.data.frame(text_df)

# Modalités de la variable code Postal
unique(text_df$`Code postal`)

# On remplace les code postaux non valides 
text_df[text_df$`Code postal`=='Rhône',]$`Code postal` = '69001'
text_df[text_df$`Code postal`=='Isère',]$`Code postal` = '38000'

# Conversion du code postal en numérique 
text_df$`Code postal` = as.numeric(text_df$`Code postal`)

# sapply(text_df[,'Code postal', drop=F],LatFromCodePostal)  ???

# Construction d'un vecteur avec tous les latitudes 
latitude = c()
for(code in text_df$`Code postal`){
  print(LatFromCodePostal(code))
  latitude = cbind(latitude,LatFromCodePostal(code))
}

# Construction d'un vecteur avec toutes les longitudes 
longitude = c()
for(code in text_df$`Code postal`){
  print(LongFromCodePostal(code))
  longitude = cbind(longitude, LongFromCodePostal(code))
}


text_df$`Code postal`

ggplot( ComByPostal(38000), aes(x = long,y=lat,fill=codesPostaux))+
  geom_polygon(colour='black',size=0.1)+
  theme(legend.position = 'none')


france_map = map_data('france')
france_carte = as.data.frame(france_map)

ggplot(france_map,aes(x = long,y=lat,group=group,fill=region))+
  geom_polygon(colour='black',size=0.1)+
  theme(legend.position = 'none')













