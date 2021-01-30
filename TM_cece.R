setwd("D:/Documents/M2 SISE/Text Mining/Projet")

#chargement des données
library(readxl)
data=read_xlsx("BDD_CEDA_dec_2020_complete - ANONYME.xlsx", na="NC")

#exploration
nrow(data)
summary(data)

#valeurs manquantes
#recherche de NA
sapply(data,function(x) sum(is.na(x)))

#enlever les formulaire avec la case "attentes" vides
data <- subset(data, is.na(data$Attentes)==F)
#il reste 150 obervations

#Famille
data$Famille=as.factor(data$Famille)
table(data$Famille)
#recodage
data$Famille.reg[data$Famille == "1 parent (mère)"] <- "1 parent"
data$Famille.reg[data$Famille == "1 parent"] <- "1 parent"
data$Famille.reg[data$Famille == "1 parent (père)"] <- "1 parent"
data$Famille.reg[data$Famille == "1 parent / famille d'accueil"] <- "famille accueil"
data$Famille.reg[data$Famille == "1 parents (mère)"] <- "1 parent"
data$Famille.reg[data$Famille == "2 parents"] <- "2 parents"
data$Famille.reg[data$Famille == "2 parents (mère + beau-père)"] <- "2 parents"
data$Famille.reg[data$Famille == "famille accueil"] <- "famille accueil"
table(data$Famille.reg)

#Migration de l'enfant
data$`Migration de l'enfant`=as.factor(data$`Migration de l'enfant`)
table(data$`Migration de l'enfant`)
#recodage
data$Migration.reg[data$`Migration de l'enfant` == "non"] <- "Non"
data$Migration.reg[data$`Migration de l'enfant` == "Non"] <- "Non"
data$Migration.reg[data$`Migration de l'enfant` == "oui"] <- "Oui"
data$Migration.reg[data$`Migration de l'enfant` == "Oui"] <- "Oui"
table(data$Migration.reg)

#Age enfant
tmp=data$Age
tmp1=strsplit(tmp, "a")
res=c()
n=length(tmp1)
for (i in 1:n){
  res=c(res,tmp1[[i]][[1]])
}
res
length(res)
data$anneeEnfant=as.numeric(res)
#recodage
data$anneeEnfant.reg[data$anneeEnfant < 3] <- "perinatalite"
data$anneeEnfant.reg[data$anneeEnfant < 11 & data$anneeEnfant >= 3] <- "enfant"
data$anneeEnfant.reg[data$anneeEnfant >= 11 ] <- "ado"

#Profession père
data$`Profession père`=as.factor(data$`Profession père`)
table(data$`Profession père`)
nlevels(data$`Profession père`) #137 métiers 
head(sort(table(data$`Profession père`), decreasing = T),10)
#a regrouper selon CSP
#https://www.insee.fr/fr/information/2406153

#Profession mère
data$`Profession mère`=as.factor(data$`Profession mère`)
table(data$`Profession mère`)
nlevels(data$`Profession mère`) #105 métiers 
head(sort(table(data$`Profession mère`), decreasing = T),10)

#DDN père
#data$`DDN père`=as.numeric(data$`DDN père`)
#data$`DDN père`=as.Date(data$`DDN père`, origin = "1899-12-30")
#DDN mère
data$`DDN mère`=as.numeric(data$`DDN mère`)
data$`DDN mère`=as.Date(data$`DDN mère`, origin = "1899-12-30")

#age parents au moment de la reception du questionnaire
library(lubridate, warn.conflicts = FALSE)
data$agePere=floor(time_length(interval(data$`DDN père`, data$`date réception questionnaire`), "years"))
data$ageMere=floor(time_length(interval(data$`DDN mère`, data$`date réception questionnaire`), "years"))

#age ou ils ont eu leurs enfants
data$agePereEnfant=data$agePere-data$anneeEnfant
data$ageMereEnfant=data$ageMere-data$anneeEnfant

# Fratrie (total avec l'enfant)
data$`Fratrie (total avec l'enfant)`=as.factor(data$`Fratrie (total avec l'enfant)`)
table(data$`Fratrie (total avec l'enfant)`)

#Place dans la fratrie
table(data$`Place dans la fratrie`)
#recodage
data$Place.reg[data$`Place dans la fratrie` == "1"] <- "1"
data$Place.reg[data$`Place dans la fratrie` == "1/2 (jumeaux)"] <- "1"
data$Place.reg[data$`Place dans la fratrie` == "2"] <- "2"
data$Place.reg[data$`Place dans la fratrie` == "2 (jumeaux)"] <- "2"
data$Place.reg[data$`Place dans la fratrie` == "2/3 (jumeaux)"] <- "2"
data$Place.reg[data$`Place dans la fratrie` == "3"] <- "3"
data$Place.reg[data$`Place dans la fratrie` == "3/4 (jumeaux)"] <- "3"
data$Place.reg[data$`Place dans la fratrie` == "4"] <- "4"
data$Place.reg[data$`Place dans la fratrie` == "5"] <- "5"
table(data$Place.reg)

#5 jumeaux
#faire une variable jumeaux oui/non ?



#Sujet 1
#Quelles sont les attentes des parents vis à vis d'une consultation en centre
#spécialisé ? Ces attentes sont elles modulées par l'âge des enfants ? La place de
#l'enfant dans la fratrie ? etc... 

text=data$Attentes
library(dplyr)
text_df <- tibble(line = 1:186, text = text)

library("tidytext")
library("tidyverse")
devtools::install_github("ThinkRstat/stopwords")
library("stopwords")

books_tidy <- text_df %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords_iso$fr) %>%
  count(word, sort = TRUE) %>%
  head(150)
