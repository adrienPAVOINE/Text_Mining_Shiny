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

       #CSP
data$`Profession mère` <- sapply(data$`Profession mère`, function(x) trimws(x) )
data$`Profession père` <- sapply(data$`Profession père`, function(x) trimws(x) )



csp.list <- c( "A la retraite" = "retraite", "acheteur industriel" = "cadres", "adjoint administratif" = "employes", 'AESH-CO'='PI', 'agent administratif' = 'employes', 'agent de fabrication'= 'ouvriers', 'agent de maintenance'='PI', 
               'agent de production'='ouvriers', 'agent de protection de site'='employes', 'agent de recouvrement'='employes', 'agent de sécurité'='employes', 'agent de sécurité incendie'='employes', 'agent de voyage'='artisans, commercants', "agent d'école"="employes", "Agent d'entretien" ="ouvriers", "Agent SNCF"="ouvriers", "agent technique"="employes", "agent territorial"="employes", "aide à domicile"="employes",
               "aide aux migrants en association"="autres", "aide ménagère"="employes", "aide soignante"="employes", "aide-soignante"="employes", "animateur HSE"="PI", "animatrice centre de loisir"="PI", "architecte"="cadres", "Artisan"="artisans, commercants", "artiste peintre"="cadres", "assistante administrative"="employes", "assistante ADV"="employes", "assistante commerciale"="PI", "assistante marketing"="PI", "assistante maternelle"="employes", "assistante administrative et commerciale"="PI", "assitante maternelle"="employes", "auto-entrepreneur"="artisans, commercants", 
               "auxiliaire de périculture"="employes", "auxilliaire de vie"="employes", "AVS"="PI", "brancardier"="employes", "cadre"="cadres", "cadre commercial"="cadres", "cadre dirigeant carrière pierre"="cadres", "Cadre informatique"="cadres", "cadre SNCF"="cadres", "cantonier BA"="ouvriers", "cantonnier"="ouvriers", "cariste"="ouvriers", "carrosier"="ouvriers", "chargé de mission chef de chantier"="PI", "chargé de recherche IRSTEA"="cadres", "chargée de clientèle"="PI", "chargée de clientèle assurance"="PI", "chargée de recrutement"="cadres", "chauffeur poids lourd"="ouvriers", "chauffeur routier"="ouvriers",
               "chef chantier forreur"="PI", "chef de cuisine"="PI", "Chef de projet"="cadres", "chef de projet en finance"="cadres", "Chef d'entreprise"="artisans, commercatnts", "Chef gérant"="artisans, commercants", "chirurgien-dentiste"="cadres", "coiffeur"="employes", "coiffeuse"="employes", "comptable"="employes", "conducteur travauc"="ouvriers", "conseiller carrières"="cadres", "conseiller client"="PI", "constructeur de décors"="ouvriers", "contrôleur de gestion"="cadres", "courtier en finance"="cadres", "cuisinier"="employes", "dans le désamiantage"="ouvriers", "Décédé"="NC", "dentiste"="cadres", "dessinateur"="PI", "dessinatrice"="PI",
               "directeur administratif et financier"="cadres", "Directeur d'agence"="cadres", "directeur des services techniques"="cadres", "directeur général ameublement"="cadres", "directeur projet"="cadres", "éclairagiste"="ouvriers", "écrivain"="cadres", "éducatrice spécialisée"="PI", "Electricien"="ouvriers", "employé d'étage"="employes", "employé en quincaillerie"="employes", "employée commerciale"="employes", "employée service client"="employes", "Enseignant"="cadres", "enseignante"="cadres", "enseignant chercheur"="cadres", "enseignant en collège"="cadres", "Entrepreneur BTP"="artisans, commercants", "entrepreneur indépendant"="artisans, commercants", "expert fonctionel SAP"="PI", "expert prêt les assurance"="PI", "Exploitant maritime"="PI", "façadier"="ouvriers", "facteur"="employes", "fonctionnaire"="employes", "fonctionnaire "="employes", 
               "formateur informatique"="PI", "formatrice"="PI", "formatrice informatique"="PI", "frigoriste"="ouvriers", "gérant"="artisans, commercant", "Gérant / commercial"="artisans, commercants", "gérant coiffure"="artisans, commercants", "gérant contrôle de patrimoine immobilier"="artisans, commercants", "gouvernante"="PI", "graphiste"="PI", "Grutier"="ouvriers", "historienne d'art"="cadres", "hotelier sans emploi"="chomage", "infirmier"="PI", "infirmière"="PI", "informaticien"="PI", "ingénieur"="cadres", "ingénieur chef de projet"="cadres", "ingénieur des ventes"="cadres", "ingénieur EDF"="cadres", "ingéneiru en recherche d'emploi"="chomage", "ingénieur étude informatique"="cadres", "ingénieur étude TP"="cadres", "ingénieur informatique"="cadres", "ingénieur/chef de laboratoire"="cadres", "ingénieure"="cadres", "ingénieure commerciale"="cadres", "interimaire"="ouvriers", "intérimaire"="ouvriers", "interprète de conférence"="PI", "maçon coffreur"="ouvriers", "magasinier"="ouvriers", "maître d'œuvre"="PI", "maître nageur sauveteur"="PI", "masseur kinésithérapeute"="PI", "masseur-kinésithérapeute"="PI", "médecin"="cadres", "Médecin"="cadres", "menuisier"="ouvriers", "mère au foyer"="chomage", "MGR"="PI", "miliataire"="employes","mise en rayon"="employes", "monteur/dépanneur pneumatique"="ouvriers", 
               "musicien"="cadres", "NC"="NC", "opérateur logistique"="PI", "opératrice logistique"="PI", "Ouvrier"="ouvriers", "Ouvrier professionnel"="ouvriers", "peintre en batiment"="ouvriers", "père au foyer"="chomage", "Pharmacien"="cadres", "policier"="employes", "professeur"="PI", "professeur d'arts plastiques"="cadres", "professeur de mathématiques"="cadres", "professeur des écoles"="PI", "professeur d'espagnol"="cadres", "projecteur bureau d'études"="cadres", "psychologue"="cadres", "puericultrice"= "employes", "réceptionnaire après vente"="employes", "recherche"="chomage", "responsable auto"="artisans, commercants", "responsable exdort"="artisans, commercants", "Restaurateur"="artisans, commercants", "retraité (consultant)"="retraite", "sans activité"="chomage", "sans emploi"="chomage", "sans profession"="chomage", "scientifique"="cadres", "secrétaire comptable"="employes", "secrétaire médicale"="employes", "secrétaire, mère au foyer actuellement"= "chomage", "Sommelier"="PI", "soudeur"="ouvriers", "taxi"="ouvriers", "technicien"="ouvriers", "technicien cinéma"="PI", "technicien contrôle qualité"="ouvriers", "Technicien paies"="ouvriers", "technicien SAV"="ouvriers", "technicien soudeur"="ouvriers", "tehnicien pharma"="ouvriers", "téléconseillère"="employes", "Tourneur fraiseur"="ouvriers", "traiteur"="employes", "transport de personnes"="employes", "urbaniste"="cadres", "vendeuse ameublement"="employes","vétérinaire"="cadres", 
               "vétérinaire directeur du développement clinique"="cadres", "Conseillère clientèle"="PI", "Assistante de direction"="PI", "Sans profession"="chomage", "Enseignante"="cadres"
               , "commerce" = "artisans, commercants" , "policière en Algérie"="employes", "Secrétaire"= "employes", "Vendeuse spécialisée en thé"= "employes", "Assistante maternelle"="employes", "technicienne chimiste"="ouvriers", "aide soigante"="employes", "gestionnaire de scolarité"="PI", "assitante administrative et commerciale"="PI", "employée de vie scolaire"="employes", "Educatrice"="employes", "Au foyer"="chomage", "DRH"="cadres", "Travailleur social"="employes", "ATSEM"="employes", "Conseillaire clientèle"="PI", "esthéticienne"="employes", "Gestionnaire RH"="PI", "Chargée de mission / agent territorial"="employes", "Chef d'équipe"="PI", "operations MGR"="PI", "Ingénieur/chef de laboratoire"="cadres", "Ingénieur informatique"="cadres", "ouvrier"="ouvriers", "pharmacien"="cadres", "artisan"="artisans, commercants", "chef de projet"="cadres", "gérant contrôle tehnique automobile"="artisans, commercants", "enseignant"="cadres", "carreleur"="ouvriers", "restaurateur"="artisans, commercants", "chef chantier foreur"="PI", "gestion de patrimoine immobilier"="cadres", "ingénieur en recherche d'emploi"="chomage", "militaire"="employes", "agent d'entretien"="ouvriers")


sante <- c( 'AESH-CO', "aide à domicile",
            "aide aux migrants en association", "aide soignante","aide-soignante",
            "assistante maternelle", "assitante maternelle",
            "auxiliaire de périculture", "auxilliaire de vie", "AVS"="PI", "brancardier",
            "chirurgien-dentiste","dentiste", "éducatrice spécialisée", "Enseignant", "enseignante", "enseignant en collège", 
            "infirmier", "infirmière","masseur kinésithérapeute", "masseur-kinésithérapeute", "médecin", "Médecin", 
            "Pharmacien", "Assistante maternelle", "aide soigante", "Educatrice", "ATSEM","pharmacien","enseignant", "Enseignant")



data$csp_pere <- csp.list[data[['Profession père']]]
data$csp_mere <- csp.list[data[['Profession mère']]]

for (j in 1:183){
  if (is.na(data$`Profession mère`[j])){
    data$`Profession mère`[j] = ""
  }
  if (is.na(data$`Profession père`[j])){
    data$`Profession père`[j] = ""
  }
}    


data$parents.sante <- "non"   
for (i in sante){    
  for (j in 1:183){   
    if (data$`Profession mère`[j] == i | data$`Profession père`[j] == i) {
      data$parents.sante[j] = "oui"  
    }
  }
}


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
