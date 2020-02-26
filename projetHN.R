install.packages(c("tidyverse","xml2","rvest","jsonlite"))

library(tidyverse)
library(xml2)
library(rvest)
library(jsonlite)

#Les fonctions
##Supprimer les caracteres accentues et espace superflus
delete.accent <- function(x) {
  x <- chartr("????????????????", "eeeeEEEEaAcCIIii", x)
  x <- gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",x, perl=T)
  return(x)
}

##Collecter les donnees sur le site (webscapping)
collecte.Data <- function(n){
  url.fonction <- paste('https://www.populationdata.net/palmares/',n,'/',sep="")
  pays.fonction <- str_to_upper(delete.accent(read_html(url.fonction) %>% html_nodes('td:nth-child(2)') %>% html_text()))
  info.fonction <- as.double(sub(",",".",gsub("[^0-9{,}]","",delete.accent(read_html(url.fonction) %>% html_nodes('td:nth-child(4)') %>% html_text()))))
  return(data.frame("Pays"=pays.fonction,"Info"=info.fonction))
}

##Remplacer le nom des pays pour faire concorder les donnees
remplacer.nom.pays <- function(v1,v2,data){
  j <- 1
  for (i in c(1:length(v1))) {
    data[data==v1[i]] <- v2[j]
    j <- j+1
  }
  return (data)
}
#Fin les fonctions

##Creation de la premiere df pour la jointure
#Utilisation du webscrapping pour la recup?ration d'information sur le site ci dessous
url <- "https://www.populationdata.net/palmares/esperance-de-vie/"
pays <- str_to_upper(delete.accent(read_html(url) %>% html_nodes('td:nth-child(2)') %>% html_text()))
continent <- str_to_upper(delete.accent(read_html(url) %>% html_nodes('td:nth-child(3)') %>% html_text()))

#Creation de la premiere df contenant les pays et les continents
information.pays <- data.frame("Continent"=continent)
information.pays$Pays <- pays

#Nom des pages pour la fonction
table_name <- c("esperance-de-vie","mortalite-infantile","ipe","mortalite","tourisme",
                "pib-par-habitant","natalite","population")

#Application de la fonction de collecte de donnees depuis le site pour actualiser les information de la premiere df
for(i in table_name)
  information.pays <- merge(information.pays,collecte.Data(i),by.x = "Pays", by.y = "Pays", all = TRUE)

#Renommer les colonnes de information.pays
colnames(information.pays) <- c("Pays","Continent","Esperance_vie","Mortalite_inf","Indice_perf_env","Mortalite",
                                "tourisme","pib-par-habitant","Natalite","Superficie")

##Creation de la deuxieme df pour la jointure
#Utilisation du webscrapping pour la recup?ration d'information sur le site ci dessous
url <- "https://jeretiens.net/tous-les-pays-du-monde-et-leur-capitale/" 
capitals_2 <- str_to_upper(delete.accent(read_html(url) %>% html_nodes("tr+ tr td:nth-child(2)") %>% html_text()))
pays_2 <- str_to_upper(delete.accent(read_html(url) %>% html_nodes("tr+ tr td:nth-child(1)") %>% html_text()))

#Remplacement du nom de certains pays pour assurer une certaine concordance lors de la jointure
v1 <- c("BIELORUSSIE","BIRMANIE","BOSNIE-HERZEGOVINE","GRENADE (ILES DE LA)","ILE MAURICE","ILES COOK","MACEDOINE","MARSHALL (ILES)","REPUBLIQUE TCHEQUE","SAINT-KITTS-ET-NEVIS","SAO TOME ET PRINCIPE",
        "SEYCHELLES","SWAZILAND","TIMOR-ORIENTAL")
v2 <- c("BELARUS (BIELORUSSIE)","MYANMAR (BIRMANIE)","BOSNIE-ET-HERZEGOVINE","GRENADE","MAURICE","COOK","MACEDOINE DU NORD","MARSHALL","TCHEQUIE","SAINT-CHRISTOPHE-ET-NIEVES","SAO TOME-ET-PRINCIPE",
        "SEYCHELLES","ESWATINI (SWAZILAND)","TIMOR ORIENTAL")
pays_2 <- remplacer.nom.pays(v1,v2,pays_2)

#Creation de la deuxieme df contenant les pays et leur capitale
collecte.pays.capitals <- data.frame("Pays"=pays_2)
v1 <- c("SAINT JOHN'S","BUENOS-AIRES","SUCRE (OU LA PAZ)","LA HAVANE","ATHENES","KOWEIT","JERUSALEM-EST","SAINT-DOMINGUE","SRI JAYAWARDENAPURA","DOUCHANBE","FANAFUTI")
v2 <- c("SAINT JOHN","BUENOS AIRES","LA PAZ","HAVANA","ATH?NES","KOWE?T","JERUSALEM EST","SANTO DOMINGO","KOTTE","DOUCHANB?","FUNAFUTI")
capitals_2 <- remplacer.nom.pays(v1,v2,capitals_2)
collecte.pays.capitals$Capitals <- capitals_2

# Jointure entre la dataframe information.pays (premiere df) et collecte.pays.capitals (deuxieme df)
collecte <- merge(collecte.pays.capitals,information.pays,by.x = "Pays", by.y = "Pays", all = TRUE)
#Suppression des pays ne possedant pas assez d'informations significatives 
collecte <- collecte[-c(160,163,166,193,199:256),]

## Cr?ation de la fonction pour l'utilisation api openweather et la collecte des informations necessaires
api.Data <- function(n){
  url_api  <- paste("http://api.openweathermap.org/data/2.5/weather?q=",n,"&units=metric&appid=9ada210033e2363be58a9fac5b682c4f&lang=fr",sep="")
  api_data <- fromJSON(url_api)
  
  if(length(api_data$coord$lon) == 0)
    longitude <- NA
  else
    longitude <-api_data$coord$lon
  
  if(length(api_data$coord$lat) == 0)
    latitude <- NA
  else
    latitude <- api_data$coord$lat
  
  if(length(api_data$main$temp) == 0)
    temperature <- NA
  else
    temperature <- api_data$main$temp
  
  if(length(api_data$main$temp_max) == 0)
    temp_max <- NA
  else
    temp_max <- api_data$main$temp_max
  
  if(length(api_data$main$temp_min) == 0)
    temp_min <- NA
  else
    temp_min <- api_data$main$temp_min
  
  if(length(api_data$main$humidity) == 0)
    humidity <- NA
  else
    humidity <- api_data$main$humidity
  
  if(length(api_data$weather$description) == 0)
    temps <- NA
  else
    temps <- api_data$weather$description
  
  Sys.sleep(runif(1,0.75,1.5))
  return(c("Capitals"=n,"longitude"=longitude,"latitude"=latitude,"temp_actu"=temperature,"temp_max"=temp_max,"temp_min"=temp_min,
           "humidity"=humidity,"type_temps"=temps))
}

#Creation de la df qui contiendra les inforamtions retourn?es par l'appelle de la fonction api.data
collecte.api <- data.frame(matrix(1,1,8))
#Application de la fonction api.data sur l'ensemble des capitales pr?sente dans df collecte
for(i in collecte$Capitals[1:194])
  collecte.api <- rbind(collecte.api,api.Data(i))

#Suppression de la premiere ligne utilis?e pour initialiser la df collecte.api
collecte.api <- collecte.api[-1,]
#Renommage des colonnes de la df en vue de faciliter la compr?hension et la jointure qui va suivre
colnames(collecte.api) <- c("Capitals","Longitude","Latitude","Temp_actu","Temp_max","Temp_min","Humidity","Type_temps")

#Jointure entre la df collecte et la df collecte.api
collecte <- merge(collecte,collecte.api, by.x="Capitals", by.y ="Capitals", all = TRUE)
#Renommage des differentes ligne de notre df finale et suppression de la colonne pays
collecte$Pays <- collecte$Pays
collecte$Type_temps <- as.factor(collecte$Type_temps)
collecte$Humidity <- as.numeric(collecte$Humidity)
collecte$Longitude <- as.numeric(collecte$Longitude)
collecte$Latitude <- as.numeric(collecte$Latitude)
collecte$Temp_actu <- as.numeric(collecte$Temp_actu)
collecte$Temp_max <- as.numeric(collecte$Temp_max)
collecte$Temp_min <- as.numeric(collecte$Temp_min)
head(collecte, 8)
collecte <- as_tibble(rownames_to_column(collecte))


