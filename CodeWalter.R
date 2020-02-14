# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Projet Humanités Numériques Web scrapping et utilisation des API's # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # 
# Jeu de données pays du monde # 
# # # # # # # # # # # # # # # # 


# Installation des libraries necessaires pour le web scraping
install.packages(c("tidyverse","xml2","rvest","jsonlite"))

library(tidyverse)
library(xml2)
library(rvest)
library(jsonlite)

# # Fonction formatage des accents 
# delete.accent <- function(x) {
#   x <- chartr("éèëêÉÈËÊàÀçÇÏÎîï", "eeeeEEEEaAcCIIii", x)
#   return(x)
# }

# Fonction qui permettra faire le boucle pour coller chaque morceau de l'url où nous allons extraire l'information de chaque rubrique   
collecte.Data <- function(n){
  url.fonction <- paste('https://en.populationdata.net/rankings/',n,'/',sep="")
  pays.fonction <- str_to_upper(read_html(url.fonction) %>% html_nodes('td:nth-child(2)') %>% html_text())
  info.fonction <- as.double(sub(",",".",gsub("[^0-9{,}]","",read_html(url.fonction) %>% html_nodes('td:nth-child(4)') %>% html_text())))
  return(data.frame("Pays"=pays.fonction,"Info"=info.fonction))
}

# # Fonction qui permettra changer les differences dans les noms des pays pour faire bien marcher la jointure avec les deux tables contenants les pays 
# remplacer.nom.pays <- function(v1,v2,data){
#   # Tous les champs à remplacer ont un NA comme valeur de contient
#   j <- 1
#   for (i in c(1:length(v1))) {
#     data[data==v1[i]] <- v2[j]
#     j <- j+1
#   }
#   return (data)
# }
# 
# #v1 <- c("BELARUS (BIELORUSSIE)","MYANMAR (BIRMANIE)","BOSNIE-ET-HERZEGOVINE","GRENADE","MAURICE","COOK","MACEDOINE DU NORD","MARSHALL","TCHEQUIE","SAINT-CHRISTOPHE-ET-NIEVES","SAO TOME-ET-PRINCIPE",
# #        "SEYCHELLES","SLOVAQUIE","ESWATINI (SWAZILAND)","TIMOR ORIENTAL")
# #v2 <- c("BIELORUSSIE","BIRMANIE","BOSNIE-HERZEGOVINE","GRENADE (ILES DE LA)","ILE MAURICE","ÎLES COOK","MACEDOINE","MARSHALL (ILES)","REPUBLIQUE TCHEQUE","SAINT-KITTS-ET-NEVIS","SAO TOME ET PRINCIPE",
# #       "SEYCHELLES","SLOVAQUIE","SWAZILAND","TIMOR-ORIENTAL")
# #pays <- remplacer.nom.pays(v1,v2,pays)

# Récuperation du pays et continent qui permettront construire la dataframe qui fera la jointure avec la dataframe de la fonction collecte.Data
url <- "https://en.populationdata.net/rankings/rankings-life-expectancy/"
pays <- str_to_upper(read_html(url) %>% html_nodes('td:nth-child(2)') %>% html_text())
continent <- str_to_upper(read_html(url) %>% html_nodes('td:nth-child(3)') %>% html_text())
information.pays <- data.frame( "Pays"=pays, "Continent"=continent)

  
# Vecteur qui contiendra chaque morceau d'url passant comme paramètre de la fonction collecte.Data  pour construire l'url de chaque rubrique
table_name <- c("rankings-life-expectancy","infant-mortality","epi","mortality","tourism",
                "gdp-per-capita","birth-rate","population")
# Boucle pour parcourir le vector et avoir l'index de la fonction collecte.Data
for(i in table_name)
  # Jointure entre la première dataframe qui contient le pays et le continant avec la dataframe qui donne la fonction collecte.Data en utilisant l'index de la boucle précédente
  information.pays <- merge(information.pays,collecte.Data(i),by.x = "Pays", by.y = "Pays", all = TRUE)
# Nommage de colonnes du dataframe pour chaque rubrique extraite depuis le site "Esperance_vie","Mortalite infantile","Indice perf env","Mortalite" avec la dataframe du "pays" et "continent" 
colnames(information.pays) <- c("Pays","Continent","Esperance_vie","Mortalite_inf","Indice_perf_env","Mortalite",
                                "tourisme","pib-par-habitant","Natalite","Superficie")

# Récuperation pays et capitals qui permettron construire la dataframe qui contient les capitals et le pays qui permettra faire la jointure avec la dataframe information.pays
url <- "https://www.boldtuesday.com/pages/alphabetical-list-of-all-countries-and-capitals-shown-on-list-of-countries-poster" 
capitals_2 <- str_to_upper(read_html(url) %>% html_nodes("tr+ tr td+ td") %>% html_text())
pays_2 <- str_to_upper(read_html(url) %>% html_nodes("tr+ tr td:nth-child(1)") %>% html_text())
collecte.pays.capitals <- data.frame("Pays"=pays_2,"Capitals"=capitals_2)
# Jointure entre la dataframe collecte.pays.capitals avec information.pays
collecte <- merge(collecte.pays.capitals,information.pays,by.x = "Pays", by.y = "Pays", all = TRUE)
# Nettoyage des pays qu'on va pas utiliser car il ont pas d'information significative 
collecte <- collecte[-c(262:272),]
collecte




rownames(collecte) <- collecte$Pays
collecte$Pays <- NULL






# utilisation api openweather
api.Data <- function(n){
  url_api  <- paste("http://api.openweathermap.org/data/2.5/weather?q=",n,"&units=metric&appid=9ada210033e2363be58a9fac5b682c4f&lang=fr",sep="")
  api_data <- fromJSON(url_api)
  longitude <- api_data$coord$lon
  latitude <- api_data$coord$lat
  temperature <- api_data$main$temp
  temp_max <- api_data$main$temp_max
  temp_min <- api_data$main$temp_min
  humidity <- api_data$main$humidity
  temps <- api_data$weather$description
  vitesse_vent <- api_data$wind$speed
  direction_vent <- api_data$wind$deg
  
  return(data.frame("ville"=n,"longitude"=longitude,"latitude"=latitude,"temp_actu"=temperature,"temp_max"=temp_max,"temp_min"=temp_min,
                    "humidity"=humidity,"type_temps"=temps,"vitesse_vent"=vitesse_vent,"direct_vent"=direction_vent))
  
}


print(nuevascap)
api.Data("bogota")

collecte.pays.capitals$Capitals

#names(api_data)
for (i in collecte.pays.capitals$Capitals) {
  for (j in api.Data(i)) {
    if(is.null(j) | is.na(j))
      j <- NaN
    print(api.Data(i))
  }
ville_temps <- api.Data(i)
temps_info <- data.frame(ville_temps) 
return(temps_info)
}
  
  
