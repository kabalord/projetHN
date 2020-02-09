# installer les packages dplyr, ggplot2, readr, purrr, tidyr, tibble
install.packages("tidyverse")
# charger les packages dplyr, ggplot2, readr, purrr, tidyr, tibble
library("tidyverse")

# web scraping capitals dans le monde
install.packages("rvest")
library("rvest")

url <- "https://fr.wikipedia.org/wiki/Liste_des_capitales_du_monde_par_population?fbclid=IwAR3YMXWKKGmN9oGxa69M5RHU2AT8Jm-95AaQTVKshf0bH0Nf4Ix83_Bp_T8" 
capitals <- read_html(url) %>% html_nodes("td:nth-child(3)") %>% html_text()
capitals
data <- matrix(capitals)
data


indices <- c(1:243)



# Package jsonlite permet de lire ces données et de les rendre exploitables
install.packages("jsonlite")
library("jsonlite")

# utilisation api projet
library(httr)
url_site  <- "http://api.openweathermap.org/data/2.5/weather?q=pekin&appid=9ada210033e2363be58a9fac5b682c4f"
site_data <- fromJSON(url_site)
names(site_data)
site_data$visibility

