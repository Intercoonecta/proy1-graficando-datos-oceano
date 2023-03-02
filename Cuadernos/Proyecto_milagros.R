# Proyecto 1 
## Graficando datos sobre el océano
### *Mentora:Denisse Fierro Arcos*

#El objetivo de este proyecto es crear una base de datos con ejemplos 
#mostrando cómo creargráficos con datos de diversas fuentes, incluyendo
#a datos de la NOAA, NASA, Modelos CMIP6 e ISIMIP, ARGOS, entre otros.
#Los ejemplos deberán mostrar el flujo de trabajo completo desde acceder
#a los datos, manipularlos de cualquier forma que sea necesaria y 
#finalmente crear un gráfico.

#Lenguaje: R, Python o con una combinación de ambos lenguajes.

### Por:Milagros Castillo
library(reticulate)
library(tidync)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(CopernicusMarine)
library(sen2r)
# Paquetes importantes en CopernicusMarine
install.packages("crayon", "jsonlite", "leaflet", "purrr", "readr", "rvest",
"sf","stringr", "xml2",dependencies = T)
# https://api.obis.org/v3/occurrence/00041a43-7eb6-41f7-b481-1f4ca6aca7df

download.file(
  url = "https://marine.copernicus.eu/access-data/ocean-monitoring-indicators/global-ocean-chlorophyll-trend-map-observations", 
  destfile = "trend-map-observations"
)
View(url)
download.file(
  url = "https://marine.copernicus.eu/access-data/ocean-monitoring-indicators/global-ocean-acidification-mean-sea-water-ph-trend-map", 
  destfile = "global-ocean-acidification-mean-sea-water-ph-trend-map"
)
View(url)



