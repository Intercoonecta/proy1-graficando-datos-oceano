# Cargar paquetes
library(rgbif)
library(plyr)
library(ggplot2)
library(lubridate)
library(leaflet)

# Filtrar por nombre científicos de las tortugas marinas
taxon_name <- c("Caretta caretta", "Chelonia mydas", "Dermochelys coriacea", "Eretmochelys imbricata", "Lepidochelys olivacea")

# Definir los límites geográficos del Mar Caribe
bounding_box <- c(-96.80, 5.50, -58.50, 25.90)

# Definir el periodo de tiempo
start_date <- "1990-01-01"
end_date <- "2023-01-01"

# Descargar los datos de GBIF y covnertir a objetos JSON  
occ_list <- occ_search(
  scientificName = taxon_name,
  geometry = bounding_box,
  hasCoordinate = TRUE,
  eventDate = paste0(start_date, ",", end_date),
  limit = 1000
)
summary(occ_list)
str(occ_list)
names(occ_list)
# Convertir la lista de objetos JSON a un data.frame
occurrences <- ldply(occ_list, data.frame)

# Crear un gráfico que diferencie las especies y su cambio en el tiempo
ggplot(occurrences, aes(x = year(eventDate), fill = scientificName)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(x = "Año", y = "Número de ocurrencias", title = "Ocurrencias de tortugas marinas en el Mar Caribe") +
  scale_fill_manual(values = c("orange", "green", "blue", "violet", "red"), name = "Especies", labels = c("Caretta caretta", "Chelonia mydas", "Dermochelys coriacea", "Eretmochelys imbricata", "Lepidochelys olivacea"))


install.packages("leaflet")
# Cargar librerías necesarias
library(leaflet)
library(dplyr)

# Leer datos de GBIF
occ <- occ_search(
  scientificName = "Cheloniidae",
  geometry = "-92,9.5,-59.5,25",
  hasCoordinate = TRUE,
  limit = 10000
)

# Convertir a un data frame
occ_df <- occ2df(occ)

# Seleccionar registros a partir de 1900
occ_df <- occurrences %>% filter(year(eventDate) >= 1900)

# Crear mapa
map <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = occ_df,
             ~decimalLongitude, ~decimalLatitude)#,
#popup = paste("Especie: ", scientificName, "<br>",
# "Fecha: ", eventDate, "<br>",
#  "País: ", country, "<br>",
# "Proyecto: ", datasetName))

# Visualizar mapa
map

