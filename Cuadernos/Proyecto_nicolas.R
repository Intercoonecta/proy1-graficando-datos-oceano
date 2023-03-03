#Instalar programas de ser necesario
install.packages("rgbif")
install.packages("dplyr")

#Llamar librerias
library(rgbif)
library(dplyr)
library(ggplot2)
library(ggmap)

bbox <- c(-98.13, 7.23, -55.81, 28.77) # Coordenadas del mar Caribe en  --> área de interés

turtles <- occ_search(
  scientificName = "Cheloniidae", # tortugas marinas 5 spp.
  hasCoordinate = TRUE, # Filtrar sólo registros con coordenadas válidas
  geometry = bbox, # mar Caribe
  year = "2000,2023", # periodo de interés
  limit = 50000 # Límite máx. permitido por OBIS-SEAMAP
)

# Selección de las columnas de interés y renombramiento
turtles <- turtles$data %>%
  select(
    gbifID, 
    species, 
    eventDate, 
    decimalLatitude, 
    decimalLongitude 
  ) %>%
  rename(
    id = gbifID,
    especie = species,
    fecha = eventDate,
    latitud = decimalLatitude,
    longitud = decimalLongitude
  )
# Conteodel número de registros para cada sp.
turtles_counts <- turtles %>%
  count(especie, sort  = TRUE)

#Histograma de barras que muestra la abundancia de cada especie
ggplot(turtles_counts, aes(x = especie, y = n)) +
  geom_bar(stat = "identity") +
  xlab("Especie") +
  ylab("Número de registros") +
  ggtitle("Abundancia de tortugas marinas en el Caribe desde el año 2000")

# No. registros para  sp. y año
turtles_counts_year <- turtles %>%
  mutate(anio = format(as.Date(fecha), "%Y")) %>%
  count(especie, anio, sort = TRUE)

# Abundancia total por año de cada especie en el tiempo
ggplot(turtles_counts_year, aes(x = anio, y = n, color = especie, group = especie)) +
  geom_line(linewidth = 1) +
  xlab("Año") +
  ylab("Número de registros") +
  ggtitle("Abundancia total de tortugas marinas en el mar Caribe (2000-2022)")

# Descarga del mapa de Googlemaps
map_BN <- get_map(location = bbox, maptype = "toner-lite", zoom = 6)

# Mapa con todos los registros en B/N
ggmap(map_BN) +
  geom_point(data = turtles, aes(x = longitud, y = latitud), alpha = 0.5) +
  xlab("Longitud") +
  ylab("Latitud") +
  ggtitle("Registros de tortugas marinas en el Caribe (2000-2023)")

# Descarga del mapa de registro a CLR
map_CL <- ggmap(get_map(location = bbox, maptype = "toner-lite"))

#Mapa de registros dif. por colores
map_CL +
  geom_point(data = turtles, aes(x = longitud, y = latitud, color = especie)) +
  scale_color_manual(values = c("black", "blue", "red", "orange", "purple", "green")) +
  xlab("Longitud") +
  ylab("Latitud") +
  ggtitle("Tortugas marinas en el mar Caribe (2000-2023)")






####interactivo
library(leaflet)

# No. registros para cada especie y ubicación
turtles_counts_loc <- turtles %>%
  group_by(especie, latitud, longitud) %>%
  summarize(n = n())

map <- leaflet(turtles_counts_loc) %>%
  addTiles() %>%
  addMarkers(
    lat = ~latitud,
    lng = ~longitud,
    popup = ~paste("<strong>Especie:</strong> ", especie, "<br>",
                   "<strong>Registros:</strong> ", n, "<br>")
  ) %>%
  addLegend(
    title = "Especies",
    colors = unique(turtles_counts_loc$especie),
    labels = unique(turtles_counts_loc$especie),
    position = "bottomleft"
  )
