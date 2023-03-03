##########################################
# Hackaton en Ciencia Marina (en español)#
##########################################
## Proyecto 1 
## Graficando datos sobre el océano
### *Mentora:Denisse Fierro Arcos*

#El objetivo de este proyecto es crear una base de datos con ejemplos 
#mostrando cómo creargráficos con datos de diversas fuentes, incluyendo
#a datos de la NOAA, NASA, Modelos CMIP6 e ISIMIP, ARGOS, entre otros.
#Los ejemplos deberán mostrar el flujo de trabajo completo desde acceder
#a los datos, manipularlos de cualquier forma que sea necesaria y 
#finalmente crear un gráfico.

#Lenguaje: lenguaje R.
### Por:Milagros Castillo
#############################################################
#                Prueba practicando con datos               #
#############################################################

#############################################################
#            Diferentes Paquete que pueden ser usados       #
#############################################################
library(reticulate)
library(tidync)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(CopernicusMarine)
library(sen2r)
library(ncdf4)
library(raster)
library(sf)
library(leaflet)
library(lubridate)
library(rasterVis)
library(ggplot2)
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

#########################################################################
#Ejemplo del Sitio Web:https://atlas.mercator-ocean.fr/s/dCTPqL77Ydposfi#
#########################################################################
# Crear datos en carpeta
dir.create("data")

# Descargar datos de muestra CMEMS
download.file(url = "https://github.com/dmarch/Rworkshop-MarineData4America/raw/main/data/global-analysis-forecast-phy-001-024-monthly_1624214790015.nc",
              destfile="data/global-analysis-forecast-phy-001-024-monthly_1624214790015.nc", mode = 'wb')

# Download Galapagos Islands MPA
download.file(url = "https://github.com/dmarch/Rworkshop-MarineData4America/raw/main/data/GalapagosMPA.gpkg",
              destfile="data/GalapagosMPA.gpkg", mode = 'wb')
# Inspeccionar archivos NetCDF
# Establezca la ruta para el archivo
ncfile <- "data/global-analysis-forecast-phy-001-024-monthly_1624214790015.nc"

# Importar NetCDF
nc <- nc_open(ncfile)
print(nc)
#Importar NetCDF as Raster
# Banda única
#importar NetCDF con ráster
sst_single <- raster(ncfile)
sst_single
plot(sst_single)
### Multiband

# importar archivo multi-band NetCDF 
sst_multi <- brick(ncfile)

# imprimir resumen de brick
sst_multi

# plot brick dataset
levelplot(sst_multi)
#############################################
# Análisis Raster
#############################################

### Resumen de estadísticas

# calculo de promedio y desviación
sst_mean <- calc(sst_multi, fun = mean)
sst_sd <- calc(sst_multi, fun = sd)

# plot raster dataset
plot(sst_mean, main = "Promedio SST")
plot(sst_sd, main = "Desviación Estandar SST")

# convertir raster a data.frame
sst_df <- as.data.frame(sst_mean, xy=TRUE, na.rm=TRUE)

# importar capa de países de Natural Earth
countries <- ne_countries(scale = "medium", returnclass = "sf")

# plot
ggplot()+
  # add raster layer
  geom_raster(aes(x=x, y=y, fill=layer), data=sst_df) +
  # define color palette of raster layer
  scale_fill_distiller(palette = "Spectral", name = "SST (ºC)") + 
  # add countries layers
  geom_sf(fill=grey(0.9), color=grey(0.6), lwd = 0.2, data=countries) +
  # define spatial extent
  coord_sf(xlim = range(sst_df$x), ylim = range(sst_df$y), expand = F, ndiscr = 500) +
  # labels
  labs(title = "Sea Surface Temperature (SST)",
       subtitle = "Annual average estimated from monthly products for 2020",
       x = "Longitude",
       y = "Latitude") +
  # theme
  theme_bw() 



### Extraer valores de un modelo numérico

# Límites de importación del Área Marina Protegida de las Islas Galápagos
mpa <- st_read("data/GalapagosMPA.gpkg")

# basic plot
plot(st_geometry(mpa))

# Create color palette for CMEMS maps
palRaster <- colorNumeric("Spectral", domain = sst_mean@data@values, reverse = TRUE,
                          na.color = "transparent")

# Plot the boundary of MPA with a base map
leaflet(mpa) %>% 
  # add base map
  addProviderTiles("Esri.OceanBasemap") %>%
  # add raster map
  addRasterImage(sst_mean, colors = palRaster, opacity = 0.8) %>%
  # add legend
  addLegend(pal = palRaster, values = values(sst_mean), title = "SST (ºC)") %>%
  # add MPA boundary
  addPolygons(color = "green")

# extract values from MPA and summarize values using the mean and standard deviation
mpa_sst_avg <- extract(sst_multi, mpa, fun=mean, na.rm=T)
mpa_sst_sd <- extract(sst_multi, mpa, fun=sd, na.rm=T)

# get date
date_sst <- sst_multi %>%
  # get time stamps from multi raster
  getZ() %>%
  # parse character to POSIXct class (time)
  parse_date_time("Ymd HMS") %>%
  # get the first day of each month
  floor_date("month")

# generate data.frame with three new columns (time, mean, sd)
mpa_sst <- data.frame(date = date_sst, sst_avg = c(mpa_sst_avg), sst_sd = c(mpa_sst_sd))

# inspect data.frame
mpa_sst

# plot data
ggplot(mpa_sst, aes(x = date)) +
  # add ribbon to represent mean +- SD
  geom_ribbon(aes(ymin = sst_avg-sst_sd, ymax = sst_avg+sst_sd),  alpha=.2, linetype=0, fill="steelblue") +
  # add line to represent mean value
  geom_line(aes(y = sst_avg), size = 1, color="steelblue") +
  # define frequency of x-axis and date labels
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  # plot labels labels
  labs(title = "Sea Surface Temperature (SST) in Galapagos Islands MPA",
       # note we use `expression()` to add +- symbol
       subtitle = expression(Monthly~values~(mean %+-% SD)~from~2020),
       x = "",
       y = "SST (ºC)") +
  # theme
  theme_bw() 
