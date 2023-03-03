#Patagonia Argentina
#Temperatura superficial del mar 2017

library(tidyverse)
library(rerddap)
library(raster)

servidores <- servers()
servidores
glimpse(servidores)

url_CW<-servidores %>%
  filter(str_detect(str_to_lower(name), "coastwatch"))


coastwatch_url <- servidores %>% 
  filter(short_name == "CSWC") %>% 
  pull(url)


SST <- ed_search(query = "SST", 
                          which = "griddap",
                          url = coastwatch_url)



head(SST$info)


info(SST$info$dataset_id[1])
info(SST$info$dataset_id[2])
info(SST$info$dataset_id[3])
info(SST$info$dataset_id[4])

SST <- ed_search_adv(query = "SST",
                              protocol = "griddap",
                              url = coastwatch_url,
                              keywords = "monthly",
                              maxLat = -40,
                              minLat = -58,
                              maxLon = -57,
                              minLon = -69,
                              minTime = "2017",
                              maxTime = "2022")

head(SST$info)
info(SST$info$dataset_id[1])
info(SST$info$dataset_id[2])
info(SST$info$dataset_id[3])
info(SST$info$dataset_id[4])

SST_PATAR <- griddap(SST$info$dataset_id[1], 
                   time = c("2017-01-01", "2017-12-31"),
                   latitude = c(-58, -40),
                   longitude = c(-69, -57),
                   fmt = "nc",
                   store = disk(path = "Documentos_Euge/"))

archivo <- list.files("Documentos_Euge", pattern = ".nc", full.names = T )
archivo

#EMPEZAMOS A GRAFICAR

SST_PATAR <- raster(archivo)
SST_PATAR
raster::plot(SST_PATAR[[1]], col = RColorBrewer:: brewer.pal(9, "Reds"))
#Grafiqué la primera banda de SST_PATAR (Tiene 12 por los 12 meses del 2017)

#Ahora calculo el promedio anual para toda la zona y luego lo grafico
SST_PATAR_ANUAL <- raster::mean(SST_PATAR, 12)
raster::plot(SST_PATAR_ANUAL, col = RColorBrewer:: brewer.pal(9, "PuRd"))
SST_PATAR_ANUAL

SST_PATAR$data #Veo los datos de mi archivo

#Calculo el promedio durante abril 2017 (Época de grandes inundaciones en CR) 
SST_PATAR_ABRIL17 <- SST_PATAR$data %>% 
 filter(lubridate::month(time) == 4) %>% 
  group_by(latitude, longitude) %>% 
  summarise(SST_Abril = mean(sst, na.rm = T))

head(SST_PATAR_ABRIL17) #Veo la tabla con los datos y la nueva columna de SST_Abril

#Capa de continentes
tierra <- rnaturalearth::ne_countries(returnclass = "sf")

#Graficamos SST_PATAR_ABRIL17
SST_PATAR_ABRIL17 %>% 
  ggplot(aes(x = longitude, y = latitude))+
  geom_contour_filled(aes(z = SST_Abril), binwidth = 2.0)+
  scale_fill_brewer(palette = "PuRd")+
  geom_sf(data = tierra, inherit.aes = F)+
  lims(x = c(-75, -50), y = c(-60, -40))+
  theme_bw()

#FIN
