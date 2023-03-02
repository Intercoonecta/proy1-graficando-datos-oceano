#Golfo San Jorge, Argentina
#Temperatura, salinidad y clorofila superficial

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
                              maxLat = -41,
                              minLat = -47,
                              maxLon = -60,
                              minLon = -67,
                              minTime = "2017",
                              maxTime = "2022")

head(SST$info)
info(SST$info$dataset_id[1])
info(SST$info$dataset_id[2])
info(SST$info$dataset_id[3])
info(SST$info$dataset_id[4])

SST_GSJ <- griddap(SST$info$dataset_id[1], 
                   time = c("2017-01-01", "2022-09-16"),
                   latitude = c(-47, -41),
                   longitude = c(-67, -60),
                   fmt = "nc",
                   store = disk(path = "Documentos/"))

archivo <- list.files("Documentos", pattern = ".nc", full.names = T )
archivo

#EMPEZAMOS A GRAFICAR

SST_GSJ <- raster(archivo)
SST_GSJ

raster::plot(SST_GSJ[[1]], col=)
