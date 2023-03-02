install.packages(c("rgbif", "RColorBrewer", "vioplot")) 
install.packages("scrubr")
library(rgbif)
library(RColorBrewer)
library(vioplot)
library(scrubr)
library(maps)
library(ggplot2)

gbif_data <- occ_data(scientificName = "Testudines", hasCoordinate = TRUE, geometry='POLYGON((30.1 10.1,40 40,20 40,10 20,30.1 10.1))', limit=200000)
?occ_data
#str(gbif_data)
names(gbif_data)
names(gbif_data$meta)
names(gbif_data$data)

#Filtrar por generos
genus_list <- gbif_data$data$genus
genus_list
#Limpiar terrestres
#genus_list[is.na(genus_list)] <- "Chitra"
#genus_list

myspecies_coords <- gbif_data$data[ , c("decimalLongitude", "decimalLatitude", "individualCount", "coordinateUncertaintyInMeters")]
head(myspecies_coords)
# map the occurrence data:
map("world", xlim = range(myspecies_coords$decimalLongitude), ylim = range(myspecies_coords$decimalLatitude))  
points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], pch = ".")

