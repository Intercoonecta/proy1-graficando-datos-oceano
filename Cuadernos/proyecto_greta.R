
#-------------------------------------------------------------------------------
# proyecto_greta.R      DOWNLOADING COPERNICUS MARINE SERVICE DATA
#-------------------------------------------------------------------------------
# This script downloads and visualizes Sea Surface Temperature (SST) model data.
#-------------------------------------------------------------------------------
# Output provides annual average SST for western Mediterranean region in 2022. 

### PART I-----DOWNLOAD DATA----------------------------------------------------
# load packages
library(CopernicusMarine)
library(tidyverse)
library(raster)

# check out products list
x <- copernicus_products_list()

# filter to selection
selection <- filter(x, str_detect(str_to_lower(mainVariables), "temp")) %>% 
  filter(str_detect(tempResolutions, "Monthly")) %>% filter(str_detect(areas, "Mediterranean Sea"))

# check variable list "MEDSEA_ANALYSISFORECAST_PHY_006_013" product
selection$mainVariables[1]

# select product of interest
my_product <- selection$product_id[1]

# check product details for layer info
pr_info <- copernicus_product_details(my_product) # find layer 

# download
copernicus_download_motu(
  username = "gjankauskaite",
  password = "",
  destination   = "datos_greta", # folder created previously
  product       = my_product,
  layer         = "cmems_mod_med_phy-tem_anfc_4.2km_P1M-m", # monthly data
  variable      = "thetao", # Sea water potential temperature
  output        = "netcdf",
  region        = c(-4, 30, 17, 45), # xmin ymin xmax ymax 
  timerange     = c("2022-01-01", "2022-12-31"), 
  verticalrange = c(0, 2),
  sub_variables = c("thetao")
)


### PART II-----IMPORT, PLOT AND CALCULATE ANUAL AVERAGE------------------------
# import single file 
ncfile <- list.files("datos_greta", full.names = T, pattern = ".nc")
sst_single <- raster(ncfile)
# print file summary
sst_single
# plot
plot(sst_single)


# import multi-band NetCDF file
sst_multi <- raster::brick(ncfile) # or "stack()"
# print a summary of the brick
sst_multi
# fix brick dates
dates <- getZ(sst_multi)
dates <- as.Date((dates/1440), origin = "1900-01-01")
sst_multi <- sst_multi %>% setZ(dates, name = "Date") %>% setNames(dates) # change random numbers to dates
# check 
sst_multi
# plot brick
library("rasterVis")
levelplot(sst_multi) 


# Calculate and plot average and SD
sst_mean <- calc(sst_multi, fun = mean)
sst_sd <- calc(sst_multi, fun = sd)
plot(sst_mean, main = "Average SST")
plot(sst_sd, main = "Standard deviation SST")


## PART III-----PLOT WITH GGPLOT2-----------------------------------------------
# Prepare data for plotting with ggplot2 package
library(rgdal)
library(rnaturalearth)
library(ggplot2)

# import MPA limits - Corredor de Migración de Cetáceos del Mediterráneo (COMICET)
mpa <- readOGR("C:/Users/greta/OneDrive - Universitat de Valencia/Documentos/Courses/HACKATON/proy1-graficando-datos-oceano/datos_greta/COMICET_PA.gpkg") 
plot(mpa)
# import countries layer from Natural Earth 
countries <- ne_countries(scale = "medium", returnclass = "sf")


# convert raster to data.frame
sst_df <- as.data.frame(sst_mean, xy=TRUE, na.rm=TRUE)
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
  # add PA polygon
  geom_polygon(col = "navy", lwd = 0.6, alpha = 4/10, fill = "darkturquoise", data = mpa, aes(x = long, y = lat, group = group)) +
  # labels
  labs(title = "Sea Surface Temperature (SST)",
       subtitle = "Annual average estimated from monthly products for 2022",
       x = "Longitude",
       y = "Latitude") +
  # theme
  theme_bw() 


# plot with leaflet package
library(leaflet)
# create color palette for CMEMS maps
palRaster <- colorNumeric("Spectral", domain = sst_mean@data@values, reverse = TRUE, na.color = "transparent")
# plot the boundary of MPA with a base map
leaflet(mpa) %>% 
  # add base map
  addProviderTiles("Esri.OceanBasemap") %>%
  # add raster map
  addRasterImage(sst_mean, colors = palRaster, opacity = 0.8) %>%
  # add legend
  addLegend(pal = palRaster, values = values(sst_mean), title = "SST (ºC)") %>%
  # add MPA boundary
  addPolygons(color = "blue")


### PART IV-----ZONAL STATISTICS EXTRACT MPA VALUES-----------------------------
# mean
mpa_sst_avg <- raster::extract(sst_multi, mpa, fun=mean, na.rm=T)
# sd
mpa_sst_sd <- extract(sst_multi, mpa, fun=sd, na.rm=T)


# convert to df
mpa_sst <- data.frame(date = dates, sst_avg = c(mpa_sst_avg), sst_sd = c(mpa_sst_sd)) %>% print()
# plot data
ggplot(mpa_sst, aes(x = date)) +
  # add ribbon to represent mean +- SD
  geom_ribbon(aes(ymin = sst_avg-sst_sd, ymax = sst_avg+sst_sd),  alpha=.2, linetype=0, fill="steelblue") +
  # add line to represent mean value
  geom_line(aes(y = sst_avg), size = 1, color="steelblue") +
  # define frequency of x-axis and date labels
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + # originally was "scale_x_datetime"
  # plot labels labels
  labs(title = "Sea Surface Temperature (SST) in Cetacean Migration Corridor MPA",
       # note we use `expression()` to add +- symbol
       subtitle = expression(Monthly~values~(mean %+-% SD)~from~2022),
       x = "",
       y = "SST (ºC)") +
  # theme
  theme_bw() 

