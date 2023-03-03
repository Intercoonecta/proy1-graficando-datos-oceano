
#-------------------------------------------------------------------------------
# proyecto_greta.R      DOWNLOADING COPERNICUS MARINE SERVICE DATA
#-------------------------------------------------------------------------------
# This script downloads and visualizes Sea water potential temperature (T) data.
#-------------------------------------------------------------------------------
# Output provides annual average SST for western Mediterranean region in 2022. 

### PART I-----DOWNLOAD DATA----------------------------------------------------
# load packages
library(CopernicusMarine)
library(tidyverse)

# check out products list
x <- copernicus_products_list()

# filter to selection
selection <- filter(x, str_detect(str_to_lower(mainVariables), "temp")) %>% 
  filter(str_detect(tempResolutions, "Monthly")) %>% filter(str_detect(areas, "Mediterranean Sea"))

# check variable list of "MEDSEA_ANALYSISFORECAST_PHY_006_013" product
selection$mainVariables[1]

# select product of interest
my_product <- selection$product_id[1]

# check product details for layer info
pr_info <- copernicus_product_details(my_product) # pick a layer and copy

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
library(raster)
library(rasterVis)
# import single file 
ncfile <- list.files("datos_greta", full.names = T, pattern = ".nc")
swt_single <- raster(ncfile)
# print file summary
swt_single
# plot
plot(swt_single)


# import multi-band NetCDF file
swt_multi <- raster::brick(ncfile)
# print a summary of the brick
swt_multi
# fix brick dates
dates <- getZ(swt_multi)
dates <- as.Date((dates/1440), origin = "1900-01-01")
swt_multi <- swt_multi %>% setZ(dates, name = "Date") %>% setNames(dates) # change random numbers to dates
# check 
swt_multi
# plot brick
levelplot(swt_multi) 



# Calculate and plot average and SD
swt_mean <- calc(swt_multi, fun = mean)
swt_sd <- calc(swt_multi, fun = sd)
plot(swt_mean, main = "Average Sea Water Potential Temperature (T)")
plot(swt_sd, main = "Standard deviation")


## PART III-----PLOT WITH GGPLOT2-----------------------------------------------
# Prepare data for plotting with ggplot2 package
library(rgdal)
library(rnaturalearth)
library(ggplot2)

# import MPA limits - Corredor de Migración de Cetáceos del Mediterráneo (COMICET)
mpa <- readOGR("C:/Users/greta/OneDrive - Universitat de Valencia/Documentos/Courses/HACKATON/proy1-graficando-datos-oceano/datos_greta/COMICET_PA.gpkg") 
# import countries layer from Natural Earth 
countries <- ne_countries(scale = "medium", returnclass = "sf")


# convert raster to data.frame
swt_df <- as.data.frame(swt_mean, xy=TRUE, na.rm=TRUE)
# plot
ggplot()+
  # add raster layer
  geom_raster(aes(x=x, y=y, fill=layer), data=swt_df) +
  # define color palette of raster layer
  scale_fill_distiller(palette = "Spectral", name = "T (ºC)") + 
  # add countries layers
  geom_sf(fill=grey(0.9), color=grey(0.6), lwd = 0.2, data=countries) +
  # define spatial extent
  coord_sf(xlim = range(swt_df$x), ylim = range(swt_df$y), expand = F, ndiscr = 500) +
  # add PA polygon
  geom_polygon(col = "navy", lwd = 0.6, alpha = 4/10, fill = "darkturquoise", data = mpa, aes(x = long, y = lat, group = group)) +
  # labels
  labs(title = "Sea Water Potential Temperature (T)",
       subtitle = "Annual average estimated from monthly products for 2022",
       x = "Longitude",
       y = "Latitude") +
  # theme
  theme_bw() 


# plot with leaflet package
library(leaflet)
# create color palette for CMEMS maps
palRaster <- colorNumeric("Spectral", domain = swt_mean@data@values, reverse = TRUE, na.color = "transparent")
# plot the boundary of MPA with a base map
leaflet(mpa) %>% 
  # add base map
  addProviderTiles("Esri.OceanBasemap") %>%
  # add raster map
  addRasterImage(swt_mean, colors = palRaster, opacity = 0.8) %>%
  # add legend
  addLegend(pal = palRaster, values = values(swt_mean), title = "T (ºC)") %>%
  # add MPA boundary
  addPolygons(color = "blue")


### PART IV-----ZONAL STATISTICS EXTRACT MPA VALUES-----------------------------
# mean
mpa_swt_avg <- raster::extract(swt_multi, mpa, fun=mean, na.rm=T)
# sd
mpa_swt_sd <- extract(swt_multi, mpa, fun=sd, na.rm=T)


# convert to df
mpa_swt <- data.frame(date = dates, swt_avg = c(mpa_swt_avg), swt_sd = c(mpa_swt_sd)) %>% print()
# plot data
ggplot(mpa_swt, aes(x = date)) +
  # add ribbon to represent mean +- SD
  geom_ribbon(aes(ymin = swt_avg-swt_sd, ymax = swt_avg+swt_sd),  alpha=.2, linetype=0, fill="steelblue") +
  # add line to represent mean value
  geom_line(aes(y = swt_avg), size = 1, color="steelblue") +
  # define frequency of x-axis and date labels
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + # originally was "scale_x_datetime"
  # plot labels labels
  labs(title = "Annual Average Sea Water Potential Temperature (T) in Cetacean Migration Corridor MPA",
       # note we use `expression()` to add +- symbol
       subtitle = expression(Monthly~values~(mean %+-% SD)~from~2022),
       x = "",
       y = "T (ºC)") +
  # theme
  theme_bw() 

