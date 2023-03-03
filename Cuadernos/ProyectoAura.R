#Proyecto 1 Hackaton GRAFICANDO DATOS OCÉANO
####### AURA BUENFIL#########

#Anomalías de sst en el Océano Pacífico Oriental

library(satin) #paquete satin

cop <- read.cmems("global-reanalysis-phy-001-031-grepv2-mnstd-monthly_1666976487037.nc") #lectura de variables fisicas de copernicus
# Reso -- 27 km
# mensuales desde 1993 - 2020
lapply(cop, function(x) {y <- x@attribs; y$longname}) # que variables contiene

#Se calculan las anomalías
sst <- cop$thetao_mean # selecciono únicamente la temperatura
sst.ym <- satinMean(sst, by = "%Y-%m") #estima la media de sst anual
sst.m <- satinMean(sst, by = "%m") #estima la media de sst mensual
sst.anom <- anomaly(X = sst.ym, Y = sst.m) #se estima la anomalía
unique(sst.ym@attribs$labels) # se repiten para cada año
sst.m@attribs$labels

#establecer ejes y geografica
mt <- paste(month.abb, rep(2000:2021, each = 12)) # meses
xli <- c(-145, -70)
yli <- c(-15, 20)
zli <- c(-7.5, 7.5)
at.x <- seq(-150, -70, 30)
at.y <- seq(-30, 40, 10)

#Se establece paleta de colores
sch <- c("darkblue", "blue", "cyan", "white", "yellow", "red", "red4")
cs <- 0.5 #limite separación
un <- expression(paste("SST monthly anomalies (°C)")) #etiqueta de la barra de colores/ leyenda
z <- sst.anom@data[, , 1]
pal <- satinPalette(zmin = zli[1], zmax = zli[2], col.sep = cs, scheme = sch)
cbp <- pal$palette
cbb <- pal$breaks

#Representación visual de anomalías

x11(); par(mar=c(0,0,0,0), oma=c(4.5, 4.5, 3.5, 4.5))
layout(matrix(c(1:4, 17, 5:8, 17, 9:12, 17, 13:16, 17), ncol=5, byrow = TRUE))

plot.satin(sst.anom, period=177, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, adj = 0.5, pos = 4, xaxt="n",atx=at.x, aty=at.y)
axis(2, at=at.y, labels = FALSE)
plot.satin(sst.anom, period=203, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=215, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=265, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=177, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=204, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=217, colbar=FALSE, xlim=xli, ylim=yli,
           zlim=zli, col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
axis(2, at=at.y, labels = FALSE)
plot.satin(sst.anom, period=277, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=181, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=205, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=217, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=277, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=182, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, atx=at.x, aty=at.y)
plot.satin(sst.anom, period=206, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
plot.satin(sst.anom, period=218, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, xaxt="n", yaxt="n")
sp::degAxis(1, at =at.x, labels=FALSE)
plot.satin(sst.anom, period=278, colbar=FALSE, xlim=xli, ylim=yli, zlim=zli,
           col.sep=cs, scheme=sch, yaxt="n", atx=at.x)
#text(-135, 30, mt[12])
mtext(c("Latitude", "Longitude"), side=c(2, 1), outer=TRUE, line = 3)
mtext(c("2007-2007 event                                                                                                                         ", 
        " 2009-2009 event                                                         ", 
        "    2010-2010 event  ", "                                                    
                                                             2014-2014 event"), 
      side= 3, outer=TRUE, line = 0.2, cex = 0.8)
par(mar=c(0, 1, 0, 0))
satin::imageScale(z, col=cbp, breaks=cbb, axis.pos=4, las=2)
mtext(un, side=4, line=2.8, outer=TRUE)
