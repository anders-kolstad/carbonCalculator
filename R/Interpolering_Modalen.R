#Packages --------------
library(readxl)
library(rgdal)
library(raster)
library(ggplot2)
library(gstat)
library(rgeos)
library(plyr)
library(dplyr)
library(sf)
library(maps)
library(maptools)
library(dplyr)
library(spatialEco)
library(stars)
library(automap)

#Importer data ------------
modalen_punkter<-readOGR(dsn="C:/Users/martef/DokumenterIntern/PhD prosjekter/WP3_NINA_GRAN/Modalen", layer="modalen_punkter")
shp_modalen<-readOGR(dsn="C:/Users/martef/DokumenterIntern/PhD prosjekter/WP3_NINA_GRAN/Modalen", layer="modalen_shp")
shp_myr<- readOGR(dsn="C:/Users/martef/DokumenterIntern/PhD prosjekter/WP3_NINA_GRAN/Modalen", layer="Modalen_myrareal")
#Visualisere dataene ------------
plot(shp_modalen)
points(modalen_punkter, pch=1, col='red', cex=1)

df_modalen<-fortify(shp_modalen)
df_modalen_punkter <- data.frame(modalen_punkter)

p_modalen <- ggplot()
p_modalen <- p_modalen + geom_polygon( data=df_modalen, aes(x=long, y=lat, group=group),
                                           color="black", fill="lightblue", size = .1 ) +
  geom_point(data=df_modalen_punkter,
             aes(x=coords.x1,
                 y=coords.x2), pch=20, cex=1.2, col="red")
p_modalen

st_crs(modalen_punkter) 
st_crs(shp_modalen) 

#skape grid for interpolering:----------
grid <- raster(extent(shp_modalen))
res(grid) <- 1

proj4string(grid)<-proj4string(modalen_punkter)
gridpolygon <- rasterToPolygons(grid)
plot(gridpolygon)

plot(gridpolygon, cex=1.5)
points(modalen_punkter, pch=1, col='red', cex=1)

proj4string(gridpolygon)<-proj4string(shp_modalen)

proj4string(modalen_punkter)<-proj4string(gridpolygon)
proj4string(modalen_punkter)<-proj4string(grid)


st_crs(shp_modalen)
st_crs(modalen_punkter)
st_crs(grid)
st_crs(gridpolygon)

# Interpolering IDW ----------------

gs <- gstat(formula=Dybde~1, locations=modalen_punkter, nmax=5, set=list(idp = 0))
nn <- interpolate(grid, gs)
## [inverse distance weighted interpolation]
crop_IDW <- mask(nn, shp_modalen)
plot(crop_IDW)

summary(crop_IDW)
#var1.pred
#Min.         0.20
#1st Qu.      0.48
#Median       0.60
#3rd Qu.      0.76
#Max.         1.18

raster_IDW_modalen<-raster(nn)
raster_modalen<-raster(crop_IDW)
idw.output=as.data.frame(nn)

IDW.output_crop <- na.omit(idw.output, col.name = "var1.pred") 
write.csv(IDW.output_crop,"Data/IDW.output.modalen.csv", row.names = FALSE)
writeRaster(raster_IDW_modalen,'IDW_modalen.tif', overwrite=TRUE)
writeRaster(crop_IDW,'IDW_modalen3.tif', overwrite=TRUE)


sum(IDW.output_crop)
#29776.18


#Visualere med interpolering --------
plot(crop_IDW)
plot(shp_myr)
points(modalen_punkter, pch=1, col='red', cex=1)

df_modalen<-fortify(shp_modalen)
df_modalen_punkter <- data.frame(modalen_punkter)
df_myr <- fortify (shp_myr)
df_IDW <- fortify(crop_IDW)

p_modalen <- ggplot()
p_modalen <- p_modalen + geom_raster(data=raster_IDW_modalen)+ 
  geom_polygon( data=df_modalen, aes(x=long, y=lat),
                                       color="black", fill="lightblue", size = .1 ) +
  geom_polygon(data=df_myr,
               aes(x=long, y=lat),
               color="black", size = .1 ) +
  geom_point(data=df_modalen_punkter,
             aes(x=coords.x1,
                 y=coords.x2), pch=20, cex=1.2, col="red")
p_modalen

#Beregning karbon------------
#Grovestimat karbon
# carbon content (kg per m2) = 10 (conversion factor from g cm-3 to kg m2) * thickness of peat (cm) * dry bulk density (in g m-3) 
# * fraction organic matter of dry matter (0.98?) x carbon proportion (0.480-0.54)
29776.18*100*10*0.1*0.95*0.5
29776.18*100*10*0.15*0.95*0.5
#1414369
#2121553
#1414369 kg = 1414 tonn C på myrarealet

#Grovestimat CO2
1414369*3.67
#5190734 kg = 5190 tonn CO2

#Ny kjøring av IDW---------
gs2 <- gstat(formula=Dybde~1, locations=modalen_punkter, nmax=7, set=list(idp = 0))
nn2 <- interpolate(grid, gs2)
## [inverse distance weighted interpolation]
crop_IDW2 <- mask(nn2, shp_modalen)
plot(crop_IDW2)

idw.output2=as.data.frame(nn2)
IDW.output2 <- na.omit(idw.output2, col.name = "var1.pred")
sum(IDW.output2)

#Annet ---------------
IDW.output_crop_maroy$Dybde_gruppe <- cut(IDW.output_crop_maroy$var1.pred,c(0,10,60,110,Inf))
head(IDW.output_crop_maroy)


IDW.output_crop_maroy %>%
  group_by(Dybde_gruppe) %>%
  summarize(volum= sum(var1.pred, na.rm = TRUE), n=n())

#  Dybde_gruppe    volum     n
#<fct>           <dbl> <int>
# 1 (0,10]           437.    59
# 2 (10,60]       231394.  5476
# 3 (60,110]     1322563  16116
# 4 (110,Inf]    1775737  11591

59+5476+16116+11591
#33242 m2
#Ifl QGIS-polygon er arealet 33044 m2, så ikke så aller verst nærliggende

