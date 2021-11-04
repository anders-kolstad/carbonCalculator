# Top ---------------------------------------------------------------------
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

# Importer data -------------------------------------------------------------

Torvdybder_Maroy <- read.csv("C:/Users/martef/DokumenterIntern/Kinn/Kartdata/maroy_punkter.csv", sep=",")
Data_Torvdybder_Maroy <- read.csv("C:/Users/martef/DokumenterIntern/Kinn/Kartdata/data_maroy_torvdybder.csv", sep=";")

Torvdybder_Rikmyr <- read.csv("C:/Users/martef/DokumenterIntern/Kinn/Kartdata/rikmyr_punkter.csv", sep=",")
Data_Torvdybder_Rikmyr <- read.csv("C:/Users/martef/DokumenterIntern/Kinn/Kartdata/data_rikmyr_torvdybder.csv", sep=";")

Torvdybder_Lista1 <- read.csv("C:/Users/martef/DokumenterIntern/Kinn/Kartdata/lista1_punkter.csv", sep=",")
Data_Torvdybder_Lista1 <- read.csv("C:/Users/martef/DokumenterIntern/Kinn/Kartdata/data_lista1_torvdybder.csv", sep=";")

Torvdybder_Lista2 <- read.csv("C:/Users/martef/DokumenterIntern/Kinn/Kartdata/lista2_punkter.csv", sep=",")
Data_Torvdybder_Lista2 <- read.csv("C:/Users/martef/DokumenterIntern/Kinn/Kartdata/data_lista2_torvdybder.csv", sep=";")


#Import med readOGR gir SpatialPolygonDataFrame ---------
maroy_kartpunkter<-readOGR(dsn="C:/Users/martef/DokumenterIntern/Kinn/Kartdata", layer="maroy_punkter")
shp_maroy<-readOGR(dsn="C:/Users/martef/DokumenterIntern/Kinn/Kartdata", layer="maroy_avgrensing")

rikmyr_kartpunkter<-readOGR(dsn="C:/Users/martef/DokumenterIntern/Kinn/Kartdata", layer="rikmyr_punkter")
shp_rikmyr<-readOGR(dsn="C:/Users/martef/DokumenterIntern/Kinn/Kartdata", layer="rikmyr_avgrensing")

lista1_kartpunkter<-readOGR(dsn="C:/Users/martef/DokumenterIntern/Kinn/Kartdata", layer="lista1_punkter")
shp_lista1<-readOGR(dsn="C:/Users/martef/DokumenterIntern/Kinn/Kartdata", layer="lista1_avgrensing")

lista2_kartpunkter<-readOGR(dsn="C:/Users/martef/DokumenterIntern/Kinn/Kartdata", layer="lista2_punkter")
shp_lista2<-readOGR(dsn="C:/Users/martef/DokumenterIntern/Kinn/Kartdata", layer="lista2_avgrensing")

# Sette sammen data fra torvdybde-listene inn i shapefila fra QGIS
maroy_kartpunkter@data$name <- as.numeric(maroy_kartpunkter@data$name)
maroy_kartpunkter <- merge(maroy_kartpunkter,Data_Torvdybder_Maroy,by="name")

rikmyr_kartpunkter@data$name <- as.numeric(rikmyr_kartpunkter@data$name)
rikmyr_kartpunkter <- merge(rikmyr_kartpunkter,Data_Torvdybder_Rikmyr,by="name")

lista1_kartpunkter@data$name <- as.numeric(lista1_kartpunkter@data$name)
lista1_kartpunkter <- merge(lista1_kartpunkter,Data_Torvdybder_Lista1,by="name")

lista2_kartpunkter@data$name <- as.numeric(lista2_kartpunkter@data$name)
lista2_kartpunkter <- merge(lista2_kartpunkter,Data_Torvdybder_Lista2,by="name")

#Gjøre om dybder til numerisk
maroy_kartpunkter@data$Dybde <- as.numeric(maroy_kartpunkter@data$Dybde)
rikmyr_kartpunkter@data$Dybde <- as.numeric(rikmyr_kartpunkter@data$Dybde)
lista1_kartpunkter@data$Dybde <- as.numeric(lista1_kartpunkter@data$Dybde)
lista2_kartpunkter@data$Dybde <- as.numeric(lista2_kartpunkter@data$Dybde)


#Visualisering-------------------------------------------------

#plot(maroy_kartpunkter) #Dette går ikke

plot(shp_maroy)
points(maroy_kartpunkter, pch=1, col='red', cex=1)
# Dette fungerer

plot(shp_rikmyr)
points(rikmyr_kartpunkter, pch=1, col='red', cex=1)

plot(shp_lista1)
points(lista1_kartpunkter, pch=1, col='red', cex=1)

plot(shp_lista2)
points(lista2_kartpunkter, pch=1, col='red', cex=1)

#ggplot krever data frames, og ikke spatial data frames

#Gjøre om spatial-filene til rene data frames: -----------
df_maroy<-fortify(shp_maroy)
df_maroy_punkter <- data.frame(maroy_kartpunkter)

df_rikmyr<-fortify(shp_rikmyr)
df_rikmyr_punkter <- data.frame(rikmyr_kartpunkter)

df_lista1<-fortify(shp_lista1)
df_lista1_punkter <- data.frame(lista1_kartpunkter)

df_lista2<-fortify(shp_lista2)
df_lista2_punkter <- data.frame(lista2_kartpunkter)

#Visualisere med ggplot: ------------
p_myr_maroy <- ggplot()
p_myr_maroy <- p_myr_maroy + geom_polygon( data=df_maroy, aes(x=long, y=lat, group=group),
                               color="black", fill="lightblue", size = .1 ) +
  geom_point(data=df_maroy_punkter,
             aes(x=coords.x1,
                 y=coords.x2), pch=20, cex=1.2, col="red")
p_myr_maroy


p_myr_rikmyr <- ggplot()
p_myr_rikmyr <- p_myr_rikmyr + geom_polygon( data=df_rikmyr, aes(x=long, y=lat, group=group),
                                     color="black", fill="lightblue", size = .1 ) +
  geom_point(data=df_rikmyr_punkter,
             aes(x=coords.x1,
                 y=coords.x2), pch=20, cex=1.2, col="red")
p_myr_rikmyr

p_myr_lista1 <- ggplot()
p_myr_lista1 <- p_myr_lista1 + geom_polygon( data=df_lista1, aes(x=long, y=lat, group=group),
                                             color="black", fill="lightblue", size = .1 ) +
  geom_point(data=df_lista1_punkter,
             aes(x=coords.x1,
                 y=coords.x2), pch=20, cex=1.2, col="red")
p_myr_lista1

p_myr_lista2 <- ggplot()
p_myr_lista2 <- p_myr_lista2 + geom_polygon( data=df_lista2, aes(x=long, y=lat, group=group),
                                             color="black", fill="lightblue", size = .1 ) +
  geom_point(data=df_lista2_punkter,
             aes(x=coords.x1,
                 y=coords.x2), pch=20, cex=1.2, col="red")

p_myr_lista2

#Ordne opp i koordinatsystem: ---------------------------
#Sjekk av koordinatsystem:
#st_crs(maroy_kartpunkter) #WGS 84
#st_crs(shp_maroy) #WGS 84

#Gjøre om WGS84 til ETRF89-UTM33:
maroy_kartpunkter_UTM33 <- spTransform(maroy_kartpunkter, CRS("+init=epsg:25833")) 
# proj4string(maroy_kartpunkter) <-CRS("+init=epsg:25833") 
#Det gis en advarsel for proj4string om at man projekterer nytt koordinatsystem
#på lag som har koordinatsystem allerede angitt.
#Bør nok bruke spTransform, selv om resultatet ser likt ut.
                                                      
#st_crs(maroy_kartpunkter_UTM33) #ETRS89 / UTM zone 33N
#st_crs(maroy_kartpunkter) #ETRS89 / UTM zone 33N

shp_maroy_UTM33 <- spTransform(shp_maroy, CRS("+init=epsg:25833")) 
#st_crs(shp_maroy_UTM33)

#Tilsvarende for de andre myrene:
#st_crs(rikmyr_kartpunkter) #WGS 84
#st_crs(shp_rikmyr) #WGS 84

rikmyr_kartpunkter_UTM33 <- spTransform(rikmyr_kartpunkter, CRS("+init=epsg:25833")) 
shp_rikmyr_UTM33 <- spTransform(shp_rikmyr, CRS("+init=epsg:25833")) 
#st_crs(rikmyr_kartpunkter_UTM33)

lista1_kartpunkter_UTM33 <- spTransform(lista1_kartpunkter, CRS("+init=epsg:25833")) 
shp_lista1_UTM33 <- spTransform(shp_lista1, CRS("+init=epsg:25833")) 

lista2_kartpunkter_UTM33 <- spTransform(lista2_kartpunkter, CRS("+init=epsg:25833")) 
shp_lista2_UTM33 <- spTransform(shp_lista2, CRS("+init=epsg:25833")) 

rikmyr_kartpunkter_UTM33 <- sp.na.omit(rikmyr_kartpunkter_UTM33, col.name = "Dybde") 
lista1_kartpunkter_UTM33 <- sp.na.omit(lista1_kartpunkter_UTM33, col.name = "Dybde") 

#Gjøre om på dataframes også:-----------
df_maroy<-fortify(shp_maroy_UTM33)
df_maroy_punkter <- data.frame(maroy_kartpunkter_UTM33)

df_rikmyr<-fortify(shp_rikmyr_UTM33)
df_rikmyr_punkter <- data.frame(rikmyr_kartpunkter_UTM33)

df_lista1<-fortify(shp_lista1_UTM33)
df_lista1_punkter <- data.frame(lista1_kartpunkter_UTM33)

df_lista2<-fortify(shp_lista2_UTM33)
df_lista2_punkter <- data.frame(lista2_kartpunkter_UTM33)

#----------------------------------------------

# To potensielle strategier videre: kjøre på med interpolering for hver enkelt myr med et kvadratisk omriss
#fra geopunkter, og deretter klippe til omriss for myra fra polygon. likt som for Tydal
# ELLER legge til null-punkter i omrisset rundt myra for å anta at der er det 0 i torvdybde

#1. lage kvadratisk avgrensning rundt hver myr -------------------

#Avgrensing for Marøy:

#plot(shp_maroy_UTM33)
#Trengs ikke kjøres på nytt ved nye gjennomganger

#use the locator to click 4 points beyond the extent of the plot
#and use those to set your x and y extents
#locator(4)
#Denne trengs ikke kjøres på nytt ved nye gjennomganger

x.range_maroy <- as.numeric(c(-25361.20,-24993.81))
y.range_maroy <- as.numeric(c(6859205,6859495))

## now expand your range to a grid with spacing that you'd like to use in your interpolation
#here we will use 1m grid cells:
grd_maroy <- expand.grid(x=seq(from=x.range_maroy[1], to=x.range_maroy[2], by=1), y=seq(from=y.range_maroy[1], to=y.range_maroy[2], by=1))

## convert grid to SpatialPixel class
coordinates(grd_maroy) <- ~ x+y
gridded(grd_maroy) <- TRUE

## test it out - this is a good way of checking that your sample points are all well within your grid. If they are not, try some different values in you r x and y ranges:
plot(grd_maroy, cex=1.5)
points(maroy_kartpunkter_UTM33, pch=1, col='red', cex=1)

#Dette fungerte! Har nå et kvadratisk grid (på 1x1m?) som dekker alle dybdepunkter med god margin.

#Gjøre samme for de andre myrene: --------------
#Rikmyra:

#plot(shp_rikmyr_UTM33)
#locator(4)
#Disse trengs ikke kjøres på nytt ved nye gjennomganger

x.range_rikmyr <- as.integer(c(-25924.84, -25697.14))
y.range_rikmyr <- as.integer(c(6856659,6856836))

grd_rikmyr <- expand.grid(x=seq(from=x.range_rikmyr[1], to=x.range_rikmyr[2], by=1), y=seq(from=y.range_rikmyr[1], to=y.range_rikmyr[2], by=1))

coordinates(grd_rikmyr) <- ~ x+y
gridded(grd_rikmyr) <- TRUE

plot(grd_rikmyr, cex=1.5)
points(rikmyr_kartpunkter_UTM33, pch=1, col='red', cex=1)

#Lista1:
#plot(shp_lista1_UTM33)
#locator(4)
#Disse trengs ikke kjøres på nytt ved nye gjennomganger

x.range_lista1 <- as.integer(c(2550.419, 2610.549))
y.range_lista1 <- as.integer(c(6862917,6862952))

grd_lista1 <- expand.grid(x=seq(from=x.range_lista1[1], to=x.range_lista1[2], by=1), 
                          y=seq(from=y.range_lista1[1], to=y.range_lista1[2], by=1))

coordinates(grd_lista1) <- ~ x+y
gridded(grd_lista1) <- TRUE

plot(grd_lista1, cex=1.5)
points(lista1_kartpunkter_UTM33, pch=1, col='red', cex=1)


#Lista2:
#plot(shp_lista2_UTM33)
#locator(4)
#Disse trengs ikke kjøres på nytt ved nye gjennomganger

x.range_lista2 <- as.integer(c(2523.545, 2633.281))
y.range_lista2 <- as.integer(c(6863046,6863107))

grd_lista2 <- expand.grid(x=seq(from=x.range_lista2[1], to=x.range_lista2[2], by=1), 
                          y=seq(from=y.range_lista2[1], to=y.range_lista2[2], by=1))

coordinates(grd_lista2) <- ~ x+y
gridded(grd_lista2) <- TRUE

plot(grd_lista2, cex=1.5)
points(lista2_kartpunkter_UTM33, pch=1, col='red', cex=1)

#Sette likt koordinatsystem for grid som for kartlaga -----------------
#st_crs(grd_maroy) #har ikke et koordinatsystem!

#Setter koordinatsystem for grd
proj4string(grd_maroy) <-crs(maroy_kartpunkter_UTM33) #koordinatsystem settes til samme som for det andre datasettet
proj4string(grd_rikmyr) <-crs(rikmyr_kartpunkter_UTM33) #koordinatsystem settes til samme som for det andre datasettet
proj4string(grd_lista1) <-crs(lista1_kartpunkter_UTM33) #koordinatsystem settes til samme som for det andre datasettet
proj4string(grd_lista2) <-crs(lista2_kartpunkter_UTM33) #koordinatsystem settes til samme som for det andre datasettet

# Interpolering av hver myr for seg ----------------------------------------------

# Kriging - Variogram av Marøy, 1.runde -----------------------------------------------------
#Kriging is a little more involved than IDW as it requires the construction of a semivariogram model to describe 
#the spatial autocorrelation pattern for your particular variable. We’ll start with a variogram cloud
variogcloud<-variogram(Dybde~1, locations=maroy_kartpunkter_UTM33, data=maroy_kartpunkter_UTM33, cloud=TRUE)
plot(variogcloud)


#The values in the cloud can be binned into lags with and plotted with a very similar function
semivariog<-variogram(Dybde~1, locations=maroy_kartpunkter_UTM33, data=maroy_kartpunkter_UTM33)
plot(semivariog)

#Disse kjører ok

#From the empirical semivariogram plot and the information contained in the semivariog gstat object, 
#we can estimate the sill, range and nugget to use in our model semivariogram.

#the range (the point on the distance axis where the semivariogram starts to level off)
#The Sill (the point on the y axis where the semivariogram starts to level off)
#The nugget is where the points seems to intercept on the y-axis (at x=0)
#the partial sill is the sill minus the nugget
#Using this information we’ll generate a model semivariogram using the vgm() function in gstat.

#range: 80
#sill: 6000
#nugget: 500
#partial sill: 5500

#first check the range of model shapes available in vgm
vgm()

#the data looks like it might be an exponential shape, so we will try that first with the values estimated from the empirical 
model.variog<-vgm(psill=5500, model="Lin", nugget=500, range=80)

#We can now fit this model to a sample variogram to see how well it fits and plot it

fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

#try some alternative models to see if the fit is any better

model.variog<-vgm(psill=5500, model="Sph", nugget=500, range=80)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

model.variog2<-vgm(psill=5500, model="Bes", nugget=500, range=80)
fit.variog<-fit.variogram(semivariog, model.variog2)
plot(semivariog, fit.variog)

#kun Lin, Sph, Bes som lar seg kjøre. Exp, Nug, Gau, Cir fungerer ikke. Tror jeg går for Sph

#Use the krige() function in gstat along with the model semivariogram just generated to generate an ordinary/simple Kriged surface 
# - again, check ?krige to see what the various options in the function are.
?krige

## [using ordinary kriging]
krig_maroy<-krige(formula=Dybde ~ 1, locations=maroy_kartpunkter_UTM33, newdata=grd_maroy, model=model.variog)

krig.output_maroy=as.data.frame(krig_maroy)
names(krig.output_maroy)[1:3]<-c("long","lat","var1.pred")

#Generate a plot of the kriged surface in ggplot2

plot<-ggplot(data=krig.output_maroy,aes(x=long,y=lat))#start with the base-plot and add the Kriged data to it
layer1<-c(geom_tile(data=krig.output_maroy,aes(fill=var1.pred)))#then create a tile layer and fill with predicted
layer2<-c(geom_polygon( data=shp_maroy_UTM33, aes(x=long, y=lat, group=group),
                        color="black", fill=NA, size = .1, alpha = .5))#then create an outline
plot+layer1+layer2+
  scale_fill_gradient(low="#FEEBE2", high="#7A0177")+coord_equal()

#Prøve ordinary kriging en gang til, men med annen fit "Bes": ---------------
krig_maroy2<-krige(formula=Dybde ~ 1, locations=maroy_kartpunkter_UTM33, newdata=grd_maroy, model=model.variog2)

krig.output_maroy2=as.data.frame(krig_maroy2)
names(krig.output_maroy2)[1:3]<-c("long","lat","var1.pred")

#Generate a plot of the kriged surface in ggplot2

plot<-ggplot(data=krig.output_maroy2,aes(x=long,y=lat))#start with the base-plot and add the Kriged data to it
layer1<-c(geom_tile(data=krig.output_maroy2,aes(fill=var1.pred)))#then create a tile layer and fill with predicted
layer2<-c(geom_polygon( data=shp_maroy_UTM33, aes(x=long, y=lat, group=group),
                        color="black", fill=NA, size = .1, alpha = .5))#then create an outline
plot+layer1+layer2+
  scale_fill_gradient(low="#FEEBE2", high="#7A0177")+coord_equal()


# Eksporte interpolateringene av Marøy kriging ------------------------------------------

raster_OK_maroy<-raster(krig_maroy)
writeRaster(raster_OK_maroy,'OK_Maroy.tif', overwrite=TRUE)
write.csv(krig.output_maroy,"Data/krig.output.Maroy_square.csv", row.names = FALSE)

#Croppe til verdier kun innefor myrpolygonet:
crop_krig_mask_maroy<-raster::mask(raster_OK_maroy,shp_maroy_UTM33)
plot(crop_krig_mask_maroy) 
#lager NA's for punkter utafor maska

#Lag ny csv med kun verdier innenfor myrpolygon:
krig.output_mask_maroy=as.data.frame(crop_krig_mask_maroy)
krig.output_mask_maroy <- na.omit(krig.output_mask_maroy, col.name = "var1.pred") 
write.csv(krig.output_mask_maroy,"Data/krig.output.mask_Maroy.csv", row.names = FALSE)

#Tilsvarende for ordinary kriging runde 2:
raster_OK_maroy2<-raster(krig_maroy2)
writeRaster(raster_OK_maroy2,'OK_Maroy2.tif', overwrite=TRUE)
write.csv(krig.output_maroy2,"Data/krig.output.Maroy2_square.csv", row.names = FALSE)


#Croppe til verdier kun innefor myrpolygonet:
crop_krig_mask_maroy2<-raster::mask(raster_OK_maroy2,shp_maroy_UTM33)
plot(crop_krig_mask_maroy2) #denne gir riktigere cropping. Ser ganske så riktig ut, 
#lager NA's for punkter utafor maska

#Lag ny csv med kun verdier innenfor myrpolygon:
krig.output_mask_maroy2=as.data.frame(crop_krig_mask_maroy2)
krig.output_mask_maroy2 <- na.omit(krig.output_mask_maroy2, col.name = "var1.pred") 
write.csv(krig.output_mask_maroy2,"Data/krig.output.mask_Maroy2.csv", row.names = FALSE)

summary(krig.output_maroy2)
#Min -15.55
#Max 201.82

#Noen enkle beregninger: -------------------
sum(crop_krig_mask_maroy@data@values, na.rm=TRUE)
#3292187
#3258242

mean(crop_krig_mask_maroy@data@values, na.rm=TRUE)
#99.03697
#98.01583

summary(crop_krig_mask_maroy@data@values)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  14.89   66.35   89.48   99.04  130.42  201.82   24298 

#  25.25   67.81   88.06   98.02  126.34  205.99   73846 
#Grovestimat karbon
# carbon content (kg per m2) = 10 (conversion factor from g cm-3 to kg m2) * thickness of peat (cm) * dry bulk density (in g m-3) 
# * fraction organic matter of dry matter (0.98?) x carbon proportion (0.480-0.54)
3292187*10*0.1*0.98*0.5
#1613172 kg = 1613 tonn C på myrarealet

#Grovestimat CO2
1613172*3.67
#5920341 kg = 5920.3 tonn CO2


#Annen interpolering - IDW ------------------------------------

idw_maroy<-idw(formula=Dybde ~ 1, locations=maroy_kartpunkter_UTM33, newdata=grd_maroy)
#Den kjører ok
idwdf_maroy = as.data.frame(idw_maroy)


#Plot av IDW uten punkter, men med stasjon og myrtyper
ggplot()+
  geom_tile(data = idwdf_maroy, aes(x = x, y = y, fill = var1.pred))+
  geom_polygon( data=df_maroy, aes(x=long, y=lat),
                color="black", fill=NA,  size = 1)+
  scale_fill_gradientn(colors = terrain.colors(10))+
  theme_bw()

raster_IDW_maroy<-raster(idw_maroy)
writeRaster(raster_IDW_maroy,'IDW_Maroy.tif', overwrite=TRUE)
write.csv(idwdf_maroy,"Data/IDW.output.Maroy_square.csv", row.names = FALSE)

crop_idw_mask_maroy<-raster::mask(raster_IDW_maroy,shp_maroy_UTM33)
plot(crop_idw_mask_maroy) #denne gir riktigere cropping. Ser ganske så riktig ut, 

IDW.output_mask_maroy=as.data.frame(crop_idw_mask_maroy)
IDW.output_mask_maroy <- na.omit(IDW.output_mask_maroy, col.name = "var1.pred") 
write.csv(IDW.output_mask_maroy,"Data/IDW.output.mask_Maroy.csv", row.names = FALSE)
summary(IDW.output_mask_maroy)
# var1.pred     
#Min.   : 40.28  
#1st Qu.: 79.67  
#Median : 91.94  
#Mean   : 97.31  
#3rd Qu.:116.21  
#Max.   :163.14 

#Denne metoden ser ut til å gi et enda snevrere estimat enn ordinary kriging

#Nytt forsøk med IDW, men annen funksjon:----------
grid_maroy <-raster(grd_maroy)
gs <- gstat(formula=Dybde~1, locations=maroy_kartpunkter_UTM33, nmax=5, set=list(idp = 0))
nn <- interpolate(grid_maroy, gs)
## [inverse distance weighted interpolation]
crop_IDW_maroy <- mask(nn, shp_maroy_UTM33)
plot(crop_IDW_maroy)

summary(crop_IDW_maroy@data@values)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#7.4    67.0    87.6   100.2   131.2   210.4   73846 

#Denne gir mye bedre bredde

IDW.output_crop_maroy=as.data.frame(crop_IDW_maroy)
IDW.output_crop_maroy <- na.omit(IDW.output_crop_maroy, col.name = "var1.pred") 
write.csv(IDW.output_crop_maroy,"Data/IDW.output.crop_Maroy_2.csv", row.names = FALSE)
raster_IDW_crop_maroy<-raster(crop_IDW_maroy)
writeRaster(raster_IDW_crop_maroy,'IDW_crop_Maroy.tif', overwrite=TRUE)
writeRaster(crop_IDW_maroy, 'IDW_crop_Maroy2.tif', overwrite=TRUE)

#Kriging rikmyra -------------
#Lage variogram cloud
variogcloud<-variogram(Dybde~1, locations=rikmyr_kartpunkter_UTM33, data=rikmyr_kartpunkter_UTM33, cloud=TRUE)
plot(variogcloud)

#The values in the cloud can be binned into lags with and plotted with a very similar function
semivariog<-variogram(Dybde~1, locations=rikmyr_kartpunkter_UTM33, data=rikmyr_kartpunkter_UTM33)
plot(semivariog)

#From the empirical semivariogram plot and the information contained in the semivariog gstat object, 
#we can estimate the sill, range and nugget to use in our model semivariogram.

#the range (the point on the distance axis where the semivariogram starts to level off)
#The Sill (the point on the y axis where the semivariogram starts to level off)
#The nugget is where the points seems to intercept on the y-axis (at x=0)
#the partial sill is the sill minus the nugget
#Using this information we’ll generate a model semivariogram using the vgm() function in gstat.

#range: 60
#sill: 20000
#nugget: 2500
#partial sill: 17500

#first check the range of model shapes available in vgm
vgm()

#teste ut ulike modeller 
model.variog<-vgm(psill=17500, model="Sph", nugget=2500, range=60)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)
#Sliter med at de beste modellene likevel sier at det er "no convergence after 200 iterations". Kan jeg overse det?

model.variog2<-vgm(psill=17500, model="Lin", nugget=2500, range=60)
fit.variog2<-fit.variogram(semivariog, model.variog2)
plot(semivariog, fit.variog2)

#Denne gir advarsel om "Singular model in variogram fit"
help(fit.variogram)

model.variog3<-vgm(psill=17500, model="Sph", range=60)
fit.variog3<-fit.variogram(semivariog, model.variog3)
plot(semivariog, fit.variog3)
#Denne kjører uten advarsler, og ser tålelig bra ut

model.variog4<-vgm(psill=17500, model="Lin", range=60)
fit.variog4<-fit.variogram(semivariog, model.variog4)
plot(semivariog, fit.variog4)
#Denne siste er helt uaktuell

#Antar nr 3 er beste fit

## [using ordinary kriging]
krig_rikmyr<-krige(formula=Dybde ~ 1, locations=rikmyr_kartpunkter_UTM33, newdata=grd_rikmyr, model=model.variog3)

krig.output_rikmyr=as.data.frame(krig_rikmyr)
names(krig.output_rikmyr)[1:3]<-c("long","lat","var1.pred")

#Generate a plot of the kriged surface in ggplot2

plot<-ggplot(data=krig.output_rikmyr,aes(x=long,y=lat))#start with the base-plot and add the Kriged data to it
layer1<-c(geom_tile(data=krig.output_rikmyr,aes(fill=var1.pred)))#then create a tile layer and fill with predicted
layer2<-c(geom_polygon( data=shp_rikmyr_UTM33, aes(x=long, y=lat),
                        color="black", fill=NA, size = .1, alpha = .5))#then create an outline
plot+layer1+layer2+
  scale_fill_gradient(low="#FEEBE2", high="#7A0177")+coord_equal()

#Eksportere data rikmyr ------------
raster_OK_rikmyr<-raster(krig_rikmyr)
writeRaster(raster_OK_rikmyr,'OK_rikmyr.tif', overwrite=TRUE)
write.csv(krig.output_rikmyr,"Data/krig.output.rikmyr_square.csv", row.names = FALSE)

#Croppe til verdier kun innefor myrpolygonet:
crop_krig_mask_rikmyr<-raster::mask(raster_OK_rikmyr,shp_rikmyr_UTM33)
plot(crop_krig_mask_rikmyr) 
#lager NA's for punkter utafor maska

#Lag ny csv med kun verdier innenfor myrpolygon:
krig.output_mask_rikmyr=as.data.frame(crop_krig_mask_rikmyr)
krig.output_mask_rikmyr <- na.omit(krig.output_mask_rikmyr, col.name = "var1.pred") 
write.csv(krig.output_mask_rikmyr,"Data/krig.output.mask_rikmyr.csv", row.names = FALSE)

summary(krig.output_mask_rikmyr)
summary(krig.output_rikmyr)
#Min 112.2
#Mean 203
#Max 299.3

#Ikke fornøyd med denne, da den sterkt undervurderer dybden på myra!
#Jeg har jo mange punkter med mer enn 3 m dybde opp mot 4,5 m, og disse er borte...


#Test IDW rikmyr-------------
idw_rikmyr<-idw(formula=Dybde ~ 1, locations=rikmyr_kartpunkter_UTM33, newdata=grd_rikmyr)
idwdf_rikmyr = as.data.frame(idw_rikmyr)
summary(idwdf_rikmyr)
#Min 175
#Mean 248
#Max 306

#Denne gir bare noen få cm dypere maksimum, men mye høyere minimum

#Plot av IDW uten punkter, men myromriss

ggplot()+
  geom_tile(data = idwdf_rikmyr, aes(x = x, y = y, fill = var1.pred))+
  geom_polygon( data=df_rikmyr, aes(x=long, y=lat),
                color="black", fill=NA,  size = 1)+
  scale_fill_gradientn(colors = terrain.colors(10))+
  theme_bw()

#tm_shape(idw_rikmyr) +
 # tm_raster("var1.pred")

raster_IDW_maroy<-raster(idw_maroy)
writeRaster(raster_IDW_maroy,'IDW_Maroy.tif', overwrite=TRUE)
write.csv(idwdf_maroy,"Data/IDW.output.Maroy_square.csv", row.names = FALSE)

crop_idw_mask_maroy<-raster::mask(raster_IDW_maroy,shp_maroy_UTM33)
plot(crop_idw_mask_maroy) #denne gir riktigere cropping. Ser ganske så riktig ut, 

IDW.output_mask_maroy=as.data.frame(crop_idw_mask_maroy)
IDW.output_mask_maroy <- na.omit(IDW.output_mask_maroy, col.name = "var1.pred") 
write.csv(IDW.output_mask_maroy,"Data/IDW.output.mask_Maroy.csv", row.names = FALSE)
summary(IDW.output_mask_maroy)
# var1.pred     
#Min.   : 40.28  
#1st Qu.: 79.67  
#Median : 91.94  
#Mean   : 97.31  
#3rd Qu.:116.21  
#Max.   :163.14 

#Denne metoden ser ut til å gi et enda snevrere estimat enn ordinary kriging

#Nytt forsøk med IDW, men annen funksjon:----------
grid_rikmyr <-raster(grd_rikmyr)
gs <- gstat(formula=Dybde~1, locations=rikmyr_kartpunkter_UTM33, nmax=5, set=list(idp = 0))
nn <- interpolate(grid_rikmyr, gs)
## [inverse distance weighted interpolation]
crop_IDW_rikmyr <- mask(nn, shp_rikmyr_UTM33)
plot(crop_IDW_rikmyr)

summary(crop_IDW_rikmyr@data@values)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 66.0   170.8   251.4   248.7   312.6   421.4   27044  

#Denne gir mye bedre bredde

IDW.output_crop_rikmyr=as.data.frame(crop_IDW_rikmyr)
IDW.output_crop_rikmyr <- na.omit(IDW.output_crop_rikmyr, col.name = "var1.pred") 
write.csv(IDW.output_crop_rikmyr,"Data/IDW.output.crop_rikmyr.csv", row.names = FALSE)
raster_IDW_crop_rikmyr<-raster(crop_IDW_rikmyr)
writeRaster(raster_IDW_crop_rikmyr,'IDW_crop_rikmyr.tif', overwrite=TRUE)

#IDW for Lista1, med siste funksjon:----------
grid_lista1 <-raster(grd_lista1)
gs <- gstat(formula=Dybde~1, locations=lista1_kartpunkter_UTM33, nmax=5, set=list(idp = 0))
nn <- interpolate(grid_lista1, gs)
## [inverse distance weighted interpolation]
crop_IDW_lista1 <- mask(nn, shp_lista1_UTM33)
plot(crop_IDW_lista1)

summary(crop_IDW_lista1@data@values)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 56.00   88.80   95.40   91.46   98.20  101.20    1435   
# 66.0   124.7   124.7   119.3   135.7   135.7    1435

#Denne er jeg litt usikker på. Den er mer snever igjen.

#idw2_lista1<-idw(formula=Dybde ~ 1, locations=lista1_kartpunkter_UTM33, newdata=grd_lista1)
#idwdf_lista1 = as.data.frame(idw2_lista1)
#plot(idw2_lista1)
#summary(idwdf_lista1)

#Denne gikk ikke i det hele tatt! Bare tull!

#Eksporterer den som har nmax=5
IDW.output_crop_lista1=as.data.frame(crop_IDW_lista1)
IDW.output_crop_lista1 <- na.omit(IDW.output_crop_lista1, col.name = "var1.pred") 
write.csv(IDW.output_crop_lista1,"Data/IDW.output.crop_lista1.csv", row.names = FALSE)
raster_IDW_crop_lista1<-raster(crop_IDW_lista1)
writeRaster(raster_IDW_crop_lista1,'IDW_crop_lista1.tif', overwrite=TRUE)

grid_lista1 <-raster(grd_lista1)
gs <- gstat(formula=Dybde~1, locations=lista1_kartpunkter_UTM33, nmax=3, set=list(idp = 0))
nn <- interpolate(grid_lista1, gs)
## [inverse distance weighted interpolation]
crop_IDW_lista1_1 <- mask(nn, shp_lista1_UTM33)
plot(crop_IDW_lista1_1)

#Eksporterer den som har nmax=3
IDW.output_crop_lista1_1=as.data.frame(crop_IDW_lista1)
IDW.output_crop_lista1_1 <- na.omit(IDW.output_crop_lista1_1, col.name = "var1.pred") 
write.csv(IDW.output_crop_lista1_1,"Data/IDW.output.crop_lista1_1.csv", row.names = FALSE)
raster_IDW_crop_lista1_1<-raster(crop_IDW_lista1)
writeRaster(raster_IDW_crop_lista1_1,'IDW_crop_lista1_1.tif', overwrite=TRUE)

#IDW for Lista2, med siste funksjon:----------
grid_lista2 <-raster(grd_lista2)
gs <- gstat(formula=Dybde~1, locations=lista2_kartpunkter_UTM33, nmax=3, set=list(idp = 0))
nn <- interpolate(grid_lista2, gs)
## [inverse distance weighted interpolation]
crop_IDW_lista2 <- mask(nn, shp_lista2_UTM33)
plot(crop_IDW_lista2)

summary(crop_IDW_lista2@data@values)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 51.00   52.20   52.20   58.19   57.40   83.60    5719  

# 40.00   40.00   60.00   58.46   66.33   95.67    5719 

#Denne er jeg også litt usikker på. Den virker en del snever.
#Blei bedre ved å justere ned antall "nærmeste naboer" som skulle veies inn


IDW.output_crop_lista2_2=as.data.frame(crop_IDW_lista2)
IDW.output_crop_lista2_2 <- na.omit(IDW.output_crop_lista2_2, col.name = "var1.pred") 
write.csv(IDW.output_crop_lista2_2,"Data/IDW.output.crop_lista2_2.csv", row.names = FALSE)
raster_IDW_crop_lista2_2<-raster(crop_IDW_lista2)
writeRaster(raster_IDW_crop_lista2_2,'IDW_crop_lista2_2.tif', overwrite=TRUE)


#Noen enkle beregninger for alle myrene: -------------------
#Endelige flater å regne fra:

#IDW.output_crop_maroy
#IDW.output_crop_rikmyr
#IDW.output_crop_lista1
#IDW.output_crop_lista1_1
#IDW.output_crop_lista2

#Summeringer Marøy-------------
#Summer volum
sum(IDW.output_crop_maroy)
#3330131

summary(IDW.output_crop_maroy)
#var1.pred    
#Min.   :  7.4  
#1st Qu.: 67.0  
#Median : 87.6  
#Mean   :100.2  
#3rd Qu.:131.2  
#Max.   :210.4  



#Grovestimat karbon
# carbon content (kg per m2) = 10 (conversion factor from g cm-3 to kg m2) * thickness of peat (cm) * dry bulk density (in g m-3) 
# * fraction organic matter of dry matter (0.98?) x carbon proportion (0.480-0.54)
3330131*10*0.1*0.96*0.5
#10* kg = 1598 tonn C på myrarealet

#Grovestimat CO2
1598463*3.67
#5866359 kg = 5866 tonn CO2


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








# Summeringer rikmyr ------------
sum(IDW.output_crop_rikmyr)
#3367004

summary(IDW.output_crop_rikmyr)
#var1.pred    
#Min.   : 66.0  
#1st Qu.:170.8  
#Median :251.4  
#Mean   :248.7  
#3rd Qu.:312.6  
#Max.   :421.4  

IDW.output_crop_rikmyr$Dybde_gruppe <- cut(IDW.output_crop_rikmyr$var1.pred,c(0,10,60,110,160,210,Inf))
head(IDW.output_crop_rikmyr)


IDW.output_crop_rikmyr %>%
  group_by(Dybde_gruppe) %>%
  summarize(volum= sum(var1.pred, na.rm = TRUE), n=n())

#Dybde_gruppe    volum     n
#<fct>           <dbl> <int>
# 1 (60,110]       75426.   768
# 2 (110,160]     351175.  2416
# 3 (160,210]     210143.  1094
# 4 (210,Inf]    2730260.  9262


768+2416+1094+9262
#13540 m2
#13461 m2, ifl QGIS-polygon

#Grovestimat karbon
# carbon content (kg per m2) = 10 (conversion factor from g cm-3 to kg m2) * thickness of peat (cm) * dry bulk density (in g m-3) 
# * fraction organic matter of dry matter (0.98?) x carbon proportion (0.480-0.54)
3367004*10*0.1*0.96*0.5
#1616162 kg = 1616 tonn C på myrarealet


#Summeringer Lista 1 -----------
sum(IDW.output_crop_lista1)
#69603.8

summary(IDW.output_crop_lista1)
#var1.pred     
#Min.   : 56.00  
#1st Qu.: 88.80  
#Median : 95.40  
#Mean   : 91.46  
#3rd Qu.: 98.20  
#Max.   :101.20  


IDW.output_crop_lista1$Dybde_gruppe <- cut(IDW.output_crop_lista1$var1.pred,c(0,10,60,Inf))
head(IDW.output_crop_lista1)


IDW.output_crop_lista1 %>%
  group_by(Dybde_gruppe) %>%
  summarize(volum= sum(var1.pred, na.rm = TRUE), n=n())


#Dybde_gruppe  volum     n
#<fct>         <dbl> <int>
#1 (10,60]       2408     43
#2 (60,Inf]     67196.   718

43+718
#761 m2
#Ifl QGIS 759 m2

#Grovestimat karbon
# carbon content (kg per m2) = 10 (conversion factor from g cm-3 to kg m2) * thickness of peat (cm) * dry bulk density (in g m-3) 
# * fraction organic matter of dry matter (0.98?) x carbon proportion (0.480-0.54)
69603.8*10*0.1*0.96*0.5
#33409.82 kg = 33,4 tonn C på myrarealet


sum(IDW.output_crop_lista1_1)
#90813

summary(IDW.output_crop_lista1_1)
#var1.pred    
#Min.   : 66.0  
#1st Qu.:124.7  
#Median :124.7  
#Mean   :119.3  
#3rd Qu.:135.7  
#Max.   :135.7 

IDW.output_crop_lista1_1$Dybde_gruppe <- cut(IDW.output_crop_lista1_1$var1.pred,c(0,10,60,Inf))
head(IDW.output_crop_lista1_1)


IDW.output_crop_lista1_1 %>%
  group_by(Dybde_gruppe) %>%
  summarize(volum= sum(var1.pred, na.rm = TRUE), n=n())

#Dybde_gruppe volum     n
#<fct>        <dbl> <int>
#1 (60,Inf]     90813   761

 

#Summeringer Lista 2----------
sum(IDW.output_crop_lista2)
#67987.33
summary(IDW.output_crop_lista2)
#var1.pred    
#Min.   :40.00  
#1st Qu.:40.00  
#Median :60.00  
#Mean   :58.46  
#3rd Qu.:66.33  
#Max.   :95.67 

IDW.output_crop_lista2$Dybde_gruppe <- cut(IDW.output_crop_lista2$var1.pred,c(0,10,60,Inf))
head(IDW.output_crop_lista2)


IDW.output_crop_lista2 %>%
  group_by(Dybde_gruppe) %>%
  summarize(volum= sum(var1.pred, na.rm = TRUE), n=n())

#Dybde_gruppe  volum     n
#<fct>         <dbl> <int>
#1 (10,60]      28862    611
#2 (60,Inf]     39125.   552

611+552
#1163 m2
#Ifl QGIS 1157 m2

#Grovestimat karbon
# carbon content (kg per m2) = 10 (conversion factor from g cm-3 to kg m2) * thickness of peat (cm) * dry bulk density (in g m-3) 
# * fraction organic matter of dry matter (0.98?) x carbon proportion (0.480-0.54)
67987.33*10*0.1*0.96*0.5
#32633.92 kg = 32,6 tonn C på myrarealet

sum(IDW.output_crop_lista2_2)
#67677.8

summary(IDW.output_crop_lista2_2)
#var1.pred    
#Min.   :51.00  
#1st Qu.:52.20  
#Median :52.20  
#Mean   :58.19  
#3rd Qu.:57.40  
#Max.   :83.60  

IDW.output_crop_lista2_2$Dybde_gruppe <- cut(IDW.output_crop_lista2_2$var1.pred,c(0,10,60,Inf))
head(IDW.output_crop_lista2_2)


IDW.output_crop_lista2_2 %>%
  group_by(Dybde_gruppe) %>%
  summarize(volum= sum(var1.pred, na.rm = TRUE), n=n())

#Dybde_gruppe  volum     n
#<fct>         <dbl> <int>
#1 (10,60]      47721.   899
#2 (60,Inf]     19957.   264
 