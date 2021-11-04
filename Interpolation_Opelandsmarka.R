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

# Importer data -------------------------------------------------------------

Torvdybder_Opelandsmarka_myr1 <- read.csv("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/Torvdybder_Opelandsmarka_myr1.csv", sep=";")
View(Torvdybder_Opelandsmarka_myr1)
Torvdybder_Opelandsmarka_myr2 <- read.csv("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/Torvdybder_Opelandsmarka_myr2.csv", sep=";")
View(Torvdybder_Opelandsmarka_myr2)
Torvdybder_Opelandsmarka_myr3 <- read.csv("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/Torvdybder_Opelandsmarka_myr3.csv", sep=";")
View(Torvdybder_Opelandsmarka_myr3)



#Import med readOGR gir SpatialPolygonDataFrame
shp_ommyr<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="Opelandsmarka Voss")
poly_myr1 <- readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="poly_myr1")
poly_myr2 <- readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="poly_myr2")
vossmyr_1<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="vossmyr_1")
shp_myr1 <- st_read("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/poly_myr1.shp")
vossmyr_2<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="vossmyr_2")
shp_myr2 <- st_read("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/poly_myr2.shp")
vossmyr_3<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="vossmyr_3")
omriss_myr <- readOGR(dsn = "C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="punktomriss_myr" )
punktomriss_myr1 <- readOGR(dsn = "C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="punktomriss_myr1" )
punktomriss_myr2 <- readOGR(dsn = "C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="punktomriss_myr2" )

#Gjøre om dybder til numerisk og fjerne NA
vossmyr_1@data$Dybde <- as.numeric(vossmyr_1@data$Dybde)
vossmyr_1_2 <- sp.na.omit(vossmyr_1, col.name = "Dybde") 

#Usikker om jeg trenger disse import-metodene og fortify ------------------------------
#Import med st_read gir vanlig dataframe 
#shp_ommyr2<-st_read("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/Opelandsmarka Voss.shp")
#shp_omriss_myr <- st_read("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/punktomriss_myr.shp")

#st_crs(shp_ommyr)

#df<-fortify(shp_ommyr)
#df2<-fortify(shp_omriss_myr)
#df_myr1 <-fortify(poly_myr1) 
#df_myr2 <-fortify(poly_myr2)

# Visualisering -------------------------------------------

plot(shp_ommyr)
plot(omriss_myr)
plot(vossmyr_1)
plot(vossmyr_2)
plot(vossmyr_3)
plot(poly_myr1)
plot(poly_myr2)

p_myr <- ggplot()
p_myr <- p_myr + geom_polygon( data=df, aes(x=long, y=lat, group=group),
                               color="black", fill="lightblue", size = .1 ) +
  geom_point(vossmyr_1, pch=20, cex=1.2, col="red") +
  geom_point(vossmyr_2, pch=20, cex=1.2, col="blue") +
  geom_point(vossmyr_3, pch=20, cex=1.2, col="green")
p_myr

# Forsøk på boundary layer fra myromriss ----------------------------
df_myr1_b<- as.matrix(df_myr1)
hull_myr1 <- chull(df_myr1_b)
coords_myr1_hull <- df_myr1_b[c(hull_myr1, hull_myr1[1]), ]  # closed polygon
plot(df_myr1_b, pch=19)
lines(coords_myr1_hull, col="red")

# To potensielle strategier videre: kjøre på med interpolering for hver enkelt myr med et kvadratisk omriss
#fra geopunkter, og deretter klippe til omriss for myra fra polygon. likt som for Tydal
# ELLER legge til null-punkter i omrisset rundt myra for å anta at der er det 0 i torvdybde

#1. lage kvadratisk avgrensning rundt hver myr og interpolere etter disse -------------------

#Avgrensing for Myr 1
plot(poly_myr1)

#use the locator to click 4 points beyond the extent of the plot
#and use those to set your x and y extents
locator(4)

x.range_myr1 <- as.integer(c(366151.6,366348.0))
y.range_myr1 <- as.integer(c(6721786,6721997))

## now expand your range to a grid with spacing that you'd like to use in your interpolation
#here we will use 1m grid cells:
grd_myr1 <- expand.grid(x=seq(from=x.range_myr1[1], to=x.range_myr1[2], by=1), y=seq(from=y.range_myr1[1], to=y.range_myr1[2], by=1))

## convert grid to SpatialPixel class
coordinates(grd_myr1) <- ~ x+y
gridded(grd_myr1) <- TRUE

#Gjøre samme for myr2
plot(poly_myr2)

#use the locator to click 4 points beyond the extent of the plot
#and use those to set your x and y extents
locator(4)

x.range_myr2 <- as.integer(c(366354.3,366413.7))
y.range_myr2 <- as.integer(c(6721708,6721795))

## now expand your range to a grid with spacing that you'd like to use in your interpolation
#here we will use 1m grid cells:
grd_myr2 <- expand.grid(x=seq(from=x.range_myr2[1], to=x.range_myr2[2], by=1), y=seq(from=y.range_myr2[1], to=y.range_myr2[2], by=1))

## convert grid to SpatialPixel class
coordinates(grd_myr2) <- ~ x+y
gridded(grd_myr2) <- TRUE

## test it out - this is a good way of checking that your sample points are all well within your grid. If they are not, try some different values in you r x and y ranges:
plot(grd_myr1, cex=1.5)
points(vossmyr_1, pch=1, col='red', cex=1)
title("Interpolation Grid and Sample Points")

plot(grd_myr2, cex=1.5)
points(vossmyr_2, pch=1, col='red', cex=1)

#Dette fungerte! Har nå et kvadratisk grid på 1x1m som dekker alle dybdepunkter med god margin.


# Interpolering av hver myr for seg ----------------------------------------------

# Kriging - Variogram -----------------------------------------------------
#Kriging is a little more involved than IDW as it requires the construction of a semivariogram model to describe 
#the spatial autocorrelation pattern for your particular variable. We’ll start with a variogram cloud
variogcloud<-variogram(Dybde~1, locations=vossmyr_1_2, data=vossmyr_1_2, cloud=TRUE)
plot(variogcloud)


#The values in the cloud can be binned into lags with and plotted with a very similar function
semivariog<-variogram(Dybde~1, locations=vossmyr_1_2, data=vossmyr_1_2)
plot(semivariog)

#Disse kjører nå ok

#From the empirical semivariogram plot and the information contained in the semivariog gstat object, 
#we can estimate the sill, range and nugget to use in our model semivariogram.

#the range (the point on the distance axis where the semivariogram starts to level off) is around 60
#The Sill (the point on the y axis where the semivariogram starts to level off) is around 4300
#The nugget is where the points seems to intercept on the y-axis (at x=0) looks to be around 1800 
#so the partial sill (the sill minus the nugget) is around 2500
#Using this information we’ll generate a model semivariogram using the vgm() function in gstat.

#first check the range of model shapes available in vgm
vgm()

#the data looks like it might be an exponential shape, so we will try that first with the values estimated from the empirical 
model.variog<-vgm(psill=2500, model="Lin", nugget=1800, range=70)

#We can now fit this model to a sample variogram to see how well it fits and plot it

fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

#try some alternative models to see if the fit is any better

model.variog<-vgm(psill=2500, model="Mat", nugget=1800, range=60)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)


#kun linear og mat som lar seg kjøre, Mat ser noe bedre ut, men sliter med convergence, mens lin sliter med singular model

#Use the krige() function in gstat along with the model semivariogram just generated to generate an ordinary/simple Kriged surface 
# - again, check ?krige to see what the various options in the function are.
#?krige

## [using ordinary kriging]
krig<-krige(formula=Dybde ~ 1, locations=vossmyr_1_2, newdata=grd_myr1, model=model.variog)
#error, grid og data ikke samme koordinatsystem

st_crs(vossmyr_1_2) #"WGS 84 / UTM zone 32N",  ID["EPSG",32632]]
st_crs(grd_myr1) #har ikke et koordinatsystem!

#Setter koordinatsystem for grd
proj4string(grd_myr1) <-crs(vossmyr_1_2) #koordinatsystem settes til samme som for det andre datasettet

#Prøver kriging igjen
krig<-krige(formula=Dybde ~ 1, locations=vossmyr_1_2, newdata=grd_myr1, model=model.variog)

krig.output=as.data.frame(krig)
names(krig.output)[1:3]<-c("long","lat","var1.pred")

#Generate a plot of the kriged surface in ggplot2

plot<-ggplot(data=krig.output,aes(x=long,y=lat))#start with the base-plot and add the Kriged data to it
layer1<-c(geom_tile(data=krig.output,aes(fill=var1.pred)))#then create a tile layer and fill with predicted
layer2<-c(geom_polygon( data=poly_myr1, aes(x=long, y=lat, group=group),
                        color="black", fill=NA, size = .1, alpha = .5))#then create an outline
plot+layer1+layer2+
  scale_fill_gradient(low="#FEEBE2", high="#7A0177")+coord_equal()


# Export interpolations for QGIS ------------------------------------------

raster_OK_Opm1<-raster(krig)
writeRaster(raster_OK_Opm1,'OK_Opm1.tif', overwrite=TRUE)
write.csv(krig.output,"Data/krig.output.Opm_1.csv", row.names = FALSE)

# Crop the grid to the shape of the station -------------------------------
#cropped_krig<-st_crop(raster_OK$grid, shp_myr1$geometry)
#denne gir bare error, får ikke til å få til valid layer names uavhengig om jeg bruker shp-fila eller poly-fila

crop_krig<-raster::crop(raster_OK_Opm1,poly_myr1)
plot(crop_krig) #cropper kun til extent
crop_krig_mask<-raster::mask(crop_krig,poly_myr1)
plot(crop_krig_mask) #denne gir riktigere cropping. Ser ganske så riktig ut, 
#men lager NA's for punkter utafor maska
sum(crop_krig_mask@data@values, na.rm=TRUE)
#757025.7

mean(crop_krig_mask@data@values, na.rm=TRUE)
#114.753
summary(crop_krig_mask@data@values)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#49.22   95.20  118.06  114.75  139.61  230.00   26331

#Grovestimat karbon
77697.63*0.1*0.5*1000
#3884882 kg = 3884.882 tonn karbon

#Grovestimat CO2
3884882*3.67
#14257517 kg = 14257.517 tonn CO2

#Gjenta kriging og videre med myr 2 ------------------------------------
variogcloud<-variogram(Dybde~1, locations=vossmyr_2, data=vossmyr_2, cloud=TRUE)
plot(variogcloud)

semivariog<-variogram(Dybde~1, locations=vossmyr_2, data=vossmyr_2)
plot(semivariog)

vgm()
model.variog<-vgm(psill=1500, model="Lin", nugget=250, range=30)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

model.variog<-vgm(psill=1500, model="Mat", nugget=250, range=30)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

model.variog<-vgm(psill=1500, model="Exp", nugget=250, range=30)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

model.variog<-vgm(psill=1500, model="Gau", nugget=250, range=30)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

model.variog<-vgm(psill=1500, model="Bes", nugget=250, range=30)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

#Bes ser ut til å være beste fit, går for den

st_crs(vossmyr_2) #"WGS 84 / UTM zone 32N",  ID["EPSG",32632]]
st_crs(grd_myr2) #har ikke et koordinatsystem!

#Setter koordinatsystem for grd
proj4string(grd_myr2) <-crs(vossmyr_2) #koordinatsystem settes til samme som for det andre datasettet

#Prøver kriging igjen
krig<-krige(formula=Dybde ~ 1, locations=vossmyr_2, newdata=grd_myr2, model=model.variog)

krig.output=as.data.frame(krig)
names(krig.output)[1:3]<-c("long","lat","var1.pred")

plot<-ggplot(data=krig.output,aes(x=long,y=lat))#start with the base-plot and add the Kriged data to it
layer1<-c(geom_tile(data=krig.output,aes(fill=var1.pred)))#then create a tile layer and fill with predicted
layer2<-c(geom_polygon( data=poly_myr2, aes(x=long, y=lat, group=group),
                        color="black", fill=NA, size = .1, alpha = .5))#then create an outline
plot+layer1+layer2+
  scale_fill_gradient(low="#FEEBE2", high="#7A0177")+coord_equal()

raster_OK_Opm2<-raster(krig)
writeRaster(raster_OK_Opm2,'OK_Opm2.tif', overwrite=TRUE)
write.csv(krig.output,"Data/krig.output.Opm_2.csv", row.names = FALSE)

crop_krig<-raster::crop(raster_OK_Opm2,poly_myr2)
crop_krig_mask<-raster::mask(crop_krig,poly_myr2)
plot(crop_krig_mask) #denne gir riktigere cropping. Ser ganske så riktig ut, 
#men lager NA's for punkter utafor maska
sum(crop_krig_mask@data@values, na.rm=TRUE)
#150862.8
mean(crop_krig_mask@data@values, na.rm=TRUE)
#71.56677
summary(crop_krig_mask@data@values)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#30.00   55.82   67.80   71.57   85.53  130.00    1461 