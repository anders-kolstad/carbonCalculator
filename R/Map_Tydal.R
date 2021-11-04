
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


install.packages("maptools")
library(Rtools)




# Import data -------------------------------------------------------------


Feltdata_Tydal_rawdata <- read_excel("Data/Feltdata_Tydal_rawdata.xlsx", 
                                            col_types = c("text", "numeric", "numeric", 
                                                                    "numeric", "text", "text", "numeric", 
                                                                    "numeric", "text", "text", "text", 
                                                                    "text", "text", "text", "text", "text"))
#Import med readOGR gir SpatialPolygonDataFrame
shp_stasjon<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="stasjon_Setermyra")
shp_myrtyper<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="naturtyper_Setermyra")
sp_data<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="dybdepunkter_Tydal2")



#Import med st_read gir vanlig dataframe
shp_myrtyper_2<-st_read("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/naturtyper_Setermyra.shp")
shp_stasjon_2<-st_read("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/stasjon_Setermyra.shp")
sp_data_2<-st_read("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/dybdepunkter_Tydal2.shp")
#Disse inneholder all informasjonen gitt i tabellen i shapefilene, men har ikke long-lat oppgitt i tabellform
#Long-lat er tydeligvis integrert på en eller annen måte, for de lar seg plotte riktig med plot(),
#men de lar seg ikke bruke direkte i ggplot()

#new_filename <- st_read("folder/newfilename.shp")
#%>% st_transform(st_crs(filename)) Kan tilføyes ved import for å få samme CRS-system for alle filene 

# Inspection of data ------------------------------------------------------
st_crs(shp_stasjon)
plot(shp_stasjon)

st_crs(shp_myrtyper)
plot(shp_myrtyper)

st_crs(shp_stasjon)==st_crs(shp_myrtyper)

df<-fortify(shp_stasjon)
df2<-fortify(shp_myrtyper)
#combine.df<-bind_rows(df, df2)

#Rydde opp i dobbel navngivning (grupper fra ulike shapefiler heter begge 0.1- gjøre om en gruppe)
#levels(combine.df$group) <- c(levels(combine.df$group),"15.1") #må legge til nytt faktor-nivå først
#combine.df$group[1:7]<-"15.1" #gjøre om navn på første gruppe

#ggplot()+
#  geom_polygon(data=combine.df, aes(x=long, y=lat, group=group, fill=NULL))
  
p_myr <- ggplot()
p_myr <- p_myr + geom_polygon( data=df2, aes(x=long, y=lat, group=group),
              color="black", fill="lightblue", size = .1 )
p_myr

p_station <- ggplot()
p_station <- p_station +  geom_polygon( data=df, aes(x=long, y=lat, group=group),
                          color="black", fill="lightyellow",  size = 1, alpha = .3)
p_station

#p_station_myr <- p_myr + p_station #funker ikke å kombinere de direkte

p_station_myr <- ggplot() +
  geom_polygon( data=df2, aes(x=long, y=lat, group=group),
                               color="black", fill="lightblue", size = .1 ) +
  
  geom_polygon( data=df, aes(x=long, y=lat, group=group),
                color="black", fill="lightyellow",  size = 1, alpha = .5)
p_station_myr #Dette funker!


# Work with the point data towards interpolation --------------------------

#st_crs(sp_data)
#plot(sp_data)

#df3<-fortify(sp_data) #dette funker ikke. Hvorfor ikke? Hva er det med datatypen som gjør det problematisk?

df3<- as.data.frame(sp_data)
P_station_myr_punkt <- p_station_myr + 
  geom_point(data=df3, aes(x=X, y=Y))
P_station_myr_punkt

#Test av det å lage "hull" rundt alle dybdepunkter
#myPoly <- as.matrix(df3[,c("X", "Y")])
#hull <- chull(myPoly)
#coords <- myPoly[c(hull, hull[1]), ]  # closed polygon
#plot(myPoly, pch=19)
#lines(coords, col="red")
# spsample for å lage raster
# https://gis.stackexchange.com/questions/43801/how-to-create-cohesive-spatial-pixels-from-spatial-points-dataset


# Create boundary layer from peatland boundary ----------------------------
#df2_m<- as.matrix(df2)
#hull_myr <- chull(df2_m)
#coords_myr_hull <- df2_m[c(hull_myr, hull_myr[1]), ]  # closed polygon
#plot(df2_m, pch=19)
#lines(coords_myr_hull, col="red")

#Nope, dette blir ikke bra nok. Blir mer lik en større grov sirkel, og følger ikke linjene til myra.


# Create boundary layer from points ---------------------------------------
#plot(sp_data)

#use the locator to click 4 points beyond the extent of the plot
#and use those to set your x and y extents
#locator(4)

x.range <- as.integer(c(332752.0,333080.4))
y.range <- as.integer(c(6993248,6993631))

## now expand your range to a grid with spacing that you'd like to use in your interpolation
#here we will use 1m grid cells:
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=1), y=seq(from=y.range[1], to=y.range[2], by=1))

## convert grid to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

## test it out - this is a good way of checking that your sample points are all well within your grid. If they are not, try some different values in you r x and y ranges:
plot(grd, cex=1.5)
points(sp_data, pch=1, col='red', cex=1)
title("Interpolation Grid and Sample Points")

#Dette fungerte! Har nå et kvadratisk grid på 1x1m som dekker alle dybdepunkter med god margin.


# Test interpolation IDW --------------------------------------------------

#Sjekk av koordinatsystem for de to aktuelle datasettene
#st_crs(sp_data) #ETRS89 / UTM zone 33N, BBOX[46.4,12,84.01,18.01]],  ID["EPSG",25833]]
#st_crs(grd) #har ikke et koordinatsystem!

#Setter koordinatsystem for grd
#proj4string(grd) <-CRS("+init=epsg:25833")
proj4string(grd) <-crs(sp_data) #Heller kjøre denne, slik at koordinatsystem settes til samme som for det andre datasettet

#Da skal det ikke trengså gjøre dette:
#De har likevel ikke identiske koordinatsystemer etterpå
#Gjør om koordinatsystemet for sp_data også
#proj4string(sp_data) <-CRS("+init=epsg:25833")


#Kjører en test med IDW
idw<-idw(formula=Peat_depth ~ 1, locations=sp_data, newdata=grd)
#Den kjører nå!
idwdf = as.data.frame(idw)

#Plot av IDW med punkter
ggplot()+
  geom_tile(data = idwdf, aes(x = x, y = y, fill = var1.pred))+
  geom_point(data = df3, aes(x = X, y = Y),
             shape = 4)+
  scale_fill_gradientn(colors = terrain.colors(10))+
  theme_bw()

####Noen forsøk på å lage grid, må ses over seinere
#extent(shp_myrtyper)[1]
#tempras <- raster(shp_myrtyper)
#tempras

#coordinates(grd) <- ~ x+y
#gridded(grd) <- TRUE
#grid <- as(tempras, "SpatialPixels")
#grid <- SpatialPixels(griddf)

#myX <- seq(extent(shp_myrtyper)[1], extent(shp_myrtyper)[2], by=1)
#myY <- seq(extent(shp_myrtyper)[3], extent(shp_myrtyper)[4], by=1)
#griddf<- expand.grid(x=myX, y=myY)

#grid <- SpatialGrid(griddf)

#plot av IDW med punkter og med stasjon lagt over
ggplot()+
  geom_tile(data = idwdf, aes(x = x, y = y, fill = var1.pred))+
  geom_polygon( data=df, aes(x=long, y=lat, group=group),
                color="red", fill=NA,  size = 2)+
  geom_point(data = df3, aes(x = X, y = Y),
             shape = 4)+
  scale_fill_gradientn(colors = terrain.colors(10))+
  theme_bw()

#Plot av IDW uten punkter, med stasjon og myrtyper lagt til
ggplot()+
  geom_tile(data = idwdf, aes(x = x, y = y, fill = var1.pred))+
  geom_polygon( data=df2, aes(x=long, y=lat, group=group),
                color="black", fill=NA, size = .1)+
  geom_polygon( data=df, aes(x=long, y=lat, group=group),
                color="red", fill=NA,  size = 2)+
  scale_fill_gradientn(colors = terrain.colors(10))+
  theme_bw()

# Kriging - Variogram -----------------------------------------------------
#Kriging is a little more involved than IDW as it requires the construction of a semivariogram model to describe 
#the spatial autocorrelation pattern for your particular variable. We’ll start with a variogram cloud
variogcloud<-variogram(Peat_depth~1, locations=sp_data, data=sp_data, cloud=TRUE)
plot(variogcloud)

#Hurra! Da har jeg laga min første variogram-sky ;)!

#The values in the cloud can be binned into lags with and plotted with a very similar function
semivariog<-variogram(Peat_depth~1, locations=sp_data, data=sp_data)
#plot(semivariog)
#semivariog

#From the empirical semivariogram plot and the information contained in the semivariog gstat object, 
#we can estimate the sill, range and nugget to use in our model semivariogram.

#the range (the point on the distance axis where the semivariogram starts to level off) is around 90
#The Sill (the point on the y axis where the semivariogram starts to level off) is around 1.00
#The nugget looks to be around 0.3 (so the partial sill is around 0.7)
#Using this information we’ll generate a model semivariogram using the vgm() function in gstat.

#first check the range of model shapes available in vgm
#vgm()

#the data looks like it might be an exponential shape, so we will try that first with the values estimated from the empirical 
model.variog<-vgm(psill=0.7, model="Exp", nugget=0.3, range=90)

#We can now fit this model to a sample variogram to see how well it fits and plot it

#fit.variog<-fit.variogram(semivariog, model.variog)
#plot(semivariog, fit.variog)

#try some alternative models to see if the fit is any better

model.variog<-vgm(psill=0.7, model="Sph", nugget=0.3, range=90)
fit.variog<-fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)

#model.variog<-vgm(psill=0.7, model="Lin", nugget=0.3, range=90)
#fit.variog<-fit.variogram(semivariog, model.variog)
#plot(semivariog, fit.variog)

#Hm, usikker. Synes kanskje (visuelt) at modell 2 med "Sph" (spherical) passer best?

#Use the krige() function in gstat along with the model semivariogram just generated to generate an ordinary/simple Kriged surface 
# - again, check ?krige to see what the various options in the function are.
#?krige

## [using ordinary kriging]
krig<-krige(formula=Peat_depth ~ 1, locations=sp_data, newdata=grd, model=model.variog)
#Kjører, men med noen warnings

krig.output=as.data.frame(krig)
names(krig.output)[1:3]<-c("long","lat","var1.pred")

#Generate a plot of the kriged surface in ggplot2

plot<-ggplot(data=krig.output,aes(x=long,y=lat))#start with the base-plot and add the Kriged data to it
layer1<-c(geom_tile(data=krig.output,aes(fill=var1.pred)))#then create a tile layer and fill with predicted
layer2<-c(geom_polygon( data=df2, aes(x=long, y=lat, group=group),
                        color="black", fill=NA, size = .1, alpha = .5))#then create an outline
plot+layer1+layer2+geom_polygon( data=df, aes(x=long, y=lat, group=group),
                                 color="red", fill=NA,  size = 2)+
  scale_fill_gradient(low="#FEEBE2", high="#7A0177")+coord_equal()


# Export interpolations for QGIS ------------------------------------------

raster_IDW<-raster(idw)
writeRaster(raster_IDW,'IDW_1.tif')
raster_krig<-raster(krig)
writeRaster(raster_krig, 'krig_1.tif')
write.csv(krig.output,"Data/krig.output.csv", row.names = FALSE)

# Crop the grid to the shape of the station -------------------------------
krig_raster<-raster("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/krig_1.tif")

cropped_krig<-st_crop( krig_raster$grid, shp_stasjon_2$geometry)

crop_krig<-raster::crop(krig_raster,shp_stasjon)
plot(crop_krig) #cropper kun til extent
crop_krig_mask<-raster::mask(crop_krig,shp_stasjon)
plot(crop_krig_mask) #denne gir riktigere cropping. Den kutter dog ikke helt, 
                      #men lager NA's for punkter utafor maska
sum(crop_krig_mask@data@values, na.rm=TRUE)
#77697.63
mean(crop_krig_mask@data@values, na.rm=TRUE)
summary(crop_krig_mask@data@values)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.5963  1.3704  2.0965  2.0490  2.6382  3.7361    2471

#Grovestimat karbon
77697.63*0.1*0.5*1000
#3884882 kg = 3884.882 tonn karbon

#Grovestimat CO2
3884882*3.67
#14257517 kg = 14257.517 tonn CO2

