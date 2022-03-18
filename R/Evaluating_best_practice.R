## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------
install.packages("NCmisc")


## -------------------------------------------------------------------------------------------------------------------------------------------
p <- list.functions.in.file("Untitled5")


## ---- echo=FALSE----------------------------------------------------------------------------------------------------------------------------
library(readxl)
library(writexl)
library(rgdal)
library(raster)
library(ggplot2)
library(gstat)
library(sf)
library(broom)
library(ggthemes)
library(viridis)
library(sp)
library(spatialEco)
library(spm)
library(tmap)
library(Metrics)
library(rlist)
library(dplyr)
library(tmaptools)
library(shinyjs)
library(rgeos)
library(automap)



## -------------------------------------------------------------------------------------------------------------------------------------------

shp<-readOGR(dsn="C:/Users/martef/OneDrive - NTNU/Documents/Jobb-pc/PhD prosjekter/WP3 NINA GRAN/Data/Rydda datasett for kalkulator/Tydal", layer="stasjon_Setermyra")

df <- read.csv("C:/Users/martef/OneDrive - NTNU/Documents/Jobb-pc/PhD prosjekter/WP3 NINA GRAN/Data/Rydda datasett for kalkulator/Tydal/Torvdybder_Tydal.csv", sep=";")

# Make spatial dataframe of the peat depths points

dfs <- st_as_sf(x = df, 
                        coords = c("x", "y"),
                        crs = "+init=epsg:25833")

# Make sf file from sp shapefile
sf_shp <- st_as_sf(shp)

#Check projections
sf::st_crs(shp)
sf::st_crs(dfs)

#set same projection on all files
proj4string(shp)<-crs(dfs)

#make spatial file from data frame file
dfsp <- as(dfs, Class="Spatial")

#Crop dfsp to the extent of the station area
dfsp_station <- dfsp[shp,]

#Create sf of point data
sf_station <- st_as_sf(dfsp_station)




## -------------------------------------------------------------------------------------------------------------------------------------------
tmap_mode("view")
  tm_shape(sf_shp)+
  tm_polygons() +
  tm_shape(dfs)+
  tm_dots(col="black", size=0.01, alpha=0.5, )



## -------------------------------------------------------------------------------------------------------------------------------------------
data_Tydal <- tmap_mode("plot")+
                tm_shape(sf_shp)+
                 tm_polygons() +   
              tm_shape(dfsp_station)+
              tm_dots(col="Dybde", alpha=1, palette="-viridis", size=1 ) +
              tm_layout(legend.outside = TRUE)

data_Tydal


## -------------------------------------------------------------------------------------------------------------------------------------------
grid <- raster(extent(shp)) #create a raster grid from the extent of the peatland
res(grid) <- 1              #set resolution of the grid to 1x1m
proj4string(grid)<-crs(dfs) #set similar projection to the grid as to the datapoints

grid_sp <-as(grid, "SpatialPixels") #convert the grid from raster to spatialpixels

grid_sp@grid@cellsize       #check that cell size is 1x1

grid_crop <- grid_sp[shp,]  #crop the grid to only include the peatland
plot(grid_crop)


## ---- echo=FALSE----------------------------------------------------------------------------------------------------------------------------
neighbors = length(dfsp_station$Dybde)
power = c(seq(from = 1, to = 4, by = 1))
neigh = c((1), seq(2,30,by = 2), c(length=(neighbors)))

temp <- data.frame()

for (i in power) {
  for (j in neigh) {
    
    temp2 <- NULL
    temp3 <- NULL
    temp4 <- NULL

    run = paste(i, j, sep="_")

    print(run)
    temp2 <- idw(Dybde ~ 1, dfsp_station, grid_crop, nmax=j, idp=i)
    temp3 <- as.data.frame(temp2@data)
    temp4 <- sum(temp3$var1.pred)
    temp5 <- cbind(run, temp4)
    temp  <- rbind(temp, temp5)
  }
} 




## -------------------------------------------------------------------------------------------------------------------------------------------
volume <- temp
volume <-dplyr::rename(volume, volume=temp4)
volume <- tidyr::separate(volume, 
                        run, 
                        into = c("power", "nn"),
                        sep = "_",
                        remove=F)
volume$power <- as.numeric(volume$power)
volume$nn <- as.numeric(volume$nn)
volume$volume <- as.numeric(volume$volume)



## -------------------------------------------------------------------------------------------------------------------------------------------
max <- max(volume$volume)
min <- min(volume$volume)
mean <- mean(volume$volume)
sd <- sd(volume$volume)


Description <- c("mean", "min", "max", "SD")
Results_volume <- data.frame(Description, Results = c(mean, min, max, sd)) 

Results_volume


## -------------------------------------------------------------------------------------------------------------------------------------------
area <- st_area(sf_shp)


## -------------------------------------------------------------------------------------------------------------------------------------------
mean <- mean(dfsp_station$Dybde)


## -------------------------------------------------------------------------------------------------------------------------------------------
roughvol <- mean*area
roughvol


