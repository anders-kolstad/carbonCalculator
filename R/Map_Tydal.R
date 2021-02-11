
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

install.packages("maps")
library(Rtools)




# Import data -------------------------------------------------------------


Feltdata_Tydal_rawdata <- read_excel("Data/Feltdata_Tydal_rawdata.xlsx", 
                                       +     col_types = c("text", "numeric", "numeric", 
                                                           +         "numeric", "text", "text", "numeric", 
                                                           +         "numeric", "text", "text", "text", 
                                                           +         "text", "text", "text", "text", "text"))
shp_stasjon<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="stasjon_Setermyra")
shp_myrtyper<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="naturtyper_Setermyra")
sp_data<-readOGR(dsn="C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data", layer="dybdepunkter_Tydal2")

# Inspection of data ------------------------------------------------------
st_crs(shp_stasjon)
plot(shp_stasjon)

st_crs(shp_myrtyper)
plot(shp_myrtyper)

df<-fortify(shp_stasjon)
df2<-fortify(shp_myrtyper)
combine.df<-bind_rows(df, df2)

#Rydde opp i dobbel navngivning (grupper fra ulike shapefiler heter begge 0.1- gjøre om en gruppe)
levels(combine.df$group) <- c(levels(combine.df$group),"15.1") #må legge til nytt faktor-nivå først
combine.df$group[1:7]<-"15.1" #gjøre om navn på første gruppe

ggplot()+
  geom_polygon(data=combine.df, aes(x=long, y=lat, group=group, fill=NULL))
  
p_myr <- ggplot()
p_myr <- p_myr + geom_polygon( data=df2, aes(x=long, y=lat, group=group),
              color="black", fill="lightblue", size = .1 )
p_myr

p_station <- ggplot()
p_station <- p_station +  geom_polygon( data=df, aes(x=long, y=lat, group=group),
                          color="black", fill="lightyellow",  size = 1, alpha = .3)
p_station

p_station_myr <- p_myr + p_station #funker ikke å kombinere de direkte

p_station_myr <- ggplot() +
  geom_polygon( data=df2, aes(x=long, y=lat, group=group),
                               color="black", fill="lightblue", size = .1 ) +
  
  geom_polygon( data=df, aes(x=long, y=lat, group=group),
                color="black", fill="lightyellow",  size = 1, alpha = .5)
p_station_myr #Dette funker!



# Work with the point data towards interpolation --------------------------

st_crs(sp_data)
plot(sp_data)
