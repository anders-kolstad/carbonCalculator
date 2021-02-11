
# Top ---------------------------------------------------------------------


# Tutorial ggplot2 maps ---------------------------------------------------

# load United States state map data
MainStates <- map_data("state") 
US_counties <- map_data("county")
worldmap <- map_data("world")

# read the state population data
StatePopulation <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv", as.is = TRUE)
# str(MainStates)
# str(StatePopulation)

#plot all states with ggplot2, using black borders and light blue fill
ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )

#Test a few more maps
ggplot() + 
  geom_polygon( data=US_counties, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )

ggplot() + 
  geom_polygon( data=worldmap, aes(x=long, y=lat, group=group),
                color="white", fill="darkblue" )

# Use the dplyr package to merge the MainStates and StatePopulation files
MergedStates <- inner_join(MainStates, StatePopulation, by = "region")
# str(MergedStates)

# Create a Choropleth map of the United States
p <- ggplot()
p <- p + geom_polygon( data=MergedStates, 
                       aes(x=long, y=lat, group=group, fill = population/1000000), 
                       color="white", size = 0.2) 

p <- p + scale_fill_continuous(name="Population(millions)", 
                               low = "lightgreen", high = "darkgreen",limits = c(0,40), 
                               breaks=c(5,10,15,20,25,30,35), na.value = "grey50") +
  
  labs(title="State Population in the Mainland United States")

p <- p + guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10, 
                                      label.theme = element_text(color = "darkgreen", size =10, angle = 45)))
p

p2 <- ggplot()
p2 <- p2 + geom_polygon( data=MergedStates, 
                       aes(x=long, y=lat, group=group, fill = log(population)), 
                       color="purple", size = 1) 
p2
p2 <- p2 + scale_fill_continuous(name="Population(millions)", 
                               low = "white", high = "darkred",limits = c(13,18), 
                               breaks=c(13,14,15,16,17,18), na.value = "grey50") +

#Overlay two polygon maps
p3 <-  ggplot()
p3 <- p3 + geom_polygon( data=US_counties, aes(x=long, y=lat, group=group),
                           color="darkblue", fill="lightblue", size = .1 )
  
p3 <- p3 +  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue",  size = 1, alpha = .3)

  labs(title="State Population in the Mainland United States")

p3
