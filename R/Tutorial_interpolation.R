# Packages needed ####

# We will need some packages for (spatial) data processing
library(tidyverse) # wrangling tabular data and plotting
library(sf) # processing spatial vector data
library(sp) # another vector data package necessary for continuity
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!

# And a lot of different packages to test their interpolation functions
library(gstat)  # inverse distance weighted, Kriging
library(fields) # Thin Plate Spline
library(interp) # Triangulation
library(mgcv)   # Spatial GAM
library(automap)# Automatic approach to Kriging

# Finally, some packages to make pretty plots
library(patchwork)
library(viridis)

# Ste 1: Creating a grid template ####

# First let's define a bounding Box, a rectangle that contains all our data
# points. There are many ways to do this but I am keeping it as simple as
# possible on purpose here
bbox <- c(
  "xmin" = min(pts_NH4$X), #the example dataset is called pts_NH4
  "ymin" = min(pts_NH4$Y),
  "xmax" = max(pts_NH4$X),
  "ymax" = max(pts_NH4$Y)
)

grd_template <- expand.grid(
  X = seq(from = bbox["xmin"], to = bbox["xmax"], by = 20),
  Y = seq(from = bbox["ymin"], to = bbox["ymax"], by = 20) # 20 m resolution
)

grid_plot <- ggplot() +
  geom_point(data = grd_template, aes(x = X, y = Y), size = 0.01) +
  geom_point(data = pts_NH4,
             mapping = aes(x = X, y = Y, color = NH4), size = 3) +
  scale_color_gradientn(colors = c("blue", "yellow", "red")) +
  coord_cartesian( #zooming in so we can actually see something
    xlim = c(408000, 409000), ylim = c(5815000, 5816000)) +
  theme_bw()

grid_plot

# Step 1b: Creating a grid template 2 ####
sf_NH4 <- st_as_sf(pts_NH4, coords = c("X", "Y"), crs = 25833)

alt_grd_template_sf <- sf_NH4 %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_make_grid(
    cellsize = c(20, 20),
    what = "centers"
  ) %>%
  st_as_sf() %>%
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  mutate(Z = 0)

#Those commands will create a fairly similar grid to the one we made above, 
#but there are some differences.

#The alt_grd_template_sf does actually contain all sample points. 
#alt_grd_template_sf was created using functions from the sf package 
#and comes in a data structure provided by that package (a sfc, or simple feature column, to be specific). 
#For continuity I converted the object into a data.frame.

# Step 2: Rasterize the grid ####
# {raster} expects a PROJ.4 string, see https://epsg.io/25833
crs_raster_format <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

grd_template_raster <- grd_template %>% 
  dplyr::mutate(Z = 0) %>% 
  raster::rasterFromXYZ( 
    crs = crs_raster_format)

# Let's also carry with us the raster from the alternative approach
alt_grd_template_raster <- alt_grd_template_sf %>% 
  raster::rasterFromXYZ(
    crs = crs_raster_format
  )

# Fit a model ####
#The algorithm of our choice must be fitted to our data 
#before we can use that fit to make interpolations. 

# We start with functions that return model objects as this is the most 
# common case

# Nearest Neighbor
fit_NN <- gstat::gstat( # using package {gstat} 
  formula = NH4 ~ 1,    # The column `NH4` is what we are interested in
  data = as(sf_NH4, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)

# Inverse Distance Weighting
fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
  formula = NH4 ~ 1,
  data = as(sf_NH4, "Spatial"),
  nmax = 10, nmin = 3,
  set = list(idp = 0.5) # inverse distance power
)

# Thin Plate Spline Regression
fit_TPS <- fields::Tps( # using {fields}
  x = as.matrix(pts_NH4[, c("X", "Y")]), # accepts points but expects them as matrix
  Y = pts_NH4$NH4,  # the dependent variable
  miles = FALSE     # EPSG 25833 is based in meters
)

# Generalized Additive Model
fit_GAM <- mgcv::gam( # using {mgcv}
  NH4 ~ s(X, Y),      # here come our X/Y/Z data - straightforward enough
  data = pts_NH4      # specify in which object the data is stored
)

# Next we use a couple of functions that have a slightly different modus
# operandi as they in fact already return interpolated Z values.

# Triangular Irregular Surface
fit_TIN <- interp::interp( # using {interp}
  x = pts_NH4$X,           # the function actually accepts coordinate vectors
  y = pts_NH4$Y,
  z = pts_NH4$NH4,
  xo = grd_template$X,     # here we already define the target grid
  yo = grd_template$Y,
  output = "points"
) %>% bind_cols()

# Automatized Kriging  
fit_KRIG <- automap::autoKrige(      # using {automap}
  formula = NH4 ~ 1,                 # The interface is similar to {gstat} but
  input_data = as(sf_NH4, "Spatial") # {automap} makes a lot of assumptions for you
) %>% 
  .$krige_output %>%  # the function returns a complex object with lot's of metainfo
  as.data.frame() %>% # we keep only the data we are interested in
  dplyr::select(X = x1, Y = x2, Z = var1.pred) 

## [using ordinary kriging]

# Interpolate ####
# Triangular Irregular Surface
interp_TIN <- raster::rasterFromXYZ(fit_TIN, crs = crs_raster_format)

# Automatized Kriging
interp_KRIG <- raster::rasterFromXYZ(fit_KRIG, crs = crs_raster_format)

# Note that you can inspect the result of your interpolation anytime with a 
# simple `plot()`, if it is a {raster*} object
plot(interp_KRIG)

#Many R functions are specifically build around the 
#conventions defined for model objects, 
#so you do not really have to worry why things work, 
#because they usually just do work, 
#as long as you remember were to insert which object. 
#We will take advantage of this and just plug all our remaining model objects, 
#those that do not yet hold any interpolated values at our target grid, 
#nto the raster::interpolate function and have it take care of all the rest.

# Nearest Neighbor
interp_NN <- interpolate(grd_template_raster, fit_NN)

## [inverse distance weighted interpolation]

# Inverse Distance Weighting
interp_IDW <- interpolate(grd_template_raster, fit_IDW)

## [inverse distance weighted interpolation]

# Thin Plate Spline Regression
interp_TPS <- interpolate(grd_template_raster, fit_TPS)

#Generalized Additive Model
#This one require that we take a step down in the R-univers,
#as stats::interpolate does not seem to work.
#But by falling back on the lower-level stats::predict 
#we can make it work with just a little more effort
interp_GAM <- grd_template %>% 
  mutate(Z = predict(fit_GAM, .)) %>% 
  rasterFromXYZ(crs = crs_raster_format)

# Step 4: Visualize ####
plot_my_rasters <- function(raster_object, raster_name){
  
  df <- rasterToPoints(raster_object) %>% as_tibble()
  colnames(df) <- c("X", "Y", "Z")
  
  ggplot(df, aes(x = X, y = Y, fill = Z)) +
    geom_raster() +
    ggtitle(label = raster_name) +
    scale_fill_viridis(option = "C") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}

rasterlist <- list(
  "Nearest Neighbor" = interp_NN, 
  "Inverse Distance Weighted" = interp_IDW, 
  "Kriging" = interp_KRIG, 
  "Thin Plate Spline Regression" = interp_TPS,
  "Triangular Irregular Surface" = interp_TIN, 
  "Generalized Additive Model" = interp_GAM
)

plotlist <- map2(
  rasterlist,
  names(rasterlist),
  plot_my_rasters
)

# Note that the next trick only works because of library(patchwork)
(plotlist[[1]] + plotlist[[2]]) /
  (plotlist[[3]] + plotlist[[4]]) /
  (plotlist[[5]] + plotlist[[6]])