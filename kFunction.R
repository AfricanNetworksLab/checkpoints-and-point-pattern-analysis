# Calculate bivariate Ripley's K-function
# Reading in spatial data with sf
# and transforming it to 
# David Russell, January 2026

###### Read in libraries ######
install.packages(c("sf","spatstat","maptools","tidyverse"))
library(sf)
library(spatstat)
library(maptools)
library(tidyverse)

###### Read in data ######
# Make sure shapefiles contain projected data

# Read in points
pointsShapefile <- st_read(file.choose())

# Read in study window shapefile and convert to owin
windowShapefile <- st_read(file.choose())

window.lon <- st_coordinates(windowShapefile)[,1]
window.lat <- st_coordinates(windowShapefile)[,2]

window.owin <-  owin(poly = list(x=rev(window.lon),y=rev(window.lat)))


###### Create spatstat ppp object ######
points.lon <- st_coordinates(pointsShapefile)[,1]
points.lat <- st_coordinates(pointsShapefile)[,2]

points.ppp <- ppp(x = points.lon,
                  y = points.lat,
                  window = window.owin,
                  marks = as.factor(pointsShapefile$type)) 
# Make sure marks column name is correct


###### Perform K-function analysis ######

# Run homogeneous K-cross (bivariate K-function)
# Make sure names match
homogeneousK <- envelope(points.ppp, Kcross, i = "iName", j = "jName", correction = "iso")

# Run inhomogeneous K-cross
# Make sure names match
inhomogeneousK <- envelope(points.ppp, Kcross.inhom, i = "iName", j = "jName", correction = "iso")

