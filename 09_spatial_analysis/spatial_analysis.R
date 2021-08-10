#################################################################
#                                                               #
#                        SPATIAL ANALYSIS                       #
#                   Natasha Ramsden 10/08/2021                  #
#                                                               #
#################################################################

# Data info ----
# Satellite data of the Loch Tay area of Scotland, available 
# from https://scihub.copernicus.eu/

# Working with spatial data can be complicated due to the many different 
# file formats and large size they can have. To simplify this tutorial, 
# we will use a Sentinel 2 satellite image collected on the 27th June 2018 
# and downloaded from the Copernicus Hub. This website provides free and open 
# access to satellite data, but please note the files are very large and may 
# take some time to download. The image used in this tutorial was cropped to 
# reduced its size and corrected for atmospheric correction in SNAP, 
# the free open source esa toolbox, and saved as a ‘geoTIF’ file 
# (georeferenced image) to make it easier to import. 
# See Coing Club for intro tutorial into SNAP

# Alternatively, for large scale analysis where downloading huge files is not 
# an option, Google Earth Engine is a powerful tool providing an online code
# editor where users can work with a large selection of databases, whilst 
# harnessing the power of the Google servers. (also coding club tutorials)

# Satellite data mostly consist of reflectance data, which can be defined as a
# measure of the intensity of the reflected sun radiation by the earth’s 
# surface. Reflectance is measured for different wavelength of the 
# electromagnetic spectrum. The Sentinel 2 optical sensor measures reflectance 
# at 13 wavelength bandwidths, or bands for short. In satellite images, 
# these data are stored in rasters, or a matrix data structure, where each 
# pixel stores the data for the 13 wavelengths. Therefore, Sentinel 2 data 
# contains several raster layers, one for each spectral band. More information 
# on Sentinel 2 can be accessed here: https://en.wikipedia.org/wiki/Sentinel-2

# RASTER: a grid of equal size cells, or pixels in satellite images,  
# commonly used to represent spatially continuous data. The cells can have 
# one or more values, or even no values for the variable of interest. In the 
# trimmed multispectral image we will be using, each cell contains reflectance 
# data for 12 spectral bands.


# Libraries ----
# install.packages("rasterVis")
library(sp)  # defines set of classes to represent spatial data
library(rgdal)  # read/save spatial data files (& used by package raster)
library(raster)  # create, read, mainpulate and save raster data
library(ggplot2)
# library(viridisLite)  # needed for loading viridis
library(viridis)  # aesthetically pleasing colour palette, colour-blind friendly
library(rasterVis)


# Working directory ----
setwd("09_spatial_analysis")

# Load data ----
tay <- raster("data/taycrop.tif")


# 1. Explore raster data

# get properties
tay
# class      : RasterLayer 
# band       : 1  (of  12  bands)
# dimensions : 507, 848, 429936  (nrow, ncol, ncell)
# resolution : 9.217891e-05, 9.217891e-05  (x, y)
# extent     : -4.320218, -4.242051, 56.45366, 56.50039  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : taycrop.tif 
# names      : taycrop 

# crs is coordinate reference system, which here is the Universal Trans 
# Mercator (UTM) with datum WGS84

# create individual raster layers for each spectral band in the raster tay
b1 <- raster('data/taycrop.tif', band=1)
b2 <- raster('data/taycrop.tif', band=2)
b3 <- raster('data/taycrop.tif', band=3)
b4 <- raster('data/taycrop.tif', band=4)
b5 <- raster('data/taycrop.tif', band=5)
b6 <- raster('data/taycrop.tif', band=6)
b7 <- raster('data/taycrop.tif', band=7)
b8 <- raster('data/taycrop.tif', band=8)
b9 <- raster('data/taycrop.tif', band=9)
b10 <- raster('data/taycrop.tif', band=10)
b11 <- raster('data/taycrop.tif', band=11)
b12 <- raster('data/taycrop.tif', band=12)

# check whether bands have same extent, number of rows and columns, projection,
# resolution and origin
compareRaster(b2, b3)

# NOTE: Checking the coordinate systems and extents of rasters is a very 
# useful skill - quite often when you have problems with working with multiple 
# raster objects, it’s because of differences in coordinate systems or extents.

# can plot the bands using plot/images functions
# plot function only plots 100,000 pixels, but image stretches the view
# if Error in plot.new() : figure margins too large -> 
# par(mar=c(5.1, 4.1, 4.1, 2.1))

plot(b8)
image(b8)

plot(b8)
zoom(b8)  # run this line then click twice on plot to define a box to zoom

# or an extent can be cropped and plotted from plot image using same double click:
plot(tay)
e <- drawExtent()  # run and click twice to define box
cropped_tay <- crop(b7, e)
plot(cropped_tay)


# 2. Visualise Spectral Bands

# bands can be plotted w diff colour palettes, eg viridis, to improve vis
# and save:
png("images/tayplot.png", width = 4, height = 4,
    units = "in", res = 300)
image(b8, col = viridis_pal(option="D")(10),
      main = "Sentinel 2 Image of Loch Tay")
dev.off()

# to view the plot without saving, only need second line of above
image(b8, col = viridis_pal(option="D")(10),
      main = "Sentinel 2 Image of Loch Tay")




