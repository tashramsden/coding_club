#################################################################
#                                                               #
#                        SPATIAL ANALYSIS                       #
#                   Natasha Ramsden 10/08/2021                  #
#                                                               #
#################################################################

# Data info (from Coding Club) ----
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


# 1. Explore raster data ----

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

## if Error in plot.new() : figure margins too large -> 
## par(mar=c(5.1, 4.1, 4.1, 2.1))

image(b8)

plot(b8)
zoom(b8)  # run this line then click twice on plot to define a box to zoom

# or an extent can be cropped and plotted from plot image using same double click:
plot(tay)
e <- drawExtent()  # run and click twice to define box
cropped_tay <- crop(b7, e)
plot(cropped_tay)


# 2. Visualise Spectral Bands ----

# a. viridis ----
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


# b. RGB ----
# red-green-blue plot of a multi-layered object for more realistic rendition
# layers/bands represent different bandwidths in visible em spectrum 
# (corresponding to red, blue and green) combined -> naturalistic colour 
# rendition of the earth surface.

# first, create raster stack:
# (multi-layered raster object, of the red(b4), green(b3) and blue(b2) bands)

# this code specifies how we want to save the plot
png('images/RGB.png', width = 5, height = 4, units = "in", res = 300)
tayRGB <- stack(list(b4, b3, b2))  # creates raster stack
plotRGB(tayRGB, axes = TRUE, stretch = "lin", 
        main = "Sentinel RGB colour composite")
dev.off()

# to just view and not save
tayRGB <- stack(list(b4, b3, b2))
plotRGB(tayRGB, axes = TRUE, stretch = "lin",
        main = "Sentinel RGB colour composite")


# c. FCC - false colour positive ----
# red, green, and blue bands replaced to accentuate vegetation

# In FCC, red band replaced by near infrared band (band 8 in Sentinel 2),
# the green band by red, and the blue band by green. 
# -> creates an image where the vegetation stands out in red. 
# (help(plotRGB)) for more information and other arguments for the function

# rasterVis package provides ways to enhance vis and analysis of raster data:
# https://oscarperpinan.github.io/rastervis/
# levelplot function allows level and contour plots to be made of raster 
# objects with elevation data, such as LIDAR, and plot3D allows 3D mapping. 

# don't have elevation data from Sentinel 2, but the package’s gplot 
# function allows plotting of a uni or multivariate raster object using
# ggplot2-like syntax.

# basic
gplot(b8) +
  geom_raster(aes(x = x, y = y, fill = value))

# better
gplot(b8) +
  geom_raster(aes(x = x, y = y, fill = value)) +  # value is specific value (reflectance) of each pixel
  scale_fill_viridis_c(name = "Reflectance") +
  coord_quickmap() +
  ggtitle("West of Loch Tay, raster plot") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1))

# save plot
ggsave("images/ggtay.png", scale = 1.8, dpi = 300)

# visualise all the bands together:
# use facet_wrap in gplot, first create stack of all bands (on top of each other)
t <- stack(b1,b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)

# faceted plots:
gplot(t) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(name = "Reflectance") +
  facet_wrap(~variable) +
  coord_quickmap()+
  ggtitle("Sentinel 2 Loch Tay, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("images/allbands.png", scale = 2, dpi = 300)

# or for quick visualisation, original file can be loaded as raster brick 
# and plotted using ‘plot’.
s_tay <- brick('data/taycrop.tif')
plot(s_tay)

# NOTICE: difference in colour and range of legend between the different bands
# Different earth surfaces reflect the solar radiation differently and each
# raster layer represents how much incident solar radiation is reflected at 
# a particular wavelength bandwidth. 

# Bands 6 to 9 are in the Near Infrared Range (NIR). Vegetation reflects more
# NIR than other wavelengths but water absorbs NIR, therefore the lighter areas
# with high reflectance values are likely to be vegetation and the dark blue,
# low reflectance value areas, likely to be water. Also note that the 
# Sentinel 2 bands have 3 levels of spatial resolution, 10 m, 20 m, and 
# 60 m:
# 10 m resolution band 2, band 3, band 4 and band 8
# 20 m resolution band 5, band 6, band 7, band 11 and band 12
# 60 m resolution band 1, band 9 and band 10


# 3. Manipulate rasters: NDVI and KMN classification ----

