#################################################################
#                                                               #
#                   EFFICIENT DATA MANIPULATION                 #
#                USE PIPES TO STREAMLINE YOUR CODE              #
#                   Natasha Ramsden 17/06/2021                  #
#                                                               #
#################################################################

# Aims:
# Chain together multiple lines of codes with pipes %>%
# Use dplyr to its full potential
# Automate advanced tasks like plotting without writing a loop

# Data Info ----
# We are working with a subset of a larger dataset of trees within the City of 
# Edinburgh*. Work with subset of this large dataset (over 50 thousand trees!) 
# to the Special Landscape Area around Craigmillar Castle. trees.csv.

# *(Copyright City of Edinburgh Council, contains Ordnance Survey data © Crown 
# copyright and database right 2019)


# Libraries ----
library(dplyr)
library(ggplot2)

# Set working directory ----
setwd("07_data_vis_2")

# Load data ----
trees <- read.csv("trees.csv", header=TRUE)
head(trees)


# Count the number of trees for each species 

trees.grouped <- group_by(trees, CommonName)
trees.summary <- summarise(trees.grouped, count = length(CommonName))   
# use length to count the no of rows (trees) for each group (species)
trees.summary <- tally(trees.grouped)  # does same as above

# USING THE PIPE | Ctrl + Shift + M | %>%  ----
# takes data frame created on left, passes it to function on right. 
# don't have to create intermediary objects
# avoids repeating object name in every function

trees.summary <- trees %>%  # the data frame object passed in the pipe
  group_by(CommonName) %>%  # don't need to name object, just grouping variable
  tally()   

# NOTE: Pipes only work on data frame objects, 
# and functions outside the tidyverse often require that you specify the 
# data source with a full stop dot .


# Count no of Ash, Rowan and Pine by age group

trees.subset <- trees %>%
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>% 
  group_by(CommonName, AgeGroup) %>% 
  tally()


# UP TO 2. More functions of dplyr ----



