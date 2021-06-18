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
library(tidyr)

# Set working directory ----
setwd("07_data_vis_2")

# Load data ----
trees <- read.csv("trees.csv", header=TRUE)
head(trees)


# 1. Intro to pipes ----

# Count the number of trees for each species 

trees.grouped <- group_by(trees, CommonName)
trees.summary <- summarise(trees.grouped, count = length(CommonName))   
# use length to count the no of rows (trees) for each group (species)
trees.summary <- tally(trees.grouped)  # does same as above

# USING THE PIPE | Ctrl + Shift + M | %>% 
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


# 2. More functions of dplyr ----

# summarise_all()
summ.all <- summarise_all(trees, mean)
# only 2 columns numeric - mean calculated, the rest have missing values


# ifelse()
# give ifelse() a conditional statement to evaluate + values to return 
# if true/false
vector <- c(4, 13, 15, 6)
ifelse(vector < 10, "A", "B") # if num < 10 return A, otherwise return B


# case_when()
# good for re-classifying values or factors
# extension of ifelse() - can assign more than 2 outcomes
# assign outcomes with tilde ~
vector2 <- c("What am I?", "A", "B", "C", "D")
case_when(vector2 == "What am I?" ~ "I am the walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")


# 3. Changing factor levels or create categorical variables ----

# mutate() + case_when() = good for changing factor levels/creating new var 
# based on existing ones

# create Genus column
# from LatinName can see many tree spp in same genera, 
# eg birch (Betula), willow (Salix)
# use character string search w grepl function

# first show species names
unique(trees$LatinName)

# create Genus column
trees.genus <- trees %>%
  mutate(Genus = case_when(  # create column and specify conditions
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus", 
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus")
    )

# lots of typing! - but quicker than assigning each tree a genus individually

# here genus is first word of LatinName - use separate() - do above much easier!
trees.genus2 <- trees %>% 
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ",
                  remove = FALSE) %>%   # remove = FALSE means keep LatinName
  dplyr::select(-Species)  # remove new spp column


# create new height category variable Height.cat
# eg Height has 5 levels, we want 3
trees.genus <- trees.genus %>% 
  mutate(Height.cat = case_when(
    Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
    Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
    Height == "20 to 25 meters" ~ "Tall")
    )

# reorder factor levels
# levels automatically sorted alphabetically, this not always useful
trees.genus$Height.cat <- factor(trees.genus$Height.cat)
levels(trees.genus$Height.cat)  # show levels in default order

trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c("Short", "Medium", "Tall"),  # chosen order of levels
                                 labels = c("SHORT", "MEDIUM", "TALL")  # match sure these match order!
                                 )

levels(trees.genus$Height.cat)  # new order and levels


# 4. Advanced piping ----

# subset data to containfewer genera
trees.five <- trees.genus %>% 
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# map all trees
(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, 
                   size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
  )

# map each genus separately - using pipes
# might want to facet plots instead, but sometimes want separate plots
# do() means almost any R function can be used within pipe - as long as data 
# provided as data = . where needed

# map for each genus
tree.plots <- trees.five %>% 
  group_by(Genus) %>% 
  do(plots =  # the plotting call for do function
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, 
                      size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle",
                          sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),  # centre title
             legend.position = "bottom")
       )


tree.plots$plots  # show plots
# tree.plots object where plots saved as lists within plots column created
# do() function allows extrenal functions to be used in pipes BUT tricky 
# and being depreciated

# save plots to file
tree.plots %>% 
  do(., ggsave(.$plots, 
               filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""),
               device = "png", height = 12, width = 16, units = "cm"))

# alternative purr package to save files
# UP TO HERE  - LOOK AT PURR PAGE! (then Sticking things together with paste())









