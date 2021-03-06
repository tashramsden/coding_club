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
library(purrr)

# Set working directory ----
setwd("07_data_manipulation_2")

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

# subset data to contain fewer genera
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
# tree.plots <- trees.five %>% 
#   group_by(Genus) %>% 
#   do(plots =  # the plotting call for do function
#        ggplot(data = .) +
#        geom_point(aes(x = Easting, y = Northing, 
#                       size = Height.cat), alpha = 0.5) +
#        labs(title = paste("Map of", .$Genus, "at Craigmillar Castle",
#                           sep = " ")) +
#        theme_bw() +
#        theme(panel.grid = element_blank(),
#              axis.text = element_text(size = 14),
#              legend.text = element_text(size = 12),
#              plot.title = element_text(hjust = 0.5),  # centre title
#              legend.position = "bottom")
#        )
# 
# tree.plots$plots  # show plots
# # tree.plots object where plots saved as lists within plots column created
# # do() function allows external functions to be used in pipes BUT tricky 
# # and being depreciated
# 
# # save plots to file
# tree.plots %>% 
#   do(., ggsave(.$plots, 
#                filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""),
#                device = "png", height = 12, width = 16, units = "cm"))


# SINCE do() BEING DEPRECIATED - use below instead for same result!
# alternative purr package to save files
# see https://www.brodrigues.co/blog/2017-03-29-make-ggplot2-purrr/ for explanation

# map2 from purrr

tree_plots <- trees.five %>%
  group_by(Genus) %>%
  nest() %>%
  mutate(plot = map2(data, Genus, ~ggplot(data = .) + 
                       geom_point(aes(x = Easting, y = Northing, 
                                      size = Height.cat), alpha = 0.5) +
                       labs(title = paste("Map of", Genus, "at Craigmillar Castle",
                                          sep = " ")) +
                       theme_bw() +
                       theme(panel.grid = element_blank(),
                             axis.text = element_text(size = 14),
                             legend.text = element_text(size = 12),
                             plot.title = element_text(hjust = 0.5),
                             legend.position = "bottom")))
print(tree_plots)

map2(paste0(getwd(), "/", "map-", tree_plots$Genus, ".png", sep = ""),
     tree_plots$plot, ggsave, device = "png", height = 12, width = 16, units = "cm")


# Sticking things together with paste()
# equivalent of f strings in python
 # see above file paths to save plots and plot titles for egs


# 5. Challenge yourself! ----

# The Craigmillar Castle team would like a summary of the different species 
# found within its grounds, but broken down in four quadrants (NE, NW, SE, SW).
# You can start from the trees.genus object created earlier.

# Can you calculate the species richness (e.g. the number of different species) 
# in each quadrant?

# They would also like to know how abundant the genus Acer is (as a % of the 
# total number of trees) in each quadrant.

# Finally, they would like, for each quadrant separately, a bar plot showing 
# counts of Acer trees in the different age classes, ordered so they read from
# Young (lumping together juvenile and semi-mature trees), Middle Aged,
# and Mature.

vertical_centre <- (max(trees.genus$Northing) + min(trees.genus$Northing)) / 2
horizontal_centre <- (max(trees.genus$Easting) + min(trees.genus$Easting)) / 2

trees.genus <- trees.genus %>%
  mutate(Quadrant = case_when(
    Northing <= vertical_centre & Easting <= horizontal_centre ~ "SW",
    Northing <= vertical_centre & Easting > horizontal_centre ~ "SE",
    Northing > vertical_centre & Easting <= horizontal_centre ~ "NW",
    Northing > vertical_centre & Easting > horizontal_centre ~ "NE"
  ))
    
# Check quadrant assignment successful
# map trees according to quadrant
(map.all <- ggplot(trees.genus) +
    geom_point(aes(x = Easting, y = Northing, 
                   size = Height.cat, colour = Quadrant), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# species richness per quadrant
quad_summary <- trees.genus %>% 
  group_by(Quadrant) %>% 
  summarise(count = length(CommonName),
            spp_richness = length(unique(LatinName)))


# Acer abundance
acer_abund <- trees.genus %>% 
  group_by(Quadrant, Genus) %>% 
  tally() %>%  # count no of trees of each genus
  group_by(Quadrant) %>% 
  mutate(total = sum(n)) %>%  # calculate total no of trees per quadrant
  filter(Genus == "Acer") %>%   # only keep acers
  mutate(acer_percent = n / total * 100) %>% 
  dplyr::select(-Genus)

# plot percentage of acers in each quadrant
ggplot(acer_abund) +
  geom_col(aes(x = Quadrant, y = acer_percent)) +
  labs(x = 'Quadrant', y = '% Proportion of Acer') +
  theme_bw()


# reorder to descending % order
acer_abund2 <- acer_abund
acer_abund2$Quadrant <- reorder(acer_abund3$Quadrant, -acer_abund$acer_percent)

ggplot(acer_abund2) +
  geom_col(aes(x = Quadrant, y = acer_percent)) +
  labs(x = 'Quadrant', y = '% Proportion of Acer') +
  theme_bw()


# bar plots of acer by age category

acers <- trees.genus %>% 
  filter(Genus == "Acer")

acers$AgeGroup <- factor(acers$AgeGroup,
                        levels = c('Juvenile', 'Semi-mature', 'Middle Aged', 'Mature'),
                        labels = c('Young', 'Young', 'Middle Aged', 'Mature'))

levels(acers$AgeGroup)

acer_plots <- acers %>%
  group_by(Quadrant) %>%
  nest() %>%
  mutate(plot = map2(data, Quadrant, ~ggplot(data = .) + 
                       geom_bar(aes(x = AgeGroup)) +
                       labs(title = paste("Age Distribution of Acers in", Quadrant, "of Craigmillar Castle",
                                          sep = " "), x = 'Age group', y = 'Number of trees') +
                       theme_bw() +
                       theme(panel.grid = element_blank(),
                             axis.text = element_text(size = 14),
                             legend.text = element_text(size = 12),
                             plot.title = element_text(hjust = 0.5),
                             legend.position = "bottom")))


print(acer_plots)

map2(paste0(getwd(), "/", "acers-", acer_plots$Quadrant, ".png", sep = ""),
     acer_plots$plot, ggsave, device = "png", height = 12, width = 16, units = "cm")

