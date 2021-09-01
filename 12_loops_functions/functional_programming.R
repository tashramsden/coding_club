#################################################################
#                                                               #
#                    FUNCTIONAL PROGRAMMING                     #
#                      LOOPS AND FUNCTIONS                      #
#                   Natasha Ramsden 10/08/2021                  #
#                                                               #
#################################################################

# Aims ----
# What is functional programming
# Building a simple function
# Functions in loops
# Functions with lapply
# Conditional statements
# BONUS: Write a loop to plot multiple graphs

# Set working directory ----
setwd("12_loops_functions")

# Libraries ----
library(gridExtra)  # for arranging plots in panel

# Import data ----
# The data contains info on tree stems surveyed in four 1Ha plots at
# fieldsites around southern Africa

# trees_bicuar contains data for trees in Bicuar National Park in 
# southwest Angola
trees_bicuar <- read.csv("data/trees_bicuar.csv")
head(trees_bicuar)
str(trees_bicuar)

# trees_mlunguya conatins data for trees in southern Mozambique
trees_mlunguya <- read.csv("data/trees_mlunguya.csv")


# 1. Writing functions ----

# writing functions for simple operations that would be repeated throughout 
# code v useful

# functions have the following basic syntax:
example.fn <- function(x, y) {
  # Perform an action using x and y
  x + y
}

# test the function
example.fn(x = 1, y = 2)

## Convention is to name functions with . not _

# A function to calculate basal area of each stem in m^2 from the diameter (cm)
# (basal area is cross-sectional area of trunk if cut parallel to ground)

basal.area <- function(dbh){
  (pi*(dbh)^2/40000)
}

basal.area(dbh = trees_bicuar$diam)

# can add indeterminate number of args using ... operator
# e.g. we want to extend basal.area() function so that it can compute the 
# combined basal area of multiple vectors of diameter measurements, eg from
# mulitpl sites:

basal.area <- function(...){
  (pi*c(...)^2/40000)
}

basal.area(trees_bicuar$diam, trees_mlunguya$diam)

# can assign output to new object, eg a column in trees_bicuar:
trees_bicuar$ba <- basal.area(dbh = trees_bicuar$diam)


# 2. Functions in loops ----

# for() loops iterate through number of items and perform action on each
# basic syntax:

for(i in list){
  #PERFORM SOME ACTION
}

# eg calculate basal area for all field sites at once and add data as new
# column to respective dataset

# rather than: (problem if 100 sites...)
trees_bicuar$ba <- basal.area(trees_bicuar$diam)
trees_mlunguya$ba <- basal.area(trees_mlunguya$diam)

# instead:
# create list of items
trees <- list("trees_bicuar" = trees_bicuar,
              "trees_mlunguya" = trees_mlunguya)

# list items in lists accessed w double square brackets 
# eg trees[[1]] == trees_bicuar

for( i in 1:length(trees) ){
  trees[[i]]$ba <- basal.area(trees[[i]]$diam)
}

# often data not separated into multiple dataframes but single dataframe w 
# column to group diff datasets

# eg trees_mlunguya has column year, perform basal area calc on each year 
# in dataset then find out whether mean basal area of stems in plots has 
# changed over time

# first, separate trees_mlunguya into list of dataframes, each based on 
# contents of year column
trees_mlunguya_list <- split(trees_mlunguya, trees_mlunguya$year)

# Then, run a for() loop to fill an empty list with the mean basal area 
# of each year:

mean_ba_list <- list()

for( i in 1:length(trees_mlunguya_list) ){
  ba <- basal.area(trees_mlunguya_list[[i]]$diam)
  mean_ba <- mean(ba)
  year <- mean(trees_mlunguya_list[[i]]$year)
  dat <- data.frame(year, mean_ba)
  mean_ba_list[[i]] <- dat
}

# During each iteration, this loop creates a number of intermediate data 
# objects (ba, mean_ba, year), and eventually returns a dataframe (dat) 
# with a single row and two columns, one for year and one for mean basal 
# area. Each of these dataframes are then stored as a list item in the new 
# list mean_ba_list.

# the intermediate calculation could be stored in its own function:
ba.mean.year <- function(dbh, year){
  data.frame(
    mean_ba = mean(basal.area(dbh)),
    year = mean(year)
  )    
}

ba.mean.year(trees_mlunguya_list[[1]]$diam, trees_mlunguya_list[[1]]$year)

# use new function in for loop:
mean_ba_list <- list()

for( i in 1:length(trees_mlunguya_list) ){
  mean_ba_list[[i]] <- ba.mean.year(
    trees_mlunguya_list[[i]]$diam,
    trees_mlunguya_list[[i]]$year)
}

# Note that this for() loop now contains a custom function (ba.mean.year()), 
# which itself contains a custom function (basal.area()), demonstrating that 
# there is really no limit to the complexity you can create with functional 
# programming tools like loops and function calls. You can even have loops 
# within loops, and loops in functions!


# 3. lapply() family ----

# for() loops are useful, but because R stores everything as new object with
# each iteration loops can become slow if complex/running lots of 
# processes/iterations

# alternative - lapply() and apply family of functions
# lapply() runs operations on lists, similar to for()
# eg calculate mean basal area per year same as above
lapply(trees_mlunguya_list, function(x){
  ba.mean.year(dbh = x$diam, year = x$year)
  })

# first arg gives list to be iterated over
# second defines an unnamed function, x will be replaced w each list item as
#   lapply() iterates over them
# code in curly brackets is the unnamed function which contains the
#   custom function ba.mean.year()

# faster and easier to read!


# want to find mean height of trees in trees_bicuar for each taxonomic family

# create list of vectors of height (rather than dataframes) where each list 
# is a diff family of spp
bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family)

# run lapply()
lapply(bicuar_height_list, mean, na.rm = TRUE)
# didn't have to use curly brackets/anonymous function - just passed mean
# supplied arg to mean by specifying afterwards (na.rm = TRUE)

# use sapply() for more readable output
# sapply() simplifies output of lapply() to a vector - with elements in the 
#vector named from items in orig list
sapply(bicuar_height_list, mean, na.rm = TRUE)

# sapply() won’t be able to simplify the output of every lapply() loop, 
# especially if the output is complex, but for this example, where we only have
# a single named decimal number, sapply works well.


# 4. Conditional statements ----

# In trees_bicuar there is column height_method (how height data was recorded)
# some measured w long stick, others w laser!
# measurements w stick generally 1m short of actual tree 
# vs measurements w laser accurate to +/- 0.1m

# simple correction: add 1m to every measurement w stick
#                    round every measurement w laser to nearest 0.1m

# A common forestry metric to assess growth of a forest plot over time is 
# “Lorey’s Mean Height”. Lorey’s mean height is calculated by multiplying 
# tree height by the basal area of the tree, then dividing the sum of 
# this calculation by the total plot basal area. 

# We can construct a function which measures Lorey’s mean height for each plot,
# but adjust height depending on method of measurement - use ifelse()

# ifelse() if TRUE, performs first action, otherwise second

stick.adj.lorey <- function(height, method, ba){
  height_adj <- ifelse(method == "stick", height + 1, round(height, digits = 1))
  lorey_height <- sum(height_adj * ba, na.rm = TRUE) / sum(ba, na.rm = TRUE)
  return(lorey_height)
}

# test function using lapply()

trees_bicuar_list <- split(trees_bicuar, trees_bicuar$plotcode)

lapply(trees_bicuar_list, function(x){stick.adj.lorey(
  height = x$height, method = x$height_method, ba = x$ba
)})

# ifelse() statements can also be used in conjunction with logical TRUE/FALSE 
# function arguments to determine whether certain actions are taken. 
# For example, we can write a function that calculates summary statistics 
# on the trunk diameter measurements for a given fieldsite, and we can use 
# TRUE/FALSE arguments to let the user decide whether certain statistics are 
# calculated:

diam.summ <- function(dbh, mean = TRUE, median = TRUE, ba = TRUE){
  mean_dbh <- ifelse(mean == TRUE,
                     mean(dbh),
                     NA)
  median_dbh <- ifelse(median == TRUE, 
                       median(dbh), 
                       NA)
  mean_ba <- ifelse(ba == TRUE, 
                    mean(basal.area(dbh)), 
                    NA)
  return(as.data.frame(na.omit(t(data.frame(mean_dbh, median_dbh, mean_ba)))))
}

diam.summ(dbh = trees_bicuar$diam, median = FALSE, ba = FALSE)
diam.summ(dbh = trees_bicuar$diam)  # args were given default values so if not 
# specified, mean, median and ba will be default, ie TRUE


# 5. Loops to plot multiple graphs ----

# Create multiple graphs of population trends from the Living Planet Index
# for a number of vertebrate species from 1970 to 2014.

# Data 
LPI <- read.csv("data/LPI_data_loops.csv")

# Single plot examining Griffon vulture change between 1970-2014 in 
# Croatia and Italy:

vulture <- filter(LPI, Common.Name == "Griffon vulture / Eurasian griffon")
vultureITCR <- filter(vulture, Country.list == c("Croatia", "Italy"))

(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                              # Changing point size
    geom_smooth(method = lm, aes(fill = Country.list)) +                # Adding a linear model fit and colour-coding by country
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels = c("Croatia", "Italy")) +               # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                # Removing the background grid lines                
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),           # Adding a 0.5cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),              # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position = c(0.9, 0.9)))               # Setting the position for the legend - 0 is left/bottom, 1 is top/right

# above - using theme_bw() but making lots of modifications
# want to reuse to have consistent plots
# make function for own personalised theme:

theme.my.own <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9))
}

# now can make same plot but instead of all code, just add theme.my.own

(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                                
    geom_smooth(method = lm, aes(fill = Country.list)) +                    
    theme.my.own() +  # Adding new theme
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               
    scale_colour_manual(values = c("#EE7600", "#00868B"),               
                        labels = c("Croatia", "Italy")) +                 
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear"))


# again! - now filter data just for UK
LPI.UK <- filter(LPI, Country.list == "United Kingdom")

# Pick 5 species and make scatterplots with linear model fits that show 
# how the population has varied through time
house.sparrow <- filter(LPI.UK, Common.Name == "House sparrow")
great.tit <- filter(LPI.UK, Common.Name == "Great tit")
corn.bunting <- filter(LPI.UK, Common.Name == "Corn bunting")
meadow.pipit <- filter(LPI.UK, Common.Name == "Meadow pipit")

(house.sparrow_scatter <- ggplot(house.sparrow, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "House sparrow"))

(great.tit_scatter <- ggplot(great.tit, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Great tit"))

(corn.bunting_scatter <- ggplot(corn.bunting, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Corn bunting"))

(meadow.pipit_scatter <- ggplot(meadow.pipit, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Meadow pipit"))


# arrange all 4 plots in panel using gridExtra package and save file
panel <- grid.arrange(house.sparrow_scatter, great.tit_scatter, 
                      corn.bunting_scatter, meadow.pipit_scatter, ncol = 2)
ggsave(panel, file = "plots/Pop_trend_panel.png", width = 10, height = 8)
dev.off() # to close the image


# still repeating lots of code (and only 4 graphs) - use loop

# first, create list of spp 
Sp_list <- list(house.sparrow, great.tit, corn.bunting, meadow.pipit)

# loop!
for (i in 1:length(Sp_list)) {
  # create dataframe for each spp
  data <- as.data.frame(Sp_list[i])
  # object to hold spp name
  sp.name <- unique(data$Common.Name)
  # create plot
  plot <- ggplot(data, aes(x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = sp.name)
  # save plots as .pdf (could be .png)
  ggsave(plot, file = paste("plots/", sp.name, ".pdf", sep = ""),
         scale = 1)
  # also print plot to screen
  print(plot)
}

