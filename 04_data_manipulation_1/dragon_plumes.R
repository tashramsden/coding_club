##%######################################################%##
#                                                          #
####              Length of fire plumes (cm)            ####
####          breathed by dragons of different          ####
####          species when fed different spices         ####
####             Natasha Ramsden 10/05/2021             ####
#                                                          #
##%######################################################%##

# Objectives ----

# Answer the following:
# Which spice triggers the most fiery reaction?
# And the least?

# tidy data
# create boxplot for each species showing effect on plume size

# HOWEVER, field assistant made some mistakes that need correcting:

# 1. fourth treatment wasn't paprika, it was turmeric

# 2. calibration error with measuring device for tabasco trail - 
# but only for Hungarian Horntail species
# all measurments are 30cm higher than they should be

# 3. lengths given in cmbut really it would make sense to convert them to meters

# Set working directory ----
setwd("04_data_manipulation_1")

# Libraries ----
library(tidyr)
library(dplyr)


# Importing data ----
dragon_data <- read.csv("dragons.csv", header = TRUE)
head(dragon_data)
str(dragon_data)

# dragons will be the working copy of the data set
dragons <- dragon_data


# Tidying the data ----

# species should be a factor not str
dragons$species <- as.factor(dragons$species)
str(dragons)

# the fourth treatment was turmeric not paprika
dragons <- rename(dragons, turmeric = paprika)

# tidy format
dragons <- gather(dragons, spice, length,
                  c(tabasco, jalapeno, wasabi, turmeric))

# hungarian horntail measurements for tabasco all 30cm too high
original <- dragons[dragons$species == "hungarian_horntail" &
                      dragons$spice == "tabasco", ]
correct_values <- c(original$length) - 30
correct_values

# overwrite original values with corrected ones
dragons[dragons$species == "hungarian_horntail" &
          dragons$spice == "tabasco", ]$length <- correct_values


# Alternative!
# dragons.2 <- mutate(dragons, tabasco = ifelse(species == 'hungarian_horntail', tabasco - 30, tabasco))
# This creates (overwrites) the column tabasco using the following logic: if the species is Hungarian Horntail, deduct 30 from the values in the (original) tabasco column; if the species is NOT horntail (i.e. all other species), write the original values.


# add a new column for length in m not cm
dragons <- mutate(dragons, length_m = length / 100)


# Create data set for each species so they can be plotted separately ----

horntail <- filter(dragons, species == 'hungarian_horntail')
shortsnout <- filter(dragons, species == 'swedish_shortsnout')
welsh.green <- filter(dragons, species == 'welsh_green')


# Plot the results for each species ----

# quick plots
boxplot(length_m ~ spice, data = horntail)
boxplot(length_m ~ spice, data = shortsnout)
boxplot(length_m ~ spice, data = welsh.green)


par(mfrow=c(1, 3))      # splits your plotting device into 3 columns where 
                        # the plots will appear, so all the plots will be side 
                        # by side.

boxplot(length_m ~ spice, data = horntail,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Hungarian Horntail')


boxplot(length_m ~ spice, data = welsh.green,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Welsh Green')


boxplot(length_m ~ spice, data = shortsnout,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Swedish Shortsnout')


# Conclusions ----

# Jalapeno highest (statistically?) for all
# Turmeric lowest for all