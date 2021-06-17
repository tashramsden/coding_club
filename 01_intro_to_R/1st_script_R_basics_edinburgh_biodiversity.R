# Coding Club Workshop 1 - R Basics
# Learning how to import and explore data, and make graphs about Edinburgh's biodiversity
# Written by Natasha Ramsden 03/05/2021 

# install.packages("package-name") - in console
# library("package-name")

library(dplyr)

setwd("01_intro_to_R")

edidiv <- read.csv("edidiv.csv")

# Check data set read in correctly
head(edidiv) # Displays the first few rows
tail(edidiv) # Displays the last rows
str(edidiv)  # Tells you whether the variables are continuous, integers, categorical or characters

# The taxonGroup variable shows as a character, but we want it to be a factor (catergorical variable)
edidiv$taxonGroup <- as.factor(edidiv$taxonGroup)
class(edidiv$taxonGroup) # Tells you what type of variable we're dealing with

# More exploration
dim(edidiv) # Displays number of rows and columns
summary(edidiv) # Gives summary of data
summary(edidiv$taxonGroup) # Gives summary of that particular variable

# Our edidiv object has occurrence records of various species collected in Edinburgh from 2000 to 2016. 
# To explore Edinburgh’s biodiversity, we will create a graph showing how many species were recorded in each taxonomic group.

# species richness = tot num spp in a given place/group
# split edidiv into taxonomic groups

Beetle <- filter(edidiv, taxonGroup == "Beetle") # The first argument of the function is the data frame, the second argument is the condition you want to filter on.
Bird <- filter(edidiv, taxonGroup == "Bird")
Butterfly <- filter(edidiv, taxonGroup == "Butterfly")
Dragonfly <- filter(edidiv, taxonGroup == "Dragonfly")
Flowering.Plants <- filter(edidiv, taxonGroup == "Flowering.Plants")
Fungus <- filter(edidiv, taxonGroup == "Fungus")
Hymenopteran <- filter(edidiv, taxonGroup == "Hymenopteran")
Lichen <- filter(edidiv, taxonGroup == "Lichen")
Liverwort <- filter(edidiv, taxonGroup == "Liverwort")
Mammal <- filter(edidiv, taxonGroup == "Mammal")
Mollusc <- filter(edidiv, taxonGroup == "Mollusc")

# unique() identifies different observations in a group
# combined with length() this counts the num of diff obs in a group
beetle_sp <- length(unique(Beetle$taxonName))
bird_sp <- length(unique(Bird$taxonName))
butterfly_sp <- length(unique(Butterfly$taxonName))
dragonfly_sp <- length(unique(Dragonfly$taxonName))
flower_sp <- length(unique(Flowering.Plants$taxonName))
fungus_sp <- length(unique(Fungus$taxonName))
hym_sp <- length(unique(Hymenopteran$taxonName))
lichen_sp <- length(unique(Lichen$taxonName))
liverwort_sp <- length(unique(Liverwort$taxonName))
mammal_sp <- length(unique(Mammal$taxonName))
mollusc_sp <- length(unique(Mollusc$taxonName))

# Create a vector and plot it
biodiv <- c(beetle_sp, bird_sp, butterfly_sp, dragonfly_sp, flower_sp, fungus_sp, hym_sp, lichen_sp, liverwort_sp, mammal_sp, mollusc_sp)
names(biodiv) <- c("Beetle",
                   "Bird",
                   "Butterfly",
                   "Dragonfly",
                   "Flowering.Plants",
                   "Fungus",
                   "Hymenopteran",
                   "Lichen",
                   "Liverwort",
                   "Mammal",
                   "Mollusc")

barplot(biodiv)
# help(barplot) # for help with barplot
# help(par) # for help with general plotting

png("sp_richness.png", width=1600, height=600)  # look up the help for this function: you can customise the size and resolution of the image
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5,
        main="Figure 1. Species richness of several taxa in Edinburgh. Records are based on data from the NBN Gateway during the period 2000-2016.")
dev.off()
# The cex code increases the font size when greater than one (and decreases it when less than one). 


# Create a dataframe and plot it

# Creating an object called "taxa" that contains all the taxa names
taxa <- c("Beetle",
          "Bird",
          "Butterfly",
          "Dragonfly",
          "Flowering.Plants",
          "Fungus",
          "Hymenopteran",
          "Lichen",
          "Liverwort",
          "Mammal",
          "Mollusc")
# Turning this object into a factor, i.e. a categorical variable
taxa_f <- factor(taxa)

# Combining all the values for the number of species in an object called richness
richness <- c(beetle_sp, bird_sp, butterfly_sp, dragonfly_sp, flower_sp, fungus_sp, hym_sp, lichen_sp, liverwort_sp, mammal_sp, mollusc_sp)

# Creating the data frame from the two vectors
biodata <- data.frame(taxa_f, richness)

# Saving the file
write.csv(biodata, file="biodata.csv")  # it will be saved in your working directory

png("sp_richness2.png", width=1600, height=600)
barplot(biodata$richness, names.arg=c("Beetle",
                                      "Bird",
                                      "Butterfly",
                                      "Dragonfly",
                                      "Flowering.Plants",
                                      "Fungus",
                                      "Hymenopteran",
                                      "Lichen",
                                      "Liverwort",
                                      "Mammal",
                                      "Mollusc"),
        xlab="Taxa", ylab="Number of species", ylim=c(0,600))
dev.off()




# Wingspan data
data <- read.csv("wingspan_data.csv")
unique(data$bird_sp)

sparrow_data <- filter(data, bird_sp == "sparrow")
kingfisher_data <- filter(data, bird_sp == "kingfisher")
eagle_data <- filter(data, bird_sp == "eagle")
humming_data <- filter(data, bird_sp == "hummingbird")

mean_sparrow <- mean(sparrow_data$wingspan)
mean_king <- mean(kingfisher_data$wingspan)
mean_eagle <- mean(eagle_data$wingspan)
mean_humming <- mean(humming_data$wingspan)

spp <- c("Sparrow", "Kingfisher", "Eagle", "Hummingbird")
spp_f <- factor(spp)

wingspan <- c(mean_sparrow, mean_king, mean_eagle, mean_humming)

wingdata <- data.frame(spp_f, wingspan)

write.csv(wingdata, file="mean_wingspan_data.csv")

png("mean_wingspan.png", width=800, height=600)
barplot(wingdata$wingspan, names.arg=c("Sparrow",
                                       "Kingfisher",
                                       "Eagle",
                                       "Hummingbird"),
        xlab="Species", ylab="Mean wingspan (cm)", ylim=c(0, 200), col="gold")
dev.off()


