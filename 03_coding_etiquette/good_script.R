# Etiquette ----

# Organising scripts into sections ----

# Comments and headings are great
# Adding 4 - after a comment will make that line into a Section
# View sections in the button in the top right of the script window that has 5 
# stacked lines
# you can also collapse sections which is useful for long scripts
# Edit | Folding | Collapse all will collapse all sections / Expand all

# Typical sections:
# Introduction - what does this script do? author names, contact details, date
# Libraries - packages - and add comments for what each package is for
# Functions - define any functions here
# Setting the working directory - helps keep all data, scripts, image outputs 
# etc in a single folder
# Importing data - what data are you using, where is it stored?
# The different sections of the analysis - what is the logical workflow?
# Outputs of analysis - 
# create separate folder for eg images
# save plots as pdf AND png - png good for inserting into docs and 
# presentations, pdf vector based- dont lose quality when zoomed


# Following a coding syntax etiquette ----

# Naming files and objects
# Be specific when naming files - easier to find later -and end in .R, no crazy
# spaces/characters
# Object names should be concise and meaningful/specific
# object, variable and function names should be lowercase
# function names should be verbs, eg. calc.sp.richness
# script file - separate words w _
# object/variables - lowercase, separate w _
# functions - lowercase, separate w .

# Spaces
# put spaces around operators, = etc and always after the comma not before
# : and :: don't need spaces
# in-line commenting - place 2 spaces after code, then #, then 1 space, comment

# Curly braces
# opening curly brace should never go on its own line and should always 
# be followed by a new line
# closing curly brace - always on own line, unless followed by else
# always indent code inside curly braces

# Line length
# official convention = 80 chars per line
# RStudio - Tools | Global options | code | display | show margin | 80chars
# Pipes %>% 
# keep pipe operator at end of line, continue pipe on next line
# ggplot2
# keep the + at end of line, continue adding new layers on next line

# Indentation
# if a command runs over multiple lines, indent second line to line up w start
# of definition

# Tidying up old scripts and data frames
# better to do as you go along!
# use RStudio to help! code | reformat code -> this will add spaces BUT
# will also add a new line after every comma! (back up scripts before trying!) 
# DO IT YOURSELF!
# renaming old objects and variables - find and replace
# can do this only on selected lines by selecting then ticking "in selection"
# eg
names(dataframe) <- gsub(".", "_", names(dataframe), fixed = TRUE)
# This code takes all of the variable names in the imaginary dataset `dataframe`
# and replaces `.` with `_`
# Depending on the naming style you are using, you might want to go the other 
# way around and use `.` in all variable names
names(dataframe) <- tolower(names(dataframe))
# This code makes all of the variable names in the imaginary dataset lowercase
colnames(dataframe)[colnames(dataframe) == 'Old_Complicated_Name'] <- 'new.simple.name'
# Renaming an individual column in the imaginary dataset
# RStudio add-ins
# access add-ins by clicking Tools | Addins
# for full list of RStudio plugins, run 
install.packages('addinslist')

# eg for boxes around titles!
devtools::install_github("ThinkRstat/littleboxes")



# Example ----

# Introduction ----

##%######################################################%##
#                                                          #
####    Analysing vertebrate population change based    ####
####             on the Living Planet Index             ####
####                   Data available                   ####
####       from http://www.livingplanetindex.org/       ####
####                  Natasha Ramsden                   ####
####       natasha.ramsden18@gmail.com 05-05-2021       ####
#                                                          #
##%######################################################%##

# Libraries ----
library(tidyr)  # Formatting data for analysis
library(dplyr)  # Manipulating data
library(ggplot2)  # Visualising results
library(readr)  # Manipulating data

# Defining functions ----
# A custom ggplot2 function
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}

# Working directory ----
setwd("03_coding_etiquette")
# getwd()

# Import data ----
LPI <- read.csv("LPIdata_CC.csv")

# Formatting data ----
LPI2 <- gather(LPI, "year", "abundance", 9:53)  # Transforming the data from wide to long format, some blank cells may disappear
# gather function requires tidyr package
LPI2$year <- parse_number(LPI2$year)  # Do you see awkward Xs before all the years? This gets rid of them.
names(LPI2)  # Check what the different variables are called
names(LPI2) <- tolower(names(LPI2))  # Make all variable names lower case

# When manipulating data it's always good check if the variables have stayed how we want them
# Use the str() function
str(LPI2)

# Abundance is a character variable, when it should be numeric, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)

# Calc summary stats for each biome in the LPI database ----
levels(LPI2$biome)  # list all biomes

LPI_biome_summ <- LPI2 %>%  # use of pipe operator
  group_by(biome) %>%  # Group by biome
  summarise(populations = n())  # Create columns, number of populations

# Visualising the number of populations in each biome with ggplot2 package ---- 
# Adding brackets around this means it is called - otherwise would create barplot and then have to call barplot to see it
(barplot <- ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +  
   theme.LPI() +                     # Use of personal theme function
   ylab("Number of populations") +
   xlab("Biome") +
   theme(legend.position = "none"))  # Removal of legend for simplicity

# Saving the plots ----
png(file="img/biome_pop.png", width = 1000, height = 2000)  # Note that png() uses pixel values for width and height
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()  # This tells R you are done with the plotting and it can save the file

pdf(file="img/biome_pop.pdf",  width = 13.33, height = 26.66)  # pdf() uses inches
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()
