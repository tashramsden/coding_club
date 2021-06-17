##%######################################################%##
#                                                          #
####                 Data manipulation 1                ####
####                     The basics                     ####
####             Natasha Ramsden 06/05/2021             ####
#                                                          #
##%######################################################%##

# Introduction ----

# Basic base R for data manipulation
# Tidy data and using tidyr
# Common and useful dplyr functions

# Analysing the EmpetrumElongation.csv dataset

# This dataset represents annual increments in stem growth, measured on 
# crowberry shrubs (Empetrum hermaphroditum) on a sand dune system. The Zone 
# field corresponds to distinct zones going from closest (2) to farthest 
# (7) from the sea.


# Libraries ----
library(tidyr)
library(dplyr)

# Set working directory ----
setwd("04_data_manipulation_1")

# Import data ----
elongation <- read.csv("EmpetrumElongation.csv", header = TRUE)

# Check import and preview data
head(elongation)  # first few rows
str(elongation)  # data type of each variable

# Selecting data with $
elongation$Indiv  # prints all ID codes in data set
length(unique(elongation$Indiv))  # returns no of distinct shrubs in dataset 
# = 114

# Selecting data with []
elongation[2,5]  # 2nd row, 5th column
elongation[6, ]  # all info for row 6

# Combination of $ and []
elongation[6, ]$Indiv  # returns ID of shrub in 6th row
# Easy to call columns by their names rather than counting where they are!

# Selecting using logical operators
# access values for individual w ID 603
elongation[elongation$Indiv == 603, ]


# Logical operators ----

# R will evaluate the expression and return obs for which the condition is met
# == equals exactly
# < <= smaller than, smaller than/equal to
# > >= biger than, bigger than/equal to
# != not equal to
# %in% belongs to one of the following (usually follwed by vector of poss values)
# & AND operator - chain together two conditions where both must be met
# | OR operator - chain together two conditions where at least one must be met
# ! NOT operator - to specify things that should be omitted

elongation[elongation$Zone < 4, ]  # data for zones 2-3

elongation[elongation$Zone <= 4, ]  # data for zones 2,3,4
elongation[!elongation$Zone >= 5, ]  # exactly equivalent to above

# data for zones 2 and 7
elongation[elongation$Zone == 2 | elongation$Zone == 7, ]  
# data for shrubs in zone 2 whose ID is between 300 and 400
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ]


# Useful vector sequences ----
# c(300:400) - numbers between 300 and 400
# seq() - create seq, incrementing by specified amount
# eg. seq(300, 400, 10) 
# rep() - create repetitions of elements
# eg rep(c(1,2), 3) - gives 1 2 1 2 1 2


# Changing variable names and values in a data frame ----

# create a working copy of our object (so not overwritten)
elong2 <- elongation

names(elong2)  # returns column names
names(elong2)[1] <- "zone"  # changes Zone to zone
names(elong2)[2] <- "ID"  # change Indiv to ID

# suppose there's a mistake in the data
# value 5.1 for individual 373 in yr 2008 should be 5.7

# option 1: use row and column no
# compact, need to know exactly where value is - might change if data updated
elong2[1,4] <- 5.7
# option 2: use logical conditions for more control
# longer but more control - will run even if obs moves in dataset
elong2[elong2$ID == 373, ]$X2008 <- 5.7


# Creating a factor ----

# check the classes
str(elong2)
# zone shows as int but should be factor
elong2$zone <- as.factor(elong2$zone)  # converting original class
# now a factor w 6 levels

# change factor levels too
# see levels
levels(elong2$zone)
# overwrite original levels w new names
# must be same length & pay attention to order!
levels(elong2$zone) <- c("A", "B", "C", "D", "E", "F")


# Tidy data and using tidyr----
# each row represents an observations
# each column represents a variable
# = much longer dataframe (more rows) - often called long format
# each grouping factor will have its own column - can be split directly in R

# thinking about tidy data before importing is good too - makes life easier!
# once in right format, easy to analyse and visualise results


# Gather ----

# tidy elongation dataset - want separate columns for year and stem length
# use tidyr gather() function 
elongation_long <- gather(elongation, Year, Length,
          # in this order: data frame, key, value
                          c(X2007, X2008, X2009, X2010, X2011, X2012))
                          # specify which columns to gather
# here, we want the lengths (value) to be gathered by year (key)

# the REVERSE! spread() is the inverse function
elongation_wide <- spread(elongation_long, Year, Length)

# the above gather() where we specified which columns to gather was fine 
# because there were only a few to gather

# but, if there's a huge dataset w 100 genes eg, 
# could specify column numbers instead
elongation_long2 <- gather(elongation, Year, Length, c(3:8))


# Easy to visualise now! ----
boxplot(Length ~ Year, data = elongation_long,
        xlab = "year", ylab = "Elongation (cm)",
        main = "Annual growth of Empetrum hermaphroditum")


# dplyr - most common and useful functions!

# Rename variables ----
# rename() - 1st var=data frame
#          - 2nd (and 3rd etc) var takes the form New name = Old name
elongation_long <- rename(elongation_long, zone = Zone, id = Indiv,
                          year = Year, length = Length)
str(elongation_long)

# base R equivalent - names(elongation_long) <- c("zone", "indiv", "year", "length")


# Filter rows and select columns ----
# filter() - subset rows w logical operators
# select() - specify which columns to keep
# select() often clashes w functions in other packages!
# !!!! use notation dplyr::select()

# Filter rows ----
# keep obs from zones 2 and 3 only, and from yrs 2009 to 2011
# can use multiple diff condition separated by comma
elong_subset <- filter(elongation_long, zone %in% c(2, 3), 
                       year %in% c("X2009", "X2010", "X2011"))

# For comparison, the base R equivalent would be (not assigned to an object here):
# elongation_long[elongation_long$zone %in% c(2,3) & elongation_long$year %in% c("X2009", "X2010", "X2011"), ]

# used %in% here to match exact list of values
# if want to keep obs within a numeric range - can use:
# 2 logical statements in filter(), eg length > 4 & length <= 6.5
# or
# between(), eg between(length, 4, 6.5) - avoids repetition, easy

# Select columns ----
# eg let's get rid of zone column
elong_no.zone <- dplyr::select(elongation_long, id, year, length)
# or
elong_no.zone <- dplyr::select(elongation_long, -zone)  
# minus removes the column

# For comparison, the base R equivalent would be (not assigned to an object here):
# elongation_long[ , -1]  # removes first column

# select() lets you rename and reorder columns on the fly
elong_no.zone <- dplyr::select(elongation_long, Year = year,
                               Shrub.ID = id, Growth = length)


# mutate() your dataset by creating new columns ----

# create a new column
# eg total growth
# first on the original wide-format elongation data
elong_total <- mutate(elongation, total.growth = X2007 + X2008 + X2009 + 
                        X2010 + X2011 + X2012)

# now on the tidy long-format data elongation_long
# group_by() and summarise()


# group_by() certain factors to perform operations on chunks of data ----
# don't see visible change to data, creates an internal grouping structure
# each subsequent function will use these groups and not whole dataset as input
# useful for summary stats
elong_grouped <- group_by(elongation_long, id)
# data now grouped by id - same shrubs grouped

# elong_grouped and elongation_long look the same
# but, let's use summarise() to calculate tot growth for each individual 
# over the years


# summarise() data with a range of summary statistics ----

# SUMMARISING OUR DATA
summary1 <- summarise(elongation_long, total.growth = sum(length))
summary2 <- summarise(elong_grouped, total.growth = sum(length))

# summary1 = sum of ALL growth increments in dataset (all individuals and years)
# summary2 = tot growth PER INDIVIDUAL

# diff summary stats
summary3 <- summarise(elong_grouped, total.growth = sum(length),
                      mean.growth = mean(length),
                      sd.growth = sd(length))

# we do lose all other columns not specified at grouping stage or 
# in summary operation
# ALWAYS CREATE A NEW OBJECT FOR SUMMARISED DATA -so that full data not lost!
# can then merge back some info later, see below...


# ..._join() datasets based on shared attributes ----

# Let’s imagine that the growth data we have been working with actually comes 
# from an experiment where some plants where warmed with portable 
# greenhouses (W), others were fertilised (F), some received both 
# treatments (WF) and some were control plants (C). We will import this data 
# from the file EmpetrumTreatments.csv, which contains the details of which 
# individuals received which treatments, and join it with our main dataset 
# elongation_long. We can do this because both datasets have a column 
# representing the ID of each plant: this is what we will merge by.

# diff methods will handle data not shared by both tables in diff ways 
# think about which obs need to keep/which want to be dropped
# if in doubt full_join() will keep everything
# https://dplyr.tidyverse.org/reference/join.html

# eg keep all info in elong_long and have treatment code of every individual 
# so we will use left_join()

treatments = read.csv("EmpetrumTreatments.csv", header = TRUE, sep = ";")
head(treatments)

# join the 2 dataframes by ID code
# the column names are spelled differently so we need to tell the function 
# which columns represent a match
# we have 2 columns that contain the same info in both datasets: 
# zone and individual ID
experiment <- left_join(elongation_long, treatments, 
                        by = c("id" = "Indiv", "zone" = "Zone"))
# new object is same length as first data frame and treatemtns corresponding to 
# each plant have been added

# equivalent base R (works well too)
# experiment2 <- merge(elongation_long, treatments, 
                     # by.x = c("zone", "indiv"), by.y = c("Zone", "Indiv"))
# same result!

# let's check if the treatments had any effect on growth
boxplot(length ~ Treatment, data = experiment, ylab = "Stem elongation (cm)")
# effects of warming (W) and fertilisation (F) treatments on crowberry growth 
# (fictional data!)









