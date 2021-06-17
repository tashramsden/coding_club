############################################################
#                                                          #
#                  Data visualisation 1                    #
#               Natasha Ramsden 12/05/2021                 #
#                                                          #
############################################################

# Introduction ----


# Set working directory ----
setwd("06_data_vis_1")

# Libraries ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Import data from the Living Planet Index ----
# population trends of vertebrate species from 1970 to 2014
LPI <- read.csv("LPIdata_CC.csv")

# Tidy data ----
LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)

# Remove X from in front of years
LPI2$year <- parse_number(LPI2$year)

# Check structure
str(LPI2)

# Abundance is a character varaible but should be numeric
LPI2$abundance <- as.numeric(LPI2$abundance)


# Very large dataset - first few graphs will focus on one species
# all species
unique(LPI2$Common.Name)

# The following will focus on the song thrushes
thrush <- filter(LPI2, Common.Name == "Song thrush")
head(thrush)

# There are lots of NAs in this dataframe
# Get rid of these using na.omit()
thrush <- na.omit(thrush)


# Histograms to visualise data distribution ----

# with base R graphics
base_hist <- hist(thrush$abundance)

# with ggplot
# entire ggplot code in brackets so that graph created and shown 
(thrush_hist <- ggplot(thrush, aes(x = abundance)) +
    geom_histogram())

# default settings not great
# lots of unnecessary grey space, axis labels small, bars blend with each other
# let's beautify!
(thrush_hist <- ggplot(thrush, aes(x = abundance)) +
    # Change bandwidth and colours
    geom_histogram(binwidth = 250, colour = "#698B22", fill = "#B3EE3A") +
    # Add a line for mean abundance
    geom_vline(aes(xintercept = mean(abundance)), 
               colour = "#00008B", linetype = "dashed", size=1) +
    # Changing theme to get rid of grey background
    theme_bw() +
    ylab("Count\n") +
    xlab("\nSong thrush abundance") +
    # Change font size of axis labels and title
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),
          # face="plain" is default - can change to italic, bold etc
          panel.grid = element_blank(),
          # Add 1cm margin around plot
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

# Histogram of Song thrush abundance in populations included in the LPI dataset.
# Blue line shows mean abundance.

# Data v skewed - typical distribution of count abundance data

# ggplot2 jargon ----

# geom
# geometric object - defines type of plot
# reads data in the aesthetics mapping to know which variables to use

# aes
# aesthetics - specify data sources and variables
# AND the properties of the graph which depend on those variables
# For instance, if you want all data points to be the same colour, you would define the colour = argument outside the aes() function;
# if you want the data points to be coloured by a factor’s levels (e.g. by site or species), you specify the colour = argument inside the aes().

# stat
# applies some statistical transformation to the data
# stat_smooth(method = 'lm') displays a linear regression line and confidence interval ribbon on top of a scatter plot (defined with geom_point())

# theme
# set of visual parameters
# can use pre-defined themes/create own/modify existing
# Examples of elements within themes are axis.text, panel.grid, legend.title, and so on. You define their properties with elements_...() functions: element_blank() would return something empty (ideal for removing background colour), while element_text(size = ..., face = ..., angle = ...) lets you control all kinds of text properties.

# Elements build on top as code progresses - elements written later could hide previous elements


# Colourpicker ----
# hex codes
# install.packages("colourpicker")
# Go into addins - Colour picker - can see all R colours and pick combos
# c("#B3EE3A", "#698B22", "#00008B")



# Scatter plot to examine population change over time ----

# Let's say we're interested in how the SOng thrush populations have changed
# between 1970 and 2017 in the United Kingdom and Poland

# Filter data to get records only from UK and Poland 
thrushUKPO <- filter(thrush, Country.list %in% c("United Kingdom", "Poland"))

# Using default base graphics
plot(thrushUKPO$year, thrushUKPO$abundance, col = c("#B3EE3A", "#FF7F50"))

# Using default ggplot2 graphics
(thrush_scatter <- ggplot(thrushUKPO, aes(x = year, y = abundance, 
                                          colour = Country.list)) +
    geom_point())
# ggplot2 allows points to be coloured diff based on country

# Beautify!
(thrush_scatter <- ggplot(thrushUKPO, aes(x = year, y = abundance, 
                                          colour = Country.list)) +
    # Change point size
    geom_point(size = 2) +
    # Addlinear model fit, colour by country
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    theme_bw() +
    # add custom colours for solid geoms (ribbon)
    scale_fill_manual(values = c("#B3EE3A", "#FF7F50")) +
    # add custom colours for lines and points
    scale_colour_manual(values = c("#B3EE3A", "#FF7F50"), 
                        # add custom labels for legend
                        labels = c("Poland", "United Kingdom")) +
    ylab("Song thrush abundance\n") +
    xlab("\nYear") +
    # making the years at a bit of an angle)
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = c(0.75, 0.8)))                                 # Setting legend position - 0 is left/bottom, 1 is top/right)     

# Population trends of Song thrush in Poland and the United Kingdom.
# Data points represent raw data with a linear model fit and 95% confidence 
# intervals. Abundance is measured in number of breeding individuals.


# Special characters/superscript in axes ----
# If your axis labels need to contain special characters or superscript, you can get ggplot2 to plot that, too. It might require some googling regarding your specific case, but for example, this code ylabs(expression(paste('Grain yield',' ','(ton.', ha^-1, ')', sep=''))) will create a y axis with a label reading Grain yield (ton. ha-1).



# Boxplot to examine whether thrush abundance differs between Poland and the UK ----

# Box plots are very informative as they show the median and spread of your 
# data, and allow you to quickly compare values among groups. If some 
# boxes don’t overlap with one another, you probably have significant 
# differences, and it’s worth to investigate further with statistical tests.

(thrush_boxplot <- ggplot(thrushUKPO, aes(Country.list, abundance)) +
   geom_boxplot())

# Beautify
(thrush_boxplot <- ggplot(thrushUKPO, aes(Country.list, abundance)) +
    geom_boxplot(aes(fill = Country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#B3EE3A", "#FF7F50")) +
    scale_colour_manual(values = c("#B3EE3A", "#FF7F50")) +
    ylab("Song thrush abundance\n") +
    xlab("\nCountry") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                     
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.position = "none"))  # Removing legend - not needed with only 2 factors

# Song thrush abundance in Poland and the UK


# Barplot to compare species richness of a few European countries ----

# We are now going to calculate how many species are found in the LPI dataset 
# for some European countries, and plot the species richness.

# Calculating species richness
# Using pipes %>% from dplyr (use ctrl shift m) ----

richness <- LPI2 %>% filter(Country.list %in% c("United Kingdom", "Germany",
                                                "France", "Netherlands",
                                                "Italy")) %>% 
  group_by(Country.list) %>% 
  mutate(richness = (length(unique(Common.Name))))
# creates new column based on how many unique common names there are in a country

# Plotting species richness
(richness_barplot <- ggplot(richness, aes(x = Country.list, y = richness)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    theme_bw() +
    ylab("Species richness\n") +                             
    xlab("Country")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

# Species richness in five European countries (based on LPI data).


# Lots of repeated code - and important to have consistent formatting acorss 
# graphs for the same project - see data visualisation 2 on how to reuse themes 
# in all your ggplots


# Using facets and creating panels ----

# Sometimes, displaying all the data on one graph makes it too cluttered. 
# If we wanted to examine the population change of thrushes across all the 
# countries, rather than Poland and the uK, we would have 5 populations 
# on the same graph:

# Plot the population change for all countries
(thrush_scatter_all <- ggplot(thrush, aes (x = year, y = abundance,
                                           colour = Country.list)) +
   geom_point(size = 2) +                                               # Changing point size
   geom_smooth(method = "lm", aes(fill = Country.list)) +               # Adding linear model fit, colour-code by country
   theme_bw() +
   ylab("Song thrush abundance\n") +                             
   xlab("\nYear")  +
   theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
         axis.text.y = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                        
         panel.grid = element_blank(),                                   # Removing the background grid lines               
         plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
         legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
         legend.title = element_blank(),                                 # Removing the legend title
         legend.position = "right")) 

# This is very cluttered! 
# And the strange (possibly erroneous) values for Sweden in 70s mean 
# that we can't see trends for other countries clearly

# By adding a facetting layer, we can split the data in multiple facets 
# representing the different countries. This is done using facet_wrap().

# Plot the population change for countries individually
(thrush_scatter_facets <- ggplot(thrush, aes (x = year, y = abundance, 
                                              colour = Country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = Country.list)) +               # Adding linear model fit, colour-code by country
    facet_wrap(~ Country.list, scales = "free_y") +  # THIS LINE CREATES THE FACETTING
    theme_bw() +
    ylab("Song thrush abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))   


# Population change of Song thrush across the world, from the LPI dataset.

# Some useful arguments to include in facet_wrap()are nrow = or ncol = , 
# specifying the number of rows or columns, respectively. You can also see 
# that we used scales = "free_y", to allow different y axis values because of 
# the wide range of abundance values in the data. You can use “fixed” when 
# you want to constrain all axis values.


# Note: some of these population trends do weird things, possibly because there 
# are many sub-populations being monitored within a country, so in practice we 
# probably would not fit a single regression line per country.


# Might want to arrage multiple figures together to create a panel
# Do this using grid.arrange() from the package gridExtra

# This is rubbish! - graphs too stretched, everything wrong!
grid.arrange(thrush_hist, thrush_scatter, thrush_boxplot, ncol = 1)

# Fixing the problems - adding ylab() again overrides the previous settings
(panel <- grid.arrange(
  thrush_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +   # adding labels to the different plots
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  thrush_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  thrush_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.text = element_text(size = 12, face = "italic"),     
          legend.title = element_blank(),                                   
          legend.position = c(0.85, 0.85)), # changing the legend position so that it fits within the panel
  
  ncol = 1)) # ncol determines how many columns you have

# If you want to change the width or height of any of your pictures, you can 
# add either ` widths = c(1, 1, 1) or heights = c(2, 1, 1)` for example, to the
# end of your grid arrange command. In these examples, this would create three 
# plots of equal width, and the first plot would be twice as tall as the other 
# two, respectively. This is helpful when you have different sized figures or 
# if you want to highlight the most important figure in your panel.

# To get around the too stretched/too squished panel problems, we will save the 
# file and give it exact dimensions using ggsave from the ggplot2 package. The 
# default width and height are measured in inches. If you want to swap to 
# pixels or centimeters, you can add units = "px" or units = "cm" inside the 
# ggsave() brackets, e.g. ggsave(object, filename = "mymap.png", width = 1000,
# height = 1000, units = "px". The file will be saved to wherever your working 
# directory is, which you can check by running getwd() in the console.

ggsave(panel, file = "thrush_panel2.png", width = 5, height = 12)

# Examining Song thrush populations from the LPI dataset. (a) shows histogram 
# of abundance data distribution, (b) shows a boxplot comparison of abundance 
# in Poland and the UK, and (c) shows population trends between 1970 and 2014 
# in Poland and the UK.





# Challenge ----

# 1. Choose 2 species from the LPI data and display their population trends 
# over time, using a scatterplot and a linear model fit

# 2. Using the same 2 species, filter the data to include only records from 
# 5 countries, and make a boxplot to compare how the abundance of those 2 
# species varies between the 5 countries.

# 1. ----

# choose sepcies
unique(LPI2$Common.Name)

# Population trends will be explored of:
# 'Reindeer / Caribou'
# 'Beluga whale'

arctic <- filter(LPI2, Common.Name %in% c('Reindeer / Caribou', 'Beluga whale'))

# Remove NAs
arctic <- na.omit(arctic)
str(arctic)

# basic
(arctic_scatter <- ggplot(arctic, aes(x = year, y = abundance)) +
    geom_point() +
    facet_wrap(~ Common.Name, scales = "free_y"))

# beautify!
(arctic_scatter <- ggplot(arctic, aes(x = year, y = abundance)) +
  geom_point(aes(colour = Country.list), size = 1.5, alpha = 0.6) +  # alpha controls transparency
  facet_wrap(~ Common.Name, scales = "free_y") +
  geom_smooth(method = "lm", aes(fill = Country.list, colour = Country.list)) +
  theme_bw() +
  ylab("Abundance\n") +
  xlab("\nYear") +
  scale_fill_manual(values = c("#1C86EE", "#8B3A62", "#6959CD", "#008B8B")) +
  scale_colour_manual(values = c("#1C86EE", "#8B3A62", "#6959CD", "#008B8B")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),                        
        panel.grid = element_blank(),                          
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        # plot.margin = unit(c(1,1,1,1), units = , "cm"),                
        legend.text = element_text(size = 12, face = "italic"),         
        legend.title = element_blank()))                                


# 2. ----
# Using the same 2 species, filter the data to include only records from 
# 5 countries, and make a boxplot to compare how the abundance of those 2 
# species varies between the 5 countries.

# basic
(arctic_box <- ggplot(arctic, aes(x = Country.list, y = abundance)) +
  geom_boxplot() +
  facet_wrap(~ Common.Name, scales = "free_y"))

# beautify
(arctic_box <- ggplot(arctic, aes(x = Country.list, y = abundance)) +
  geom_boxplot(aes(fill = Country.list)) +
  facet_wrap(~ Common.Name, scales = "free_y") +
  theme_bw() +
  scale_fill_manual(values = c("#1C86EE", "#8B3A62", "#6959CD", "#008B8B")) +
  scale_colour_manual(values = c("#1C86EE", "#8B3A62", "#6959CD", "#008B8B")) +
  ylab("Abundance\n") +
  xlab("\nCountry") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                     
        panel.grid = element_blank(),                     
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        strip.background = element_blank()))

# Not great because of high-abundance outliers for reindeer in Canada - let's 
# remove them for now (wouldn't do that for an analysis!)

(arctic_box <- ggplot(filter(arctic, abundance < 8000), aes(x = Country.list, y = abundance)) +
    geom_boxplot(aes(fill = Country.list)) +
    facet_wrap(~ Common.Name, scales = "free_y") +
    theme_bw() +
    scale_fill_manual(values = c("#1C86EE", "#8B3A62", "#6959CD", "#008B8B")) +
    scale_colour_manual(values = c("#1C86EE", "#8B3A62", "#6959CD", "#008B8B")) +
    ylab("Abundance\n") +
    xlab("\nCountry") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                     
          # plot.margin = unit(c(1,1,1,1), units = , "cm"),
          strip.background = element_blank(),
          legend.position = "none"))


(panel <- grid.arrange(
  arctic_scatter + ggtitle("Population change over time") +
    ylab("Abundance") + 
    xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = "cm")),
  
  arctic_box + ggtitle("Population size across countries") +
    ylab("Abundance") +
    xlab("Country") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = "cm")),
  
  ncol = 1))

# Instead, can use egg package - lines up plots together regardless of whether
# they have a legend or not

library(egg)

ggarrange(arctic_scatter + labs(title = 'Population change over time'), 
          arctic_box + labs(title = 'Population size across countries'))

# Population trends and abundance of two Arctic species across their range 
# according to the LPI dataset.

