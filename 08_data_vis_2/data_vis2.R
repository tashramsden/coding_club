#################################################################
#                                                               #
#                      DATA VISUALISATION 2                     #
#                   Vegetation of Magical Lands                 #
#                   Natasha Ramsden 10/08/2021                  #
#                                                               #
#################################################################

# Data info 
# Made-up data consisting of the abundance and height of different 
# plant species occurring in two magic lands: Hogsmeade and Narnia.

# Libraries ----
library(dplyr)  # For data manipulation
library(ggplot2)  # For data visualisation

# Working directory ----
setwd("08_data_vis_2")

# Load data ----
magic_veg <- read.csv("magic_veg.csv")

# Data exploration ----
str(magic_veg)
# land - the location within the land of magic (two possible lands: 
#                                               Narnia and Hogsmeade)
# plot - the plot number within each land
# year - the year the measurement was taken
# species - the species name (or code), Note that these are fake species!
# height - the imaginary canopy height at that point
# id - the id of each observation

# 1. Customise histograms ----

# no of spp in each plot
species_counts <- magic_veg %>% 
  group_by(land, plot) %>% 
  summarise(Species_number = length(unique(species)))

(hist <- ggplot(species_counts, aes(x = plot)) +
    geom_histogram())
# the above doesn't look right
# because using summarised data, have to tell R that already know how many spp
# there are per plot, do this by specifying stat:
(hist <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_histogram(stat = "identity"))

# Note: an equivalent alternative is to use geom_col (for column), which takes 
# a y value and displays it
(col <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_col())

# this is better, but spp from the 2 lands are grouped 
# (only grouped by plot not also land)
# separate lands by adding colour:
(hist <- ggplot(species_counts, aes(x = plot, y = Species_number,
                                    fill = land)) +
    geom_histogram(stat = "identity"))

# make the bars next to each other not stacked:
(hist <- ggplot(species_counts, aes(x = plot, y = Species_number,
                                    fill = land)) +
    geom_histogram(stat = "identity", position = "dodge"))

# plot only shows plot numbers 2, 4 and 6, want 1-6
# can use breaks = c(1, 2, 3, 4, 5, 6) or breaks = 1:6
# and add limits to plot axes
(hist <- ggplot(species_counts, aes(x = plot, y = Species_number,
                                    fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = 1:6) +
    scale_y_continuous(limits = c(0, 50)))

# a. Add titles, subtitles, captions, axis labels ----
(hist <- ggplot(species_counts, aes(x = plot, y = Species_number,
                                    fill = land)) +
   geom_histogram(stat = "identity", position = "dodge") +
   scale_x_continuous(breaks = 1:6) +
   scale_y_continuous(limits = c(0, 50)) +
   labs(title = "Species Richness by Plot",
        subtitle = "In the Magical Lands",
        caption = "Data from the Ministry of Magic",
        x = "\n Plot Number",
        y = "Number of Species \n"))

# theme()
# change font size of text, bold and italics (face = 'italic' or face = 'bold'),
# center title (hjust = 0.5)
# Note: if we wanted to specify different options for the x and y axis, we 
# could use axis.text.x or axis.title.x and axis.text.y or axis.title.y and 
# specify separate characteristics for each axis.

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number,
                                    fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = 1:6) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species Richness by Plot",
         x = "\n Plot Number",
         y = "Number of Species \n") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "italic"),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))


# b. Change plot background ----

# built in theme_bw() makes background white
# remove grid lines: panel.grid = element_blank():
# panel.grid encompasses several options: panel.grid.major, which in turn 
# governs panel.grid.major.x and panel.grid.major.y and the same for 
# panel.grid.minor

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number,
                                    fill = land)) +
   geom_histogram(stat = "identity", position = "dodge") +
   scale_x_continuous(breaks = 1:6) +
   scale_y_continuous(limits = c(0, 50)) +
   labs(title = "Species Richness by Plot",
        x = "\n Plot Number",
        y = "Number of Species \n") +
   theme_bw() +
   theme(panel.grid = element_blank(),
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 12, face = "italic"),
         plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))


# c. Fix legend, customise colours ----

# use the scale_...() functions to customise color code and legend

# scale_fill_manual(values = c("your-colour-1", "your-colour-2", ...)) function
# lets you decide on custom colour values for SOLID elements 
# (bars, boxplots, ribbons, etc.),

# its counterpart scale_colour_manual() works exactly the same for line 
# elements (points in a scatter plot, regression lines, box or column outlines,
# etc.). 

# make sure to include as many colours as there are factor levels in your data

# see data vis 1 for colour picker package info

# and change legend title

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number,
                                    fill = land)) +
   geom_histogram(stat = "identity", position = "dodge") +
   scale_x_continuous(breaks = 1:6) +
   scale_y_continuous(limits = c(0, 50)) +
   scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                     name = "Land of Magic") +
   labs(title = "Species Richness by Plot",
        x = "\n Plot Number",
        y = "Number of Species \n") +
   theme_bw() +
   theme(panel.grid = element_blank(),
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 12, face = "italic"),
         plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
         legend.title = element_text(face = "bold"),
         legend.position = "bottom",
         legend.box.background = element_rect(color = "grey", size = 0.3)))

# here legend labels are already named correctly, but can be specified if not:
# labels = c("xxx", "xxx")
# make sure new labels listed in same order as factor levels!

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number,
                                    fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = 1:6) +
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                      labels = c("HOGSMEADE", "NARNIA"),
                      name = "Land of Magic") +
    labs(title = "Species Richness by Plot",
         x = "\n Plot Number",
         y = "Number of Species \n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "italic"),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# legend.title allows you to change the font size of the legend, or its 
# formatting (e.g. bold).
# The legend.position can be defined with accepted positions such as "bottom", 
# but you can also do legend.position = c(0.1, 0.8), which would bring the 
# legend to the top left hand corner (corresponding to the x and y values on
# the graph). This is a neat trick in some cases, where you have lots of blank 
# space within your plot itself and want to fine-tune the legend position.
# Finally, we’ve used legend.box.background = element_rect() to create a light
# grey rectangle that surrounds the legend.

# Save plot
# specify dimensions, resolution, file type (.png, .pdf etc)
# will save to wd
ggsave("images/magical-land-sp-richness.png", width = 7, height = 5, dpi = 300)


# 2. Create own colour palette ----

# great websites:
    # Colour Brewer 
        # https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
        # can specify colourblind-safe palettes
    # coolors
        # https://coolors.co/

# can link colour palettes to factor levels (useful when creating multiple 
# plots, want consistent colour-coding)

# eg here w only 2 lands, easy to keep track of colours, but w more categories 
# better to assign factors to colour

# create fake dataframe of values for more magical lands:
land <- factor(c("Narnia", "Hogsmeade", "Westeros", "The Shire", "Mordor",
                 "Forbidden Forest", "Oz"))
counts <- as.numeric(c(55, 48, 37, 62, 11, 39, 51))
more_magic <- data.frame(land, counts)

# need as many colours as there are factor levels
length(levels(more_magic$land))  # 7 levels

# create colour palette
magic.palette <- c("#698B69", "#5D478B", "#5C5C5C", "#CD6090", "#EEC900",
                   "#5F9EA0", "#6CA6CD")
# linking factor names w colours
names(magic.palette) <- levels(more_magic$land)

# bar plot w all factors
(hist <- ggplot(more_magic, aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,  # using our palette here
                      name = "Land of Magic") +                
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# consistent colour scheme if drop some factors 
# (using filter in the first line)
(hist <- ggplot(filter(more_magic, land %in% c("Hogsmeade", "Oz", "The Shire")), aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,                       # using our palette ensures that colours with no corresponding factors are dropped
                      name = "Land of Magic") +                
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Shades and gradients

# So far we’ve used scale_colour_manual() and scale_fill_manual() to define 
# custom colours for factor levels. But if variable is continuous rather than 
# categorical, can’t assign colour to every value -> want colour scheme to go 
# from light to dark according to the values, use scale_colour_gradient() 
# and scale_fill_gradient()
# https://ggplot2.tidyverse.org/reference/scale_gradient.html
# basically, just have to set your low = and high = colour values and the 
# function will do the rest for you


# 3. Customise boxplots ----

# boxplots more informative

# want to manipulate dataset to include year as well
yearly_counts <- magic_veg %>% 
  group_by(land, plot, year) %>% 
  summarise(Species_number = length(unique(species))) %>% 
  ungroup() %>% 
  mutate(plot = as.factor(plot))

# basic boxplot first
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot())

# customise
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                      breaks = c("Hogsmeade","Narnia"),
                      name="Land of magic",
                      labels=c("Hogsmeade", "Narnia")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# save boxplot
ggsave("images/magical-sp-rich-boxplot1.png", width = 7, height = 5, dpi = 300)


# Box, bar, dot…?
#
# Bar plots are very commonly used to show differences or ranking among groups.
# A problem with them, especially if used without a measure of uncertainty 
# (e.g. error bars), is that what they display is a range of values starting
# from 0. If the variable you are plotting can reasonably have values of zero, 
# then that’s fine, but often it’s improbable. For instance, we wouldn’t 
# imagine that our lands of magic could be completely devoid of any life form 
# and therefore have a species richness of zero. Same holds true if you’re 
# comparing body weight, plant height, and a great majority of ecological 
# variables!
#   
# An easy alternative is a dot plot, which you could have done by summarising 
# the species_counts data to get a mean and standard deviation of species 
# counts for each land. You’d then use geom_point(aes(x = land, y = mean)) 
# rather than geom_histogram(), and add your uncertainty with 
# geom_errorbar(aes(x = land, ymin = mean - sd, ymax = mean + sd).

# create summarised data
summary <- species_counts %>% 
  group_by(land) %>% 
  summarise(mean = mean(Species_number),
            sd = sd(Species_number))
# make dot plot
(dot <- ggplot(summary, aes(x = land, y = mean, colour = land)) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    geom_point(size = 3) + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_colour_manual(values = c('#CD5C5C', '#6CA6CD'), 
                        labels = c('HOGSMEADE', 'NARNIA'), 
                        name = 'Land of Magic') +                   
    labs(title = 'Average species richness', 
         x = '', y = 'Number of species \n') + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , 'cm'), 
          legend.title = element_text(face = 'bold'),
          legend.position = 'bottom', 
          legend.box.background = element_rect(color = 'grey', size = 0.3)))

# Boxplots, just like dot plots, give a more accurate idea of the range of 
# values in your data: but remember that the thicker line in the box represents 
# the median, not the mean!


# a. Reordering factors ----

# often want to plot values in a specific order, eg Narnia before Hogsmeade
yearly_counts$land <- factor(yearly_counts$land,
                             levels = c("Narnia", "Hogsmeade"),
                             labels = c("Narnia", "Hogsmeade"))
# from this point, ggplot will always put Narnia before Hogsmeade

# also remember to change legend order!!!
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# can also reorder plot numbers on x axis, eg plot 6 before the rest
yearly_counts$plot <- factor(yearly_counts$plot, 
                             levels = c("6", "1", "2", "3", "4", "5"),
                             labels = c("6", "1", "2", "3", "4", "5"))
# plot boxplot
(boxplot2 <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))


# 4. Plot regression lines

# explore plants heights and how they may have changed over time

# data manip to extract just heights:
heights <- magic_veg %>% 
  filter(!is.na(height)) %>%  # remove NAs
  group_by(year, land, plot, id) %>% 
  summarise(Max_Height = max(height)) %>%  # calculate max height
  ungroup() %>%  # ungroup so that pipe not confused!
  group_by(year, land, plot) %>% 
  summarise(Height = mean(Max_Height))  # calculate mean max height

# basic scatterplot
(basic_mm_scatter <- ggplot(heights, aes(x = year, y = Height, colour = land)) +
    geom_point() +
    theme_bw())

# can see fairly clear trends, try simple straight line through this using 
# stat_smooth and specify linear model (lm)

(basic_mm_scatter_line <- ggplot(heights, aes(x = year, y = Height, 
                                              colour = land)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm"))

# more base fits: http://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/

# BUT can see relationship not linear, can use diff smoothing equation
# quadratic fit - more compliacted than standard fits provided by R
# can use any fit w ggplot2, just add equation:

(improved_mm_scat <- ggplot(heights, aes(x = year, y = Height, 
                                              colour = land)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))

# data is nested (species within plots within magic lands) and come from 
# diff years -> mixed-effects modelling better - (later tutorial)

# diff fits, eg "loess" gives smoothed curve
(diff_scatter_line <- ggplot(heights, aes(x = year, y = Height, 
                                              colour = land)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "loess"))


# 5. Create ggplot theme ----

# code for theme() repeated in most graphs - lots of code, less readable
# can create customised theme and apply to graphs
# can include as many elements as wanted, only relevant elements 
# considered when applied to graph

theme_coding <- function(){  # creating a new theme function
  theme_bw() +               # using a predefined theme as a base
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, 
                                     hjust = 1),  # customising lots of things
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
}

# plot using theme
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_coding()  # short and sweeeeet!
)

# And if you need to change some elements (like the legend that encroaches 
# on the graph here), you can simply overwrite:
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_coding() +                      # this contains legend.position = c(0.9, 0.9)
    theme(legend.position = "right")      # this overwrites the previous legend position setting
)


# 6. Challenge ----

# using % endemism (below), plot species richness as bar plot, coloured 
# w shade representing % of endemism

# and order bars in order of endemism, from low to high %

more_magic <- more_magic %>% 
  mutate(endemic = c(0.54, 0.32, 0.66, 0.80, 0.14, 0.24, 0.39))

more_magic$endemic <- more_magic$endemic * 100

# simple plot
(endemism_bar <- ggplot(more_magic, aes(x = land, y = counts, fill = endemic)) +
    geom_histogram(stat = "identity"))

# customised
(endemism_bar <- ggplot(more_magic, aes(x = reorder(land, endemic),  # reorder by endemism in-line
                                        y = counts, fill = endemic)) +
    geom_histogram(stat = "identity") +
    scale_fill_gradient(name = "% Endemism \n",
                        low = "#a1d99b",
                        high = "#31a354") +
    labs(title = "Species Richness and Endemism in Magical Lands\n", 
         x = "Magical Land", y = "Species Richness\n") +
    theme_coding() +
    theme(legend.position = "right",
          legend.title = element_text(size = 12),
          plot.title = element_text(size = 14)))

ggsave("images/magical_land_sp_rich_endemism.png", 
       width = 8, height = 5, dpi = 300)



