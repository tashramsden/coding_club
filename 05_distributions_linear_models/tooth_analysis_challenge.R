############################################################
#                                                          #
#         Analysing ToothGrowth dataset describing         #
#       tooth growth in guinea pigs under different        #
#                  vitamin C treatments                    #
#               Natasha Ramsden 11/05/2021                 #
#                                                          #
############################################################

# Aims ----

# Answer the following:

# 1. Are higher doses of vitamin C beneficial for tooth growth?

# 2. Does the method of administration (orange juice, OJ, or ascorbic acis, VC)
# influence the effectc of the dose?

# 3. What would be the predicted tooth length of a guinea pig given 1mg of 
# vitamin C as ascorbic acid?


# Libraries ----
library(ggplot2)
library(dplyr)


# Set working directory ----
setwd("05_distributions_linear_models")


# Defining ggplot2 theme for plotting ----
theme.clean <- function(){
  theme_bw()+
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
          legend.position = "right")
}


# Import data ----
# ToothGrowth is a built-in dataset
tooth_growth <- datasets::ToothGrowth
head(tooth_growth)
str(tooth_growth)


# Variables ----

# Explanatory
# dose is categorical, 3 categories - 0.5, 1, 2
# supp (supply) also categorical, 2 categories - VC and OJ

# Response
# len (tooth length) is continuous

# POISSON DISTRIBUTION


# Sort out data ----
# Change dose to categorical variable (factor) 
tooth_growth$dose <- as.factor(tooth_growth$dose)


# Visualise ----
(tooth_growth.p <- ggplot(tooth_growth, aes(x = dose, y = len)) +
   geom_boxplot(aes(colour = supp)) +
   theme.clean())


# Model ----
tooth_growth.m <- lm(len ~ dose*supp, data = tooth_growth)
summary(tooth_growth.m)


# Conclusions ----

# Model is highly significant
# Together, dose and supply explain around 77% of the variation in tooth growth


# 1. Are higher doses of vitamin C beneficial for tooth growth?

# Yes, higher doses significantly increase tooth length 
# BUT (see 2.)


# 2. Does the method of administration (orange juice, OJ, or ascorbic acis, VC)
# influence the effectc of the dose?

# The effect of dose on tooth growth depends on the administartion method


# 3. What would be the predicted tooth length of a guinea pig given 1mg of 
# vitamin C as ascorbic acid?

# 16.77

# growth for dose 0.5 (OJ) = 13.230
# + 9.470 (extra growth for dose 1, OJ)
# - 5.250 (decrease in growth linked to VC supply (from VC supply (for dose 0.5))
# - 0.680 (decrease in growth for interaction between dose 1 and VC supply)
# 13.23 + 9.47 - 5.25 - 0.68 = 16.77

