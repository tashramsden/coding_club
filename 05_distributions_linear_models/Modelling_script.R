############################################################
#                                                          #
#          Practicing modelling using different            #
#                   data distributions                     #
#               Natasha Ramsden 11/05/2021                 #
#                                                          #
############################################################

# Libraries ----
library(agridat)
library(ggplot2)
library(dplyr)


# Set working directory
setwd("05_distributions_linear_models")


# Defining ggplot2 theme for plotting
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


# Common data distributions ----

# Gaussian 
#        - Continuous data (normal distribution and homoscedasticity assumed)
#        - lm, mixed-effects models

# Poisson 
#        - Count abundance data (integer values, zero-inflated data, left-skewed data)
#        - glm, glmm

# Binomial 
#        - Binary variables (TRUE/FALSE, 0/1, presence/absence data)
#        - glm, glmm

# Porportional (like binomial) but if more than 2 outcomes
#        -  chi-squared


# Choosing model structure ----

# let hypothesis guide you!
# eg
skylark.m <- lm(abundance ~ treatment + farm.area)
# here, chiefly interested in effects of treatment on skylark abundance
# but we also need to acknowledge that these treatments are not the only thing influencing abundance
# eg prop higher on bigger farms

# and maybe depends on where birds are to begin/where farms are
skylark.m <- lm(abundance ~ treatment + farm.area + latitude + longitude)

# and what if exp didn't go as planned, eg only 2 visits to one farm when most had 3
skylark.m <- lm(abundance ~ treatment + farm.area + latitude + longitude + visits)

# a simple model is usually preffered
# but if there are strong reasons to include a term in the model - then it should be there!

# Overfitting - AVOID
# if not enough variation in dataset (often beacuse too small)
# then data can be v tailored to the specific dataset but not representative of 
# general process/relationships
# model begins to describe random var in data rather than relationships between variables
# DETECT - cross-validation - testing model on new data - does it still work?
# OR compare predicted R squared to normal R squared - big diff - not general model- doesn't work if data changed - overfit
# see https://statisticsbyjim.com/regression/overfitting-regression-models/


# Collinearity - AVOID
# if 2 vars are v correlated, they prob explain similar amounts of var in
# data - but same variation, not diff/complementary
# don't include both variables in model!


# Linear models ----

# E.g. 1 CATEGORICAL explanatory VARIABLE ----
# sample dataset about apple yield in relation to different factors
# dataset part of the agridat package

# Investigate effects of spacing on apple yield
# hypothesis: closer apple trees, more resource competition, reduced yield

# Import data ----
apples <- agridat::archbold.apple
head(apples)
summary(apples)
str(apples)

# Explore the data ----
# in dataset only 3 spacing categories: 6, 10 and 14m
# not continuous! so make spacing a factor instead
apples$spacing2 <- as.factor(apples$spacing)

(apples.p <- ggplot(apples, aes(spacing2, yield)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Spacing (m)", y = "Yield (kg)"))

# plots shows yield quite similar across diff spacing distances
# trend is towards higher yield at higher spacing but range in data across 
# categories almost entirely overlap

# Run model to explicitly test the hypothesis ----
apples.m <- lm(yield ~ spacing2, data = apples)
summary(apples.m)
# Call:
# lm(formula = yield ~ spacing2, data = apples)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -92.389 -30.577  -3.516  33.192 117.628 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  120.566      7.382  16.332  < 2e-16  # estimates (and their error) for each factor level - so 120 is mean for first factor
# spacing210    35.924     11.073   3.244 0.001659  # 2nd level is 35 higher than 1st
# spacing214    44.107     10.966   4.022 0.000121  # 3rd 44 higher than 1st
# 
# (Intercept) ***
#   spacing210  ** 
#   spacing214  ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 43.67 on 89 degrees of freedom
# (28 observations deleted due to missingness)
# Multiple R-squared:  0.1742,	Adjusted R-squared:  0.1556 
# F-statistic: 9.385 on 2 and 89 DF,  p-value: 0.0002003

# About 15% variance explained by predictor (R squared values - bteween 0 and 1)
# (adjucted R squared takes into account how many terms your model has and how 
# many data points are available in the response variable)
# p < 0.05 so significant effect of spacing


# Yield does significantly differ between spacing categories
# reject null hypothesis
# apple yield higher when spacing greater

# Is this a good model? ----
# tells us that spacing has significant effect
# but maybe not an IMPORTANT one compared to other possible factors influencing yield
# only explains about 15% of var


# E.g. 2 - CONTINUOUS explanatory VARIABLE ----
# Another model, data and question 

# ilri.sheep dataset from agridat package
# Is the weight of lambs at weaning a function of their age at weaning?
# hypothesis: lambs that are weaned later are also heavier


# Import the data ----
sheep <- agridat::ilri.sheep

sheep <- filter(sheep, ewegen == "R")  # there are confounding variables in this dataset that we don't want to take into account. We'll only consider lambs that come from mothers belonging to the breed "R".
head(sheep)  # we will focus on weanwt (wean weight) and weanage

# Model ----
sheep.m1 <- lm(weanwt ~ weanage, data = sheep)
summary(sheep.m1)

# Intercept is lamb weight at age 0 = 2.60(wouldn't be weaned!) (value of Y when X = 0)
# weanage (below intercept) is slope of graph
# = 0.08 = increase in weight for each day

# y = mx + c
# lamb weight = 2.60 + 0.08(age)

# age at weaning explains about 20% of variation in lamb weight
# v highly significant model (tends to happen w large datasets)


# what if sex of lamb also influences weight gain?
sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep)
summary(sheep.m2)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   3.65829    1.24107   2.948  0.00347  # weight at age 0 for female (ref group)
# weanage       0.06469    0.01308   4.946 1.31e-06  # effect of age
# sexM         -2.51878    1.76234  -1.429  0.15406  # diff in intercept value from ref group for males
# weanage:sexM  0.03392    0.01874   1.810  0.07133  # diff in slope for males (interaction term)


# Calculations from model outputs ----

# calculate estimated weight of female sheep at 100 days of weaning age?
# female is reference group in the model
# intercept = 3.66
# slope = 0.065
# weight = 0.06(age) + 3.66
# days = 100 -> weight = 9.66 kg

# what about males?
# intercept = 3.65829 - 2.51878 = 1.13951
# slope = 0.06469 + 0.03392 = 0.09861
# weight = 0.099(age) + 1.14
# days = 100 -> weight = 11.04 kg

# Visualise relationship ----
(sheep.p <- ggplot(sheep, aes(x = weanage, y = weanwt)) +
   geom_point(aes(colour = sex)) +
   # scatter plot, coloured by sex
   labs(x = "Age at weaning (days)", y = "Wean weight (kg)") +
   stat_smooth(method = "lm", aes(fill = sex, colour = sex)) +  # adding regression lines for each sex
   scale_colour_manual(values = c("#FFC125", "#36648B")) +
   scale_fill_manual(values = c("#FFC125", "#36648B")) +
   theme.clean())


# Our data shows weight at weaning increases sig with weaning date
# and that there is only a marginal diff between rate of males' and females' weight gain
# plot shows all this clearly


# Linear regression, linear model, ANOVA ----
# all fundamentally the same
# https://www.theanalysisfactor.com/why-anova-and-linear-regression-are-the-same-analysis/

# linear reg and linear model are complete synonyms
# used when quantifying effect of CONTINUOUS explanatory var on CONTINUOUS response
# what is the change in Y for 1 unit change in X?
# eg sheep above

# ANOVA - Analysis of variance
# when quantifying effect of DISCRETE/CATEGORICAL explanatory var on CONTINUOUS response
# instead of getting slope to predict response, get estimate for each category
# eg apples above


# Checking assumptions ----

# as well as checking if model makes sense from eco perspective
# need to check if it meets assumptions of linear model

# 1. are the residuals normally distributed?
# (residuals = diff between observed and predicted values of the dependent variable)

# 2. are the data homoscedastic?
# (is the variance in the data around the same at all values of the predictor varaible?)

# 3. are the observations independent?

# 1. Checking that the residuals are normally distributed
apples.resid <- resid(apples.m)  # extract residuals
shapiro.test(apples.resid)  # Shapiro-Wilk test
# The null hypothesis of normal distribution is accepted: 
# there is no significant difference (p > 0.05) from a normal distribution

# 2. Checking for homoscedasticity
bartlett.test(apples$yield, apples$spacing2)
bartlett.test(yield ~ spacing2, data = apples)
# 2 ways of writing the code to get same results
# Null hypothesis of homoscedasticity is accepted

# The assumptions of a linear model are met (we can imagine that the data 
# points are independent - since we didn’t collect the data, we can’t really know).

# If assumptions not met - can consider transforming the data
# using a logarithmic transformation or a square root transformation


# Examine model fit further ----
plot(apples.m)  # press enter in command line to see plots
# produces 4 plots:
# 1. Residuals versus fitted values
# 2. Q-Q plot of standardized residuals
# 3. a scale-location plot (square roots of standardized residuals versus fitted values)
# 4. a plot of residuals versus leverage that adds bands corresponding to Cook’s distances of 0.5 and 1.

# can help to identify any outliers that have disproportionate influence on model
# and can confirm model has run alright - eg. QQ want data plots to follow the line
# see below for more info
# https://data.library.virginia.edu/diagnostic-plots/

# ABOVE were all GENERAL linear models



# Generalised linear models ----
# Poisson and binomial - data not normal/homoscedastic

# E.g. 3 - Poisson distribution ----

# Import data ----
shag <- read.csv("shagLPI.csv", header = TRUE)
str(shag)

shag$year <- as.numeric(shag$year)  # transform year from character into numeric variable

# Histogram to assess data distribution ----
(shag.hist <- ggplot(shag, aes(pop)) + 
   geom_histogram() + 
   theme.clean())

# pop variable represents a COUNT - integer (whole shags)
# so POISSON distribution appropriate here

# Model ----
shag.m <- glm(pop ~ year, family = poisson, data = shag)
summary(shag.m)
# shag abundance varies significantly based on the predictor year

# Visualise ----
(shag.p <- ggplot(shag, aes(x = year, y = pop)) +
   geom_point(colour = "#483D8B") +
   geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 0.6) +
   scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
   theme.clean() +
   labs(x = " ", y = "Earopean Shag Abundance"))
# Figure 1. European shag abundance on the Isle of May, Scotland, between 
# 1970 and 2006. Points represent raw data and model fit represents a 
# generalised linear model with 95% confidence intervals.


# E.g. 4 - Binomial distribution ----

# Import data ----
weevil_damage <- read.csv("Weevil_damage.csv", header = TRUE)

# We can examine if damage to Scot’s pine by weevils (a binary, TRUE/FALSE 
# variable) varies based on the block in which the trees are located. You can 
# imagine that different blocks represent different Scot’s pine populations, 
# and perhaps some of them will be particularly vulnerable to weevils?

# Because of binary nature of response - BINOMIAL model is appropriate

head(weevil_damage)
str(weevil_damage)

# make block a factor (categorical variable)
weevil_damage$block <- as.factor(weevil_damage$block)

# Model ----
weevil.m <- glm(damage_T_F ~ block, family = binomial, data = weevil_damage)
summary(weevil.m)
# outputs:
# looks like prob of pine tree enduring damage from weevils does vary 
# significantly based on the block in which the tree was located

# binomial models type of logistic regression - relies on log odd ratios
# greater estimates mean bigger influence on vars - not linear relationship

# Don't get R squared to asses goodness of fit
# can get at that by looking at diff between
# Null deviance (variability explained by a null model, eg glm(damage_T_F ~ 1))
# and the Residual deviance (amount of var that remains after you're explained some away by your explanatory variable)
# bigger reduction in variance = better the model is at explaining a relationship



