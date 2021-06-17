# Introduction ----
# Fixing this bad sript so that it is properly fomatted!
# Could all do with more comments!!!

# Libraries ----
library(tidyr)  # Formatting data for analysis
library(dplyr)  # Manipulating data
library(ggplot2)  # Visualising results
library(readr)  # Manipulating data

# Functions ----
theme.LPI <- function() {
  theme_bw() +
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

# Import data ----
LPI_data <- read.csv("LPIdata_CC.csv")

# Formatting data ----
LPI_data2<-gather(LPI_data,"year","abundance",9:53)

LPI_data2$year <- parse_number(LPI_data2$year)
names(LPI_data2)
names(LPI_data2) <- tolower(names(LPI_data2))
LPI_data2$abundance <- as.numeric(LPI_data2$abundance)

# Calc summary stats for each biome in the LPI database ----
levels(LPI_data2$biome)

lpiBiomes <- LPI_data2 %>%
  group_by(biome)%>%
  summarise(Pop. = n())
lpiBiomes[1:5,1:2]


# Visualising the number of populations in each biome with ggplot2 package ---- 

type = "bar"
plot <- ggplot(LPI_data2, aes(biome, color = biome)) +
  {if (type=="bar") geom_bar() else geom_point(stat="count")} +
	theme.LPI() + ylab("Number of populations") + xlab("Biome") +
	theme(legend.position = "none")
plot  # plot the ggplot plot

type = "point"
plot <- ggplot(LPI_data2, aes(biome, color = biome)) +
  {if(type == "bar") geom_bar() else geom_point(stat = "count")} +
	theme.LPI() + ylab("Number of populations") + xlab("Biome") +
	theme(legend.position = "none")
plot

type = "bar"
pdf(file="plot1.pdf",  width = 13.33, height = 26.66)
ggplot(LPI_data2, aes(biome, color = biome)) +
  {if(type=="bar")geom_bar() else geom_point(stat="count")} +
	theme.LPI() + ylab("Number of populations") + xlab("Biome") +
	theme(legend.position = "none") 
dev.off()



