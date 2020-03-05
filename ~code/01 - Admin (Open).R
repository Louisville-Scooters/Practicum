##########################################################################
# This script reads in:
# 1. Creates and sets a computer-agnostic working directory
# 2. Turns off scientific notation
# 3. Loads necessary packages
# 4. Sets themes and color palettes
# 5. Writes helper functions
#
# Please update this whenever you create a function that everyone will need to use
# or load a new package
##########################################################################

# Data Directory and Working Directory
data_directory <- file.path(stringr::str_remove(here::here(), 
                                                "\\/Eugene\\/Eugene - Practicum|\\/Ophelia\\/Ophelia - Practicum|\\/Xinyi\\/Xinyi - Practicum"), 
                        "~data")

setwd(here::here())

# Scientific Notation
options(scipen = 999)

# For flow map
options(repos = c(CRAN = "http://www.stats.bris.ac.uk/R/"))

# Load Packages
library(sf)
library(measurements)
library(tidycensus)
library(tidyverse)
library(tmap)
library(lubridate)
library(knitr)
library(kableExtra)
library(rgeos)
library(raster)
library(spatstat)
library(data.table)
library(janitor)
library(vroom)
library(here)
library(dplyr)
library(sp)
library(viridis)
library(maptools)
library(stringr)
library(grid)
library(gridExtra)
library(corrplot)
library(osmdata)
library(FNN)
library(janitor)
library(caret)
library(furr)
#install.packages('osmdata')

# Palettes and Themes
paletteY <- c("#F9F871","#FFD364","#FFAF6D","#FF8F80","#F87895", "D16BA5")
palette5 <- c("#25CB10", "#5AB60C", "#8FA108","#C48C04", "#FA7800")

plotTheme <- theme(
  plot.title =element_text(size=15),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.2),
  axis.ticks=element_blank())

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

# Helper functions
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Projections
DC_proj <- 2283 # Northern Virginia: https://epsg.io/2283
LV_proj <- 2246 # https://www.spatialreference.org/ref/epsg/2246/
KC_proj <-2817 # https://spatialreference.org/ref/epsg/2817/
MNP_proj <- 2812 # https://www.spatialreference.org/ref/epsg/2812/
AU_proj <- 2246 
CH_proj <- 3529 #https://www.spatialreference.org/ref/?search=Illinois
