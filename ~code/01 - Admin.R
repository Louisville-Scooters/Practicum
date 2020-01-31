# Data Directory and Working Directory
data_directory <- paste(stringr::str_remove(here::here(), "\\/Eugene\\/Eugene - Practicum|\\/Ophelia\\/Ophelia - Practicum|\\/Xinyi\\/Xinyi - Practicum"), 
                        "/~data", 
                        sep = "")

setwd(here::here())

# Scientific Notation
options(scipen = 999)

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

# Standard Projection
proj <- 2246 # https://www.spatialreference.org/ref/epsg/2246/

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

