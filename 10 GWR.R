

library(tidyverse)  # Modern data science workflow
library(sf)
library(sp)
#library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)


# Change the presentation of decimal numbers to 4 and avoid scientific notation
options(prompt="R> ", digits=4, scipen=999)

sf_cent <- all.sf2 %>%
  st_make_valid() %>%
  st_centroid()

#Nix these. Not independent (due to parent nesting on some variables) and difficulty around using polygons in GWR, esp. with a factor variable with multiple data points per polygon.

# plot(sf_cent$area)
# 
# GWRbandwidth2 <- gwr.sel(inc ~ prev + maleART + malePrev + sexualDebut25_49, data = sf_cent, adapt = T)
# 
# gwr.model = gwr(inc ~ prev + maleART + malePrev + sexualDebut25_49, 
#                 data = all.sf2,
#                 adapt=GWRbandwidth,
#                 hatmatrix=TRUE,
#                 se.fit=TRUE)