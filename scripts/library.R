if (!require("pacman")) install.packages("pacman")
pacman::p_load(
# setup  
  here,
  tidyverse,
# data preparation
  labelled,
  readxl,  
  visdat,
  data.table, # data wrangling
  janitor,
  forcats,
  gtsummary, 
  GGally,
  skimr,
  units,
# network data
  tidygraph,
  igraph,
  shp2graph, # switch between sp and igraph objects
  rgrass7, # ‘bridge’ to GRASS GIS
  link2GI, # ‘bridge’ to GRASS GIS
  dbscan,
  spNetwork,
# spatial
  osmdata, # import data from OpenStreetMap
  terra, # handle raster data
  tidyterra,
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  tmap, # mapping
  leaflet, # interactive mapping
  spatstat,
  stplanr, # contains the SpatialLinesNetwork class, which works with both  sp and sf objects.
  qgisprocess,
# survey data
  survey,
  srvyr,
  convey
)

