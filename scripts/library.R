remotes::install_github("paleolimbot/qgisprocess")
devtools::install_github("r-barnes/dggridR", vignette=TRUE)

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
  snakecase,
  fuzzyjoin,
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
  sfnetworks,
# spatial
  osmdata, # import data from OpenStreetMap
  terra, # handle raster data
  tidyterra,
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  tmap, # mapping
  mapview,
  leaflet, # interactive mapping
  spatstat,
  stplanr, # contains the SpatialLinesNetwork class, which works with both  sp and sf objects.
  qgisprocess,
  RColorBrewer,
  dggridR,
  s2,
# survey data
  survey,
  srvyr,
  convey
)
vignette('dggridR')

