install.packages('installr')
# installr()
# install.RStudio()

if (!require("qgisprocess")) remotes::install_github("paleolimbot/qgisprocess")
if (!require("dggridR")) devtools::install_github("r-barnes/dggridR", vignette=TRUE)
if (!require("rhdx")) remotes::install_github("dickoa/rhdx")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
# setup  
  here,
  tidyverse,
  DBI,
# data preparation
  haven,
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
  caret,
  biscale,
  cowplot,
  plotly,
  datawizard,
  ggbeeswarm,
  tidytext,
# network data
  tidygraph,
  igraph,
  shp2graph, # switch between sp and igraph objects
  rgrass7, # ‘bridge’ to GRASS GIS
  link2GI, # ‘bridge’ to GRASS GIS
  dbscan,
  spNetwork,
  sfnetworks,
  dbscan,
  netrankr,
# Humdata
  rhdx,
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
  rgrass,
  ggspatial,
  viridis,
  geos,
# survey data
  survey,
  srvyr,
  convey,
  vardpoor,
  laeken,
  questionr,
  # cluster
  cluster,
  stats,
  divseg,
  ppsr,
  corrr,
  corrplot,
  ggcorrplot,
  FactoMineR,
  factoextra,
  GGally,
  ggforce,
  PerformanceAnalytics,
  spatstat
)


