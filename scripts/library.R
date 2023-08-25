install.packages('installr')
# installr()
# install.RStudio()

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
  tidygraph, # network data
  igraph, # network data
  shp2graph, # switch between sp and igraph objects
  dbscan,
  spNetwork,
  sfnetworks,
  dodgr,
  geodist,
  dbscan,
  netrankr,
  rhdx, # Humdata
  osmdata, # import data from OpenStreetMap
  qgisprocess,
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
  PCAtest,
  GGally,
  ggforce,
  PerformanceAnalytics,
  spatstat
)


