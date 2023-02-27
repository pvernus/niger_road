if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here,
  osmdata,
  tidyverse,
  labelled,
  readxl,
  terra, # handle raster data
  tidyterra,
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  data.table, # data wrangling
  tmap, # mapping
  leaflet, # interactive mapping
  colorspace,
  survey,
  srvyr,
  convey,
  haven,
  skimr,
  visdat,
  janitor,
  forcats,
  spatstat,
  gtsummary, 
  GGally
)