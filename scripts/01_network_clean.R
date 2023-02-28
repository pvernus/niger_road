source("scripts/library.R") # load packages
source("R/functions.R") # load functions

# import polygons and lines as sf objects
polygons <- st_read("data_raw/hotosm_niger_roads_polygons_shp/hotosm_niger_roads_polygons.shp")
lines <- st_read("data_raw/hotosm_niger_roads_lines_shp/hotosm_niger_roads_lines.shp")

head(polygons)
head(lines)

plot(st_geometry(polygons[1, ]))
plot(st_geometry(lines[1, ]))

st_crs(polygons)
st_crs(lines)

plot(polygons)
plot(lines)

ggplot(data = lines) + geom_sf()

qgis_run_algorithm(
  "grass7:v.clean", # qgis_show_help("grass7:v.clean")
  input = lines, # Path to a vector layer to clean
  output = "data/lines_clean.shp",
  error = "data/line_errors.shp",
  type = 1,
  tool = 0, # break
  `-c` = 1
)

lines_clean <- st_read("data/lines_clean.shp")
plot(lines_clean)



qgis_argument_spec("grass7:v.clean")
qgis_show_help("grass7:v.clean")




# clean network
v_clean(lines)

# convert an sf object with LINESTRING geometries to a spatial tbl_graph
graph <- sf_to_tidygraph(lines)


# save raw data
save(polygons, lines, graph, file = "network_clean.RData")