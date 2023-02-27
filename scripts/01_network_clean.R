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


# clean network
v_clean(lines)

# convert an sf object with LINESTRING geometries to a spatial tbl_graph
graph <- sf_to_tidygraph(lines)


# save raw data
save(graph, file = ".RData")