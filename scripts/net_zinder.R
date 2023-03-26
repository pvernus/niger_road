load("data/network.RData")
load("data/adm03.RData")

# GIS ADM1 boundaries
adm_zinder <- adm03 %>% select(adm_01, id_adm1, adm_02, id_adm2, geometry) %>% filter(id_adm1 == "NER007")

# Survey welfare
zinder_welfare <- survey_welfare %>%
  filter(id_adm1 == "NER007") %>% 
  mutate(dtot_pc = dtot / hhsize,
         dtot_eqadu1 = dtot / eqadu1,
         dtot_eqadu2 = dtot / eqadu2,
         dali_pc = dali / hhsize,
         dali_eqadu1 = dali / eqadu1,
         dali_eqadu2 = dali / eqadu2,         
         dtot_pov_pc = dtot / (hhsize * def_spa * def_temp),
         dtot_pov_eqadu1 = dtot / (eqadu1 * def_spa * def_temp),
         dtot_pov_eqadu2 = dtot / (eqadu2 * def_spa * def_temp),
         pov_pc = if_else(dtot_pov_pc < zref, 1, 0),
         pov_eqadu1 = if_else(dtot_pov_eqadu1 < zref, 1, 0),
         pov_eqadu2 = if_else(dtot_pov_eqadu2 < zref, 1, 0)
  )

# define the survey design
zinder_srvy <- zinder_welfare %>% 
  as_survey_design(id = c(grappe, hhid), 
                  strata = c(region, zae), 
                  weights = hhweight, 
                  nest = TRUE)

# poverty rate per ADM2
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = zinder_srvy$pct, bins = bins)
labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  zinder_srvy$adm_02, zinder_srvy$pct) %>% 
  lapply(htmltools::HTML)

zinder_srvy %>% 
  group_by(id_adm2, pov_pc) %>% 
  summarise(pct = 100 * survey_prop()) %>% 
  inner_join(adm_zinder, by = "id_adm2") %>% 
  filter(pov_pc == 1) %>% 
  st_as_sf() %>% 
  leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolygons(
    fillColor = ~pal(pct),
    fillOpacity = 0.7,
    weight = 2,
    opacity = 1,
    color = "white",
    label = ~adm_02
    )

# estimate mean consumption per capita to create new relative poverty variable ()
zinder_srvy %>% 
  summarise(mean_dtot_pc = survey_mean(dtot_pc, vartype = "ci"), 
            mean_pcexp = survey_mean(pcexp, vartype = "ci"),
            quantile = survey_quantile(dtot_pc, c(.1, .25, .5, .75, .9)),
            median = survey_median(dtot_pc, vartype = c("ci"))
            )

leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolylines(
    data = net_zinder
  )







# net_zinder = st_filter(small_edges, adm_zinder)

## Import OSM data on ROADS (highways)
zinder_bb <- getbb('Zinder Region', format_out = 'polygon')

highway_z <- opq(bbox = zinder_bb) %>% 
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() %>%
  trim_osmdata(zinder_bb)

# clean lines, reduce file size
highway_zinder <- highway_z %>% 
  osm_poly2line() %>% # convert osmdata polygons into lines
  unique_osmdata() # reduce osm_lines to only lines not present in multi-line objects

lines_zinder <- highway_zinder$osm_lines %>% select(osm_id, highway, surface)
points_zinder <- highway_zinder$osm_points %>% select(osm_id)

# convert to shapefiles
st_write(lines_zinder, "data/lines_zinder.shp")
st_write(points_zinder, "data/points_zinder.shp")

# Step 1: Clean the network

# Clean the edges list with the GRASS v.clean function via QGIS plugin
qgis_run_algorithm(
  "grass7:v.clean", # qgis_show_help("grass7:v.clean")
  input = lines_zinder, # Path to a vector layer to clean
  output = "data/lines_zinder.shp",
  error = "data/lines_zinder_errors.shp",
  type = 1,
  tool = 0, # break
  `-c` = 1
)

# tool
# Available values:
# 0: break
# 1: snap
# 2: rmdangle
# 3: chdangle
# 4: rmbridge
# 5: chbridge
# 6: rmdupl
# 7: rmdac
# 8: bpol
# 9: prune
# 10: rmarea
# 11: rmline
# 12: rmsa

# HOW TO USE v.generalize ? 
# source: https://grass.osgeo.org/grass82/manuals/v.generalize.html



# USE WITH SF OBJECT
# source: https://cran.r-project.org/web/packages/sfnetworks/vignettes/sfn02_preprocess_clean.html
# Round coordinates to 4 digits.
st_geometry(net_zinder) = st_geometry(net_zinder) %>%
  lapply(function(x) round(x, 4)) %>%
  st_sfc(crs = st_crs(net_zinder))
simple = net_zinder %>%
  activate("edges") %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())
subdivision = convert(simple, to_spatial_subdivision)
smoothed = convert(subdivision, to_spatial_smooth)
plot(smoothed)
ways_centrality = smoothed |> 
  activate("edges") |>  
  mutate(betweenness = tidygraph::centrality_edge_betweenness(lengths))


# Step 2: cleaned sf object with LINESTRING geometries as input, and returns a spatial tbl_graph.
zinder_graph <-  sf_to_tidygraph(lines_zinder, directed = FALSE)

# Step 3; graph
graph <- zinder_graph %>%
  activate(edges) %>% # specify if we want to manipulate the edges/nodes
  mutate(length = st_length(geometry)) # add a variable describing the length of each edge (cf. weight)
graph

# length
graph %>%
  activate(edges) %>%
  as_tibble() %>%
  st_as_sf() %>%
  group_by(highway) %>%
  summarise(length = sum(length)) %>% 
  arrange(desc(length))

# Centrality measures
# source: https://igraph.org/r/doc/betweenness.html
# Arguments: 
# cutoff	= The maximum path length to consider when calculating the betweenness. If zero or negative then there is no such limit.
# normalized = TRUE

graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(),
         betweenness = centrality_betweenness(weights = length, normalized = TRUE)) %>%
  activate(edges) %>%
  mutate(centrality_edge_betweenness = centrality_edge_betweenness(weights = length))


ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))


edges_sf <- graph %>%
  activate(edges) %>%
  as_tibble() %>%
  st_as_sf()

pal <- colorNumeric(palette = "YlOrRd", domain = edges_sf$edge_betweenness)
leaflet() %>% 
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolylines(data = edges_sf)







# Map
leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolylines(data = highway_z$osm_lines,
               group = "raw") %>% 
  addPolylines(data = highway_zinder$osm_lines,
               color = "red",
               group = "clean") %>% 
  addLayersControl(
    overlayGroups = c("raw", "clean"),
    options = layersControlOptions(collapsed = FALSE)
  )
  

# half of the lines have unspecified surface
table(highway_z$osm_lines$surface, exclude = NULL)
# most lines are unclassified highways
table(highway_z$osm_lines$highway, exclude = NULL)
# overall most roads are unclassified and unpaved
highway_surface <- highway_z$osm_lines %>% 
  select(highway, surface) %>% 
  group_by(highway, surface) %>% 
  tally() %>% arrange(desc(n))

lines_z <- highway_z$osm_lines %>% 
  select(highway, surface) %>% 
  st_as_sf()













## Import OSM data on LOCALITIES (place)
place_z <- opq(bbox = bb) %>% 
  add_osm_feature(key = c('place')) %>% 
                    osmdata_sf () %>%
                    trim_osmdata (bb)

# we don't have population for most localities
place_z$osm_points %>% 
  select(place, population, postal_code, rank) %>% 
  group_by(place, population) %>% 
  tally() %>% arrange(desc(n))

# Map
leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addCircles(data = place_z$osm_points)

# few info on landuse
table(place_z$osm_polygons$landuse, exclude = NULL)
# some info on place
table(place_z$osm_polygons$place, exclude = NULL)


### Import OSM data on LANDUSE (landuse)
landuse_z <- opq(bbox = bb) %>% 
  add_osm_feature(key = c('landuse')) %>% 
  osmdata_sf () %>%
  trim_osmdata (bb)

table(landuse_z$osm_polygons$landuse, exclude = NULL)
# Map
leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolygons(data = landuse_z$osm_polygons,
              label = landuse_z$osm_polygons$landuse)



save(edges, edges_sf, graph, highway_zinder, 
     lines_clean, lines_zinder, lines_zinder_sf, 
     points_zinder, zinder_graph,
     highway_z, file = "data/zinder_data.RData")



