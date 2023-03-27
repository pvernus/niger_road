source("scripts/library.R") # load packages
source("R/functions.R") # load functions

# load("data/network.RData")

# import polygons and lines as sf objects
polygons <- st_read("data_raw/hotosm_niger_roads_polygons_shp/hotosm_niger_roads_polygons.shp")
lines <- st_read("data_raw/hotosm_niger_roads_lines_shp/hotosm_niger_roads_lines.shp") %>% 
  select(osm_id, highway, geometry) %>% 
  filter(highway %in% c('trunk', 'primary', 'secondary', 'tertiary', 
                        'trunk_link', 'secondary_link', 'tertiary_link'))

# OR get the data directly from OSM with the osmdata package
bb <- getbb('niger', format_out = 'polygon')
ner_highway <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf() %>% 
  osm_poly2line() %>% # convert osmdata polygons into lines
  unique_osmdata()

# Step 1: clean the edges list with the GRASS's v.clean function via QGIS plugin
qgis_run_algorithm(
"grass7:v.clean", # qgis_show_help("grass7:v.clean")
  input = lines, # Path to a vector layer to clean
  output = "data/lines_clean.shp",
  error = "data/line_errors.shp",
  type = 1,
  tool = 0, # break
  `-c` = 1)
lines_clean <- st_read("data/lines_clean.shp")

# Step 2: cleaned sf object with LINESTRING geometries as input, and returns a spatial tbl_graph.
ner_graph <-  sf_to_tidygraph(lines_clean, directed = FALSE)

ner_graph <- ner_graph %>% 
  activate("edges") %>%
  filter(!edge_is_multiple()) %>% # filter out edges with parallel siblings
  filter(!edge_is_loop()) # filter out edges which are loops

# Step 3: create new length variable for edges
ner_graph <- ner_graph %>%
  activate(edges) %>% # specify if we want to manipulate the edges/nodes
  mutate(length = st_length(geometry)) # add a variable describing the length of each edge (cf. weight)
ner_graph

# length by type of highway
ner_graph %>%
  activate(edges) %>%
  as_tibble() %>%
  st_as_sf() %>%
  group_by(highway) %>%
  summarise(length = sum(length)) %>% 
  arrange(desc(length))

# Centrality measures
# source: https://igraph.org/r/doc/betweenness.html

graph <- ner_graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(),
         betweenness_l = centrality_betweenness(weight = length, normalized = TRUE),
         betweenness = centrality_betweenness(normalized = TRUE)) %>%
  activate(edges) %>%
  mutate(edge_betweenness_l = centrality_edge_betweenness(weight = length), 
         edge_betweenness = centrality_edge_betweenness())

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))

lines_ner <- graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()
st_write(lines_ner, "data/lines_ner.shp")
# st_read("data/lines_ner.shp")
nodes_ner <- graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()
st_write(nodes_ner, "data/nodes_ner.shp")
# st_read("data/nodes_ner.shp")

nodes_trim <- nodes_ner %>% 
  filter(!degree == 2)


tmap_mode("view")

tm_shape(pop_sf) +
  tm_polygons("t_tl", palette = "Blues", n = 4, alpha = .5, title = "Population", group = "Population") +
  tm_shape(ipc_2020_adm2) + 
  tm_polygons("median", palette = "YlOrRd",  alpha = .5, id = "adm2_name", group = "Food Security") +
  tm_shape(lines_ner) +
  tm_lines("edge_betweenness", palette = "YlOrRd", lwd = 3, 
           group = "Roads") +
  tm_shape(lines_ner) +
  tm_lines("edge_betweenness_l", palette = "YlOrRd", lwd = 3, 
           group = "Roads (weighted)") +
  tm_shape(lines_ner) +
  tm_lines("highway", lwd = 3, group = "Type") +
  tm_shape(nodes_trim) +
  tm_bubbles(col = "betweenness", palette = "YlOrRd", alpha = .5, scale = .1,
             group = "Roads") +
  tm_shape(nodes_trim) +
  tm_bubbles(col = "betweenness_l", palette = "YlOrRd", alpha = .5, scale = .1,
             group = "Roads (weighted)") +
  tm_shape(adm03) +
  tm_borders() +
  tm_scale_bar() +
  tm_minimap()


# save raw data
save(polygons, lines, lines_clean, lines_ner, nodes_ner, ner_graph, graph, file = "data/network.RData")
