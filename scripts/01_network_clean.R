source("scripts/library.R") # load packages
source("R/functions.R") # load functions

# import polygons and lines as sf objects
polygons <- st_read("data_raw/hotosm_niger_roads_polygons_shp/hotosm_niger_roads_polygons.shp")
lines <- st_read("data_raw/hotosm_niger_roads_lines_shp/hotosm_niger_roads_lines.shp")

#head(polygons)
#head(lines)

#plot(st_geometry(polygons[1, ]))
#plot(st_geometry(lines[1, ]))

#st_crs(polygons)
#st_crs(lines)

#plot(polygons)
#plot(lines)

# Clean the edges list with the GRASS's v.clean function via QGIS plugin
qgis_run_algorithm(
  "grass7:v.clean", # qgis_show_help("grass7:v.clean")
  input = lines, # Path to a vector layer to clean
  output = "data/lines_clean.shp",
  error = "data/line_errors.shp",
  type = 1,
  tool = 0, # break
  `-c` = 1
)
# open the new (clean) file in R
lines_clean <- st_read("data/lines_clean.shp") %>% 
  st_make_valid() %>% 
  select(c(osm_id, highway, geometry))

#table(lines_clean$highway)

# give each edge a unique index
edges <- lines_clean %>%
  mutate(edgeID = c(1:n()))
# create nodes at the start and end point of each edge
nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  select(-L1) %>% 
  rename(edgeID = L2) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))
# give each node a unique index
nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)
# combine the node indices with the edges
source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)
# remove duplicate nodes
nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))
# convert to tbl_graph
graph <- tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

# create a smaller graph without non-relevant road types
road_type_rm <- c("cycleway", "escape", "living_street", "raceway", "residential", "service", "services", "steps")
small_edges <- edges %>% 
  filter(!highway %in% road_type_rm)
small_graph <- tbl_graph(nodes = nodes, edges = as_tibble(small_edges), directed = FALSE)

# create new length variable for edges
graph <- graph %>%
  activate(edges) %>% # specify if we want to manipulate the edges or the nodes
  mutate(length = st_length(geometry))
small_graph <- small_graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

road_type_ranking <- graph %>%
  activate(edges) %>%
  as_tibble() %>%
  st_as_sf() %>%
  group_by(highway) %>%
  summarise(length = sum(length)) %>% 
  arrange(desc(length))

# plot the network
ggplot() +
  geom_sf(data = small_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()) + 
  geom_sf(data = small_graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), size = 0.5)

# save raw data
save(polygons, lines, lines_clean, edges, nodes, small_graph, small_edges, graph, file = "data/network.RData")

### TO-DO ###

# plot the network as an interactive map
tmap_mode('view')

tm_shape(small_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()) +
  tm_lines() +
  tm_shape(small_graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()) +
  tm_dots() +
  tmap_options(basemaps = 'OpenStreetMap')

# centrality measures
small_graph <- small_graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length))

ggplot() +
  geom_sf(data = small_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))

