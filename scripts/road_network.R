source("scripts/library.R") # load packages
source("R/functions.R") # load functions

# load("data/network.RData")
# load("data/ner_adm.RData")

# from the rhdx package
set_rhdx_config(hdx_site = "prod")
get_rhdx_config()

search_datasets("hotosm niger roads lines", rows = 2) # search dataset in HDX, limit the results to two rows
osm_lines <- pull_dataset("hotosm_niger_roads") %>%
  get_resource(1) %>%
  read_resource()

# keep only the main roads
osm_lines <- osm_lines %>% select(osm_id, highway, geometry) %>%
  filter(highway %in% c('trunk', 'primary', 'secondary', 'tertiary',
                        'primary_link', 'secondary_link', 'tertiary_link')) %>% 
  mutate(highway = case_when(
    highway == "primary_link" ~ "primary",
    highway == "secondary_link" ~ "secondary",
    highway == "tertiary_link" ~ "tertiary",
    .default = as.character(highway)
  ))

# cast from multilinestring to linestring for sfnetworks
road_lines <- st_cast(osm_lines, "LINESTRING")

st_write(road_lines, "data/road_lines.shp", append = TRUE)

# round the coordinates
st_geometry(road_lines) = st_geometry(road_lines) %>%
  lapply(function(x) round(x, 2)) %>%
  st_sfc(crs = st_crs(road_lines))

## from sf object to igraph object
# Step 1: clean the edges list with the GRASS's v.clean function via QGIS plugin
# see: https://grass.osgeo.org/grass82/manuals/v.clean.html
qgis_run_algorithm(
  "grass7:v.clean",
  # qgis_show_help("grass7:v.clean")
  input = road_lines,
  # Path to a vector layer to clean
  output = "data/lines_clean.shp",
  error = "data/line_errors.shp",
  type = 1,
  tool = 0, # 0:break, 2:chdangle
  # break
  `-c` = 1
)
lines_clean <- st_read("data/lines_clean.shp")

## source: https://github.com/rladies/meetup-presentations_jozi/blob/master/sfnetworks-nov-2021/sfnetworks.Rmd
# convert to sfnetworks
roads <- as_sfnetwork(road_lines, directed = FALSE)
# simply network
simple = roads %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())
# subdivide edges
subdivision = convert(simple, to_spatial_subdivision)
# smooth pseudo nodes
smoothed = convert(subdivision, to_spatial_smooth)

# simplify intersections
# step 1: cluster the nodes using the dbscan method, which clusters points in Euclidean space. 
# Retrieve the coordinates of the nodes.
node_coords = smoothed %>%
  activate("nodes") %>%
  st_coordinates()

# step2: cluster the nodes with the DBSCAN spatial clustering algorithm.
# eps = 1: nodes within a distance of 1 from each other will be in the same cluster.
# minPts = 1: a node is assigned a cluster even if it is the only member of that cluster.
clusters = dbscan(node_coords, eps = .01, minPts = 1)$cluster
# Add the cluster information to the nodes of the network.
clustered = smoothed %>%
  activate("nodes") %>%
  mutate(cls = clusters)

# step 3: verify that clustered nodes are connected on the network.
clustered = clustered %>%
mutate(cmp = group_components())

# step 4: contract the network
contracted = convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

# step 5: convert back to sfnetworks object
contracted_sf <- contracted %>% 
  activate("edges") %>% 
  st_as_sf() 
ner_graph <- as_sfnetwork(contracted_sf, directed = FALSE, length_as_weight = TRUE)

autoplot(ner_graph)

# Other method to convert to spatial tbl_graph.
# ner_graph <-  sf_to_tidygraph(lines_clean, directed = FALSE, force = TRUE)

sum(which_loop(ner_graph)) #should have no self-loop
simplify_graph <- igraph::simplify(ner_graph, remove.loops = TRUE, remove.multiple = FALSE)
sum(which_loop(simplify_graph))

## Centrality measures
# source: https://igraph.org/r/doc/betweenness.html

graph <- ner_graph %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    betweenness_dist = scales::rescale(
      centrality_betweenness(weights = weight))
  ) %>% 
  activate(edges) %>%
  mutate(
    edge_betweenness_dist = scales::rescale(
      centrality_edge_betweenness(weights = weight, directed = FALSE),
      to = c(0, 1)),
    edge_betweenness = scales::rescale(
      centrality_edge_betweenness(directed = FALSE),
      to = c(0, 1))
  )


lines_ner <- graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()
st_write(lines_ner, here('data', 'lines_ner.shp'), append = TRUE)
# st_read("data/lines_ner.shp")

nodes_ner <- graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()
st_write(nodes_ner, here('data', 'nodes_ner.shp'), append = TRUE)
# st_read("data/nodes_ner.shp")


## save raw data
save(lines_ner,
     nodes_ner,
     ner_graph,
     graph,
     file = "data/network.RData")


## Map
# Betweenness Centrality (edges)
tm_shape(adm03) +
  tm_borders('grey80') +
  tm_shape(lines_ner) +
  tm_lines(
    lwd = "edge_betweenness_dist",
    scale = 4,
    col = "edge_betweenness_dist",
    palette = viridis::viridis(4)
  ) +
  tm_scale_bar(position = c("center", "bottom")) +
  tm_layout(title = "Betweenness Centrality (edges)",
            asp = 0,
            legend.stack = "horizontal") +
  tm_compass(type = "rose", size = 2)

## Map
# Betweenness Centrality (nodes)
tm_shape(adm03) +
  tm_borders('grey80') +
  tm_shape(lines_ner) +
  tm_lines(col = 'darkblue', alpha = .3) +
  tm_shape(nodes_ner) +
  tm_bubbles(
    col = "betweenness_dist",
    size = "betweenness_dist",
    palette = viridis::viridis(4),
    scale = 0.7,
    border.lwd = NA
  ) +
  tm_layout(title = "Betweenness Centrality", asp = 0) +
  tm_scale_bar(position = c("center", "bottom")) +
  tm_layout(title = "Betweenness Centrality (nodes)",
            asp = 0,
            legend.stack = "horizontal") +
  tm_compass(type = "rose", size = 2)


tmap_mode("view")

tm_shape(pop_sf) +
  tm_polygons(
    "t_tl",
    palette = "Blues",
    n = 4,
    alpha = .5,
    title = "Population",
    group = "Population"
  ) +
  tm_shape(ipc_2020_adm2) +
  tm_polygons(
    "median",
    palette = "YlOrRd",
    alpha = .5,
    id = "adm2_name",
    group = "Food Security"
  ) +
  tm_shape(lines_ner) +
  tm_lines(
    "edge_betweenness",
    palette = "YlOrRd",
    lwd = 3,
    group = "Roads"
  ) +
  tm_shape(lines_ner) +
  tm_lines(
    "edge_betweenness_l",
    palette = "YlOrRd",
    lwd = 3,
    group = "Roads (weighted)"
  ) +
  tm_shape(lines_ner) +
  tm_lines("highway", lwd = 3, group = "Type") +
  tm_shape(nodes_trim) +
  tm_bubbles(
    col = "betweenness",
    palette = "YlOrRd",
    alpha = .5,
    scale = .1,
    group = "Roads"
  ) +
  tm_shape(nodes_trim) +
  tm_bubbles(
    col = "betweenness_l",
    palette = "YlOrRd",
    alpha = .5,
    scale = .1,
    group = "Roads (weighted)"
  ) +
  tm_shape(adm03) +
  tm_borders() +
  tm_scale_bar() +
  tm_minimap()



