source(here::here('scripts', 'library.R'))

hotosm_niger_roads <- st_read(here('data_raw', 'hotosm_niger_roads_gpkg', 'hotosm_niger_roads.gpkg'))

ner_mainroad_lines <- st_cast(hotosm_niger_roads, "LINESTRING") %>% 
  filter(highway %in% c('trunk', 'trunk_link', 'primary_link', 'primary', 'secondary_link', 'secondary', 'tertiary_link', 'tertiary')) %>% 
  mutate(max_speed = case_when( # Add a speed variable
    highway %in% c('trunk', 'primary_link', 'primary') ~ 110,
    highway %in% c('secondary_link', 'secondary') ~ 90,
    highway %in% c('tertiary_link', 'tertiary') ~ 50
  ))

## Pre-processing and cleaning
# Round coordinates to 4 digits.
st_geometry(ner_mainroad_lines) = st_geometry(ner_mainroad_lines) %>%
  lapply(function(x) round(x, 4)) %>%
  st_sfc(crs = st_crs(ner_mainroad_lines))

simple = ner_mainroad_lines %>%
  as_sfnetwork() %>% 
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())
subdivision = convert(simple, to_spatial_subdivision)

subdivision_sf = st_as_sf(subdivision)
st_write(subdivision_sf, here('data', 'subdivision.gpkg'), delete_layer = TRUE)

st_write(ner_mainroad_lines, here('data', 'ner_mainroad_lines.gpkg'), delete_layer = TRUE)
save(hotosm_niger_roads, ner_mainroad_lines, file = here('data', 'ner_mainroad_lines.RData'))

edges <- as_sfnetwork(subdivision) # convert to sfnetwork format
smoothed_nodes <- convert(edges, to_spatial_smooth) # remove pseudo-nodes

# Measure nodes centrality
nodes_graph_sf <- smoothed_nodes %>% 
  activate('nodes') %>%
  mutate(centrality_eigen = tidygraph::centrality_eigen(directed = FALSE),
         centrality_betweenness = tidygraph::centrality_betweenness(directed = FALSE, normalized = TRUE) ) %>% 
  rename(node_index = .tidygraph_node_index) %>% 
  st_as_sf()

st_write(nodes_graph_sf, here('data', 'ner_nodes_graph.gpkg'), delete_layer = TRUE)

save(subdivision_sf, edges_graph, edges_dodgr, file = here('data', 'test.RData'))

# Measure edges centrality
edges_graph_sf <- as_sfnetwork(subdivision_sf) %>% 
  activate('edges') %>%
  mutate(edge_betweenness = scales::rescale(centrality_edge_betweenness(weights = NULL, directed = FALSE), 
                                            to = c(0, 1))) %>% 
  st_as_sf()

st_write(edges_graph_sf, here('data', 'ner_edges_graph.gpkg'), delete_layer = TRUE)

===

graph_full <- weight_streetnet (ner_mainroad_lines, wt_profile = "motorcar")
nrow (graph)

edges_graph <- dodgr_centrality (graph_full, contract = TRUE, edges = TRUE)


edges_graph_sf <- edges_graph %>% 
  dodgr_to_sf ()

nodes_graph_sf <- nodes_graph %>% 
  st_as_sf(coords = c("x", "y"))

st_write(edges_graph_sf, here('data', 'ner_edges_graph.gpkg'), delete_layer = TRUE)
st_write(nodes_graph_sf, here('data', 'ner_nodes_graph.gpkg'), delete_layer = TRUE)










bb <- getbb("Niger", featuretype = "country")
ner_dodgr_streetnet_sc <- opq(bbox = bb), timeout = 100) %>% 
  add_osm_feature(key = 'highway') %>% 
  osmdata_sc()

bb <- getbb("Niger", featuretype = "country")
streetnet <- dodgr_streetnet (bbox = bb, quiet = FALSE)
net <- weight_streetnet (streetnet)
graph <- net [which (!net$access != "private"), ]

st_write(ner_road_lines, here('data', 'ner_road_lines.gpkg'))

graph <- weight_streetnet(ner_road_lines, wt_profile = "motorcar")
graph_sf <- dodgr_to_sf (graph)
graph <- dodgr_contract_graph(graph)

save(hotosm_niger_roads, ner_road_lines, graph,
     file = here('data', 'ner_road_network.RData'))

load(here('data', 'grappes.RData'))
marketplace <- st_read(here('data', 'osm_points_marketplace.gpkg'))

grappe <- grappe_sf %>% 
  column_to_rownames(var = 'grappe') %>% 
  st_as_sf()

v <- dodgr_vertices(graph)

from <- match_points_to_verts (v, grappe, connected = TRUE)
to <- match_points_to_verts (v, marketplace, connected = TRUE)

from <- v$id [from]
to <- v$id [to]

dist_nearest <- dodgr_dists_nearest(graph = graph, from = from, to = to) %>% 
  rename(distance_nearest_marketplace = d)
dist_fastest <- dodgr_dists_nearest(graph = graph, from = from, to = to, shortest = FALSE) %>% 
  rename(distance_fastest_marketplace = d)

grappe_nearest_marketplace <- as.data.frame(from, col.names = 'from') %>% 
  rownames_to_column(var = "grappe") %>% 
  mutate(grappe = as.numeric(grappe)) %>% 
  left_join(dist_nearest, by = 'from', relationship = "many-to-many") %>% 
  distinct() %>% 
  select(-c(from, to))

grappe_fastest_marketplace <- as.data.frame(from, col.names = 'from') %>% 
  rownames_to_column(var = "grappe") %>% 
  mutate(grappe = as.numeric(grappe)) %>% 
  left_join(dist_fastest, by = 'from', relationship = "many-to-many") %>% 
  distinct() %>% 
  select(-c(from, to))

grappe_distance_marketplace <- inner_join(grappe_nearest_marketplace, grappe_fastest_marketplace, by = 'grappe')
  
save(grappe_distance_marketplace, file = here('data', 'grappe_distance_marketplace.RData'))

grappe_marketplace <- left_join(grappe_sf, grappe_distance_marketplace, by = 'grappe')

st_write(grappe_marketplace, here('data', 'grappe_marketplace.gpkg'), delete_layer = TRUE)
