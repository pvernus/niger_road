hotosm_niger_roads <- st_read(here('data_raw', 'hotosm_niger_roads_gpkg', 'hotosm_niger_roads.gpkg'))
ner_road_lines <- st_cast(hotosm_niger_roads, "LINESTRING") %>% 
  filter(highway %in% c('trunk', 'trunk_link', 'primary_link', 'primary', 'secondary_link', 'secondary', 'tertiary_link', 'tertiary')) %>% 
  mutate(max_speed = case_when( # Add a speed variable
    highway %in% c('trunk', 'primary_link', 'primary') ~ 110,
    highway %in% c('secondary_link', 'secondary') ~ 90,
    highway %in% c('tertiary_link', 'tertiary') ~ 50,
    highway %in% c('unclassified') ~ 20
  ))


bb <- getbb("Niger", featuretype = "country")
ner_streetnet <- opq(bbox = bb, timeout = 100) %>% 
  add_osm_feature(key = 'highway', 
                  value = c('trunk', 'primary_link', 'primary',
                            'secondary_link', 'secondary',
                            'tertiary_link', 'tertiary')) %>% 
  osmdata_sc()

graph <- weight_streetnet (ner_streetnet)
graph_sf <- dodgr_to_sf (graph)

save(hotosm_niger_roads, ner_road_lines, graph,
     file = here('data', 'ner_road_network_sc.RData'))





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
