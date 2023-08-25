regional_road_network <- st_read(here('data', 'regional_road', 'road_reduced_reg.shp'))
regional_road_lines <- st_cast(regional_road_network, "LINESTRING") %>% 
  mutate(speed = case_when( # Add a speed variable
  highway %in% c('trunk', 'primary_link', 'primary') ~ 100,
  highway %in% c('secondary_link', 'secondary') ~ 80,
  highway %in% c('tertiary_link', 'tertiary') ~ 20
  ))

st_write(regional_road_lines, here('data', 'reg_mainroad_maxspeed.shp'), append = TRUE)


# round the coordinates
st_geometry(regional_road_lines) = st_geometry(regional_road_lines) %>%
  lapply(function(x) round(x, 2)) %>%
  st_sfc(crs = st_crs(regional_road_lines))

## source: https://github.com/rladies/meetup-presentations_jozi/blob/master/sfnetworks-nov-2021/sfnetworks.Rmd
# convert to sfnetworks
roads <- as_sfnetwork(regional_road_lines, directed = FALSE)
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

save(contracted, contracted_sf, file = here('data', 'regional_road', 'contracted.RData'))

graph_reg <- as_sfnetwork(contracted_sf, directed = FALSE, length_as_weight = TRUE)

autoplot(graph_reg)

# Other method to convert to spatial tbl_graph.
# reg_graph <-  sf_to_tidygraph(lines_clean, directed = FALSE, force = TRUE)

sum(which_loop(graph_reg)) #should have no self-loop
simplify_graph_reg <- igraph::simplify(graph_reg, remove.loops = TRUE, remove.multiple = FALSE)
sum(which_loop(simplify_graph_reg))

graph_reg_admpop <- graph_reg %>% 
  activate(edges) %>%
  st_join(reg_admpop_sf, largest = TRUE)

## Centrality measures
# source: https://igraph.org/r/doc/betweenness.html

graph <- graph_reg %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    betweenness_dist = scales::rescale(
      centrality_betweenness(weights = t_tl)) # population as weight for nodes
  ) %>% 
  activate(edges) %>%
  mutate(
    edge_betweenness_dist = scales::rescale(
      centrality_edge_betweenness(weights = weight, directed = FALSE), # distance as weight for edges
      to = c(0, 1)),
    edge_betweenness = scales::rescale(
      centrality_edge_betweenness(directed = FALSE),
      to = c(0, 1))
  )


lines_reg <- graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()
st_write(lines_reg, "data/regional_road/lines_reg.shp", append = TRUE)
# st_read("data/lines_reg.shp")

nodes_reg <- graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()
st_write(nodes_reg, "data/regional_road/nodes_reg.shp", append = TRUE)
# st_read("data/nodes_reg.shp")


## save raw data
save(contracted_sf,
     simplify_graph_reg, graph_reg, graph, 
     lines_reg, nodes_reg,
     file = "data/network_reg.RData")

## Map
# Betweenness Centrality (edges)
# load("data/ner_adm.RData")
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