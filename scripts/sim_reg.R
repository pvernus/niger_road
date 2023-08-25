## SIMULATION: Floods between Dosso and Dogondoutchi ##

graph_bau <- graph_reg %>% 
  activate(nodes) %>%
  mutate(
    betweenness_bau = scales::rescale(
      centrality_betweenness(weights = weight), to = c(0, 1))
#    closeness_bau = scales::rescale(
#      centrality_closeness_harmonic(), to = c(0, 1))
  ) %>% 
  activate(edges) %>%
  mutate(
    osm_id = if_else(.tidygraph_edge_index == 582945, 329171330, osm_id), # one network line doesn't have a n osm_id
    edge_betweenness_bau = scales::rescale(
      centrality_edge_betweenness(weights = weight, directed = FALSE),
      to = c(0, 1))
  )

# Edges' osm_id between Dosso and Dogondoutchi
# OSM raw data (map)
dosso_dogondoutchi_osm <- c(693726306, 329171334, 316976600, 329171330, 687333823, 687334008, 687334009, 688358998, 693726255, 694245256, 694245257, 329171330, 688358997)
# OSM processed network data (network analysis)
dosso_dogondoutchi_net <- c(693726255, 688358998, 316976600, 693726306, 687333823, 329171334, 329171330)

graph_dosso_dogondoutchi <- graph_bau %>%
  activate(edges) %>%
  filter(osm_id %in% dosso_dogondoutchi_net) %>% 
  as_sfnetwork()

lines_dosso_dogondoutchi <- graph_dosso_dogondoutchi %>% activate(edges) %>% as_tibble() %>% st_as_sf()
st_write(lines_dosso_dogondoutchi, here('data', 'regional_road', 'lines_dosso_dogondoutchi.shp'), append = TRUE)


graph_sim_dosso_dogondoutchi <- graph_bau %>% 
  activate(edges) %>%
  filter(!osm_id %in% dosso_dogondoutchi_net) %>% 
  mutate(
    edge_betweenness_sim = scales::rescale(
      centrality_edge_betweenness(weights = weight), to = c(0, 1)),
    diff = edge_betweenness_bau - edge_betweenness_sim
  ) %>%
  activate(nodes) %>% 
  mutate(
    betweenness_sim = scales::rescale(
      centrality_betweenness(weights = weight), to = c(0, 1)),
#    closeness_sim = scales::rescale(
#      centrality_closeness_harmonic(), to = c(0, 1)),
#    diff_clos = closeness_sim - closeness_bau,
    diff_betw = betweenness_sim - betweenness_bau
)

lines_sim <- graph_sim_dosso_dogondoutchi %>% activate(edges) %>% as_tibble() %>% st_as_sf()
nodes_sim <- graph_sim_dosso_dogondoutchi %>% activate(nodes) %>% as_tibble() %>% st_as_sf()

lines_sim <- st_make_valid(lines_sim)

st_write(lines_sim, here('data', 'regional_road', 'lines_sim.shp'), append = TRUE)
st_write(nodes_sim, here('data', 'regional_road', 'nodes_sim.shp'), append = TRUE)

save(graph_reg, graph_bau, graph_sim,
  graph_dosso_dogondoutchi, lines_dosso_dogondoutchi, graph_sim_dosso_dogondoutchi, 
  lines_sim, nodes_sim,
  file = "data/network_simulation.RData")

# Interactive map

pal = colorspace::diverging_hcl(99, "Purple-Green")

tmap_mode("view")

m2 <- tm_shape(lines_sim) +
  tm_lines(col = 'grey', alpha = .3) +
  tm_shape(lines_dosso_dogondoutchi) +
  tm_lines('red') +
  tm_shape(nodes_sim) +
  tm_bubbles(
    col = "diff_betw",
    title.col = "Centralité hausse/baisse",
    size = "betweenness_sim",
    palette = pal,
    midpoint = 0,
    scale = .2,
    alpha = .8,
    border.lwd = NA,
    style = "cont"
  )

tmap_save(m2, "outputs/simulation_test_map.html", selfcontained = TRUE)

# Static map
tmap_mode("plot")

m3 <- tm_shape(adm03) +
  tm_borders(col = "grey") +
  tm_shape(lines_sim) +
  tm_lines(col = 'lightblue', alpha = .5) +
  tm_shape(nodes_sim) +
  tm_bubbles(
    col = "diff_betw",
    title.col = "Centrality gain/loss",
    border.alpha = .5,
    size = "betweenness_bau",
    legend.size.show = FALSE,
    palette = pal,
    midpoint = 0,
    scale = .4,
    style = "cont"
  ) +
  tm_scale_bar(position = c("center", "bottom")) +
  tm_layout(title = "Simulation: flooding risks in Tsernaoua", 
            title.size = .9, legend.title.size = .9, asp = 0, 
            legend.stack = "horizontal") +
  tm_compass(type = "rose", size = 2)

tmap_save(m3, "outputs/simulation_test_map.png", width=1920, height=1080, asp=0)

# Graph
ner_voronoi_cluster <- st_read(here('layers', 'ner_voronoi_cluster.shp'))

st_join(nodes_sim, ner_voronoi_cluster) %>% st_drop_geometry() %>% 
  mutate(cluster = as_factor(cluster),
         diff = as_factor(if_else(diff_betw > 0, 'Gain', 'Loss'))
  ) %>% 
  filter(!is.na(cluster)) %>% 
  ggplot(aes(x=fct_rev(cluster), y=diff_betw, colour = diff_betw)) +
  geom_jitter(width = .2, alpha = .7) +
  scale_colour_gradient2(midpoint=0, low="#842699", mid="grey90",
                         high="#0dba38", space ="Lab" )+
  geom_hline(yintercept=0)+
  ylim(-.3, .25) +
  coord_flip()+
  labs(x = 'Food Security cluster', y = 'Network (nodes) centrality gain/loss')+
  theme_minimal() +
  theme(legend.title = element_blank())


# Zoom
bbox <- st_bbox(c(xmin = 0.071411, ymin = 14.880981, xmax = 11.286161, ymax = 15.538376), crs = st_crs(4326))

m4 <- tm_shape(adm03, bbox = bbox) +
  tm_borders(col = "grey80") +
  tm_shape(lines_dosso_dogondoutchi) +
  tm_lines('red') +
  tm_shape(lines_sim) +
  tm_lines(col = 'lightblue', alpha = .5) +
  tm_shape(nodes_sim) +
  tm_bubbles(
    col = "diff_betw",
    title.col = "Centralité +/-",
    border.alpha = .5,
    size = "betweenness_bau",
    legend.size.show = FALSE,
    palette = pal,
    midpoint = 0,
    scale = .4,
    style = "cont"
  ) +
  tm_scale_bar(position = c("center", "bottom")) +
  tm_layout(asp = 0, title.size = .9, legend.title.size = .9,
            legend.stack = "horizontal") +
  tm_compass(type = "rose", size = 2)

tmap_save(m4, "outputs/simulation_test_zoom_map.png", width=1920, height=1080)

# Zoom + intersections only (degree != 2)

m5 <- tm_shape(adm03, bbox = bbox) +
  tm_borders(col = "grey80") +
  tm_shape(lines_sim) +
  tm_lines(col = 'lightblue', alpha = .5) +
  tm_shape(nodes_sim %>% filter(degree != 2)) +
  tm_bubbles(
    col = "diff_betw",
    title.col = "Centrality gain/loss",
    size = "betweenness_bau",
    legend.size.show = FALSE,
    palette = pal,
    midpoint = 0,
    scale = .7,
    style = "cont"
  ) +
  tm_shape(Tsernaoua_poly) +
  tm_polygons(col = "red") +
  tm_scale_bar(position = c("center", "bottom")) +
  tm_layout(asp = 0,
            legend.stack = "horizontal") +
  tm_compass(type = "rose", size = 2)

tmap_save(m5, "outputs/simulation_test_zoom_inters_map.png", width=1920, height=1080)
