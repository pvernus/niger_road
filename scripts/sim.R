# load("data/ipc_sf.RData")
# load("data/network.RData")
# load("data/ner_adm.RData")

ggplot(nodes_ner, aes(x = betweenness_dist)) +
  geom_histogram() +
  theme_minimal()

ggplot(lines_ner, aes(x = edge_betweenness_dist)) +
  geom_histogram() +
  theme_minimal()


gg_nodes <- nodes_ner %>% 
  filter(degree != 2) %>% 
  st_join(adm03) %>% 
  mutate(
    percent_rank_betweenness = percent_rank(betweenness_dist),
    criticality_betweenness = as_factor(case_when(
      percent_rank_betweenness >= .99 ~ "Top criticality (P99)",
      percent_rank_betweenness >= .95 & percent_rank_betweenness < .99 ~ "High criticality (P95-P99)",
      .default = "Medium - Low criticality (P90)"
    ))
         )

gg_lines <- lines_ner %>% 
  st_join(adm03) %>% 
  mutate(
    percent_rank_betweenness = percent_rank(edge_betweenness_dist),
    criticality_betweenness = as_factor(case_when(
      percent_rank_betweenness >= .99 ~ "Top criticality (P99)",
      percent_rank_betweenness >= .95 & percent_rank_betweenness < .99 ~ "High criticality (P95-P99)",
      .default = "Medium - Low criticality (P90)"
    ))
  )

tmap_mode("view")

m1 <- tm_shape(gg_nodes) +
  tm_bubbles(col = "criticality_betweenness", scale = .1, alpha = .5, border.col = NA,
             palette = c("grey90", "orange", "red")) +
tm_shape(gg_lines) +
  tm_lines(col = "criticality_betweenness", alpha = .5,
           palette = c("grey90", "orange", "red"))

tmap_save(m1, "outputs/top_criticality_betw.html", selfcontained = TRUE)

  
## SIMULATION: Floods in Tsernaoua ##

graph_bau <- ner_graph %>%
  st_join(adm03) %>% 
  activate(nodes) %>%
  mutate(
    betweenness_bau = scales::rescale(
      centrality_betweenness(weights = weight), to = c(0, 1)),
    closeness_bau = scales::rescale(
      centrality_closeness_harmonic(), to = c(0, 1))
  ) %>% 
  activate(edges) %>%
  mutate(
    edge_betweenness_bau = scales::rescale(
      centrality_edge_betweenness(weights = weight, directed = FALSE),
      to = c(0, 1))
  )

Tsernaoua_poly <- adm03 %>% 
  filter(id_adm3 == "NER005003004") # Tsernaoua

graph_sim <- graph_bau %>% 
  activate(nodes) %>%
  filter(node_is_disjoint(Tsernaoua_poly)) %>% 
  mutate(
    degree = centrality_degree(),
    betweenness_sim = scales::rescale(
      centrality_betweenness(weights = weight), to = c(0, 1)),
    closeness_sim = scales::rescale(
      centrality_closeness_harmonic(), to = c(0, 1)),
    diff_betw = betweenness_sim - betweenness_bau,
    diff_clos = closeness_sim - closeness_bau
  ) %>% 
  activate(edges) %>% 
  filter(edge_is_disjoint(Tsernaoua_poly)) %>% 
  mutate(
    edge_betweenness_sim = scales::rescale(
      centrality_edge_betweenness(weights = weight), to = c(0, 1)),
    diff = edge_betweenness_bau - edge_betweenness_sim
    ) %>% 
  as_sfnetwork()

lines_sim <- graph_sim %>% activate(edges) %>% as_tibble() %>% st_as_sf()
nodes_sim <- graph_sim %>% activate(nodes) %>% as_tibble() %>% st_as_sf()

# Interactive map

pal = colorspace::diverging_hcl(99, "Purple-Green")

tmap_mode("view")

m2 <- tm_shape(lines_sim) +
  tm_lines(col = 'grey', alpha = .3) +
tm_shape(nodes_sim) +
  tm_bubbles(
    col = "diff_betw",
    title.col = "Centrality loss/gain",
    size = "betweenness_sim",
    palette = pal,
    midpoint = 0,
    scale = .2,
    alpha = .5,
    border.lwd = NA,
    style = "cont"
  ) +
tm_shape(Tsernaoua_poly) +
  tm_polygons(col = "red")

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
tm_shape(Tsernaoua_poly) +
  tm_polygons(col = "red") +
tm_scale_bar(position = c("center", "bottom")) +
tm_layout(title = "Simulation: flooding risks in Tsernaoua", 
          title.size = .9, legend.title.size = .9, asp = 0, 
            legend.stack = "horizontal") +
tm_compass(type = "rose", size = 2)

tmap_save(m3, "outputs/simulation_test_map.png", width=1920, height=1080, asp=0)

# Zoom
bbox <- st_bbox(c(xmin = 3.0087, ymin = 12.5022, xmax = 6.9885, ymax = 15.4946), crs = st_crs(4326))

m4 <- tm_shape(adm03, bbox = bbox) +
  tm_borders(col = "grey80") +
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
  tm_shape(Tsernaoua_poly) +
  tm_polygons(col = "red") +
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

# Plot centrality gain/loss and IPC Food security status

ipc_sim <- ipc %>% 
  st_drop_geometry() %>% 
  filter(exercise_year == 2020 & chtype == "current") %>% 
  select(id_adm2, adm_02 = adm2_name, exercise_year, exercise_label, population, phase_class) %>% 
  group_by(id_adm2, exercise_year) %>% 
  summarize(
    population = mean(population, na.rm = TRUE),
    phase_class = round(mean(phase_class, na.rm = TRUE), 0)
    )

nodes_sim_ipc <- nodes_sim %>% 
  st_drop_geometry() %>% 
  select(id_adm2, adm_02, betweenness_bau, betweenness_sim, closeness_bau, closeness_sim, diff_betw, diff_clos) %>% 
  group_by(id_adm2, adm_02) %>% 
  summarize(
    diff_clos = mean(diff_clos, na.rm = TRUE),
    )

df <- nodes_sim_ipc %>% 
  left_join(ipc_sim, by = "id_adm2") %>% 
  group_by(phase_class) %>% 
  summarize(
    population = mean(population, na.rm = TRUE),
    diff_clos = mean(diff_clos, na.rm = TRUE),
    ) %>% 
  filter(!is.na(phase_class))
  
df$w <- cumsum(df$population)
df$wm <- df$w - df$population
df$wt <- with(df, wm + (w - wm)/2)

p <- ggplot(df, aes(ymin = 0))

p + geom_rect(aes(xmin = wm, xmax = w,
                  ymax = diff_clos, fill = as_factor(phase_class))) +
  labs(x = "Population", y = "Average closenness loss") +
  scale_fill_brewer(name = "Integrated Food Security Phase Classification", 
                    palette = "YlOrRd") + 
  geom_hline(yintercept = 0, col = 'grey60') +
  theme_minimal() +
  theme(legend.direction="horizontal", legend.position="bottom")

# Facet by region

nodes_sim_ipc <- nodes_sim %>% 
  st_drop_geometry() %>% 
  select(id_adm1, adm_01, id_adm2, adm_02, betweenness_bau, betweenness_sim, closeness_bau, closeness_sim, diff_betw, diff_clos) %>% 
  group_by(id_adm1, adm_01, id_adm2, adm_02) %>% 
  summarize(
    diff_clos = mean(diff_clos, na.rm = TRUE),
  )

df_facet <- nodes_sim_ipc %>% 
  left_join(ipc_sim, by = "id_adm2") %>% 
  group_by(id_adm1, adm_01, phase_class) %>% 
  summarize(
    population = mean(population, na.rm = TRUE),
    diff_clos = mean(diff_clos, na.rm = TRUE),
  ) %>% 
  filter(!is.na(phase_class))

df_facet$w <- cumsum(df_facet$population)
df_facet$wm <- df_facet$w - df_facet$population
df_facet$wt <- with(df_facet, wm + (w - wm)/2)

p + geom_rect(aes(xmin = wm, xmax = w,
                        ymax = diff_clos, fill = as_factor(phase_class))) +
  labs(x = "Population", y = "Average closenness loss") +
  scale_fill_brewer(name = "Integrated Food Security Phase Classification", 
                    palette = "YlOrRd") + 
  geom_hline(yintercept = 0, col = 'grey60') +
  theme_minimal() +
  theme(legend.direction="horizontal", legend.position="bottom") +
  facet_wrap(~adm_01, scales = "free_x")




