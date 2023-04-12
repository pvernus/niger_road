# kernel density curve
ggplot(nodes_ner, aes(x=betweenness_dist)) + 
  geom_density(alpha=.2, fill="#FF6666")

ggplot(nodes_ner) +
  geom_histogram(aes(x=betweenness_dist), alpha=.2, fill="#FF6666") +
  geom_histogram(aes(x=betweenness), alpha=.2, fill='blue') +

# If the skewness of the predictor variable is less than -1 or greater than +1, the data is highly skewed
e1071::skewness(nodes_ner$betweenness_dist)
e1071::skewness(nodes_ner$betweenness)

# positive skewness: the mean of the data values is larger than the median, the data distribution is right-skewed.

# using weight-distance tends to slightly reduce skewness


ggplot(data = nodes_ner, aes(x = betweenness_dist, y = betweenness)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()


ggplot(data = nodes_ner, aes(x = betweenness_dist, y = betweenness)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()


ggplot(nodes_ner, aes(x = factor(degree), 
                    y = betweenness_dist, 
                    color = factor(degree))) +
  geom_boxplot(width = 0.5) +
  geom_point(position = position_jitter(height = 0), 
             size = 1, alpha = 0.5) +
  labs(x = NULL, y = "betweenness_dist") +
  guides(color = "none")


nodes_ner %>% 
  st_drop_geometry() %>% 
  group_by(factor(degree)) %>% 
  summarize(mean = mean(betweenness),
            min = min(betweenness),
            max = max(betweenness),
            median = median(betweenness),
            n = n())


centr_eigen(graph)$centralization

deg <- degree(graph)
tmax <- centr_degree_tmax(graph, loops=FALSE)
centralize(deg, tmax)

nodes_ner %>% 
  arrange(desc(betweenness)) %>% 

  

  
  
is_connected(ner_graph)
count_components(ner_graph)
names(igraph::components(ner_graph))
igraph::components(ner_graph)$csize
ner_graph$membership <- igraph::components(ner_graph)$membership

transitivity(ner_graph, type = "global")
transitivity(ner_graph, type = "local")

dendrogram <- cluster_edge_betweenness(ner_graph)
plot_dendrogram(dendrogram)  
  
nodes_components <- ner_graph %>% 
  activate(nodes) %>% 
  mutate(components = factor(group_components())) %>%
  filter(components != 1) %>% 
  as_tibble() %>% st_as_sf()

tmap_mode("view")

tm_shape(nodes_components) +
  tm_bubbles(col = "components",  alpha = .3, scale = .1, popup.vars = "components") +
tm_shape(lines) +
  tm_lines(alpha = .5)
  
