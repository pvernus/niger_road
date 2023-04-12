# load("data/data.RData")
# load("data/ipc_sf.RData")
# load("data/grid3_pop.RData")

ipc_sf <- ipc %>% 
  select(id_adm1, adm1_name, id_adm2, adm2_name, exercise_code:phase2, phase35) %>% 
  mutate(prop_ph35 = round(100 * (phase35/population), 2)) %>% # new variable: share of the pop. in phase 3-5
  relocate(prop_ph35, .after = "phase35") %>% 
  merge(adm02, by = "id_adm2") %>% 
  st_as_sf()

# trend by region
data_trends <- ipc_sf %>% 
  st_drop_geometry() %>% 
  filter(exercise_year > 2015) %>% 
  group_by(exercise_year, adm1_name) %>% 
  summarize(min = min(prop_ph35),
            max = max(prop_ph35),
            median = median(prop_ph35)
            )

one_line <- function(data) {
fig <- plot_ly(data, x = ~exercise_year, y = ~max, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = 'Max.')
fig <- fig %>% add_trace(y = ~min, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', 
                         line = list(color = 'transparent'), 
                         showlegend = FALSE, name = 'Min.')
fig <- fig %>% add_trace(x = ~exercise_year, y = ~median, type = 'scatter', mode = 'lines',
                         line = list(color='rgb(0,100,80)'),
                         name = 'Median') %>% 
              add_text(x = 2019, y = 60, text = ~unique(adm1_name), color = I("black"))
fig <- fig %>% layout(title = "Median, Max. and Min. % of population in Phase 3-5 in Niger, 2016-2022",
                      paper_bgcolor='transparent', plot_bgcolor= 'transparent',
                      xaxis = list(title = "Year",
                                   gridcolor = 'rgb(236,236,236)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "% of population in Phase 3-5",
                                   gridcolor = 'rgb(236,236,236)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE))

}

df <- data_trends %>% group_by(adm1_name)
df <- df %>% do(mafig = one_line(.))
fig <- df %>% subplot(nrows = 2)
fig


data_trends |> 
  filter(exercise_year > 2017) |> 
ggplot(aes(x = exercise_year)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "honeydew2") +
  geom_line(aes(y = median), colour = "springgreen3") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold")) +
  facet_wrap(~adm1_name, nrow = 3) +
  labs(title = "Median, Max. and Min. % of population in Phase 3-5 in Niger, 2016-2022",
       caption = "source: Integrated Food Security Phase Classification (IPC)", 
       x = "Year", y = "% of population per ADM2")




# Descr. stat. on 2020-22 data            
ipc_2020_adm2 <- ipc_sf %>% 
  filter(exercise_year >= 2020) %>% 
  group_by(adm2_name, id_adm2) %>% 
  summarize(median = median(prop_ph35),
            mean = mean(prop_ph35),
            min = min(prop_ph35),
            max = max(prop_ph35),
            range = max - min,
            iqr = IQR(prop_ph35),
            sd = sd(prop_ph35),
            var = var(prop_ph35)
  )
tm_shape(ipc_2020_adm2) + tm_polygons("iqr")

bins <- c(0, 10, 20, 65)
bins_sd <- c(0, 5, 10, 15)
pal <- colorBin("YlOrRd", domain = ipc_2020_adm2$median, bins = bins)
pal_sd <- colorBin("YlOrRd", domain = ipc_2020_adm2$sd, bins = bins_sd)
pal_line <- colorNumeric('Oranges', lines_ner$centrality_edge_betweenness)

# map
map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  addPolygons(data = ipc_2020_adm2, # Add the grid layer
              color= alpha("white", 0.4),
              fillColor = ~pal(median),
              fillOpacity = 0.8,
              label = ~median,
              group = "Median") %>%
  addPolygons(data = ipc_2020_adm2, # Add the grid layer
              fillColor = ~pal_sd(sd),
              color= alpha("white", 0.4),
              fillOpacity = 0.8,
              label = ~round(sd,1),
              group = "Standard deviation") %>%
  addPolygons(data = adm03, # Add the communes (ADM3) boundaries layer
              color="black",
              fill = FALSE,
              weight = 1,
              label = ~adm_03,
              group = "Departements") %>%
  addPolylines(data = lines_ner,
               color = ~pal_line(centrality_edge_betweenness),
               group = "Road") %>% 
  addCircles(data = nodes_ner,
             color = ~pal_line(betweenness),
             fillColor = ~pal_line(betweenness),
             radius = ~datawizard::rescale(betweenness, to = c(0, 1000)),
             group = "Road") %>% 
  addLayersControl(baseGroups = c("background 1", "background 2"),
                   overlayGroups = c("Departements", "Median", "Standard deviation", "Road"),
                   position = "bottomright",
                   options = layersControlOptions(collapsed = FALSE)
  )
map




# bivariate map median/sd % pop. in phase 3-5
data <- bi_class(ipc_2020_adm2, x = median, y = sd, style = "quantile", dim = 3)
legend <-   bi_legend(pal = "GrPink",
                      dim = 3,
                      xlab = "Median",
                      ylab = "SD",
                      size = 8)
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Share of the population in IPC phase 3-5 by departements, 2020-22",
    subtitle = "Median and standard deviation (SD)") +
  bi_theme() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8))

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)



ggplot() +
  geom_sf(data = ipc_2020_adm2, aes(fill = median), alpha = .3) +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4)) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()


ipc_nodes <- st_join(nodes_ner, ipc_2020_adm2) %>%
  st_drop_geometry() %>% 
  select(nodeID, degree, betweenness, adm2_name, id_adm2, median, mean, iqr)

ggplot(ipc_nodes, aes(x = median, y = betweenness)) +
    geom_point()
  



save(ipc, ipc_sf, ipc_2020_adm2, file = "data/ipc_sf.RData")
