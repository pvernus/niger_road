load("data/data.RData")

# map
map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  addCircles(data = settlements[settlements$type == "village",], 
             lng = ~long, lat = ~lat, 
             label = ~localite_ref_name,
             color = ~"red",
             radius = 400,
             stroke = FALSE, 
             fillOpacity = .5,
             group = "village") %>% 
  addCircles(data = settlements[settlements$type == "urban_neighborhood",], 
             lng = ~long, lat = ~lat, 
             label = ~localite_ref_name,
             color = ~"green",
             radius = 400,
             stroke = FALSE, 
             fillOpacity = .5,
             group = "urban_neighborhood") %>% 
  addCircles(data = settlements[settlements$type == "hamlet",], 
             lng = ~long, lat = ~lat, 
             label = ~localite_ref_name,
             color = ~"blue",
             radius = 400,
             stroke = FALSE, 
             fillOpacity = .5,
             group = "hamlet") %>% 
  addPolygons(data = ner_grid, # Add the grid layer
              color=alpha("white", 0.4),
              fill = TRUE,
              fillColor = "blue",
              fillOpacity = 0.2,
              label = ~grid_id,
              group = "grid") %>%
  addPolygons(data = adm03, # Add the communes (ADM3) boundaries layer
              color="black",
              weight = 1,
              label = ~adm_03,
              group = "ADM3") %>%
  addLayersControl(baseGroups = c("background 1", "background 2"),
                   overlayGroups = c("village", "urban_neighborhood", "hamlet", "grid", "ADM3"),
                   position = "bottomright",
                   options = layersControlOptions(collapsed = FALSE)
  )
map



# map
map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  addCircles(data = settlements[settlements$area == "2",], 
             lng = ~long, lat = ~lat, 
             label = ~localite_ref_name,
             color = ~"green",
             radius = 500,
             stroke = FALSE, 
             fillOpacity = .2,
             group = "rural") %>% 
  addCircles(data = settlements[settlements$area == "1",], 
             lng = ~long, lat = ~lat, 
             label = ~localite_ref_name,
             color = ~"red",
             radius = 500,
             stroke = FALSE, 
             fillOpacity = .2,
             group = "urban") %>% 
  addPolygons(data = ner_grid, # Add the grid layer
              color=alpha("white", 0.4),
              fill = TRUE,
              fillColor = "blue",
              fillOpacity = 0.2,
              label = ~grid_id,
              group = "grid") %>%
  addPolygons(data = adm03, # Add the communes (ADM3) boundaries layer
              color="black",
              fillColor = FALSE,
              weight = 1,
              label = ~adm_03,
              group = "ADM3") %>%
  addLayersControl(baseGroups = c("background 1", "background 2"),
                   overlayGroups = c("urban", "ruran", "grid", "ADM3"),
                   position = "bottomright",
                   options = layersControlOptions(collapsed = FALSE)
  )
map

