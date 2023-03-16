source("scripts/library.R") # load packages
load("data/population_admin_id.RData")
load("data/settlements_id.RData")

# map
pal <- colorFactor(palette = "Set3", 
                domain = settlements_id$type)

map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  addCircles(data = settlements_id[settlements_id$type == "village",], 
             lng = ~long, lat = ~lat, 
             label = ~localite_ref_name,
             color = ~"red",
             radius = 200,
             stroke = FALSE, 
             fillOpacity = .5,
             group = "village") %>% 
  addCircles(data = settlements_id[settlements_id$type == "urban_neighborhood",], 
             lng = ~long, lat = ~lat, 
             label = ~localite_ref_name,
             color = ~"green",
             radius = 200,
             stroke = FALSE, 
             fillOpacity = .5,
             group = "urban_neighborhood") %>% 
  addCircles(data = settlements_id[settlements_id$type == "hamlet",], 
             lng = ~long, lat = ~lat, 
             label = ~localite_ref_name,
             color = ~"blue",
             radius = 200,
             stroke = FALSE, 
             fillOpacity = .5,
             group = "hamlet") %>% 
  addLayersControl(baseGroups = c("background 1", "background 2"),
                   overlayGroups = c("village", "urban_neighborhood", "hamlet"),
                   position = "bottomright",
                   options = layersControlOptions(collapsed = FALSE)
                   )
map



