source("scripts/library.R") # load packages
load("data/population_admin_id.RData")

settlements <- st_read("data_raw/ner_localites-21-10/Localités 21-10/Localites.gpkg", layer = "REACH_Localites")

settlements <- settlements %>% 
  clean_names() %>% 
  filter(type %in% c("VA", "H", "QT")) %>% # we only keep villages (VA), hamlets (H), and urban neighborhoods (QT)
  mutate(type = case_when(
    type == "VA" ~ "village",
    type == "H" ~ "hamlet",
    type == "QT" ~ "urban_neighborhood"
  )
           )

# Settlements 
settlements_id <- settlements %>% 
  mutate(id_adm3 = admin3pcode,
         id_adm2 = str_sub(id_adm3, 1, 9),
         area = if_else(type == "urban_neighborhood", 1, 2)) %>% 
  rename(long = longitude_x, lat = latitude_y, geometry = geom)

setdiff(adm03_id$id_adm3, settlements_id$id_adm3)
setdiff(adm3_pop_id$id_adm3, settlements_id$id_adm3)
# Two adm3 are missing in the settlements database: 
# "NER005007001" : Azarori, Niger
# "NER001003004" : Fachi, Niger

# save clean data
save(settlements, settlements_id, file = "data/settlements_id.RData")

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



