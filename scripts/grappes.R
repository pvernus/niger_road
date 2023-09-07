grappe_gps_ner2018 <- read_dta(here('data_raw','ner_2018_ehcvm','grappe_gps_ner2018.dta'))
ehcvm_welfare_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/ehcvm_welfare_ner2018.dta')

load(here("data", "ner_adm.RData"))

grappe_gps_survey <- grappe_gps_ner2018 |> 
  left_join(survey_welfare |> select(grappe, id_adm2, hhweight), by = "grappe") |> 
  distinct(grappe, .keep_all = TRUE)
  
grappe_gps <- grappe_gps_survey |> 
  filter(!is.na(coordonnes_gps__Latitude))

grappe_gps <- grappe_gps |> 
  rename(gps_accuracy = "coordonnes_gps__Accuracy", gps_alt = "coordonnes_gps__Altitude",
         x = "coordonnes_gps__Longitude", y = "coordonnes_gps__Latitude")

# transform to sf 
grappe_sf <- st_as_sf(grappe_gps, 
                coords = c("x", "y"),
                crs = 4326,
                agr = "constant")

grappe_new_crs <- st_transform(grappe_sf, crs = 'EPSG:32631') # transform to crs with unit in meter
grappe_buffer <- st_buffer(grappe_new_crs, dist = 10000) # 10km buffer
plot(st_geometry(grappe_buffer))
grappe_buffer <- st_transform(grappe_buffer, crs = 4326) # convert back to crs:4326

#leaflet::leaflet() %>% 
#  addTiles() %>% 
#  addMeasure(primaryLengthUnit = "meters") %>% 
#  addMarkers(data = grappe_new_crs) %>% 
#  addPolygons(data = buff)

# save data
save(grappe_sf, grappe_buffer, 
     file = here('data', 'grappes.RData'))

st_write(grappe_sf, here('layers', 'grappes.gpkg'), delete_layer = TRUE)
st_write(grappe_buffer, here('layers', 'grappe_buffer.gpkg'), delete_layer = TRUE)


