bb <- getbb("Niger", featuretype = "country")
osm_school <- bb %>%
  opq(timeout = 50) %>% 
  add_osm_feature(key = 'amenity', value = 'school') %>% 
  osmdata_sf()

school <- osm_school$osm_polygons %>% 
  st_centroid() %>% 
  select(osm_id, name)

st_write(school, here('data', 'osm_school.gpkg'), delete_layer = TRUE)

save(school, file = here('data', 'osm_school.RData'))