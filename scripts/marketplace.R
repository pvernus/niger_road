source("scripts/library.R") # load packages
source("R/functions.R") # load functions

load("data/network.RData")
# load("data/marketplace.RData")
# load("data/ner_adm.RData")

osm_marketplace <- opq(bbox = c(-0.5,11.16,16.42,23.7)) %>%
  add_osm_feature(key = 'amenity', value = 'marketplace') %>% 
  osmdata_sf()

marketplace <- osm_marketplace$osm_polygons %>% 
  select(osm_id, name, amenity, landuse, geometry)

st_reason_invalid(marketplace)
st_reason_invalid(adm03)

marketplace_adm <- marketplace[adm03, ]

marketplace_sf <- st_join(marketplace_adm, adm03) %>% 
  arrange(id_adm3) %>% 
  distinct(osm_id, .keep_all = TRUE) # remove cross-boundaries duplicates

## Key = 'landuse', value = 'retail'

osm_landuse_retail <- opq(bbox = c(-0.5,11.16,16.42,23.7)) %>%
  add_osm_feature(key = 'landuse', value = 'retail') %>% 
  osmdata_sf()

landuse_retail <- osm_landuse_retail$osm_polygons %>% 
  select(osm_id, name, amenity, landuse, geometry)

st_reason_invalid(landuse_retail)

landuse_retail_adm <- landuse_retail[adm03, ]

landuse_retail_sf <- st_join(landuse_retail_adm, adm03) %>% 
  arrange(id_adm3) %>% 
  distinct(osm_id, .keep_all = TRUE) # remove cross-boundaries duplicates 

## Key = 'shop', value = 'supermarket'

osm_shop_supermarket <- opq(bbox = c(-0.5,11.16,16.42,23.7)) %>%
  add_osm_feature(key = 'shop', value = 'supermarket') %>% 
  osmdata_sf()

shop_supermarket <- osm_shop_supermarket$osm_polygons %>% 
  select(osm_id, shop, geometry)

st_reason_invalid(shop_supermarket)

shop_supermarket_adm <- shop_supermarket[adm03, ]

shop_supermarket_sf <- st_join(shop_supermarket_adm, adm03) %>% 
  mutate(amenity = NA, landuse = NA) %>% 
  arrange(id_adm3) %>% 
  distinct(osm_id, .keep_all = TRUE) # remove cross-boundaries duplicates 

## Combine datasets

x <- rbind(marketplace_sf, landuse_retail_sf)
x <- rbind(x %>% mutate(shop = NA), shop_supermarket_sf %>% mutate(name = NA))
marketplace <- x[!duplicated(x$osm_id), ]

marketplace <- marketplace %>% relocate(shop, .after = landuse) %>% 
  select(!c(rowcacode1, rowcacode2, rowcacode3, shape_leng, shape_area))

save(marketplace, file = "data/marketplace.RData")

marketplace %>% 
  group_by(adm_01) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

tmap_mode("view")

tm_shape(adm03) +
  tm_borders() +
tm_shape(marketplace) +
  tm_bubbles(col = 'blue', alpha = .5, scale = .5)

tm_shape(adm03) +
  tm_borders() +
tm_shape(nodes_ner) +
  tm_bubbles(col = "betweenness_dist", 
             palette = viridis::viridis(4), alpha = .3, 
             border.lwd = NA, scale = .3) +
tm_shape(marketplace) +
  tm_bubbles(col = 'blue', alpha = .8, scale = .4, shape = 2)

  
