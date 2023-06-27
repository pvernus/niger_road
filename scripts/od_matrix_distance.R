od_matrix_marketplace <- read.csv(here('data_raw', 'od_matrix_marketplace.csv'))
od_matrix_main_road <- read.csv(here('data_raw', 'od_matrix_main_road.csv'))
od_matrix_school <- read.csv(here('data_raw', 'od_matrix_school.csv'))

region_main_road <- read_sf(here('data_raw', 'region_data', 'region_main_road_network.gpkg')) %>% 
  select(full_id:highway, type, route, name)

road_leon <- read_sf(here('data_raw', 'leon_data', 'shapefiles', 'Road_network.shp'))


school_osm <- read_sf(here('data_raw', 'region_data', 'schools_osm', 'Schools.shp'))

load(here('data', 'grappes.RData'))
load(here('data', 'ner_adm.RData'))

# MARKETPLACE

distance_marketplace <- od_matrix_marketplace %>% 
  rename(grappe = origin_id) %>% 
  mutate(distance_marketplace = total_cost)

grappe_distance_market <- distance_marketplace %>% 
  group_by(grappe) %>% 
  slice_max(distance_marketplace, n = 1) %>% 
  select(grappe, distance_marketplace) %>% 
  unique()

# MAIN ROAD

distance_main_road <- od_matrix_main_road %>% 
  rename(grappe = origin_id) %>% 
  mutate(distance_main_road = total_cost)

grappe_distance_main_road <- distance_main_road %>% 
  group_by(grappe) %>% 
  slice_max(distance_main_road, n = 1) %>% 
  select(grappe, distance_main_road) %>% 
  unique()

# SCHOOL

distance_school <- od_matrix_school %>% 
  rename(grappe = origin_id) %>% 
  mutate(distance_school = total_cost)

grappe_distance_school <- distance_school %>% 
  group_by(grappe) %>% 
  slice_max(distance_school, n = 1) %>% 
  select(grappe, distance_school) %>% 
  unique()

## JOIN GRAPPE_DISTANCE

grappe_distance <- left_join(grappe_sf, grappe_distance_market, by = 'grappe') %>% 
  left_join(grappe_distance_main_road, by = 'grappe') %>% 
  left_join(grappe_distance_school, by = 'grappe')

hist(grappe_distance_market$distance_marketplace)


a <- grappe_sf %>% 
  filter(grappe %in% c(114,115,127,156,171,180,236,269,279,323,345,370,387,388,412,418,425,431,432))

tmap_mode("view")

tm_shape(ner_adm02) +
  tm_borders() +
tm_shape(region_main_road) +
  tm_lines(alpha = .5, col = 'grey60') +
tm_shape(grappe_distance) +
  tm_bubbles(col = 'distance_main_road', size = .1, style = 'cont')



tm_shape(school_osm) +
  tm_bubbles(col = 'blue', size = .1, alpha = .5) +
