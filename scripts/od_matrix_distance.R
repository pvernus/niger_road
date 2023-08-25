load(here('data', 'grappes.RData'))

algs <- qgis_algorithms()
bb <- getbb("Niger", featuretype = "country")

grappe <- grappe_sf %>% 
  mutate(grappe = as.character(grappe))

## MARKETPLACE
load(here('data', 'osm_points_marketplace.RData'))

# qgis_show_help("native:shortestline")
# qgis_get_argument_specs("native:shortestline")
result <- qgis_run_algorithm("native:shortestline",
                             SOURCE = grappe_sf,
                             DESTINATION = marketplace,
                             METHOD = 0,
                             NEIGHBORS = 3,
                             OUTPUT = here('data', 'od_matrix_marketplace.csv')
)

qgis_knn_marketplace <- read_csv(qgis_extract_output(result, "OUTPUT"))

distance_to_marketplace <- qgis_knn_marketplace %>% 
  summarize(dist_to_marketplace = mean(distance), .by = grappe)

## SCHOOL
load(here('data', 'osm_school.RData'))

result <- qgis_run_algorithm("native:shortestline",
                             SOURCE = grappe_sf,
                             DESTINATION = school,
                             METHOD = 0,
                             NEIGHBORS = 3,
                             OUTPUT = here('data', 'od_matrix_school.csv')
)

qgis_knn_school <- read_csv(qgis_extract_output(result, "OUTPUT"))

distance_to_school <- qgis_knn_school %>% 
  summarize(dist_to_school = mean(distance), .by = grappe)

## MAIN ROAD
load(here('data', 'ner_mainroad_lines.RData'))

result <- qgis_run_algorithm("native:shortestline",
                             SOURCE = grappe_sf,
                             DESTINATION = ner_mainroad_lines,
                             METHOD = 0,
                             NEIGHBORS = 3,
                             OUTPUT = here('data', 'od_matrix_mainroad.csv')
)

qgis_knn_mainroad <- read_csv(qgis_extract_output(result, "OUTPUT"))

distance_to_mainroad <- qgis_knn_mainroad %>% 
  summarize(dist_to_mainroad = mean(distance), .by = grappe)

# Merge 3 datasets
distance_to_facility <- inner_join(distance_to_marketplace, distance_to_school, by = 'grappe') %>% 
  inner_join(distance_to_mainroad, by = 'grappe')

save(distance_to_marketplace, distance_to_school, distance_to_mainroad, distance_to_facility, 
     file = here('data', 'distance_to_facility.RData'))
st_write(distance_to_facility, here('data', 'distance_to_facility.gpkg'), delete_layer = TRUE)






cluster_scaled %>% select(-ends_with('.y'))



# region_main_road <- read_sf(here('data_raw', 'region_data', 'region_main_road_network.gpkg')) %>% select(full_id:highway, type, route, name)
# road_leon <- read_sf(here('data_raw', 'leon_data', 'shapefiles', 'Road_network.shp'))
# school_osm <- read_sf(here('data_raw', 'region_data', 'schools_osm', 'Schools.shp'))

load(here('data', 'grappes.RData'))
# load(here('data', 'ner_adm.RData'))

## MARKETPLACE

distance_marketplace <- od_matrix_marketplace %>% 
  rename(grappe = origin_id) %>% 
  mutate(distance_marketplace = total_cost)

distance_market_by_grappe <- distance_marketplace %>% 
  group_by(grappe) %>% 
  slice_min(distance_marketplace, n = 1) %>% 
  select(grappe, distance_marketplace) %>% 
  unique()

## MAIN ROAD

distance_main_road <- od_matrix_main_road %>% 
  rename(grappe = origin_id) %>% 
  mutate(distance_main_road = total_cost)

distance_main_road_by_grappe <- distance_main_road %>% 
  group_by(grappe) %>% 
  slice_min(distance_main_road, n = 1) %>% 
  select(grappe, distance_main_road) %>% 
  unique()

## SCHOOL

distance_school <- od_matrix_school %>% 
  rename(grappe = origin_id) %>% 
  mutate(distance_school = total_cost)

distance_school_by_grappe <- distance_school %>% 
  group_by(grappe) %>% 
  slice_min(distance_school, n = 1) %>% 
  select(grappe, distance_school) %>% 
  unique()

save(distance_market_by_grappe, distance_main_road_by_grappe, distance_school_by_grappe, file = here('data', 'road_distance.RData'))



## JOIN GRAPPE_DISTANCE

grappe_distance <- left_join(grappe_sf, distance_market_by_grappe, by = 'grappe') %>% 
  left_join(distance_main_road_by_grappe, by = 'grappe') %>% 
  left_join(distance_school_by_grappe, by = 'grappe')

hist(grappe_distance_market$distance_marketplace)


a <- grappe_sf %>% 
  filter(grappe %in% c(114,115,127,156,171,180,236,269,279,323,345,370,387,388,412,418,425,431,432))

tmap_mode("view")

tm_shape(ner_adm02) +
  tm_borders() +
tm_shape(grappe_distance) +
  tm_bubbles(col = 'distance_marketplace', style="cont", size = .1, style = 'cont')



tm_shape(school_osm) +
  tm_bubbles(col = 'blue', size = .1, alpha = .5) +
