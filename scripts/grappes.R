load(here('data','full_survey.RData'))

grappe_gps_survey <- grappe_gps_ner2018 |> 
  left_join(survey_welfare |> select(grappe, id_adm2, hhweight), by = "grappe") |> 
  distinct(grappe, .keep_all = TRUE)
  
grappe_gps <- grappe_gps_survey |> 
  filter(!is.na(coordonnes_gps__Latitude))

grappe_gps <- grappe_gps |> 
  rename(gps_accuracy = "coordonnes_gps__Accuracy", gps_alt = "coordonnes_gps__Altitude",
         x = "coordonnes_gps__Longitude", y = "coordonnes_gps__Latitude")

# transform to sf 
grappe_sf = st_as_sf(grappe_gps, 
                coords = c("x", "y"),
                crs = 4326,
                agr = "constant")

# Buffer

grappe_buffer <- st_buffer(grappe_sf, dist = 10000) # 10km radius TO BE DISCUSSED

tm_shape(grappe_buffer) +
  tm_polygons(alpha = 0) +
  tm_shape(grappe_sf) +
  tm_symbols(col = "red", size = .1)


## Cropland

geodata::cropland(source = "QED", path = here("data_raw"))
cropland_qed_2015 <- raster("data_raw/geosurvey_cropland.tif")

geodata::cropland(source = "WorldCover", path = here("data_raw"))
cropland_wcover <- raster("data_raw/WorldCover_cropland_30s.tif")

geodata::cropland(source = "GLAD", path = here("data_raw"), year = 2019)
cropland_glad_2019 <- raster("data_raw/glad_cropland_2019.tif")

geodata::cropland(source = "GLAD", path = here("data_raw"), year = 2003)
cropland_glad_2003 <- raster("data_raw/glad_cropland_2003.tif")

plot(cropland_wcover)
plot(cropland_glad_2019)

coverage_weighted_by_buffer <- function(x) {
  
  x_by_grappe <- exactextractr::exact_extract(x, grappe_buffer)
  x_by_grappe <- x_by_grappe |> 
    rbindlist(idcol = "id") |> 
    mutate(id = as.numeric(id)) |> 
    group_by(id) |> 
    summarize(wght_mean = sum(value * coverage_fraction) / sum(coverage_fraction))
  
}

cropland_qed15_grappe <- coverage_weighted_by_buffer(cropland_qed_2015) |>
  rename(cropland_qed15 = "wght_mean")
cropland_wcover_grappe <- coverage_weighted_by_buffer(cropland_wcover) |>
  rename(cropland_wcover = "wght_mean")
cropland_glad19_grappe <- coverage_weighted_by_buffer(cropland_glad_2019) |> 
  rename(cropland_glad19 = "wght_mean")
cropland_glad03_grappe <- coverage_weighted_by_buffer(cropland_glad_2003) |> 
  rename(cropland_glad03 = "wght_mean")

cropland <- grappe_sf |> 
  rowid_to_column(var = "id") |> 
  left_join(cropland_wcover_grappe, by = "id") |> 
  left_join(cropland_glad19_grappe, by = "id") |> 
  left_join(cropland_glad03_grappe, by = "id") |> 
  rowwise() |> 
  mutate(pmin = pmin(cropland_wcover, cropland_glad19),
         pmax = pmax(cropland_wcover, cropland_glad19),
         cropland = runif(1, pmin, pmax),
         cropland_loss_0319 = cropland_glad19 - cropland_glad03
         ) |> 
  st_drop_geometry()

## Crop production
  
spam_2017_ner <- read_excel(here("data_raw", "spam_2017_ner.xlsx"))

crop_production_by_cell <- spam_2017_ner |> 
  group_by(cell5m, x, y) |> 
  summarize(
    crop_diversity = simpson(ends_with("_a")),
    crop_production_total = sum(across(ends_with("_a"))),
    crop_production_pmil = sum(pmil_a),
    crop_production_sorg = sum(sorg_a),
    crop_production_cowp = sum(cowp_a),
    crop_production_vege = sum(vege_a)
  ) |> 
  ungroup()

crop_production_sf = st_as_sf(crop_production_by_cell, 
                                 coords = c("x", "y"),
                                 crs = 4326,
                                 agr = "aggregate") |> 
  rowid_to_column()

tm_shape(crop_production_sf) +
  tm_symbols(col = "crop_production_pmil", size = .1, border.alpha = 0) +
  tm_shape(grappe_buffer) +
  tm_polygons(alpha = 0)

crop_production_by_grappe <- st_intersection(crop_production_sf, grappe_buffer) |> 
  mutate(int_name = paste0(rowid, "-", grappe))

tm_shape(crop_production_by_grappe) +
  tm_symbols(col = "crop_production_pmil", size = .1, border.alpha = 0) +
  tm_shape(grappe_buffer) +
  tm_polygons(alpha = 0)

crop_production <- crop_production_by_grappe |> 
  group_by(grappe) |> 
  summarize(crop_production_diversity = mean(crop_diversity),
            crop_production_total = mean(crop_production_total),
            crop_production_pmil = mean(crop_production_pmil),
            crop_production_sorg = mean(crop_production_sorg),
            crop_production_cowp = mean(crop_production_cowp),
            crop_production_vege = mean(crop_production_vege)
  ) |> 
  st_drop_geometry()

# save data
save(grappe_sf, grappe_buffer, 
     cropland_qed_2015, cropland_wcover, cropland_glad_2019, cropland_glad_2003,
     cropland_qed15_grappe, cropland_wcover_grappe, cropland_glad19_grappe, cropland_glad03_grappe,
     cropland, 
     spam_2017_ner, crop_production_by_cell, crop_production_sf, crop_production_by_grappe, crop_production, 
     file = "data/grappes.RData")



# map 
load("data/ner_adm.RData")

tm_shape(adm03 |> filter(id_adm2 == "NER008001")) +
  tm_polygons() +
tm_shape(b_sf |> filter(id_adm2 == "NER008001")) +
  tm_dots()

# missing gps
vis_miss(grappe_gps_ner2018)

grappe_gps_ner2018 %>% 
  filter(is.na(coordonnes_gps__Latitude))

grappe_gps_survey |> 
  filter(grappe %in% c(453, 325, 329, 455, 457, 459, 461, 463, 465, 467, 106))

c <- survey_ind %>% 
  filter(grappe %in% c(453, 325, 329, 455, 457, 459, 461, 463, 465, 467, 106)) %>% 
  select(hhid, id_adm1, id_adm2, milieu, grappe)
unique(c$id_adm2)

# NER003001 : Boboye (Dosso)
# NER006008 : Kollo (Tillabéri)
# NER008001 : Ville de Niamey (Niamey)

runif_gps <- function(df, adm02) {
  
  df |>
    filter(id_adm2 == adm02) |> 
    summarize(
      min_long = min(coordonnes_gps__Longitude),
      max_long = max(coordonnes_gps__Longitude),
      unif_long = runif(1, min_long, max_long),
      min_lat = min(coordonnes_gps__Latitude),
      max_lat = max(coordonnes_gps__Latitude),
      unif_lat = runif(1, min_lat, max_lat)
    ) |> 
    select(unif_long, unif_lat)
  
}

runif_gps(b, "NER003001")
