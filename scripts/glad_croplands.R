load(here('data', 'ner_adm.RData'))
load(here('data', 'grappe_gps.RData'))

## LEON DATASETS
# Import data sets
croplands_2019 <- terra::rast(here('data_raw', 'leon_data', 'rasters', 'croplands_2019.tif'))
croplands_2019_netgain <- terra::rast(here('data_raw', 'leon_data', 'rasters', 'croplands_2019_netgain.tif'))
croplands_2019_netloss <- terra::rast(here('data_raw', 'leon_data', 'rasters', 'croplands_2019_netloss.tif'))

# terra::crs(croplands_2019)

# extract raster value for each grappe buffer and estimate the weighted average (value * coverage_fraction)
coverage_weighted_by_buffer <- function(x) {
  
  x_by_grappe <- exactextractr::exact_extract(x, grappe_buffer)
  x_by_grappe <- x_by_grappe |> 
    rbindlist(idcol = "id") |> 
    mutate(id = as.numeric(id)) |> 
    group_by(id) |> 
    summarize(wght_mean = sum(value * coverage_fraction) / sum(coverage_fraction))

}

croplands_2019_by_grappe <- coverage_weighted_by_buffer(croplands_2019)
croplands_2019_netgain_by_grappe <- coverage_weighted_by_buffer(croplands_2019_netgain)
croplands_2019_netloss_by_grappe <- coverage_weighted_by_buffer(croplands_2019_netloss)

# merge 
croplands <- grappe_sf |> 
  rowid_to_column(var = "id") |> 
  left_join(croplands_2019_by_grappe, by = "id") |>
  rename(croplands_2019 = wght_mean) |>
  left_join(croplands_2019_netgain_by_grappe, by = "id") |>
  rename(croplands_2019_netgain = wght_mean) |>
  left_join(croplands_2019_netloss_by_grappe, by = "id") |>
  rename(croplands_2019_netloss = wght_mean) %>% 
  replace_na(list(croplands_2019 = 0, croplands_2019_netgain = 0, croplands_2019_netloss = 0))


## SPAM DATASET
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

# tm_shape(crop_production_sf) +
#  tm_symbols(col = "crop_production_pmil", size = .1, border.alpha = 0) +
#  tm_shape(grappe_buffer) +
#  tm_polygons(alpha = 0)

crop_production_by_grappe <- st_intersection(crop_production_sf, grappe_buffer) |> 
  mutate(int_name = paste0(rowid, "-", grappe))

# tm_shape(crop_production_by_grappe) +
#  tm_symbols(col = "crop_production_pmil", size = .1, border.alpha = 0) +
#  tm_shape(grappe_buffer) +
#  tm_polygons(alpha = 0)

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
save(croplands, spam_2017_ner, crop_production_sf, crop_production_by_grappe, crop_production, 
     file = here('data', 'cropland_production.RData'))

################################################################################

## Other data sources (r package geodata)
## Croplands

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

# croplands <- grappe_sf |> 
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


