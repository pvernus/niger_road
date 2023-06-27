load(here('data', 'ner_adm.RData'))
load(here('data', 'grappe_gps.RData'))

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
  rename(croplands_2019_netloss = wght_mean)

save(croplands, file = here('data', 'croplands.RData'))

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


