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

# save data
save(grappe_sf, grappe_buffer, 
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
