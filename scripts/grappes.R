survey_welfare %>% 
  select(grappe, id_adm2, milieu, hhid, hhweight) %>% 
  left_join(grappe_gps_ner2018, by = "grappe")

grappe_sf = st_as_sf(grappe_gps_ner2018, 
                     coords = c("coordonnes_gps__Latitude", "coordonnes_gps__Longitude"), 
                     crs = "WGS 84", 
                     agr = "constant")

vis_miss(grappe_gps_ner2018)

grappe_gps_ner2018 %>% 
  filter(is.na(coordonnes_gps__Latitude))

survey_ind %>% 
  filter(grappe %in% c(453, 325, 329, 455, 457, 459, 461, 463, 465, 467, 106)) %>% 
  select(hhid, id_adm1, id_adm2, milieu)

a <- survey_ind %>% 
  filter(grappe %in% c(453, 325, 329, 455, 457, 459, 461, 463, 465, 467, 106)) %>% 
  select(hhid, id_adm1, id_adm2, milieu)