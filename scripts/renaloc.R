source("scripts/library.R") # load packages
load("data/population_admin_id.RData")

renaloc <- read_csv("data_raw/ObservationData_umopjtb.csv") %>% 
  clean_names()
  
renaloc_id <- renaloc %>% 
  group_by(code_region, region, code_depart, departement, code_com, commune, milieu) %>%
  summarize(nb_localite = n()) %>% 
  ungroup() %>% 
  mutate(id_adm3 = paste0("NER00", code_region, 0, code_depart, 0, code_com),
         id_adm2 = paste0("NER00", code_region, 0, code_depart),
         id_adm1 = paste0("NER00", code_region)
         )

# There are several differences in postcode coding at the departement-level between datasets, see:
# miss_in_renaloc <- adm03_id %>% anti_join(renaloc_id, by = c("id_adm1", "id_adm2"))
# miss_in_admin <- renaloc_id %>% anti_join(adm03_id, by = c("id_adm1", "id_adm2"))

# To check the problematic departements: unique(miss_in_renaloc$adm_02) ; unique(miss_in_admin$departement)
# Results: "Ville de Niamey", "Ville de Tahoua", "Tessaoua", "Ville de Zinder", "Ville De Tahoua", "Ville De Maradi"

renaloc_id <- renaloc_id %>% 
  mutate(
    sub_adm3 = case_when(
      str_detect(id_adm2, "NER008090") ~ "NER008001", # ville de Niamey
      str_detect(id_adm2, "NER004008") ~ "NER004009", # Tessaoua
      str_detect(id_adm2, "NER007090") ~ "NER007011", # ville de Zinder
      str_detect(id_adm2, "NER005090") ~ "NER005013", # ville de Tahoua
      str_detect(id_adm2, "NER004007") ~ "NER004008", # Mayahi
      str_detect(id_adm2, "NER004090") ~ "NER004007", # ville de Maradi
      .default = as.character(id_adm2)
    ),
    id_adm2 = case_when(
      id_adm2 == "NER008090" ~ "NER008001", # ville de Niamey
      id_adm2 == "NER004008" ~ "NER004009", # Tessaoua
      id_adm2 == "NER007090" ~ "NER007011", # ville de Zinder
      id_adm2 == "NER005090" ~ "NER005013", # ville de Tahoua
      id_adm2 == "NER004007" ~ "NER004008", # Mayahi
      id_adm2 == "NER004090" ~ "NER004007", # ville de Maradi
      .default = as.character(id_adm2)
    ),
    id_adm3 = paste0(sub_adm3, 0, code_com)
  ) %>% 
  select(-sub_adm3)

# Check
# adm03_id %>% anti_join(renaloc_id, by = c("id_adm1", "id_adm2", "id_adm3"))
# renaloc_test %>% anti_join(renaloc_id, by = c("id_adm1", "id_adm2", "id_adm3"))

# save data
save(renaloc, renaloc_id, file = "data/renaloc_id.RData")

tm_shape(key_id) +
  tm_polygons("area")

map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  addPolygons(
    data = key_id[key_id$area == "1",],
    label = ~nom_com,
    fillColor  = ~"red",
    fillOpacity = .5,
    group = "urban") %>% 
  addPolygons(
    data = key_id[key_id$area == "2",],
    label = ~nom_com,
    fillColor  = ~"green",
    fillOpacity = .5,
    group = "rural") %>% 
  addLayersControl(baseGroups = c("background 1", "background 2"),
                 overlayGroups = c("urban", "rural"),
                 position = "bottomright",
                 options = layersControlOptions(collapsed = FALSE)
                 )  
  