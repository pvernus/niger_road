source("scripts/library.R") # load packages

settlements <- st_read("data_raw/ner_localites-21-10/Localités 21-10/Localites.gpkg", layer = "REACH_Localites")

settlements <- settlements %>% 
  clean_names() %>% 
  filter(type %in% c("VA", "H", "QT")) %>% # we only keep villages (VA), hamlets (H), and urban neighborhoods (QT)
  mutate(type = case_when(
    type == "VA" ~ "village",
    type == "H" ~ "hamlet",
    type == "QT" ~ "urban_neighborhood"
  )
           )
# save clean data
save(settlements, file = "data/settlements.RData")

tm_shape(adm03) +
  tm_polygons() +
tm_shape(settlements) +
  tm_symbols(col = "type", scale = .2)
