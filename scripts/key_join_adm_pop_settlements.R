load("data/adm03.RData")
load("data/adm3_pop.RData")
load("data/settlements.RData")

# create new key variable to join the datasets at the admin3 level based on postcodes
# Administrative level 3 boundaries
adm03 <- adm03 %>% 
  mutate(id_adm1 = rowcacode1,
         id_adm2 = rowcacode2,
         id_adm3 = rowcacode3
         )
# Population 
adm3_pop <- adm3_pop %>% 
  mutate(id_adm1 = adm1_pcode,
         id_adm2 = adm2_pcode,
         id_adm3 = adm3_pcode
  )
# Settlements 
settlements <- settlements %>% 
  mutate(id_adm3 = admin3pcode)

setdiff(adm03$id_adm3, settlements$id_adm3)
setdiff(adm03$id_adm3, adm3_pop$id_adm3)
setdiff(adm3_pop$id_adm3, settlements$id_adm3)
# Two adm3 are missing in the settlements database: 
# "NER005007001" : Azarori, Niger
# "NER001003004" : Fachi, Niger

# Join administrative boundaries and population (polygons)
population_admin <- adm03 %>%
  left_join(adm3_pop, by = c("id_adm3", "id_adm2", "id_adm1"))

save(population_admin, file = "data/population_admin.RData")

sub_settlements <- dplyr::filter(settlements, grepl("NER00700", id_adm3)) %>% 
  select(geometry = geom) %>% 
  st_sf()
plot(sub_settlements, pch='.', col='green')

hex <- st_make_grid(sub_settlements, cellsize=.1, square=FALSE) %>%
  st_sf() %>%
  rowid_to_column('hex_id')
plot(hex)

set_in_hex <- st_join(sub_settlements, hex, join=st_within) %>%
  # Remove geometry.
  st_set_geometry(NULL) %>%
  # County the number of rows per hex_id and 
  # Store this number in a field named 'trees'.
  count(name='n_set', hex_id)
head(set_in_hex)

hex <- hex %>%
  left_join(set_in_hex, by = 'hex_id') %>%
  replace(is.na(.), 0)

plot(hex['n_set'])

hex_count = filter(hex, n_set > 0)

tmap_mode("view")

map_honeycomb = tm_shape(hex_count) +
  tm_fill(
    col = "n_set",
    palette = "Reds",
    style = "cont",
    title = "Number of settlements",
    id = "hex_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "Number of settlements: " = "n_set"
    ),
    popup.format = list(
      n_set = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_honeycomb
