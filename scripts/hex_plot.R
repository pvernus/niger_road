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