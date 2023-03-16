source("scripts/library.R") # load packages

# import country boundaries data
ner_border <- st_read("data_raw/ner_adm00_feb2018/NER_adm00_feb2018.shp") %>% st_as_sf()
st_crs(ner_border) = 4326

# Generate a dggs specifying an intercell spacing of ~25 miles
dggs <- dgconstruct(spacing=10, metric=FALSE, resround='nearest')
#Get a grid covering Niger
ner_dggrid <- dgshptogrid(dggs, "data_raw/ner_adm00_feb2018/NER_adm00_feb2018.shp")

#Plot Niger borders and the grid
ggplot() +
  geom_sf(data=ner_border, fill=NA, color="black")   +
  geom_sf(data=ner_dggrid, fill=alpha("blue", 0.4), color=alpha("white", 0.4))

# crop the grid to Niger's boundaries
sf_use_s2(FALSE)
ner_grid <- st_intersection(ner_border, ner_dggrid) %>% # crop to Niger's borders
  st_cast("MULTIPOLYGON") %>% # convert to MULTIPOLYGON
  clean_names() %>% 
  select(-c(objectid, iso2, iso3, nompay, adm_00))
# add grid ID
ner_grid <- ner_grid %>% mutate(grid_id = 1:length(lengths(ner_grid$geometry)))

# plot cropped grid
ggplot() +
  geom_sf(data=ner_border, fill=NA, color="black")   +
  geom_sf(data=ner_grid, fill=alpha("blue", 0.4), color=alpha("white", 0.4))


map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  addPolygons(data = ner_grid,
              color=alpha("white", 0.4),
              fill = TRUE,
              fillColor = "blue",
              fillOpacity = 0.2,
              label = ~grid_id
  )

addCircles(data = settlements[settlements$type == "village",], 
           lng = ~longitude_x, lat = ~latitude_y, 
           label = ~localite_ref_name,
           color = ~"red",
           radius = 200,
           stroke = FALSE, 
           fillOpacity = .5,
           group = "village")

# save data
save(ner_border, ner_dggrid, ner_grid, file = "data/ner_grid.RData")


