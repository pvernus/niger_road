source("scripts/library.R") # load packages

ner_border <- st_read("data_raw/ner_adm00_feb2018/NER_adm00_feb2018.shp") %>% 
  st_as_sf()
st_crs(ner_border) = 4326

# Generate a dggs specifying an intercell spacing of ~25 miles
dggs <- dgconstruct(spacing=10, metric=FALSE, resround='nearest')
#Get a grid covering Niger
ner_dggrid <- dgshptogrid(dggs, "data_raw/ner_adm00_feb2018/NER_adm00_feb2018.shp")

#Plot Niger's borders and the associated grid
p <- ggplot() +
  geom_sf(data=ner_border, fill=NA, color="black")   +
  geom_sf(data=ner_grid, fill=alpha("blue", 0.4), color=alpha("white", 0.4))
p

sf_use_s2(FALSE)
ner_grid <- st_intersection(ner_border, ner_dggrid) %>% # crop to Niger's borders
  st_cast("MULTIPOLYGON") %>% # convert to MULTIPOLYGON
  clean_names() %>% 
  select(-c(objectid, iso2, iso3, nompay, adm_00))
# add grid ID
ner_grid <- ner_grid %>% mutate(grid_id = 1:length(lengths(ner_grid$geometry)))

# save data
save(ner_border, ner_dggrid, ner_grid, file = "data/ner_grid.RData")


