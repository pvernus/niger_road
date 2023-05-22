load("data/grid3_settlement.RData")
load("data/ner_adm.RData")

## sf_use_s2(FALSE)

# rename Shape to geometry
st_geometry(grid3_settlement) <- "geometry"
# change GEOMETRY to MULTIPOLYGONS
grid3 <- st_cast(grid3_settlement, "MULTIPOLYGON")
# Make geometry valid, geos_method is used to overcome Ring Self-intersection issue
grid3 <- st_make_valid(grid3, geos_method = "valid_structure")

x <- st_is_valid(grid3, reason = TRUE)
x[x != "Valid Geometry"]

# Map
tmap_mode("plot")

tm_shape(grid3) +
  tm_polygons(col = 'blue')

# Join with ADM3 boundaries
pop_settlements <- adm03 %>% select(!c(rowcacode1, rowcacode2, rowcacode3, shape_leng, shape_area)) %>% 
  st_join(grid3)

# Map
pop_settlements %>% 
  group_by(id_adm3) %>% 
  summarize(n = n(),
            sum = sum(pop_un_adj, na.rm = TRUE)) %>% 
  tm_shape() +
  tm_polygons(col = "sum", alpha = .3)

save(grid3, pop_settlements, file = "data/pop_settlements.RData")



